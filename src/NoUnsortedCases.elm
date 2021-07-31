module NoUnsortedCases exposing
    ( rule
    , RuleConfig, defaults, doNotSortLiterals, doNotSortTypesFromDependencies, sortTypesFromDependenciesAlphabetically, sortListPatternsByLength, doNotLookPastUnsortable
    )

{-| Reports case patterns that are not in the "proper" order.

🔧 Running with `--fix` will automatically sort the patterns.

The proper order of custom types is the order in which they are defined in your
source files, and the order of other patterns may be specified in the rule
configuration. See the "Configuration" section below for more information.

    config =
        [ NoUnsortedCases.rule NoUnsortedCases.defaults
        ]


## Fail

    type Custom
        = Foo
        | Bar
        | Baz

    func1 c =
        case c of
            Bar ->
                "bar"

            Foo ->
                "foo"

            Baz ->
                "baz"

    func2 cs =
        case cs of
            [ Bar ] ->
                "bar"

            [ Foo ] ->
                "foo"

            [ Foo, Foo ] ->
                "foofoo"

            [ Baz ] ->
                "baz"

            _ ->
                "other"

    func3 c =
        case c of
            Nothing ->
                ""

            Just Bar ->
                "bar"

            Just Foo ->
                "foo"

            Just Baz ->
                "baz"

    func4 c1 c2 =
        case ( c1, c2 ) of
            ( Foo, Baz ) ->
                "foo baz"

            ( Foo, Bar ) ->
                "foo bar"

            ( Bar, Foo ) ->
                "bar foo"

            ( Baz, Foo ) ->
                "baz foo"

            _ ->
                "other"


## Success

    type Custom
        = Foo
        | Bar
        | Baz

    func1 c =
        case c of
            Foo ->
                "foo"

            Bar ->
                "bar"

            Baz ->
                "baz"

    func2 cs =
        case cs of
            [ Foo ] ->
                "foo"

            [ Foo, Foo ] ->
                "foofoo"

            [ Bar ] ->
                "bar"

            [ Baz ] ->
                "baz"

            _ ->
                "other"

    func3 c =
        case c of
            Just Foo ->
                "foo"

            Just Bar ->
                "bar"

            Just Baz ->
                "baz"

            Nothing ->
                ""

    func4 c1 c2 =
        case ( c1, c2 ) of
            ( Foo, Bar ) ->
                "foo bar"

            ( Foo, Baz ) ->
                "foo baz"

            ( Bar, Foo ) ->
                "bar foo"

            ( Baz, Foo ) ->
                "baz foo"

            _ ->
                "other"


## When (not) to enable this rule

This rule is useful when you want to ensure that you pattern match in a
consistent, predictable order, that is consistent with the order in which a type
was defined, as well as ensuring (optionally) that literal patterns and the like
are sorted.

This rule is not useful when you want to be able to write case patterns in
different orders throughout your codebase, e.g. if you want to emphasize what
pattern is most important at any given point or glean a tiny bit of performance
out of matching the more commonly-expected patterns first.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-no-unsorted/example --rules NoUnsortedCases
```


## Rule

@docs rule


## Configuration

@docs RuleConfig, defaults, doNotSortLiterals, doNotSortTypesFromDependencies, sortTypesFromDependenciesAlphabetically, sortListPatternsByLength, doNotLookPastUnsortable

-}

import Dict exposing (Dict)
import Dict.Extra as DictX
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type exposing (Type)
import List.Extra as ListX
import Maybe.Extra as MaybeX
import Review.Fix exposing (Fix)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable, moduleNameFor)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import Util exposing (createFix, fallbackCompareFor)


{-| Reports case patterns that are not in the "proper" order.
-}
rule : RuleConfig -> Rule
rule config =
    Rule.newProjectRuleSchema "NoUnsortedCases" initialProjectContext
        |> Rule.withDependenciesProjectVisitor (dependencyVisitor config)
        |> Rule.withModuleVisitor (moduleVisitor config)
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema


{-| Configuration for this rule. Create a new one with `defaults` and use
`doNotSortLiterals`, `sortListPatternsByLength`, etc. to alter it.
-}
type RuleConfig
    = RuleConfig
        { lookPastUnsortable : Bool
        , sortLists : SortLists
        , sortLiterals : Bool
        , sortTypesFromDependencies : SortTypesFromDependencies
        }


{-| List patterns may be sorted in one of two ways:

  - `Elementwise` -- Patterns are sorted by comparing elements sequentially at each position (from left to right). This is the same behavior as
    `List.sort`.
  - `LengthFirst` -- Shorter patterns always come before longer pattern, with patterns of the same length sorted elementwise at each position.

-}
type SortLists
    = Elementwise
    | LengthFirst


{-| Specify how to sort types that are **imported from dependencies**.

  - `DeclarationOrder` -- Sort types in the order they appear in the
    dependency's source file (or more technically in its documentation); this is
    identical to the behavior of types defined within your own modules.
  - `AlphabeticalOrder` -- Sort types alphabetically.
  - `DoNotSort` -- Do not sort types from dependencies at all. Note that this
    will render unsortable any patterns requiring types from dependencies to be
    sorted.

-}
type SortTypesFromDependencies
    = DeclarationOrder
    | AlphabeticalOrder
    | DoNotSort


{-| The default configuration, with the following behavior:

  - Literal patterns (`String`, `Int`, etc.) are sorted in the natural order for their type.

  - Types imported from dependencies are sorted in declaration order, i.e. in the order they appear in the dependency's source file (or more technically in its documentation); this is identical to the behavior of types defined within your own modules.

  - Lists are sorted elementwise, by comparing the elements sequentially at each
    position (from left to right).

  - Unsortable patterns can be looked beyond to resolve ties, for example:

```
func custom =
    case custom of
        Container { field } Bar ->
            not field

        Container { field } Baz ->
            field

        Container { field } Foo ->
            field
```

will be sorted to

    func custom =
        case custom of
            Container { field } Foo ->
                field

            Container { field } Bar ->
                not field

            Container { field } Baz ->
                field

Use `doNotSortLiterals`, `sortListPatternsByLength`, etc. to alter any of this
behavior, e.g.

    config =
        [ NoUnsortedCases.defaults
            |> NoUnsortedCases.doNotSortLiterals
            |> NoUnsortedCases.sortListPatternsByLength
            |> NoUnsortedCases.rule
        ]

-}
defaults : RuleConfig
defaults =
    RuleConfig
        { lookPastUnsortable = True
        , sortLists = Elementwise
        , sortLiterals = True
        , sortTypesFromDependencies = DeclarationOrder
        }


{-| Change the behavior of the rule to **not** sort literal patterns. If
literals are not sorted, case expressions that would require sorting literals
cannot be sorted and will thus be ignored by the rule.
-}
doNotSortLiterals : RuleConfig -> RuleConfig
doNotSortLiterals (RuleConfig c) =
    RuleConfig { c | sortLiterals = False }


{-| List patterns may be sorted in one of two ways:

  - Elementwise (**default**) -- Patterns are sorted by comparing elements
    sequentially at each position (from left to right). This is the same
    behavior as `List.sort` (which is why it is the default).
  - Length First -- Shorter patterns always come before longer pattern, with patterns of the same length sorted elementwise at each position.

Note that uncons patterns are considered the length of their matching list, with
wildcard patterns considered to have infinite length for the purposes of
sorting. This is necessary to ensure that earlier patterns are not erroneously
matched by wildcards.

**Elementwise**

    case list of
        [] ->
            ""

        [ 1 ] ->
            "1"

        [ 1, 1 ] ->
            "11"

        [ 1, 1, 1 ] ->
            "111"

        [ 1, 2 ] ->
            "12"

        [ 1, 3 ] ->
            "13"

        [ 2 ] ->
            "2"

        [ 2, 1 ] ->
            "21"

        [ 2, 2 ] ->
            "22"

        [ 2, 3 ] ->
            "23"

        [ 3 ] ->
            "3"

        _ ->
            "Too many..."

**Length First**

    case list of
        [] ->
            ""

        [ 1 ] ->
            "1"

        [ 2 ] ->
            "2"

        [ 3 ] ->
            "3"

        [ 1, 1 ] ->
            "11"

        [ 1, 2 ] ->
            "12"

        [ 1, 3 ] ->
            "13"

        [ 2, 1 ] ->
            "21"

        [ 2, 2 ] ->
            "22"

        [ 2, 3 ] ->
            "23"

        [ 1, 1, 1 ] ->
            "111"

        _ ->
            "Too many..."

-}
sortListPatternsByLength : RuleConfig -> RuleConfig
sortListPatternsByLength (RuleConfig c) =
    RuleConfig { c | sortLists = LengthFirst }


{-| Sort custom types imported from dependencies (including `Basics` types like `Maybe` and `Bool`) alphabetically, rather than by their source order in the dependency's source code.
-}
sortTypesFromDependenciesAlphabetically : RuleConfig -> RuleConfig
sortTypesFromDependenciesAlphabetically (RuleConfig c) =
    RuleConfig { c | sortTypesFromDependencies = AlphabeticalOrder }


{-| Do not sort types from dependencies at all. Note that this will render
unsortable any patterns requiring types from dependencies to be sorted.
-}
doNotSortTypesFromDependencies : RuleConfig -> RuleConfig
doNotSortTypesFromDependencies (RuleConfig c) =
    RuleConfig { c | sortTypesFromDependencies = DoNotSort }


{-| Do not look beyond unsortable patterns, rendering the following unsortable:

    func custom =
        case custom of
            Container { field } Bar ->
                not field

            Container { field } Baz ->
                field

            Container { field } Foo ->
                field

-}
doNotLookPastUnsortable : RuleConfig -> RuleConfig
doNotLookPastUnsortable (RuleConfig c) =
    RuleConfig { c | lookPastUnsortable = False }



-- * Types


{-| The project context, consisting of a map from module names to a map of type
names to orders.
-}
type alias ProjectContext =
    { customTypes :
        Dict
            ModuleName
            (Dict
                String
                { constructors : Set String
                , declarationOrder : List String
                }
            )
    }


{-| The module context, consisting of a map from module names to a map of type
names to orders.
-}
type alias ModuleContext =
    { customTypes :
        Dict
            ModuleName
            (Dict
                String
                { constructors : Set String
                , declarationOrder : List String
                }
            )
    , lookupTable : ModuleNameLookupTable
    , extractSourceCode : Range -> String
    }


{-| Any pattern that might be sortable.

  - `Constructor` -- A constructor pattern, with its type, declaration order, and any subpatterns, e.g. `Just 1 ->` becomes

```
Constructor
    { order = 0
    , subpatterns = [ Just (Literal (IntLiteral 1)) ]
    , type_ = ( [ "Basics" ], "Maybe" )
    }
```

  - `ListTupleOrUncons` -- A list, tuple, or uncons pattern, e.g. `(Nothing, Nothing) ->` becomes \`\`

```
ListTupleOrUncons
    [ Constructor
        { order = 1
        , subpatterns = []
        , type_ = ( [ "Basics" ], "Maybe" )
        }
    , Constructor
        { order = 1
        , subpatterns = []
        , type_ = ( [ "Basics" ], "Maybe" )
        }
    ]
```

  - `Literal` -- A literal pattern, e.g. `1 ->` becomes

```
Literal (IntLiteral 1)
```

  - `Wildcard` -- A wildcard or var pattern, e.g. `var ->` becomes

```
Wildcard
```

-}
type SortablePattern
    = Constructor
        { order : Int
        , subpatterns : List (Maybe SortablePattern)
        , type_ : ( ModuleName, String )
        }
    | ListTupleOrUncons (List SortablePattern)
    | Literal LiteralPattern
    | Wildcard


{-| A literal pattern. Int and Hex literals are not distinguished, as they are
sorted identically.
-}
type LiteralPattern
    = CharLiteral Char
    | StringLiteral String
    | IntLiteral Int
    | FloatLiteral Float



-- * MODULE VISITOR


{-| Visit each module, first getting types from all declarations and then
checking all expressions for `case`s.
-}
moduleVisitor : RuleConfig -> Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor config schema =
    schema
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withExpressionEnterVisitor (expressionVisitor config)


{-| The initial project context knows of no types.
-}
initialProjectContext : ProjectContext
initialProjectContext =
    { customTypes = Dict.empty
    }


{-| Create a `ProjectContext` from a `ModuleContext`.
-}
fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\metadata moduleContext ->
            { customTypes =
                moduleContext.customTypes
                    |> Dict.get []
                    |> Maybe.withDefault Dict.empty
                    |> Dict.singleton (Rule.moduleNameFromMetadata metadata)
            }
        )
        |> Rule.withMetadata


{-| Create a `ModuleContext` from a `ProjectContext`.
-}
fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable sourceCodeExtractor projectContext ->
            { customTypes = projectContext.customTypes
            , lookupTable = lookupTable
            , extractSourceCode = sourceCodeExtractor
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withSourceCodeExtractor


{-| Combine `ProjectContext`s by taking the union of known type orders.
-}
foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext prevContext =
    { customTypes =
        Dict.union newContext.customTypes prevContext.customTypes
    }



-- * DEPENDENCY VISITOR


{-| Visit all dependencies and store type order from them.
-}
dependencyVisitor : RuleConfig -> Dict String Dependency -> ProjectContext -> ( List (Error { useErrorForModule : () }), ProjectContext )
dependencyVisitor (RuleConfig config) deps context =
    let
        docToEntry : Elm.Docs.Union -> ( String, { constructors : Set String, declarationOrder : List String } )
        docToEntry { name, tags } =
            let
                constructors : List String
                constructors =
                    List.map Tuple.first tags
            in
            ( name
            , { constructors = Set.fromList constructors
              , declarationOrder =
                    case config.sortTypesFromDependencies of
                        AlphabeticalOrder ->
                            ListX.stableSortWith compare constructors

                        _ ->
                            constructors
              }
            )
    in
    if config.sortTypesFromDependencies /= DoNotSort then
        Dict.foldl
            (\_ dep acc ->
                Dependency.modules dep
                    |> List.map
                        (\{ name, unions } ->
                            ( -- Convert to a `ModuleName`
                              String.split "." name
                            , List.map docToEntry unions
                                |> Dict.fromList
                            )
                        )
                    |> Dict.fromList
                    |> (\types -> { acc | customTypes = Dict.union types acc.customTypes })
            )
            context
            deps
            |> Tuple.pair []

    else
        ( [], context )



-- * DECLARATION LIST VISITOR


{-| Visit declarations, storing custom type orders.
-}
declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor declarations context =
    let
        getCustomType : Node Declaration -> Maybe Type
        getCustomType node =
            case Node.value node of
                Declaration.CustomTypeDeclaration type_ ->
                    Just type_

                _ ->
                    Nothing

        typeConstructors : Type -> { constructors : Set String, declarationOrder : List String }
        typeConstructors type_ =
            type_.constructors
                |> List.map (Node.value >> .name >> Node.value)
                |> (\cs ->
                        { constructors = Set.fromList cs
                        , declarationOrder = cs
                        }
                   )
    in
    -- Find custom types that were defined in the module, and store them in the context.
    { context
        | customTypes =
            Dict.insert []
                (List.filterMap getCustomType declarations
                    |> List.map
                        (\type_ ->
                            ( Node.value type_.name
                            , typeConstructors type_
                            )
                        )
                    |> Dict.fromList
                )
                context.customTypes
    }
        |> Tuple.pair []



-- * EXPRESSION VISITOR


{-| Visit all expressions in a module, checking for `case`s and ensuring those
are sorted properly.
-}
expressionVisitor : RuleConfig -> Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionVisitor config node context =
    case Node.value node of
        Expression.CaseExpression { cases } ->
            -- Convert all patterns to sortable ones, if we can
            MaybeX.traverse (getSortablePattern config context << Tuple.first) cases
                -- Pair with index for stable sort
                |> Maybe.map (List.indexedMap Tuple.pair)
                |> Maybe.andThen
                    (\indexed ->
                        -- Zip with cases and sort stably
                        ListX.zip indexed cases
                            |> List.sortWith
                                (\( ( i1, p1 ), _ ) ( ( i2, p2 ), _ ) ->
                                    (\() -> compare i1 i2)
                                        |> fallbackCompareFor (comparePatterns config p1 p2)
                                )
                            -- Check if sorted
                            |> (\sorted ->
                                    if List.map Tuple.first sorted /= indexed then
                                        let
                                            range : Range
                                            range =
                                                Node.range node
                                        in
                                        -- Generate a fix if unsorted
                                        Just
                                            ( createFix context.extractSourceCode sorted
                                                |> unsortedError range
                                                |> List.singleton
                                            , context
                                            )

                                    else
                                        Nothing
                               )
                    )
                |> Maybe.withDefault ( [], context )

        _ ->
            -- Nothing to sort in non-case expressions.
            ( [], context )


{-| Given config, context, and a pattern, convert it into a pattern we know
how to sort, if possible.
-}
getSortablePattern : RuleConfig -> ModuleContext -> Node Pattern -> Maybe SortablePattern
getSortablePattern ((RuleConfig config) as ruleConfig) context node =
    let
        go : Node Pattern -> Maybe SortablePattern
        go =
            getSortablePattern ruleConfig context

        n : Node Pattern
        n =
            getActualPattern node

        makeLiteral : (a -> LiteralPattern) -> a -> Maybe SortablePattern
        makeLiteral l a =
            if config.sortLiterals then
                Just <| Literal <| l a

            else
                Nothing

        findConstructorOrder : String -> List (Node Pattern) -> ModuleName -> Maybe SortablePattern
        findConstructorOrder constructor ps moduleName =
            -- Get types for the module
            Dict.get moduleName context.customTypes
                -- Find the type that the constructor belongs to
                |> Maybe.andThen
                    (DictX.find
                        (\_ { constructors } ->
                            Set.member constructor constructors
                        )
                    )
                -- Find its constructor order
                |> Maybe.andThen
                    (\( matchedType, { declarationOrder } ) ->
                        ListX.elemIndex constructor declarationOrder
                            |> Maybe.map
                                (\order ->
                                    Constructor
                                        { type_ = ( moduleName, matchedType )
                                        , order = order
                                        , subpatterns = List.map go ps
                                        }
                                )
                    )
    in
    case Node.value n of
        -- Find declaration sorting for named patterns and their arguments
        Pattern.NamedPattern { name } ps ->
            moduleNameFor context.lookupTable n
                |> Maybe.andThen (findConstructorOrder name ps)

        -- Tuples and lists we recursively convert each subpattern
        Pattern.TuplePattern ps ->
            MaybeX.traverse go ps
                |> Maybe.map ListTupleOrUncons

        Pattern.ListPattern ps ->
            MaybeX.traverse go ps
                |> Maybe.map ListTupleOrUncons

        -- Uncons pattern we recursively convert each subpattern and convert to the equivalent list
        Pattern.UnConsPattern p1 p2 ->
            let
                cons : SortablePattern -> SortablePattern -> Maybe SortablePattern
                cons x xs =
                    case xs of
                        Wildcard ->
                            Just <| ListTupleOrUncons [ x, xs ]

                        ListTupleOrUncons rest ->
                            case Node.value <| getActualPattern p2 of
                                Pattern.ListPattern _ ->
                                    Just <| ListTupleOrUncons <| x :: rest

                                Pattern.UnConsPattern _ _ ->
                                    Just <| ListTupleOrUncons <| x :: rest

                                _ ->
                                    -- You can't cons onto anything else, so this is a type error
                                    Nothing

                        _ ->
                            -- You can't cons onto a constructor or Literal, so this is a type error
                            Nothing
            in
            Maybe.map2 cons (go p1) (go p2)
                |> MaybeX.join

        -- Var and _ are wildcards
        Pattern.AllPattern ->
            Just Wildcard

        Pattern.VarPattern _ ->
            Just Wildcard

        -- Literals can be sorted if configured to
        Pattern.CharPattern c ->
            makeLiteral CharLiteral c

        Pattern.StringPattern s ->
            makeLiteral StringLiteral s

        Pattern.IntPattern i ->
            makeLiteral IntLiteral i

        Pattern.HexPattern i ->
            makeLiteral IntLiteral i

        Pattern.FloatPattern f ->
            makeLiteral FloatLiteral f

        _ ->
            -- Remaining patterns are Unit and Record, which are not sortable, and Parens/As patterns, which we have already unwrapped
            Nothing


{-| Unwrap a pattern to get at the actual pattern inside of any parentheses or
`as` patterns.
-}
getActualPattern : Node Pattern -> Node Pattern
getActualPattern node =
    case Node.value node of
        -- Parenthesized/as patterns we just descend into
        Pattern.ParenthesizedPattern p ->
            getActualPattern p

        Pattern.AsPattern p _ ->
            getActualPattern p

        -- Other pattern are just the pattern itself
        _ ->
            node


{-| Compare two literal types, determining their order (if not a type error).
-}
compareLiteral : LiteralPattern -> LiteralPattern -> Order
compareLiteral l1 l2 =
    case ( l1, l2 ) of
        ( CharLiteral c1, CharLiteral c2 ) ->
            compare c1 c2

        ( StringLiteral s1, StringLiteral s2 ) ->
            compare s1 s2

        ( IntLiteral i1, IntLiteral i2 ) ->
            compare i1 i2

        ( FloatLiteral f1, FloatLiteral f2 ) ->
            compare f1 f2

        _ ->
            -- This is a type error, so ignore it
            EQ


{-| Compare two sortable patterns, determining their order (if not a type error).
-}
comparePatterns : RuleConfig -> SortablePattern -> SortablePattern -> Order
comparePatterns ((RuleConfig config) as ruleConfig) pat1 pat2 =
    let
        go : SortablePattern -> SortablePattern -> () -> Order
        go p1 p2 =
            \() -> comparePatterns ruleConfig p1 p2
    in
    case ( pat1, pat2 ) of
        -- Wildcards cannot be moved relative to non-wildcards, so return EQ which ensures index is used.
        ( Wildcard, _ ) ->
            EQ

        ( _, Wildcard ) ->
            EQ

        -- Literals are simply compared; if sorting literals is turned off, then LiteralPatterns are not created at all
        ( Literal l1, Literal l2 ) ->
            compareLiteral l1 l2

        --Constructors are compared by index, then by comparing subpatterns sequentially, failing if a non-sortable subpattern is encountered
        ( Constructor c1, Constructor c2 ) ->
            let
                goSubs : List (Maybe SortablePattern) -> List (Maybe SortablePattern) -> () -> Order
                goSubs pat1s pat2s () =
                    case ( pat1s, pat2s ) of
                        ( (Just p1) :: p1s, (Just p2) :: p2s ) ->
                            goSubs p1s p2s
                                |> fallbackCompareFor (go p1 p2 ())

                        ( Nothing :: p1s, Nothing :: p2s ) ->
                            -- If at the point where arguments are both unsortable, then proceed past if configured to
                            if config.lookPastUnsortable then
                                goSubs p1s p2s ()

                            else
                                EQ

                        _ ->
                            -- Lists should be even, so other cases aren't sortable
                            EQ
            in
            -- Fallback to subpatterns
            goSubs c1.subpatterns c2.subpatterns
                |> fallbackCompareFor (compare c1.order c2.order)

        -- Lists, Tuples, and Uncons
        -- Empty lists go first
        ( ListTupleOrUncons [], ListTupleOrUncons [] ) ->
            EQ

        ( ListTupleOrUncons [], ListTupleOrUncons _ ) ->
            LT

        ( ListTupleOrUncons _, ListTupleOrUncons [] ) ->
            GT

        -- Infinite match wildcards go after longer patterns
        ( ListTupleOrUncons [ Wildcard ], ListTupleOrUncons [ Wildcard ] ) ->
            EQ

        ( ListTupleOrUncons [ Wildcard ], ListTupleOrUncons (_ :: _) ) ->
            GT

        ( ListTupleOrUncons (_ :: _), ListTupleOrUncons [ Wildcard ] ) ->
            LT

        -- Finally, we compare the patterns
        ( ListTupleOrUncons (p1 :: p1s), ListTupleOrUncons (p2 :: p2s) ) ->
            case config.sortLists of
                LengthFirst ->
                    (\() ->
                        go (ListTupleOrUncons p1s) (ListTupleOrUncons p2s)
                            |> fallbackCompareFor (go p1 p2 ())
                    )
                        |> fallbackCompareFor (comparePatternListLengths p1s p2s)

                Elementwise ->
                    go (ListTupleOrUncons p1s) (ListTupleOrUncons p2s)
                        |> fallbackCompareFor (go p1 p2 ())

        -- Anything else should be a type error, so we needn't consider it
        _ ->
            EQ


{-| Compare the list lengths of two lists of `SortablePattern`, with the caveat
that a list must be infinitely long if it ends in a wildcard, with a shorter
list ending in a wildcard being "longer" (more specified) than a longer one.
-}
comparePatternListLengths : List SortablePattern -> List SortablePattern -> Order
comparePatternListLengths p1s p2s =
    case ( ListX.last p1s == Just Wildcard, ListX.last p2s == Just Wildcard ) of
        ( True, True ) ->
            -- Flip comparison if both end in wildcards
            compare (List.length p2s) (List.length p1s)

        ( True, False ) ->
            GT

        ( False, True ) ->
            LT

        ( False, False ) ->
            -- Compare normally if neither does
            compare (List.length p1s) (List.length p2s)


{-| Given a range and a fix, create an unsorted case error.
-}
unsortedError : Range -> List Fix -> Error {}
unsortedError =
    Rule.errorWithFix
        { message = "Case patterns are not sorted."
        , details =
            [ "Case patterns were found out of order.  They should be sorted as specified in the rule configuration."
            ]
        }
