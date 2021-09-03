module NoUnsortedTopLevelDeclarations exposing
    ( rule
    , RuleConfig, sortTopLevelDeclarations
    , alphabetically, exposedOrderWithPrivateLast, exposedOrderWithPrivateFirst, typesFirst, typesLast, portsFirst, portsLast
    )

{-|


## Rule

@docs rule


## Configuration

@docs RuleConfig, sortTopLevelDeclarations


## Orderings

@docs alphabetically, exposedOrderWithPrivateLast, exposedOrderWithPrivateFirst, typesFirst, typesLast, portsFirst, portsLast

-}

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose(..))
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range as Range exposing (Range)
import List.Extra as ListX
import Review.Rule as Rule exposing (Error, Rule)
import String.Extra as StringX
import Util exposing (checkSorting)


{-| Reports top-level declarations that are not in the "proper" order.

🔧 Running with `--fix` will automatically sort the declarations.

The proper order of declarations is specified in the rule configuration. See the
[Configuration](#Configuration) section below for more information.

    config =
        [ NoUnsortedTopLevelDeclarations.rule
            (NoUnsortedTopLevelDeclarations.sortTopLevelDeclarations
                |> NoUnsortedTopLevelDeclarations.portsFirst
                |> NoUnsortedTopLevelDeclarations.exposedOrderWithPrivateLast
                |> NoUnsortedTopLevelDeclarations.alphabetically
            )
        ]


## Fail

    module A exposing
        ( A, a
        , Z
        )

    {-|

    @docs A, a
    @docs Z

    -}

    type A
        = A

    z =
        zed

    type alias Z =
        A

    a =
        foo

    b =
        bar


## Success

    module A exposing
        ( A, a
        , Z
        )

    {-|

    @docs A, a
    @docs Z

    -}

    type A
        = A

    a =
        foo

    type alias Z =
        A

    b =
        bar

    z =
        zed


## When (not) to enable this rule

This rule is useful when you want to ensure that your top-level declarations are
in a consistent, predictable order.

This rule is not useful when you want to be able to write top-level declarations
in varying orders throughout your codebase, e.g. if you want to emphasize what
is most important on a case-by-case basis.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-no-unsorted/example --rules NoUnsortedTopLevelDeclarations
```

-}
rule : RuleConfig r -> Rule
rule (RuleConfig r) =
    Rule.newModuleRuleSchemaUsingContextCreator "NoUnsortedTopLevelDeclarations" initialContext
        |> Rule.withModuleDefinitionVisitor (\m c -> ( [], getModuleExports m c ))
        |> Rule.withDeclarationListVisitor (declarationVisitor <| RuleConfig { r | sortBy = List.reverse r.sortBy })
        |> Rule.fromModuleRuleSchema


{-| Configuration for this rule. Create a new one with
`sortTopLevelDeclarations` and use orderings to create a hierarchy of sorting.
-}
type RuleConfig r
    = RuleConfig
        { sortBy : List (TLD -> TLD -> Order)
        }


{-| Info about the module, as well as the source extractor.
-}
type alias Context =
    { extractSource : Range -> String
    , exports : Maybe (List String)
    , errorRange : Range
    }


{-| Information about a TLD.
-}
type alias TLD =
    { type_ : DeclarationType
    , name : String
    , exposedOrder : Maybe Int
    , range : Range
    }


{-| The ype of TLD it is.
-}
type DeclarationType
    = Function
    | Port
    | Type


{-| Create a new `RuleConfig`. Use the various orderings to then specify
primary and fallback orderings.
-}
sortTopLevelDeclarations : RuleConfig { noAlphabetical : (), noExposed : (), noType : (), noPort : () }
sortTopLevelDeclarations =
    RuleConfig { sortBy = [] }


{-| Sort declarations alphabetically. Note that this decapitalizes the first
letter before performing the comparison so as to treat types and functions the
same. For example, the following is sorted alphabetically:

    type A
        = A

    a =
        foo

    b =
        bar

    z =
        zed

    type alias Z =
        A

-}
alphabetically : RuleConfig { r | noAlphabetical : () } -> RuleConfig r
alphabetically (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 -> compare (StringX.decapitalize d1.name) (StringX.decapitalize d2.name))
                    :: r.sortBy
        }


{-| Sort TLDs in the order they are exposed by the module, with private TLDs
coming after all those that are exposed. For example, the following is sorted
by this and then alphabetically:

    module A exposing
        ( A, a
        , Z
        )

    {-|

    @docs A, a
    @docs Z

    -}

    type A
        = A

    a =
        foo

    type alias Z =
        A

    b =
        bar

    z =
        zed

-}
exposedOrderWithPrivateLast : RuleConfig { r | noExposed : () } -> RuleConfig r
exposedOrderWithPrivateLast (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.exposedOrder, d2.exposedOrder ) of
                        ( Just i1, Just i2 ) ->
                            compare i1 i2

                        ( Just _, Nothing ) ->
                            LT

                        ( Nothing, Just _ ) ->
                            GT

                        ( Nothing, Nothing ) ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort TLDs in the order they are exposed by the module, with private TLDs
coming before all those that are exposed. For example, the following is sorted
by this and then alphabetically:

    module A exposing
        ( A, a
        , Z
        )

    {-|

    @docs A, a
    @docs Z

    -}

    b =
        bar

    z =
        zed

    type A
        = A

    a =
        foo

    type alias Z =
        A

-}
exposedOrderWithPrivateFirst : RuleConfig { r | noExposed : () } -> RuleConfig r
exposedOrderWithPrivateFirst (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.exposedOrder, d2.exposedOrder ) of
                        ( Just i1, Just i2 ) ->
                            compare i1 i2

                        ( Just _, Nothing ) ->
                            GT

                        ( Nothing, Just _ ) ->
                            LT

                        ( Nothing, Nothing ) ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort TLDs so that types and type aliases always come before functions (and
ports, if they haven't been sorted already). For example, the following is
sorted by this order and then alphabetically:

    type A
        = A

    type alias Z =
        A

    a =
        foo

    b =
        bar

    z =
        zed

-}
typesFirst : RuleConfig { r | noType : () } -> RuleConfig r
typesFirst (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.type_, d2.type_ ) of
                        ( Type, Type ) ->
                            EQ

                        ( Type, _ ) ->
                            LT

                        ( _, Type ) ->
                            GT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort TLDs so that types and type aliases always come after functions (and
ports, if they haven't been sorted already). For example, the following is
sorted by this order and then alphabetically:

    a =
        foo

    b =
        bar

    z =
        zed

    type A
        = A

    type alias Z =
        A

-}
typesLast : RuleConfig { r | noType : () } -> RuleConfig r
typesLast (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.type_, d2.type_ ) of
                        ( Type, Type ) ->
                            EQ

                        ( Type, _ ) ->
                            GT

                        ( _, Type ) ->
                            LT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort TLDs so that ports always come before functions (and types, if they
haven't been sorted already). For example, the following is sorted by this order
and then alphabetically:

    port sendMessage : String -> Cmd msg

    type A
        = A

    a =
        foo

    b =
        bar

    type alias Z =
        A

    z =
        zed

-}
portsFirst : RuleConfig { r | noPort : () } -> RuleConfig r
portsFirst (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.type_, d2.type_ ) of
                        ( Port, Port ) ->
                            EQ

                        ( Port, _ ) ->
                            LT

                        ( _, Port ) ->
                            GT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort TLDs so that ports always come after functions (and types, if they
haven't been sorted already). For example, the following is sorted by this order
and then alphabetically:

    type A
        = A

    a =
        foo

    b =
        bar

    type alias Z =
        A

    z =
        zed

    port sendMessage : String -> Cmd msg

-}
portsLast : RuleConfig { r | noPort : () } -> RuleConfig r
portsLast (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.type_, d2.type_ ) of
                        ( Port, Port ) ->
                            EQ

                        ( Port, _ ) ->
                            GT

                        ( _, Port ) ->
                            LT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\extractSource () ->
            { extractSource = extractSource
            , exports = Nothing
            , errorRange = Range.emptyRange
            }
        )
        |> Rule.withSourceCodeExtractor


{-| Get an ordered list of all names exported by a module.
-}
getModuleExports : Node Module -> Context -> Context
getModuleExports m context =
    let
        r : Range
        r =
            Node.range m

        errorRange : Range
        errorRange =
            case Node.value m of
                Module.PortModule _ ->
                    -- Assume that the `port module` of a module is just the first 11 chars
                    { r | end = { row = r.start.row, column = r.start.column + 11 } }

                _ ->
                    -- Assume that the `module` of a module is just the first 6 chars
                    -- Effect modules aren't a thing
                    { r | end = { row = r.start.row, column = r.start.column + 6 } }
    in
    case Module.exposingList <| Node.value m of
        Exposing.All _ ->
            { context | errorRange = errorRange }

        Exposing.Explicit exports ->
            { context
                | exports =
                    Just <|
                        List.map
                            (\e ->
                                case Node.value e of
                                    InfixExpose s ->
                                        s

                                    FunctionExpose s ->
                                        s

                                    TypeOrAliasExpose s ->
                                        s

                                    TypeExpose { name } ->
                                        name
                            )
                            exports
                , errorRange = errorRange
            }


{-| Generate declaration info for all TLDs and then check that they are sorted.
-}
declarationVisitor : RuleConfig r -> List (Node Declaration) -> Context -> ( List (Error {}), Context )
declarationVisitor (RuleConfig { sortBy }) ds context =
    List.filterMap (getDecInfo context.exports) ds
        |> checkSorting context.extractSource "Top-level declarations" sortBy context.errorRange
        |> (\es -> ( es, context ))


{-| Given a list of module exports, generate TLD info from a `declaration`.
-}
getDecInfo : Maybe (List String) -> Node Declaration -> Maybe TLD
getDecInfo exports d =
    case Node.value d of
        FunctionDeclaration { declaration } ->
            Node.value declaration
                |> .name
                |> Node.value
                |> (\name ->
                        { type_ = Function
                        , exposedOrder = Maybe.andThen (ListX.elemIndex name) exports
                        , name = name
                        , range = Node.range d
                        }
                   )
                |> Just

        AliasDeclaration { name } ->
            Just
                { type_ = Type
                , exposedOrder = Maybe.andThen (ListX.elemIndex (Node.value name)) exports
                , name = Node.value name
                , range = Node.range d
                }

        CustomTypeDeclaration { name } ->
            Just
                { type_ = Type
                , exposedOrder = Maybe.andThen (ListX.elemIndex (Node.value name)) exports
                , name = Node.value name
                , range = Node.range d
                }

        PortDeclaration { name } ->
            Just
                { type_ = Port
                , name = Node.value name
                , range = Node.range d

                -- Ports can't be exposed
                , exposedOrder = Nothing
                }

        _ ->
            -- These are impossible
            -- Destructuring (Node Pattern) (Node Expression)
            -- InfixDeclaration Infix
            Nothing
