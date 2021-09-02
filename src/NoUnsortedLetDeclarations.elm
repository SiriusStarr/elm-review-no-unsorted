module NoUnsortedLetDeclarations exposing
    ( rule
    , RuleConfig, sortLetDeclarations
    , alphabetically, usedInExpressionFirst, usedInExpressionLast, usedInOtherDeclarationsLast, usedInOtherDeclarationsFirst, valuesBeforeFunctions, valuesAfterFunctions
    )

{-|


## Rule

@docs rule


## Configuration

@docs RuleConfig, sortLetDeclarations


## Orderings

@docs alphabetically, usedInExpressionFirst, usedInExpressionLast, usedInOtherDeclarationsLast, usedInOtherDeclarationsFirst, valuesBeforeFunctions, valuesAfterFunctions

-}

import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range as Range exposing (Range)
import List.Extra as ListX
import Review.Rule as Rule exposing (Error, Rule)
import Util exposing (allBindingsInPattern, checkSorting, countUsesIn)


{-| Reports `let` declarations that are not in the "proper" order.

🔧 Running with `--fix` will automatically sort the declarations.

The proper order of declarations is specified in the rule configuration. See the
[Configuration](#Configuration) section below for more information.

    config =
        [ NoUnsortedLetDeclarations.rule
            (NoUnsortedLetDeclarations.sortLetDeclarations
                |> NoUnsortedLetDeclarations.usedInExpressionFirst
                |> NoUnsortedLetDeclarations.alphabetically
            )
        ]


## Fail

    a =
        let
            -- These are used in the expression
            x =
                a

            y =
                b

            -- These are not
            b =
                j

            a =
                i
        in
        x + y

    b =
        let
            -- These are not used in the expression
            a =
                i

            b =
                j

            -- These are
            x =
                a

            y =
                b
        in
        x + y


## Success

    a =
        let
            -- These are used in the expression
            x =
                a

            y =
                b

            -- These are not
            a =
                i

            b =
                j
        in
        x + y


## When (not) to enable this rule

This rule is useful when you want to ensure that your `let` declarations are in
a consistent, predictable order.

This rule is not useful when you want to be able to write `let` declarations in
varying orders throughout your codebase, e.g. if you want to emphasize what
is most important on a case-by-case basis.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-no-unsorted/example --rules NoUnsortedLetDeclarations
```

-}
rule : RuleConfig r -> Rule
rule (RuleConfig r) =
    Rule.newModuleRuleSchemaUsingContextCreator "NoUnsortedLetDeclarations" initialContext
        -- Reverse sort order, as we've been cons-ing them on
        |> Rule.withExpressionEnterVisitor (expressionVisitor <| RuleConfig { r | sortBy = List.reverse r.sortBy })
        |> Rule.fromModuleRuleSchema


type alias Context =
    { extractSource : Range -> String
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\extractSource () -> { extractSource = extractSource })
        |> Rule.withSourceCodeExtractor


{-| Configuration for this rule. Create a new one with `sortLetDeclarations` and use
orderings to create a hierarchy of sorting.
-}
type RuleConfig r
    = RuleConfig
        { sortBy : List DeclCompare
        }


type alias DeclCompare =
    LetDec -> LetDec -> Order


type alias LetDec =
    { range : Range
    , index : Int
    , namesBound : List String
    , usedInExpression : Bool
    , usedInOtherDecs : Bool
    , args : List String
    }


{-| Create a new `RuleConfig`. Use the various orderings to then specify
primary and fallback orderings.
-}
sortLetDeclarations : RuleConfig { noAlphabetical : (), noArgCount : (), noUsedInOther : (), noUsedInExpression : () }
sortLetDeclarations =
    RuleConfig { sortBy = [] }


{-| Sort declarations alphabetically by the name of their binding. For
destructurings, this will be the name of the actual bindings that are made, in
order. For example, the following is sorted alphabetically:

    let
        (Opaque a) =
            i

        ( b, z ) =
            j

        { c, y } =
            k

        d =
            l
    in
    x

-}
alphabetically : RuleConfig { r | noAlphabetical : () } -> RuleConfig r
alphabetically (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 -> compare d1.namesBound d2.namesBound)
                    :: r.sortBy
        }


{-| Sort declarations with those used in the expression of the `let` block
coming first, then those that aren't. Ties will be broken by the next specified
ordering. For example, the following is sorted by this ordering and then
alphabetically:

    let
        -- These are used in the expression
        x =
            a

        y =
            b

        -- These are not
        a =
            i

        b =
            j
    in
    x + y

-}
usedInExpressionFirst : RuleConfig { r | noUsedInExpression : () } -> RuleConfig r
usedInExpressionFirst (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.usedInExpression, d2.usedInExpression ) of
                        ( True, False ) ->
                            LT

                        ( False, True ) ->
                            GT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort declarations with those used in the expression of the `let` block
coming last, with those that aren't coming first. Ties will be broken by the
next specified ordering. For example, the following is sorted by this ordering
and then alphabetically:

    let
        -- These are not used in the expression
        x =
            i

        y =
            j

        -- These are used in the expression
        a =
            x

        b =
            y
    in
    a + b

-}
usedInExpressionLast : RuleConfig { r | noUsedInExpression : () } -> RuleConfig r
usedInExpressionLast (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.usedInExpression, d2.usedInExpression ) of
                        ( False, True ) ->
                            LT

                        ( True, False ) ->
                            GT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort declarations with those used in other declarations coming after those
that are not. Ties will be broken by the next specified ordering. For example,
the following is sorted by this ordering and then alphabetically:

    let
        a =
            x

        b =
            y

        x =
            i

        y =
            j
    in
    0

-}
usedInOtherDeclarationsLast : RuleConfig { r | noUsedInOther : () } -> RuleConfig r
usedInOtherDeclarationsLast (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.usedInOtherDecs, d2.usedInOtherDecs ) of
                        ( False, True ) ->
                            LT

                        ( True, False ) ->
                            GT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort declarations with those used in other declarations coming before those
that are not. Ties will be broken by the next specified ordering. For example,
the following is sorted by this ordering and then alphabetically:

    let
        x =
            i

        y =
            j

        a =
            x

        b =
            y
    in
    0

-}
usedInOtherDeclarationsFirst : RuleConfig { r | noUsedInOther : () } -> RuleConfig r
usedInOtherDeclarationsFirst (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.usedInOtherDecs, d2.usedInOtherDecs ) of
                        ( True, False ) ->
                            LT

                        ( False, True ) ->
                            GT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort declarations that do not have arguments before those that do. Since no
type inference is performed, this does not guarantee that some things that are
functions will not be sorted with values. For example, the following is sorted
by this ordering and then alphabetically:

    let
        -- These do not have arguments
        x =
            a

        y =
            b

        -- These do
        a i =
            i

        b j =
            j
    in
    x + y

-}
valuesBeforeFunctions : RuleConfig { r | noArgCount : () } -> RuleConfig r
valuesBeforeFunctions (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( List.isEmpty d1.args, List.isEmpty d2.args ) of
                        ( True, False ) ->
                            LT

                        ( False, True ) ->
                            GT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort declarations that do not have arguments after those that do. Since no
type inference is performed, this does not guarantee that some things that are
functions will not be sorted with values. For example, the following is sorted
by this ordering and then alphabetically:

    let
        -- These have arguments
        a i =
            i

        b j =
            j

        -- These do not
        x =
            a

        y =
            b
    in
    x + y

-}
valuesAfterFunctions : RuleConfig { r | noArgCount : () } -> RuleConfig r
valuesAfterFunctions (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( List.isEmpty d1.args, List.isEmpty d2.args ) of
                        ( False, True ) ->
                            LT

                        ( True, False ) ->
                            GT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Visit expressions, checking `let` blocks for sorting.
-}
expressionVisitor : RuleConfig r -> Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor (RuleConfig { sortBy }) n context =
    case Node.value n of
        LetExpression lb ->
            let
                ( exprsToDecs, exprs ) =
                    ListX.indexedFoldl step ( [], [] ) lb.declarations

                step :
                    Int
                    -> Node LetDeclaration
                    -> ( List (List (Node Expression) -> LetDec), List (Node Expression) )
                    -> ( List (List (Node Expression) -> LetDec), List (Node Expression) )
                step index d ( dAcc, eAcc ) =
                    case Node.value d of
                        LetFunction { declaration } ->
                            let
                                { expression, arguments } =
                                    Node.value declaration

                                name : String
                                name =
                                    Node.value (Node.value declaration).name
                            in
                            ( (\es ->
                                { range = Node.range d
                                , index = index
                                , namesBound = [ name ]
                                , usedInExpression = countUsesIn lb.expression name >= 1
                                , usedInOtherDecs = List.any (\e -> countUsesIn e name >= 1) es
                                , args = List.concatMap allBindingsInPattern arguments
                                }
                              )
                                :: dAcc
                            , expression :: eAcc
                            )

                        LetDestructuring p expression ->
                            let
                                bs : List String
                                bs =
                                    allBindingsInPattern p
                            in
                            ( (\es ->
                                { range = Node.range d
                                , index = index
                                , namesBound = bs
                                , usedInExpression = List.any ((<) 0 << countUsesIn lb.expression) bs
                                , usedInOtherDecs = List.any (\e -> List.any ((<) 0 << countUsesIn e) bs) es
                                , args = []
                                }
                              )
                                :: dAcc
                            , expression :: eAcc
                            )
            in
            ( ListX.reverseMap ((|>) exprs) exprsToDecs
                |> checkSorting context.extractSource "Let declarations" sortBy (Range.combine <| List.map Node.range lb.declarations)
            , context
            )

        _ ->
            ( [], context )
