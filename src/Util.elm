module Util exposing (allBindingsInPattern, checkSorting, countUsesIn, fallbackCompareFor, makeAccessFunc, validate)

{-| Utility functions used by other modules but not specific to them.
-}

import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import List.Extra as ListX
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Error)


{-| Get all immediate child expressions of an expression.
-}
subexpressions : Node Expression -> List (Node Expression)
subexpressions expr =
    case Node.value expr of
        LetExpression letBlock ->
            let
                subExprs : Node LetDeclaration -> Node Expression
                subExprs n =
                    case Node.value n of
                        LetFunction { declaration } ->
                            Node.value declaration
                                |> .expression

                        LetDestructuring _ e ->
                            e
            in
            letBlock.expression
                :: List.map subExprs letBlock.declarations

        ListExpr es ->
            es

        TupledExpression es ->
            es

        RecordExpr setters ->
            List.map (Tuple.second << Node.value) setters

        RecordUpdateExpression record updaters ->
            Node.map (FunctionOrValue []) record
                :: List.map (Tuple.second << Node.value) updaters

        Application es ->
            es

        CaseExpression caseBlock ->
            caseBlock.expression
                :: List.map Tuple.second caseBlock.cases

        OperatorApplication _ _ e1 e2 ->
            [ e1, e2 ]

        IfBlock predExpr thenExpr elseExpr ->
            [ predExpr, thenExpr, elseExpr ]

        LambdaExpression { expression } ->
            [ expression ]

        RecordAccess record _ ->
            [ record ]

        ParenthesizedExpression e ->
            [ e ]

        Negation e ->
            [ e ]

        UnitExpr ->
            []

        Integer _ ->
            []

        Hex _ ->
            []

        Floatable _ ->
            []

        Literal _ ->
            []

        CharLiteral _ ->
            []

        GLSLExpression _ ->
            []

        RecordAccessFunction _ ->
            []

        FunctionOrValue _ _ ->
            []

        Operator _ ->
            []

        PrefixOperator _ ->
            []


{-| Recursively find all bindings in a pattern.
-}
allBindingsInPattern : Node Pattern -> List String
allBindingsInPattern pattern =
    let
        go : List (Node Pattern) -> List String
        go =
            List.concatMap allBindingsInPattern
    in
    case Node.value pattern of
        ListPattern ps ->
            go ps

        TuplePattern ps ->
            go ps

        RecordPattern ps ->
            List.map Node.value ps

        NamedPattern _ ps ->
            go ps

        UnConsPattern p ps ->
            go [ p, ps ]

        VarPattern name ->
            [ name ]

        AsPattern p name ->
            Node.value name :: go [ p ]

        ParenthesizedPattern p ->
            go [ p ]

        AllPattern ->
            []

        UnitPattern ->
            []

        CharPattern _ ->
            []

        StringPattern _ ->
            []

        IntPattern _ ->
            []

        HexPattern _ ->
            []

        FloatPattern _ ->
            []


{-| Count the uses of a given name in the scope of an expression.
-}
countUsesIn : Node Expression -> String -> Int
countUsesIn expr name =
    case Node.value expr of
        -- If the name is qualified, it isn't a variable
        FunctionOrValue [] n ->
            if n == name then
                1

            else
                0

        _ ->
            subexpressions expr
                -- Count and sum in one pass
                |> List.foldl (\e -> (+) (countUsesIn e name)) 0


{-| Use the first order, or use the second order if the first is `EQ`. This is
lazy in the second comparison and written for use in pipeline-style code, e.g.
in implementing a stable sort below:

    (\() -> compare index1 index2)
        |> fallbackCompareFor (compare element1 element2)

-}
fallbackCompareFor : Order -> (() -> Order) -> Order
fallbackCompareFor comp fallback =
    case comp of
        EQ ->
            fallback ()

        ltOrGt ->
            ltOrGt


{-| Given a source code extractor and a sorted list of ranges (with original
indices), create fixes to resort the source code to the list.
-}
createFix : (Range -> String) -> List ( Int, Range ) -> List Fix
createFix extractSource sorted =
    let
        applyFix : Int -> ( Int, Range ) -> List Fix
        applyFix newIndex ( oldIndex, range ) =
            if newIndex == oldIndex then
                []

            else
                ListX.find ((==) newIndex << Tuple.first) sorted
                    |> Maybe.map Tuple.second
                    |> Maybe.map
                        (\oldRange ->
                            [ extractSource range
                                |> String.trimRight
                                |> Fix.replaceRangeBy oldRange
                            ]
                        )
                    |> Maybe.withDefault []
    in
    List.indexedMap applyFix sorted
        |> List.concat


{-| Given context and a list of ordering functions, check if a list is sorted
and generate errors if it isn't.
-}
checkSorting : (Range -> String) -> String -> List ({ a | range : Range } -> { a | range : Range } -> Order) -> Range -> List { a | range : Range } -> List (Error {})
checkSorting extractSource errorConcerns orderings errorRange ds =
    let
        comp : { a | range : Range } -> { a | range : Range } -> Order
        comp d1 d2 =
            let
                go : List ({ a | range : Range } -> { a | range : Range } -> Order) -> Order
                go os =
                    case os of
                        [] ->
                            EQ

                        o :: os_ ->
                            (\() -> go os_)
                                |> fallbackCompareFor (o d1 d2)
            in
            go orderings

        indexed : List ( Int, { a | range : Range } )
        indexed =
            List.indexedMap Tuple.pair ds
    in
    List.sortWith
        (\( i1, d1 ) ( i2, d2 ) ->
            -- Sort stably
            (\() -> compare i1 i2)
                |> fallbackCompareFor (comp d1 d2)
        )
        indexed
        -- Check if sorted
        |> (\sorted ->
                if List.map Tuple.first sorted /= List.map Tuple.first indexed then
                    -- Generate a fix if unsorted
                    List.map (Tuple.mapSecond .range) sorted
                        |> createFix extractSource
                        |> unsortedError errorConcerns errorRange
                        |> List.singleton

                else
                    []
           )


{-| Given a range and a fix, create an unsorted case error.
-}
unsortedError : String -> Range -> List Fix -> Error {}
unsortedError errorConcerns =
    Rule.errorWithFix
        { message = errorConcerns ++ " are not sorted."
        , details =
            [ errorConcerns ++ " were found out of order.  They should be sorted as specified in the rule configuration."
            ]
        }


{-| Keep a value only if it passes a predicate. Like `Maybe.Extra.filter`, but
does not take a `Maybe` as input.
-}
validate : (a -> Bool) -> a -> Maybe a
validate pred x =
    if pred x then
        Just x

    else
        Nothing


{-| Work around `elm-syntax` sometimes including a period in record access
functions.
-}
makeAccessFunc : String -> String
makeAccessFunc accessFunc =
    if String.startsWith "." accessFunc then
        -- Work around elm-syntax behavior
        String.dropLeft 1 accessFunc

    else
        accessFunc
