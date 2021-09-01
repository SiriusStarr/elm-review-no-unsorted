module Util exposing (allBindingsInPattern, countUsesIn, createFix, fallbackCompareFor)

import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import List.Extra as ListX
import Review.Fix as Fix exposing (Fix)


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
createFix extractSourceCode sorted =
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
                            [ Fix.removeRange oldRange
                            , extractSourceCode range
                                |> Fix.insertAt oldRange.end
                            ]
                        )
                    |> Maybe.withDefault []
    in
    List.indexedMap applyFix sorted
        |> List.concat
