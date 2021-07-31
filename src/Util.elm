module Util exposing (createFix, fallbackCompareFor)

import Elm.Syntax.Expression exposing (Case)
import Elm.Syntax.Node as Node
import Elm.Syntax.Range as Range exposing (Range)
import List.Extra as ListX
import Review.Fix as Fix exposing (Fix)


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


{-| Given a source code extractor and a sorted list of patterns (with original
indices), create fixes to resort the source code to the list.
-}
createFix : (Range -> String) -> List ( ( Int, a ), Case ) -> List Fix
createFix extractSourceCode sorted =
    let
        applyFix : Int -> ( ( Int, a ), Case ) -> List Fix
        applyFix newIndex ( ( oldIndex, _ ), ( pattern, expression ) ) =
            if newIndex == oldIndex then
                []

            else
                ListX.find ((==) newIndex << Tuple.first << Tuple.first) sorted
                    |> Maybe.map
                        (Tuple.second
                            >> (\( p, e ) -> [ Node.range p, Node.range e ])
                            >> Range.combine
                        )
                    |> Maybe.map
                        (\oldRange ->
                            [ Fix.removeRange oldRange
                            , Range.combine
                                [ Node.range pattern
                                , Node.range expression
                                ]
                                |> extractSourceCode
                                |> Fix.insertAt oldRange.end
                            ]
                        )
                    |> Maybe.withDefault []
    in
    List.indexedMap applyFix sorted
        |> List.concat
