module Util exposing (createFix, fallbackCompareFor)

import Elm.Syntax.Range exposing (Range)
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


{-| Given a source code extractor and a sorted list of ranges (with original
indices), create fixes to resort the source code to the list.
-}
createFix : (Range -> String) -> List ( ( Int, a ), Range ) -> List Fix
createFix extractSourceCode sorted =
    let
        applyFix : Int -> ( ( Int, a ), Range ) -> List Fix
        applyFix newIndex ( ( oldIndex, _ ), range ) =
            if newIndex == oldIndex then
                []

            else
                ListX.find ((==) newIndex << Tuple.first << Tuple.first) sorted
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
