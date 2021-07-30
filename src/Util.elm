module Util exposing (expressionToFix, fallbackCompareFor)

import Elm.Pretty exposing (prettyExpression)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Range exposing (Range)
import Pretty exposing (pretty)


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


{-| Given a range and an expression, create a fixed string from the expression.
-}
expressionToFix : Range -> Expression -> String
expressionToFix range e =
    prettyPrintExpression 120 e
        |> reindent range.start.column


{-| Re-indent a section of generated code to ensure that it doesn't cause issues
when used as a fix.
-}
reindent : Int -> String -> String
reindent amount =
    let
        indent : String
        indent =
            String.repeat (amount - 1) " "
    in
    String.lines
        >> List.indexedMap
            (\i l ->
                -- Don't indent first line or empty lines
                if i /= 0 && not (String.isEmpty l) then
                    indent ++ l

                else
                    l
            )
        >> String.join "\n"


{-| Turn an expression into nicely-formatted, valid Elm code, breaking at a specified width.
-}
prettyPrintExpression : Int -> Expression -> String
prettyPrintExpression width expression =
    prettyExpression expression
        |> pretty width
