module NoUnsortedLetDeclarationsTest exposing (all)

import NoUnsortedLetDeclarations
    exposing
        ( alphabetically
        , rule
        , sortLetDeclarations
        , usedInExpressionFirst
        , usedInExpressionLast
        , usedInOtherDeclarationsFirst
        , usedInOtherDeclarationsLast
        , valuesAfterFunctions
        , valuesBeforeFunctions
        )
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoUnsortedLetDeclarations"
        [ passes
        , orderings
        ]


passes : Test
passes =
    describe "passes when"
        [ test "single declaration" <|
            \() ->
                """module A exposing (..)
f =
    let
        foo =
            bar
    in
    foo
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionFirst
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "no orderings" <|
            \() ->
                """module A exposing (..)
f =
    let
        foo =
            bar
        bar =
            baz
    in
    foo
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        ]


orderings : Test
orderings =
    describe "orderings"
        [ alphabeticallyTests
        , usedInExpressionFirstTests
        , usedInExpressionLastTests
        , usedInOtherDeclarationsLastTests
        , usedInOtherDeclarationsFirstTests
        , valuesBeforeFunctionsTests
        , valuesAfterFunctionsTests
        ]


alphabeticallyTests : Test
alphabeticallyTests =
    describe "alphabetically"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        bar =
            x
        baz =
            y
        foo =
            z
    in
    foo |> bar |> baz
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        foo =
            z
        bar =
            x
        baz =
            y
    in
    foo |> bar |> baz
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)
f =
    let
        bar =
            x
        baz =
            y
        foo =
            z
    in
    foo |> bar |> baz
"""
                        ]
        , test "passes sorted destructuring" <|
            \() ->
                """module A exposing (..)
f =
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
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails unsorted destructuring" <|
            \() ->
                """module A exposing (..)
f =
    let
        ( b, z ) =
            j

        (Opaque a) =
            i

        d =
            l

        { c, y } =
            k
    in
    x
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)
f =
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
"""
                        ]
        ]


usedInExpressionFirstTests : Test
usedInExpressionFirstTests =
    describe "usedInExpressionFirst"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        -- These are used in the expression
        y =
            b

        x =
            a

        -- These are not
        a =
            i

        b =
            j
    in
    x + y
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionFirst
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        a =
            i

        b =
            j

        x =
            a

        y =
            b
    in
    x + y
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionFirst
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)
f =
    let
        x =
            a

        y =
            b

        a =
            i

        b =
            j
    in
    x + y
"""
                        ]
        , test "falls back to other" <|
            \() ->
                """module A exposing (..)
f =
    let
        x =
            a

        y =
            b

        a =
            i

        b =
            j
    in
    x + y
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionFirst
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "falls back to other failing" <|
            \() ->
                """module A exposing (..)
f =
    let
        y =
            b

        x =
            a

        b =
            j

        a =
            i
    in
    x + y
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionFirst
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)
f =
    let
        x =
            a

        y =
            b

        a =
            i

        b =
            j
    in
    x + y
"""
                        ]
        ]


usedInExpressionLastTests : Test
usedInExpressionLastTests =
    describe "usedInExpressionLast"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        -- These are not used in the expression
        x =
            i

        y =
            j

        -- These are used in the expression
        b =
            y

        a =
            x
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        b =
            y

        a =
            x

        x =
            i

        y =
            j
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)
f =
    let
        x =
            i

        y =
            j

        b =
            y

        a =
            x
    in
    a + b
"""
                        ]
        , test "falls back to other" <|
            \() ->
                """module A exposing (..)
f =
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
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "falls back to other failing" <|
            \() ->
                """module A exposing (..)
f =
    let
        x =
            i

        b =
            y

        y =
            j

        a =
            x
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)
f =
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
    a + b
"""
                        ]
        ]


usedInOtherDeclarationsLastTests : Test
usedInOtherDeclarationsLastTests =
    describe "usedInOtherDeclarationsLast"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)
f =
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
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInOtherDeclarationsLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        a =
            x

        x =
            i

        b =
            y

        y =
            j
    in
    0
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInOtherDeclarationsLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)
f =
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
"""
                        ]
        ]


usedInOtherDeclarationsFirstTests : Test
usedInOtherDeclarationsFirstTests =
    describe "usedInOtherDeclarationsFirst"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)
f =
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
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInOtherDeclarationsFirst
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        a =
            x

        x =
            i

        b =
            y

        y =
            j
    in
    0
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInOtherDeclarationsFirst
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)
f =
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
"""
                        ]
        ]


valuesBeforeFunctionsTests : Test
valuesBeforeFunctionsTests =
    describe "valuesBeforeFunctions"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        x =
            a

        y =
            b

        a i =
            i

        b j =
            j
    in
    x + y
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> valuesBeforeFunctions
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        x =
            a

        a i =
            i

        y =
            b

        b j =
            j
    in
    x + y
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> valuesBeforeFunctions
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)
f =
    let
        x =
            a

        y =
            b

        a i =
            i

        b j =
            j
    in
    x + y
"""
                        ]
        ]


valuesAfterFunctionsTests : Test
valuesAfterFunctionsTests =
    describe "valuesAfterFunctions"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        a i =
            i

        b j =
            j

        x =
            a

        y =
            b
    in
    x + y
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> valuesAfterFunctions
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        x =
            a

        a i =
            i

        y =
            b

        b j =
            j
    in
    x + y
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> valuesAfterFunctions
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)
f =
    let
        a i =
            i

        b j =
            j

        x =
            a

        y =
            b
    in
    x + y
"""
                        ]
        ]


unsortedError : Review.Test.ExpectedError
unsortedError =
    Review.Test.error
        { message = "Let declarations are not sorted."
        , details =
            [ "Let declarations were found out of order.  They should be sorted as specified in the rule configuration." ]
        , under = "let"
        }
