module NoUnsortedTopLevelDeclarationsTest exposing (all)

import NoUnsortedTopLevelDeclarations
    exposing
        ( alphabetically
        , exposedOrderWithPrivateFirst
        , exposedOrderWithPrivateLast
        , portsFirst
        , portsLast
        , rule
        , sortTopLevelDeclarations
        , typesFirst
        , typesLast
        )
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoUnsortedTopLevelDeclarations"
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
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "no orderings" <|
            \() ->
                """module A exposing (..)
f =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        ]


orderings : Test
orderings =
    describe "orderings"
        [ alphabeticallyTests
        , exposedOrderWithPrivateLastTests
        , exposedOrderWithPrivateFirstTests
        , typesFirstTests
        , typesLastTests
        , portsFirstTests
        , portsLastTests
        ]


alphabeticallyTests : Test
alphabeticallyTests =
    describe "alphabetically"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)
bar =
    x
baz =
    y
foo =
    z
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when unordered" <|
            \() ->
                """module A exposing (..)
foo =
    z
bar =
    x
baz =
    y
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed """module A exposing (..)
bar =
    x
baz =
    y
foo =
    z
"""
                        ]
        , test "passes when ordered with types and aliases" <|
            \() ->
                """module A exposing (..)
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
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when unordered with types and aliases" <|
            \() ->
                """module A exposing (..)

{-| This is fine in this order too.
-}
type Bar = Bar

bar =
    x

{-| This isn't!
-}
type alias Zed = {}
foo =
    z
baz =
    y
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed """module A exposing (..)

{-| This is fine in this order too.
-}
type Bar = Bar

bar =
    x

baz =
    y
foo =
    z
{-| This isn't!
-}
type alias Zed = {}
"""
                        ]
        ]


exposedOrderWithPrivateLastTests : Test
exposedOrderWithPrivateLastTests =
    describe "exposedOrderWithPrivateLast"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing
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
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "falls back to second sorting" <|
            \() ->
                """module A exposing
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

z =
    zed

b =
    bar
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed """module A exposing
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
"""
                        ]
        , test "fails when unsorted" <|
            \() ->
                """module A exposing
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

type alias Z =
    A

a =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed """module A exposing
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
"""
                        ]
        ]


exposedOrderWithPrivateFirstTests : Test
exposedOrderWithPrivateFirstTests =
    describe "exposedOrderWithPrivateFirst"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing
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
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateFirst
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when unsorted" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

b =
    bar

type A
    = A

z =
    zed

type alias Z =
    A

a =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateFirst
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed """module A exposing
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
"""
                        ]
        ]


typesFirstTests : Test
typesFirstTests =
    describe "typesFirst"
        [ test "passes when ordered" <|
            \() ->
                """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

type A
    = A

type alias Z =
    A

z =
    zed

a =
    foo

port b: String -> Cmd msg

c =
    bar
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> typesFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when unsorted" <|
            \() ->
                """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

c =
    bar

port b: String -> Cmd msg

type A
    = A

z =
    zed

type alias Z =
    A

a =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> typesFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError True
                            |> Review.Test.whenFixed """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

type A
    = A

type alias Z =
    A

z =
    zed

a =
    foo

port b: String -> Cmd msg

c =
    bar
"""
                        ]
        ]


typesLastTests : Test
typesLastTests =
    describe "typesLast"
        [ test "passes when ordered" <|
            \() ->
                """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

z =
    zed

a =
    foo

port b: String -> Cmd msg

c =
    bar

type A
    = A

type alias Z =
    A
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> typesLast
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when unsorted" <|
            \() ->
                """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

c =
    bar

type A
    = A

z =
    zed

port b: String -> Cmd msg

type alias Z =
    A

a =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> typesLast
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError True
                            |> Review.Test.whenFixed """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

z =
    zed

a =
    foo

port b: String -> Cmd msg

c =
    bar

type A
    = A

type alias Z =
    A
"""
                        ]
        ]


portsFirstTests : Test
portsFirstTests =
    describe "portsFirst"
        [ test "passes when ordered" <|
            \() ->
                """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

port b: String -> Cmd msg

type A
    = A

type alias Z =
    A

z =
    zed

a =
    foo

c =
    bar
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> portsFirst
                            |> typesFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when unsorted" <|
            \() ->
                """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

c =
    bar

port b: String -> Cmd msg

type A
    = A

z =
    zed

type alias Z =
    A

a =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> portsFirst
                            |> typesFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError True
                            |> Review.Test.whenFixed """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

port b: String -> Cmd msg

type A
    = A

type alias Z =
    A

z =
    zed

a =
    foo

c =
    bar
"""
                        ]
        ]


portsLastTests : Test
portsLastTests =
    describe "portsLast"
        [ test "passes when ordered" <|
            \() ->
                """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

type A
    = A

type alias Z =
    A

z =
    zed

a =
    foo

c =
    bar

port b: String -> Cmd msg
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> portsLast
                            |> typesFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when unsorted" <|
            \() ->
                """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

c =
    bar

port b: String -> Cmd msg

type A
    = A

z =
    zed

type alias Z =
    A

a =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> portsLast
                            |> typesFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError True
                            |> Review.Test.whenFixed """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

type A
    = A

type alias Z =
    A

z =
    zed

a =
    foo

c =
    bar

port b: String -> Cmd msg
"""
                        ]
        ]


unsortedError : Bool -> Review.Test.ExpectedError
unsortedError portModule =
    Review.Test.error
        { message = "Top-level declarations are not sorted."
        , details =
            [ "Top-level declarations were found out of order.  They should be sorted as specified in the rule configuration." ]
        , under =
            if portModule then
                "port module"

            else
                "module"
        }
