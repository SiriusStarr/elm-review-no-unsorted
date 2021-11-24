# elm-review-no-unsorted

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/)
rules to ensure that anything (readily) sortable in Elm code is sorted in the
"proper" order.

## Provided rules

* [🔧`NoUnsortedCases`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.0.6/NoUnsortedCases/) - Reports case patterns that are not in the "proper" order.
* [🔧`NoUnsortedLetDeclarations`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.0.6/NoUnsortedLetDeclarations/) - Reports `let` declarations that are not in the "proper" order.
* [🔧`NoUnsortedRecords`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.0.6/NoUnsortedRecords/) - Reports record fields that are not in the "proper" order.
* [🔧`NoUnsortedTopLevelDeclarations`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.0.6/NoUnsortedTopLevelDeclarations/) - Reports top-level declarations that are not in the "proper" order.

## Configuration

```elm
module ReviewConfig exposing (config)

import NoUnsortedCases
import NoUnsortedLetDeclarations
import NoUnsortedRecords
import NoUnsortedTopLevelDeclarations
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoUnsortedCases.rule NoUnsortedCases.defaults
    , NoUnsortedLetDeclarations.rule
        (NoUnsortedLetDeclarations.sortLetDeclarations
            |> NoUnsortedLetDeclarations.alphabetically
        )
    , NoUnsortedRecords.rule
        (NoUnsortedRecords.defaults
            |> NoUnsortedRecords.reportAmbiguousRecordsWithoutFix
        )
    , NoUnsortedTopLevelDeclarations.rule
        (NoUnsortedTopLevelDeclarations.sortTopLevelDeclarations
            |> NoUnsortedTopLevelDeclarations.portsFirst
            |> NoUnsortedTopLevelDeclarations.exposedOrderWithPrivateLast
            |> NoUnsortedTopLevelDeclarations.alphabetically
        )
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-no-unsorted/example
```

## Changlelog

* `1.0.6` -- 🐛 Fix a bug causing `NoUnsortedRecords` to recurse infinitely in
  certain rare cases where a type variable was assigned to a value containing an
  identically-named type variable (e.g. the type variable `a` ended up being
  assigned to a value defined by the generic record `{ a | field : Int }`).
* `1.0.5` -- 🚑 Fix critical bug causing `NoUnsortedRecords` to rarely generate
  valid code that was missing fields for record updates.  It is recommended that
  you **check any record update expressions** that **contained comments** that
  were fixed by this rule prior to this version, as one or more fields may have
  been appended to the end of a comment in sorting.
* `1.0.4` -- Update to `elm-syntax` v7.2.8 to fix upstream issue with lambda
  ranges generating invalid code in fixes.
* `1.0.3` -- Fix crash with `--fix` or `--watch` due to comparison of function
  types in `ProjectContext` of `NoUnsortedRecords`.
* `1.0.2` -- Fix crash in handling of record type comparison.
* `1.0.1` -- Fix crash in handling of generic records when not all fields must
  be present.
* `1.0.0` -- Initial release
