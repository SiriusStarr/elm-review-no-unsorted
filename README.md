# elm-review-no-unsorted

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/)
rules to ensure that anything (readily) sortable in Elm code is sorted in the
"proper" order.

## Provided rules

* [🔧`NoUnsortedCases`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.0.3/NoUnsortedCases/) - Reports case patterns that are not in the "proper" order.
* [🔧`NoUnsortedLetDeclarations`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.0.3/NoUnsortedLetDeclarations/) - Reports `let` declarations that are not in the "proper" order.
* [🔧`NoUnsortedRecords`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.0.3/NoUnsortedRecords/) - Reports record fields that are not in the "proper" order.
* [🔧`NoUnsortedTopLevelDeclarations`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.0.3/NoUnsortedTopLevelDeclarations/) - Reports top-level declarations that are not in the "proper" order.

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

* `1.0.3` -- Fix crash with `--fix` or `--watch` due to comparison of function
  types in `ProjectContext` of `NoUnsortedRecords`.
* `1.0.2` -- Fix crash in handling of record type comparison.
* `1.0.1` -- Fix crash in handling of generic records when not all fields must
  be present.
* `1.0.0` -- Initial release
