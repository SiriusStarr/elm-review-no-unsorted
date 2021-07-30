# elm-review-no-unsorted

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/)
rules to ensure that cases are sorted in the "proper" order.

## Provided rules

- [🔧`NoUnsortedCases`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.0.0/NoUnsortedCases) - Reports case patterns that are not in the "proper" order.

## Configuration

```elm
module ReviewConfig exposing (config)

import NoUnsortedCases
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoUnsortedCases.rule NoUnsortedCases.defaults
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-no-unsorted/example
```
