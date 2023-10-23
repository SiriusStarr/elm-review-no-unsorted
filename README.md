# elm-review-no-unsorted

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/)
rules to ensure that anything (readily) sortable in Elm code is sorted in the
"proper" order.

## Provided rules

* [🔧`NoUnsortedCases`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.1.6/NoUnsortedCases/) - Reports case patterns that are not in the "proper" order.
* [🔧`NoUnsortedLetDeclarations`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.1.6/NoUnsortedLetDeclarations/) - Reports `let` declarations that are not in the "proper" order.
* [🔧`NoUnsortedRecords`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.1.6/NoUnsortedRecords/) - Reports record fields that are not in the "proper" order.
* [🔧`NoUnsortedTopLevelDeclarations`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.1.6/NoUnsortedTopLevelDeclarations/) - Reports top-level declarations that are not in the "proper" order.

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

## Changelog

* `1.1.6` -- 🐛 Fix bug in `NoUnsortedCases` where multiple record matches were
  not considered identical orderings if the indices of the matching fields were
  different. For example, the following record update was considered ambiguous
  before this bug fix:

  ```elm
  type alias A = { a : Int, b : String, c : Bool }
  type alias B = { b : String, c : Bool }

  a r b c = { r | b = b, c = c }
  ```

  Since `b` and `c` had different indices in the matching records, even though
  the sort order was the same.
* `1.1.5` -- 📝 Improve docs for [`NoUnsortedCases.doNotLookPastUnsortable`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.1.6/NoUnsortedCases/#doNotLookPastUnsortable)
* `1.1.4`
  * ⚡️ Improve performance when dealing with ignored files (~10% in one
    real-world test case).
  * 🐛 Bump `elm-review` to v2.12.1 for upstream bugfix.
* `1.1.3` -- Bump `elm-review` to v2.11.1 and mark rule as providing fixes.
* `1.1.2` -- 🐛 Fix a bug where, when dealing with cyclical sorting conditions,
  some low priority edges associated with the cycle but not actually responsible
  for it would be disregarded instead of only those responsible.  In practice,
  this meant that, very rarely, sortable `case` patterns would not be sorted at
  all (or would be sorted unstably) when wildcard (`_`) patterns lead to cyclic
  ordering relationships.  Since the
  [problem at the core of this](https://en.wikipedia.org/wiki/Feedback_arc_set#NP-hardness)
  is NP-hard, it is not guaranteed to be fixed but should nevertheless now occur
  substantially less often (if at all).  Please
  [open an issue](https://github.com/SiriusStarr/elm-review-no-unsorted/issues)
  if you encounter unstable sorting behavior or `case` patterns that should be
  able to be sorted but are not with this version.
* `1.1.1` -- 🚑 Fix critical bug caused by upstream bugfix in `elm-syntax`.
  Version `7.2.9` of `elm-syntax` no longer incorrectly parses record field type
  signature ranges, so `NoUnsortedRecords` (which repaired the previously
  incorrect behavior) was producing in invalid ranges for fixes, causing them to
  mangle code.  `elm-syntax` new lower bound is set to `7.2.9` and the
  workaround removed, fixing the issue.
* `1.1.0`
  * **New Features:**
    * ✨ -- Disable typechecking of unambiguous records by `NoUnsortedRecords`.
      The old default can be re-enabled with
      [`typecheckAllRecords`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.1.6/NoUnsortedRecords/#typecheckAllRecords)
    * ✨ -- Add control over subrecord support for `NoUnsortedRecords`.  Default
      behavior is to sort them when they appear in context (e.g. as part of
      their larger record) but not when they appear alone.  The old behavior did
      this unreliably and also treated custom type argument records as always
      canonical; this **old behavior may be re-enabled** (without the
      unreliability) with
      [`treatCustomTypeRecordsAsCanonical`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.1.6/NoUnsortedRecords/#treatCustomTypeRecordsAsCanonical).
      New settings for this behavior are also available with
      [`treatSubrecordsAsUnknown`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.1.6/NoUnsortedRecords/#treatSubrecordsAsUnknown)
      and
      [`treatAllSubrecordsAsCanonical`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-unsorted/1.1.6/NoUnsortedRecords/#treatAllSubrecordsAsCanonical).
  * **Bugfixes:**
    * 🚑 -- Fix critical bug causing control flow to sometimes be altered by
      `NoUnsortedCases` due to `List.sort` assuming transitivity.  New sorting
      methodology renders such issues impossible in the future.
    * 🐛 -- Fix doc comments not moving for ports with
      `NoUnsortedTopLevelDeclarations` (possibly leading to invalid code after
      fixes).  Doc comment support for ports was added manually, as `elm-syntax`
      does not parse them, and they now behave like doc comments for all other
      declarations.
    * 🐛 -- Improve handling of subrecords by `NoUnsortedRecords`. Previously,
      they were sometimes sorted and sometimes not (depending on what type
      information was available).  (See above for new configuration options
      controlling this behavior.)
    * 🐛 -- Fix bad type checking by `NoUnsortedRecords` assigning independent
      type vars to the same type, e.g. assigning all `Nothing`s to the same
      `Maybe a` value, even if they were different.
    * 🐛 -- Fix bad type checking by `NoUnsortedRecords` preserving type var
      assignment between fields (for type vars not in the known record), causing
      e.g. all `Nothing`s in a record to be required to be the same type.
    * 🐛 -- Fix bad type inference of lambda functions by `NoUnsortedRecords`.
    * 🐛 -- Fix non-functions being considered dependencies/helpers by
      `NoUnsortedTopLevelDeclarations`.  This brings actual rule behavior in
      line with that stated in the documentation.
  * **Other Changes:**
    * ⚡️ --  Significantly improve performance of `NoUnsortedRecords` (2x faster
      on some real-world codebases).  Previously, a significant amount of
      unnecessary type inference and duplicated recursion was being performed
      due to lack of laziness.
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
