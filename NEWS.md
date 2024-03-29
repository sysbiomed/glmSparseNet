# glmSparseNet
----------------------------------------------------------------

## Changes in 1.22.0

### Breaking changes

- Adds `experiment` parameter to different glm* functions.
- Changing in hashing function to use `rlang::hash` that reduces a dependency.
- Deprecates `hallmarks` function as API has been shutdown.

### Miscellaneous

- Use of native pipe instead of `{magritrr}`'s `%>%`
- Corrects styling and linter issues for better code quality and readability
- Starts to deprecates parameters using dot.case in favor of camelCase.
- Increases code coverage to `95%`.

## Changes in 1.14.3

- Corrects bug when calculating the `penalty.factor` from a matrix (as it was not applying the transformation function)

## Changes in 1.14.2

- Corrects all CRAN deprecations (includes code from `{loose.rock}` in this package)

## Changes in 1.11.0

- `{sparsebn}` has been disabled pending upstream corrections (examples
from that package failed to pass)

## Changes in version 1.2.0

- Depends on more recent version of `{loose.rock}` (previous was deprecated)
- Release of new Bioconductor 3.9

## Changes in version 1.1.0

- Official release of glmSparseNet in Bioconductor

## Changes in version 0.99.17

- Converts to 4 space indentation

## Changes in version 0.99.15

- Speeds up ppi vignette

## Changes in version 0.99.14

- Renamed functions to camelCase
- Renamed non-exported functions with '.' prefix
- Removed most methods, as suggested by Bioc reviewer
- Removed commented code
- Changed functions that handle tcga data to use TCGAutils package
- Many more described in https://github.com/Bioconductor/Contributions/issues/849#issuecomment-422594871

## Changes in version 0.99.13

- Renamed draw.kaplan to separate2groups.cox

## Changes in version 0.99.9

- Removed loose.rock functions from this package as they are published in CRAN
