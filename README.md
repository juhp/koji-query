# koji-query

Currently only supports listing one's recent Fedora Koji build tasks.

Rather similar to `koji list-tasks --quiet --mine --after=yesterday --all`
except it print the url to kojiweb.

Later I may merge in other mini-projects like koji-progress and
koji-buildlog-sizes (possibly even koji-install).

## Installation

`cabal-rpm install`, `cabal install` or `stack install`.
