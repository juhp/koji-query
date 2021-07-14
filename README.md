# koji-query

Currently only supports listing one's recent Fedora Koji build tasks.

Rather similar to `koji list-tasks --quiet --mine --after=yesterday --all`
except it print the url to kojiweb.

Later I may merge in other mini-projects like koji-progress and
koji-buildlog-sizes (possibly even koji-install).

## Installation

Either: `cabal install  --installdir=~/bin`

(on Fedora first run `cabal-rpm builddep` to save a lot of building).

or: `stack install`.

## Usage

By default it shows your Fedora Koji tasks since yesterday.

```
$ koji-query --version
0.1.0
$ koji-query --help
koji-query

Usage: koji-query [--version] [-S|--server URL] [-u|--user USER]
                  [-l|--limit INT] [(-t|--task TASKID) | (-P|--parent TASKID)]
                  [-s|--state STATE] [-a|--arch ARCH] [-d|--date DAY]
                  [-m|--method METHOD] [-p|--package PKG]
  Helper client for koji queries: https://github.com/juhp/koji-query

Available options:
  -h,--help                Show this help text
  --version                Show version
  -S,--server URL          Koji Hub [default: Fedora]
  -u,--user USER           Koji user
  -l,--limit INT           Maximum number of tasks to show [default: 20]
  -t,--task TASKID         Show task
  -P,--parent TASKID       List children tasks
  -s,--state STATE         Filter tasks by state
  -a,--arch ARCH           Task arch
  -d,--date DAY            Tasks started after date [default: yesterday]
  -m,--method METHOD       Select tasks by method: [build,buildarch,etc]
  -p,--package PKG         Filter results to specified package
```
