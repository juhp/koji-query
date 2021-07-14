# koji-query

A small cli tool to help locate Koji tasks.

Similar to `koji list-tasks --mine --quiet --all ...`,
but it shows duration kojiweb urls and build.log size,
and it uses `date` to parse a specified date string
and can filter results by package.

Later I might merge in other mini-projects like koji-progress and
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

Example:

```
$ koji-query -a aarch64 -d "last week" -s fail
```
