# Machine Curses

This is a simple command line  program used to run scripts in remote machines, it relies on the brick package
for the curses GUI and on the jq command to parse the machine list fetched in json format from DYD sigmyp software.

# Installation

Clone and run this program doing: 

```shell
cabal update
cabal sandbox init
cabal install
cabal run
```

Install the jq command line program from [here](https://stedolan.github.io/jq/).
