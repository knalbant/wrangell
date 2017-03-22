# Wrangell - A Data Wrangling DSL Implemented in Haskell

## How to Build and Test
To compile, run `make`.
To run unit tests, run `Tests/test.sh`.

## A Brief Tour
After building, there should be an executable `./wrangell`. Let's look at the syntax of the command line arguments. The command line syntax has two options:

```bash
# This will launch Wrangell REPL mode
./wrangell [inputFile.csv]

# This will run a Wrangell script
./wrangell [script.wl] [inputFile.csv] [outputFile.csv] 
```
Now, let's look at the syntax of the Wrangell language by example.  First, we need a data file that we want to work with. Get started by looking at `Examples/mpg/mpg.csv`. The first
