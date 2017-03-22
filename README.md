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
Now, let's look at the syntax of the Wrangell language by example.  First, we need a data file that we want to work with. Get started by looking at `Examples/mpg/mpg.csv`. The first few lines look like:

| No headers... | | | | | | | | |
| ------------- | ------------- | -  | -  | -  | -  | -  | -  | -  |
| 18.0 | 8. | 307.0 | 130.0 | 3504. | 12.0 | 70. | 1. | "chevrolet chevelle malibu" |
| 15.0 | 8. | 350.0 | 165.0 | 3693. | 11.5 | 70. | 1. | "buick skylark 320" |
| NA | 8. | 350.0 | 165.0 | 4142. | 11.5 | 70. | 1. | "chevrolet chevelle concours (sw)" |

Now, ultimately we would like to run a regression analysis on this dataset, to try to predict the MPG of a car. However, there are several problems with this dataset which make it difficult to quickly perform the regression:
1. The columns are not labeled with headers, so it's not clear what the data exactly is.
2. Some data is useless, and makes parsing more difficult. For example, the car names do not help for doing simple regression.
3. Some rows have incomplete data. For example, the third row has an NA to indicate a missing value.

We can use Wrangell to load in this data and perform operations to fix these issues.

### Loading Data

Start by launching Wrangell in REPL mode with this input file:
```bash
./wrangell Examples/mpg/mpg.csv
```
First, we **specify labels** for each of the columns. This will help us keep track of the columns with nice names:
```
wrangell>>> (labels mpg cylinders disp horsepower weight accel model origin name)
```
Now, we can load in the data by also specifying the **column formats**. After this, Wrangell with ensure type-sound operations on the dataset.
```
wrangell>>> (formatTable float int float float float float int int string)
```

Ok, now to check that the data has been imported correctly, we can view the data with `(printTable)`:
```
wrangell>>> (printTable)
18.0 8 307.0 130.0 3504.0 12.0 70 1 chevrolet chevelle malibu
15.0 8 350.0 165.0 3693.0 11.5 70 1 buick skylark 320
<Top> 8 350.0 165.0 4142.0 11.5 70 1 chevrolet chevelle concours (sw)
...
```

### Dropping Incomplete Data

Clearly, Wrangell has imported the data we wanted. But what is this strange looking **`<top>`** in the third row? This is how Wrangell denotes **incomplete data**. In particular, this data is incomplete because we specified the type of the column as `float`, but it actually contained the string `NA`. 

Doing regression over incomplete data is not great, so let's just get rid of rows that have incomplete data. This is easy using Wrangell's `(dropIncomplete)`:
```
wrangell>>> (dropIncomplete)
```

### Dropping Columns

Ok, so far we have imported our data, applied types and labels to the columns, and removed incomplete data. The last transformation to do before doing regression is getting rid of columns we don't want. In particular, the columns `name`, `origin`, and `model` are not helpful for the regression, so we can just drop them:
```
wrangell>>> (dropColumn name)
wrangell>>> (dropColumn origin)
wrangell>>> (dropColumn model)
```

Before we save our final data, we can quickly view the table to make sure it looks correct:
```
wrangell>>> (printTable)
18.0 8 307.0 130.0 3504.0 12.0
15.0 8 350.0 165.0 3693.0 11.5
18.0 8 318.0 150.0 3436.0 11.0
...
```
This looks great, so we can write the new data to a file:
```
wrangell>>> (outputFile "mpg_clean.csv")
```
Now just quit Wrangell with `CTRL-D` or by typing `quit`, and load `mpg_clean.csv` in a numerical computation package to do the regression!

Finally, if this data wrangling will be done repeatedly, make a script for it:
```
# This file is called mpg.wl
(formatTable float int float float float float int int string)
(labels mpg cylinders disp horsepower weight accel model origin name)

(dropIncomplete)

(dropColumn name)
(dropColumn origin)
(dropColumn model)
```
which can then be reused like so:
```
./wrangell mpg.wl [input.csv] [output.csv]
```
## Overview of provided functionality 
Wrangell inherits a great deal of its syntax from Scheme what follows is a brief rundown of what's provided. 

### Function Definition
Wrangell supports named function definitions of the form:
```
(define (func_name [args]) body)
```
As well as support for recursive functions:
```
(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))
```
as well as lambdas...
```
(lambda (x y) (+ (* 2 x) y))
```
### Constant definition
```
(define constant_name expression) 
```
e.g.: 
```
(define x (+ 1 2 3 4))
```
### if-then-else
```
(if predicate-expression then-expression else-expression)
```
e.g.:
```
(if (< n 100) (* n 2) n)
```

## Concrete Syntax for Wrangell
### Atom String
...
Atom :: = ( letter | symbol ) { letter | synmbol | digit }
...
### List [WVal]
...
List ::= '(' parseListInternals ')'
...
### String String
...
String ::= '"' { escapedChar | - ( '"' | '\' ) } '"'
...
### Bool Bool
...
Bool ::= '#' ( 't' | 'f' )
...
### Integral Integer
...
Integral ::= digit+
...
### Float Double
...
Float ::= digit+ '.' digit+
...
### Seq [WVal]
```
Seq ::= { Expr }
```
###ListInternals
...
ListInternals ::= '' | ( parseExpr { spaces parseExpr} )
...

### Expr
...
Expr ::= parseString | parseAtom | parseNumber | parseBool | parseQuoted | parseList
...

###Number
...
Number ::= parseFloat | parseDecimal | parseHex | parseOct | parseBin
...
###Quoted
...
Quoted ::= ''' parseExpr
...
###Hex
...
Hex ::= '#x' hexDigit+
...
###Oct
...
Oct ::= '#o' octDigit+
...
###Bin
...
Bin ::= '#b' ( '0' | '1' )+
...

###Decimal
...
Decimal ::= [ '#d' ] parseIntegral
...


