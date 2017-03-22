# Wrangell - A Data Wrangling DSL Implemented in Haskell

## How to Build and Test
To compile, run `make`.
To run unit tests, run `Tests/test.sh`.

# A Brief Tour
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
# Overview of provided functionality 
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
### Format Table
To load data in, Wrangell requires explicit types for the columns. Specify this using `formatTable`:
```
(formatTable [types])
```
e.g.:
```
(formatTable int int float string bool
```
### Column Labels
Wrangell supports setting labels for each column using `labels`:
```
(labels [names])
```
e.g.:
```
(labels age SSN height name gender)
```
If the input data already has CSV column headers, then instead tell Wrangell to load the labels automatically:
```
(hasHeader)
```
### Drop Incomplete Rows
Wrangell provides a convenient way to drop rows which have incomplete, i.e. `<Top>`, data:
```
(dropIncomplete)
```
### Dropping a Column
Wrangell supports dropping columns either by column index or by column label:
```
(dropColumn index or_label)
```
e.g.:
```
(dropColumn height)
```
### Transforming a Single Column
To transform a *single* column, provide a function which takes one parameter, and Wrangell whill apply this function over the specified column:
```
(transformColumn index_or_label f)
```
where `f` can be either a named function or a lambda function. E.g.:
```
(transformColumn height (lambda (h) (* 2.0 h)))
```
### Transforming on Multiple Columns
Wrangell also supports transforming a column based on input from multiple columns. Just tell Wrangell which columns the transform is a function of, provide a corresponding function, and a column to write to:
```
(transformColumns [input_columns] f output_column)
```
e.g.:
```
(transformColumns width length (lambda (w l) (* w l)) area)
```
### Folding on a Column
Wrangell supports an operation similar to [fold](https://wiki.haskell.org/Fold) from Haskell: `fold` will compute a cumulative value based on partial computations over the column. The usage is:
```
(fold index_or_label f start_value)
```
where `f` is a named function or lambda of type `b -> a -> b`. For example, we can compute the sum of a column:
```
(fold height (lambda (acc h) (+ acc h)) 0)
```
### Counting Rows
Wrangell can count the rows in the dataset:
```
(countRows)
```
### Sum a Column
As shown above, sum can be implemented using `fold`. However, Wrangell supports it for convenience:
```
(sum index_or_label)
```
e.g.:
```
(sum height)
```
### Mean of a Column
```
(mean index_or_label)
```
e.g.:
```
(mean height)
```
### Variance of a Column
```
(variance index_or_label)
```
e.g.:
```
(variance height)
```
### Standard Deviation of a Column
```
(std index_or_label)
```
e.g.:
```
(std height)
```
### Printing a Table
Wrangell allows for convenient printing of a table. This can be very useful in REPL mode to visualize data transformations.
```
(printTable)
```
### Writing to a File
Wrangell includes a language feature to write to a file, in addition to using command line arguments to write to a file:
```
(outputFile "outputFileName")
```
e.g.:
```
(outputFile "output.csv")
```
# Concrete Syntax for Wrangell
```
Spaces ::= ' ' { ' ' }
EscapedChar ::= '\' ( '"' | 'n' | 'r' | 't' | '\' )
Letter ::= 'a' | 'b' | ... | 'z' | 'A' | 'B' | ... | 'Z'
Digit ::= '0' | '1' | ... | '9'
Symbol ::= '!' | '$' | '%' | '&' | '|' | '*' | '+' | '-' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '_' | '~'
HexDigit ::= '0' | '1' | ... | '9' | 'a' | 'A' | ... | 'f' | 'F'
OctDigit ::= '0' | '1' | ... | '7'

Atom :: = ( Letter | Symbol ) { Letter | Symbol | Digit }
String ::= '"' { EscapedChar | - ( '"' | '\' ) } '"'
Bool ::= '#' ( 't' | 'f' )
Integral ::= Digit+
Float ::= digit+ '.' digit+
Hex ::= '#x' HexDigit+
Oct ::= '#o' OctDigit+
Bin ::= '#b' ( '0' | '1' )+
Decimal ::= [ '#d' ] Integral
Number ::= Float | Decimal | Hex | Oct | Bin

ListInternals ::= '' | ( Expr { Spaces Expr } { Spaces } )
List ::= '(' ListInternals ')'
Expr ::= String | Atom | Number | Bool | Quoted | List
Quoted ::= ''' Expr
Seq ::= { Expr }
```


