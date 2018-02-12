# logiczoo

A calculator for logic.

## Installing:

 - Install [Stack](https://docs.haskellstack.org/en/stable/README/), the Haskell tool
 - Clone this repo
 - `cd` into the repo
 - `$ stack install`
 - `$ logiczoo-exe`

## Usage:

`logiczoo-exe (logicaltruth | equivalent | truthtable)`

The parser recognizes the following logic operators:

 - Not: "~", "!"
 - And: "&", "^"
 - Or: "|", "v"
 - If: "->"
 - Iff: "<->"
 - Xor: "x"

Spaces are ignored.

## Examples:

To see the truth table for a sentence:
```console
$ logiczoo-exe truthtable --sentence "~(A&B)"
 A  B ---  True
 A ~B --- False
~A  B --- False
~A ~B --- False
```

To check if something is logically true (i.e. true on all models):
```console
$ logiczoo-exe logicaltruth --sentence "(Av~A)"
True

$ logiczoo-exe logicaltruth --sentence "(AvB)"
False
```

To check if two sentences are equivalent:
```console
$ logiczoo-exe equivalent --sentence1 "(~A&~B)" --sentence2 "~(AxB)"
False

$ logiczoo-exe equivalent --sentence1 "(~A&~B)" --sentence2 "~(AvB)"
True
```