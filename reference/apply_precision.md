# Apply precision (round helper)

Simple wrapper around base
[`round()`](https://rdrr.io/r/base/Round.html) used by tests and
examples.

## Usage

``` r
apply_precision(x, n = NULL)
```

## Arguments

- x:

  Numeric vector.

- n:

  Integer number of digits to keep. If NULL, return x unchanged.

## Value

Numeric vector rounded to `n`.
