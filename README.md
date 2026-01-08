
<!-- README.md is generated from README.Rmd. Please edit that file -->

# roarutility

<!-- badges: start -->

<!-- badges: end -->

The goal of roarutility is to process ROAR data. It provides convenience
functions to make some common cleaning and processing tasks with ROAR
data a bit easier.

## Installation

You can install the development version of roarutility from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("kellywentz/roarutility")
```

## Usage

### roar.read.csv

This is a basic example which shows you how to read in ROAR data and
remove opt-outs in one line of code.

``` r
library(roarutility)
new_data <- roar.read.csv("all_core_runs_2025-05-27.csv", 
              "~/Documents/GitHub/roar-technical-manual/data",
              "https://drive.google.com/file/d/11gYLqU5xT-NMDxWXGQj8WfZ8AVA_lFT9/view?usp=drive_link")
```

Notice how the output dataframe has removed all possible opt-outs from
the most up-to-date opt-out CSV.

### clean_strings

This is a basic example which shows you how clean_strings() takes in
data and outputs data that has removed extra characters from assigning
organization variables and converted empty strings to NA values.

``` r
library(roaryutility)
test_df <- data.frame(
  assigning_schools = c("[irNgj3c]", "irNgj3c"),
  age = c("", "6.7")
)

clean_df <- clean_strings(test_df) 
clean_df 
#   assigning_schools  age
# 1           irNgj3c <NA>
# 2           irNgj3c  6.7
```

Notice how the output dataframe has removed the “\[\]” characters from
the assigning_schools variable and has converted the empty string value
in age to an NA value.

### remove_empty_cols

This is a basic example which shows you how remove_empty_cols() takes in
a dataframe with a column with all NA values and outputs data that has
removed the columns with all NA values.

``` r
library(roarutility)
test_df <- data.frame(
  firstname = c("Jane", "John", NA, "Kelly"),
  lastname = c("Doe", NA, NA, "Smith"),
  middlename = c(NA, NA, NA, NA)
)

clean_df <- remove_empty_cols(test_df) 
names(clean_df) 
# [1] "firstname" "lastname" 
```

Notice how the output dataframe has removed the column “middlename”
because it consisted of all NA values.
