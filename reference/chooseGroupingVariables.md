# Choose a valid set of grouping variables from a targets/ experiment data frame.

To be useful for grouping a variable needs to be a character or a
factor, and to have a number of values greater than one but less than
the number of samples.

## Usage

``` r
chooseGroupingVariables(df)
```

## Arguments

- df:

  Input data frame

## Value

out A list of valid column names

## Examples

``` r
# `airway` contains info on the samples it's based on

require(airway)
#> Loading required package: airway
data(airway, package = "airway")

# However, not all variables are useful for grouping data. Some have a
# different value for every sample, one has the same value for all of them.

colData(airway)
#> DataFrame with 8 rows and 9 columns
#>            SampleName     cell      dex    albut        Run avgLength
#>              <factor> <factor> <factor> <factor>   <factor> <integer>
#> SRR1039508 GSM1275862  N61311     untrt    untrt SRR1039508       126
#> SRR1039509 GSM1275863  N61311     trt      untrt SRR1039509       126
#> SRR1039512 GSM1275866  N052611    untrt    untrt SRR1039512       126
#> SRR1039513 GSM1275867  N052611    trt      untrt SRR1039513        87
#> SRR1039516 GSM1275870  N080611    untrt    untrt SRR1039516       120
#> SRR1039517 GSM1275871  N080611    trt      untrt SRR1039517       126
#> SRR1039520 GSM1275874  N061011    untrt    untrt SRR1039520       101
#> SRR1039521 GSM1275875  N061011    trt      untrt SRR1039521        98
#>            Experiment    Sample    BioSample
#>              <factor>  <factor>     <factor>
#> SRR1039508  SRX384345 SRS508568 SAMN02422669
#> SRR1039509  SRX384346 SRS508567 SAMN02422675
#> SRR1039512  SRX384349 SRS508571 SAMN02422678
#> SRR1039513  SRX384350 SRS508572 SAMN02422670
#> SRR1039516  SRX384353 SRS508575 SAMN02422682
#> SRR1039517  SRX384354 SRS508576 SAMN02422673
#> SRR1039520  SRX384357 SRS508579 SAMN02422683
#> SRR1039521  SRX384358 SRS508580 SAMN02422677

# So just pick the variables that ARE useful

chooseGroupingVariables(data.frame(colData(airway)))
#> [1] "cell" "dex" 
```
