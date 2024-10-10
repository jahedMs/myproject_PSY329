PY329 Project
================
Mursal Jahed
2024-10-11

\#load packages

``` r
#If need a package, create a new line and write library
#haven allows to load a dataset (csv etc)
#dplyr helps with recode variables
#ggplot2 helps with plot making
library(haven)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)

#load dataset
load("/Users/mursal_j/Downloads/ICPSR_36561/DS0001/36561-0001-Data.rda")

#goal for today is to:  re-code variables, check for assumptions
```

``` r
new_dataset <- da36561.0001 %>%
  select(PEERCIVACT1, COMMMEANT1, CIVINTENTT1, YIIACTS_16T1, YIIACTS_17T1, GENDERT1, ETH_MCT1)

#I will be completing the remainder of this assignment with my study buddy next week. Next steps include checking for assumptions as we learned in class for the past few weeks. I will make transformations as needed. I will also be creating my composite score using YIIACTS_16T1 and YIIACTS_17T1.
```
