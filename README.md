ANCRTAdjust
=======
R Package to adjust ANC-RT data to reduce bias in estimating HIV prevalence trends

Installation
------------

Install via Github using `devtools`:

``` r
devtools::install_github("brittanyblouin/ANCRTAdjust")
```

Loading the data
----------------
We can then try the package using a syntethetic dataset that we first load.

``` r
data(ancrt)
```

``` r
> head(ancrt)
  faciluid time n_clients n_status knownpos testpos testneg  true_prv
1      F_1    1       217      217       29      37     151 0.3041475
2      F_1    2       240      240       31      40     169 0.2958333
3      F_1    3       216      216       30      38     148 0.3148148
4      F_1    4       159      159       22      28     109 0.3144654
5      F_1    5       192      192       25      32     135 0.2968750
6      F_1    6       214      214       30      39     145 0.3224299
```


After loading the data, the first step is to make sure we are following some naming convention for the 7 most imporant variables. These are:
* `faciluid`: the unique facility indentifier. 
* `time`: the calendar time over which the data was collected.
* `n_clients`: the number of women from the specified facility, during the specified time period, that attended their first ANC visit. 
* `n_status`: the number of women from the specified facility, during the specified time period, that had their HIV status ascertained at their first ANC visit, either by testing or through previous knowledge.
* `knownpos`: the number of women from the specified facility, during the specified time period, that already knew that they were HIV-positive at their first ANC visit.
* `testpos`: the number of women from the specified facility, during the specified time period, that tested positive for HIV at their first ANC visit.
* `testneg`: the number of women from the specified facility, during the specified time period, that tested negative for HIV at their first ANC visit.

Finally, some data might be available age-disaggregated. If so, the variable should be named:
* `age`: age category of pregnant women.

The function called `name_var` can automatically rename the variables for you.

``` r
ancrt <- name_var(ancrt, faciluid = "faciluid", time = "time", n_clients = "n_clients",
                  n_status = "n_status", knownpos = "knownpos", testpos = "testpos",
                  testneg = "testneg", age = "age") 
```

Data cleaning
-------------
The second step is to clean the data using the `data_clean()` function. The different available options can be found by typing `help(data_clean)` in your console.

``` r
ancrt_cleaned <- data_clean(ancrt)
```

Descriptive graphs
------------------
An inuitive way to examine the impact of the different data cleaning options is to plot the results. This is achieve using the `descriptive_plot()` function.

``` r
descriptive_plot(ancrt_cleaned)
``` 
