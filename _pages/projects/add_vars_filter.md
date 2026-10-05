---
title: "Add variables & filter data"
author: "Sarah Bolinger"
permalink: /projects/fate_glm/vars_filter/
output: 
  md_document:
    variant: gfm
    preserve_yaml: TRUE
---

## 0. SETUP

``` r
sitesSel <- c("RUTE", "RUTW")
sppSel <-   c("LETE", "CONI", "WIPL")
yearSel <-  c(2019, 2020, 2021)
```

## 1. DATA

### a. Load cleaned data:

``` r
filename <- "output/nd_cleaned_long.csv" # name of the file where your output from step 2 is stored
nestData <- read_csv(filename) 
```

### b. Filter the data:

Now we can filter to our focal sites and focal species (Wilson’s Plover,
Least Tern, & Common Nighthawk), and then see how many camera nests
remain:

<span class="text-cat">nest data before filtering:</span>

<span class="text-cat">number of nests: 43</span>

<span class="text-cat">nests with fate “NA”: </span>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 20%;">

<pre style="margin: 1;"><code class=' text-cat '>
<span class='text-cat'>final fates: </span></code></pre>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 75%;">

<pre style="margin: 10;"><code class=' text-cat '>
  A  Ca   D   F   H  Hu   S   U U-H   W 
  1   3  10   2  16   1   5   3   1   1 </code></pre>

</div>

</div>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 20%;">

<pre style="margin: 1;"><code class=' text-cat '>
<span class='text-cat'>camera fates: </span></code></pre>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 75%;">

<pre style="margin: 10;"><code class=' text-cat '>
Ca  D  H Hu  S 
 2  2  7  1  2 </code></pre>

</div>

</div>

<span class="text-cat">number of camera nests: 28</span>

<span class="text-cat">camera nests with fate “NA”: </span>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 20%;">

<pre style="margin: 1;"><code class=' text-cat '>
<span class='text-cat'>species: </span></code></pre>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 75%;">

<pre style="margin: 10;"><code class=' text-cat '>
CONI LETE SNPL WIPL 
  13   23    1    6 </code></pre>

</div>

</div>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 20%;">

<pre style="margin: 1;"><code class=' text-cat '>
<span class='text-cat'>site: </span></code></pre>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 75%;">

<pre style="margin: 10;"><code class=' text-cat '>
BROW HBRB ROCK ROJH RUTE RUTW 
   2    4    2    2   13   20 </code></pre>

</div>

</div>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 20%;">

<pre style="margin: 1;"><code class=' text-cat '>
<span class='text-cat'>number of obs: </span></code></pre>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 75%;">

<pre style="margin: 10;"><code class=' text-cat '>
 1  2  3  4  5  6  7  8  9 
 1  9 13  4  5  2  2  3  4 </code></pre>

</div>

</div>

<span class="text-cat">nests w/ only 1 obs: 20009</span>

``` r
nestData <- nestData |>
  filter(nest!=20009) |>  # found egg, probably washed out of nest; only 1 obs
  filter(site %in% sitesSel) |>
  filter(species %in% sppSel) |>
  filter(year %in% yearSel)
```

<span class="text-cat">nest data after filtering:</span>

<span class="text-cat">number of nests: 32</span>

<span class="text-cat">nests with fate “NA”: </span>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 20%;">

<pre style="margin: 1;"><code class=' text-cat '>
<span class='text-cat'>final fates: </span></code></pre>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 75%;">

<pre style="margin: 10;"><code class=' text-cat '>
  A  Ca   D   F   H  Hu   S   U U-H   W 
  1   3   6   1  13   1   3   2   1   1 </code></pre>

</div>

</div>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 20%;">

<pre style="margin: 1;"><code class=' text-cat '>
<span class='text-cat'>camera fates: </span></code></pre>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 75%;">

<pre style="margin: 10;"><code class=' text-cat '>
Ca  D  H Hu  S 
 2  1  6  1  2 </code></pre>

</div>

</div>

<span class="text-cat">number of camera nests: 22</span>

<span class="text-cat">camera nests with fate “NA”: </span>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 20%;">

<pre style="margin: 1;"><code class=' text-cat '>
<span class='text-cat'>species: </span></code></pre>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 75%;">

<pre style="margin: 10;"><code class=' text-cat '>
CONI LETE WIPL 
   9   21    2 </code></pre>

</div>

</div>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 20%;">

<pre style="margin: 1;"><code class=' text-cat '>
<span class='text-cat'>site: </span></code></pre>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 75%;">

<pre style="margin: 10;"><code class=' text-cat '>
RUTE RUTW 
  13   19 </code></pre>

</div>

</div>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 20%;">

<pre style="margin: 1;"><code class=' text-cat '>
<span class='text-cat'>number of obs: </span></code></pre>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 75%;">

<pre style="margin: 10;"><code class=' text-cat '>
 2  3  4  5  6  7  8  9 
 6 10  2  4  2  2  2  4 </code></pre>

</div>

</div>

<span class="text-cat">nests w/ only 1 obs: </span>

## 2. VARIABLES

### a. Nest fates

Deal with UH/UF fate values (if UHisH is TRUE, make “U-H” into “H”, else
make it “U”; same with UFisF & “U-F”)

``` r
# if(any(nd$field_fate == "U-H" | nd$field_fate == "U-F")) nd <- uhf_calc(nd, debug=TRUE)
nestData$field_fate_2 <- nestData$field_fate
if (any(nestData$field_fate %in% c("U-H","U-F"))) nestData <- uhf_calc( nestData,
                                                                        UHisH=params$fate$UHisH,
                                                                        UFisF=params$fate$UFisF,
                                                                        debug=params$debug)

nestData$final_fate <- coalesce(nestData$cam_fate, nestData$field_fate_2)
```

### b. Date of fate classification

Deal with UH/UF fate values (if UHisH is TRUE, make “U-H” into “H”, else
make it “U”; same with UFisF & “U-F”)

``` r
# if(any(nd$field_fate == "U-H" | nd$field_fate == "U-F")) nd <- uhf_calc(nd, debug=TRUE)
nestData$fate_date <- nestData$k
```

### c. Calculate observation interval

After a visual check of the “long” dataframe, we will calculate the
observation interval (days between each nest observation). This requires
converting some numeric variables back to integers.

We also check which status observations have an obs interval of more
than 6 and which ones have no observation interval. If observation
strings failed to parse, for example, the observation interval could be
artificially lengthened.

In addition, we will create a column of T/F whether the observation in
that row is the final observation of the nest or not. If nests do not
have a value for k, this will be NA.

``` r
# is obs of same nest as prev row? 0 = yes; 1 = no
nestData$same_ID <- c(1, diff(nestData$nest)) # diff = lagged difference
subtractPrev <- function(day) day - dplyr::lag(day)  # lag = value from prev row

nestData$obs_int <- ifelse( nestData$same_ID== 0, subtractPrev(nestData$season_day), 0)

# if any obs int is greater than 6, check to make sure we didn't miss obs
longObs  <- unique(nestData$nest[nestData$obs_int > 6])         # which rows?
noObsInt <- unique(nestData$nest[is.na(nestData$obs_int)])
no_HD    <- unique(nestData$nest[is.na(nestData$estHD) & nestData$final_fate!="H"]) ## missing est. hatch date
```

<pre style="margin: 1;"><code class=' text-cat '>
<span class='text-cat'>>> nests with long observation interval (>6 days): 10013 10212 30008 30010</span></code></pre>

<span class="text-cat">\>\> nests missing observation interval: </span>

<span class="text-cat">\>\> nests missing estimated hatch date: </span>

### d. Estimate nest age

Now we can calculate the nest initiation dates using either the
estimated hatch date or, if not available, the actual hatch date (for
hatched nests). Before the calculation, we will see which nests do not
have an estimated or real hatch date, and correct any if possible.

To calculate the initiation dates, we need to convert the hatch dates
and fate classification dates to season-days. We create a function that
calculates the initiation date based on the incubation time of the
species.

We will calculate initiation date for all nests with estimated hatch
dates. Then for each remaining nest that has a value for fate date (no
NAs), we will ask a) did it hatch? and b) does it not have an initiation
date? If both are true, use the actual hatch date to calculate
initiation; if not, just fill in the previously calculated value of
init_date.

Finally, use the estimated initiation date to estimate nest age.

``` r
nestData$estHD     <- sday(nestData$estHD)     ## convert to season-days
```

    ## Warning: 5 failed to parse.

``` r
nestData$fate_date <- sday(nestData$k) ## convert to season-days
```

    ## Warning: 148 failed to parse.

``` r
nestData <- nestData |>
  mutate(init_date = init(estHD, species)) |>  ## calculate initation date
  mutate(final_obs = ifelse(last_obs==T, status, NA)) ## what is status on final obs?

nestData <- nestData |>
  mutate(init_date = ifelse(
    final_fate=="H" & !is.na(k) & is.na(init_date), 
    init(k, species),
    init_date))

initNA   <- unique( nestData$nest[which(is.na(nestData$init_date))]  )
kNA       <- unique(nestData$nest[which(is.na(nestData$k))])
nestData$nest_age <- nestData$k - nestData$init_date
```

<pre style="margin: 1;"><code class=' text-cat '>
<span class='text-cat'>>> nests missing initiation date (& therefore age): 30902</span></code></pre>

<span class="text-cat">\>\> nests missing k value: </span>

``` r
if(!exists("dataframes")) dir.create("dataframes")
```

    ## Warning in dir.create("dataframes"): 'dataframes' already exists

``` r
saveRDS(nestData, sprintf("dataframes/ndata_ungrouped.rds"))

saveRDS(grp_ndata(nestData), sprintf("dataframes/ndata_grouped.rds"))
```

    ## `summarise()` has regrouped the output.
    ## `mutate_if()` ignored the following grouping variables:
    ## ℹ Summaries were computed grouped by nest, site, and species.
    ## ℹ Output is grouped by nest and site.
    ## ℹ Use `summarise(.groups = "drop_last")` to silence this message.
    ## ℹ Use `summarise(.by = c(nest, site, species))` for ]8;;x-r-help:dplyr::dplyr_byper-operation grouping]8;; instead.
