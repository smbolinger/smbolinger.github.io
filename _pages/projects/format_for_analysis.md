---
title: "Format nest data for analysis"
author: "Sarah Bolinger"
permalink: /projects/fate_glm/analysis_data/
output: 
  md_document:
    variant: gfm
    preserve_yaml: TRUE
---

------------------------------------------------------------------------

``` r
params <- config::get()
library(tidyr)
library(dplyr)
```

## 0. Load data —————————————————————-

We import the grouped data (1 row per nest) for MARK & the fate GLM

    ## 
    ## <span class='text-cat'>importing from:dataframes/ndata_grouped.rds
    ## 
    ## </span>

## 1. Formatting for DSR models ————————————————

This data will become 2 datasets per DSR model: one w/ camera fates
included and one without. We can remove nests with unknown final
fate–all nests with (original) fates of “U” “U?” and “U-H”–which
includes nests that were not tracked to final fate and nests whose fate
was not clear based on the field data. We’ll keep track of which nests
were removed & how many total.

``` r
# 
totNest <- length(nestData$nest)
unkField <- nestData$nest[nestData$field_fate_2 %in% params$fates$unkFates] ## unknown fate
unkFinal <- nestData$nest[nestData$final_fate %in% params$fates$unkFates] ## unknown fate
kNA     <- nestData$nest[is.na(nestData$k)] # unknown k value
unknown <- union(unkField, unkFinal)
nestRm  <- union(unknown, kNA) # which nests being removed
```

<span class="text-cat">Nests to remove: 10048 20236 30030 30902</span>

For the base DSR dataset, select the columns with i,j, & k, plus nest
info and any vars that might be used as covariates (day of season, nest
age, etc.)

``` r
# ijk     <- c(i,j,k)
ndDSR   <- nestData |> # something is setting status to 0 for every row
  filter( !is.na(k), ## nests must have k value
    final_fate %in% params$fates$knownFates) |>  ## nests must have known fate 
  select(nest, site, species, status, cov_1m, cov_5m, i, j, k, year, j_cam, k_cam, nest_age, obs_int, season_day, camera, final_fate, field_fate_2)

ndDSRfield   <- ndDSR |> ## Create the field-only dataset
  filter(field_fate_2 %in% params$fates$knownFates)  ## all fates assigned in field must be known
```

<span class="text-cat">final fate frequency: </span>

A Ca D F H Hu S W 1 3 6 1 13 1 3 1

<span class='text-cat'>

Nests in DSR model:

</span> \[1\] 10010 10013 10033 10036 10038 10048 10212 10224 10252
20200 20202 20204 20205 20209 20217 20231 20240 30005 30008 \[20\] 30009
30010 30019 30404 30901 30903 30904 30907 30909 30922

<span class="text-cat">Nests with camera:</span>

## Program MARK

If we want to use the data in Program MARK, we need the following:

- nest ID
- values for i (first found), j (last seen active), and k (last
  observed)
- fate (0=hatched, 1=failed)
- (0/1) does any other nest have this exact history?
- covariate values, if applicable
- number of occurrences (nocc) total number of days

For hatched nests, j will often == k because the final observation is of
newly-hatched chicks.

``` r
ndMARKfld <- ndDSRfield |>                  # field data (no cameras) 
  #filter(camera == FALSE) |> 
  mutate( fate   = ifelse(final_fate=="H",0,1) )|>  # in MARK, 0=hatch, 1=fail
  group_by(nest) |>   
  select(nest, i, j, k, fate, site, species, cov_5m, nest_age) |>
  summarize(across(where(is.integer), last),
            across(where(is.numeric), last),
            across(where(is.character), first)
  ) |>
  ungroup() 
ndMARKfld$nocc <- max(ndDSR$season_day)
```

For the data including the cameras, keep “fate_date” because that will
become the new “k” (the camera lets us know exactly how long the nest
survived during the final observation interval instead of guessing)

``` r
ndMARKcam <- ndDSR |>                  # field + camera data
  mutate( fate   = ifelse(final_fate=="H",0,1) )|>  # in MARK, 0=alive, 1=dead
  group_by(nest) |>   
  # summarize(lastObs = last(status),
  select(nest, i, j, k, fate, site, species, cov_5m, nest_age) |>
  summarize(across(where(is.integer), last),
            across(where(is.numeric), last),
            across(where(is.character), first)
  ) |>
  ungroup()
ndMARKcam$nocc <- max(ndDSR$season_day)
```

<span class="text-cat"> The field and camera MARK data have the required
columns:</span>
<pre style="margin: 10;"><code class=' text-cat '># A tibble: 5 × 10
   nest     i     j     k  fate cov_5m nest_age site  species  nocc
  &lt;dbl&gt; &lt;dbl&gt; &lt;dbl&gt; &lt;dbl&gt; &lt;dbl&gt;  &lt;dbl&gt;    &lt;dbl&gt; &lt;chr&gt; &lt;chr&gt;   &lt;dbl&gt;
1 10010    26    44    47     1      5       30 RUTW  LETE       58
2 10013    29    50    50     0     15       31 RUTW  WIPL       58
3 10033    35    39    42     1     NA       16 RUTW  LETE       58
4 10036    35    58    58     0     NA       32 RUTW  LETE       58
5 10038    35    49    53     1     NA       32 RUTW  LETE       58</code></pre>

<pre style="margin: 10;"><code class=' text-cat '># A tibble: 5 × 10
   nest     i     j     k  fate cov_5m nest_age site  species  nocc
  &lt;dbl&gt; &lt;dbl&gt; &lt;dbl&gt; &lt;dbl&gt; &lt;dbl&gt;  &lt;dbl&gt;    &lt;dbl&gt; &lt;chr&gt; &lt;chr&gt;   &lt;dbl&gt;
1 10010    26    44    47     1      5       30 RUTW  LETE       58
2 10013    29    50    50     0     15       31 RUTW  WIPL       58
3 10033    35    39    42     1     NA       16 RUTW  LETE       58
4 10036    35    58    58     0     NA       32 RUTW  LETE       58
5 10038    35    49    53     1     NA       32 RUTW  LETE       58</code></pre>

Now save the MARK data to csv:

``` r
# now = "830_1826"
if(file.exists("model_data") == FALSE) dir.create("model_data")

filename <- paste0("model_data/MARK_data_field.csv")
write.csv(ndMARKfld, filename)

filename <- paste0("model_data/MARK_data_cam.csv")
write.csv(ndMARKcam, filename)
```

## Nest fate GLM

Format the original imported nest data for use in the nest fate GLM.
This will only be nests that had a camera, so we can compare the field
fate and the camera fate.

#### Possible response variables:

- is_u = was nest marked “Unknown” in the field? (T/F)

- HF_mis = was nest marked “Hatched” when true fate was failed, or vice
  versa? (T/F)

- misclass = was nest marked with the incorrect fate (could be failed
  nest assigned incorrect cause of failure, which does not affect DSR)?
  (T/F)

- how_mis = in what way was nest fate misclassified? (factor, requires
  multinomial GLM)

  - “C” = correctly classified
  - “M” = misclassified
  - “N” = was marked unknown in field, newly assigned fate from camera

### Add the new vars:

``` r
ndGLM1 <- nestData |> filter(camera==TRUE)
ndGLM1 <- as.data.frame(add_vars(ndGLM1, debug=params$debug))
# ndGLM1 <- ndGLM1 |> select(nest, species, field_fate, cam_fate, final_fate, estHD, k_adj, fdate, nest_age, obs_int, how_mis, HF_mis, misclass, is_u, hatchfail, c_hatchfail, fate, cfate)
ndGLM1 <- ndGLM1 |> select(nest, species, field_fate, cam_fate, final_fate, estHD, nest_age, obs_int, fate_date, how_mis, HF_mis, misclass, is_u)
# if(TRUE){
```

<span class="text-cat"> How many NAs are there in camera fate? 0 </span>

<span class="text-cat"> And in nest age? 0 </span>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 20%;">

<pre style="margin: 1;"><code class=' text-cat '>
<span class='text-cat'>
HF_mis variable:</span></code></pre>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 75%;">

<pre style="margin: 10;"><code class=' text-cat '>
0 1 
8 2 </code></pre>

</div>

</div>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 20%;">

<pre style="margin: 1;"><code class=' text-cat '>
<span class='text-cat'>
misclass variable:</span></code></pre>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 75%;">

<pre style="margin: 10;"><code class=' text-cat '>
0 1 
7 3 </code></pre>

</div>

</div>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 20%;">

<pre style="margin: 1;"><code class=' text-cat '>
<span class='text-cat'>how_mis variable:</span></code></pre>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 75%;">

<pre style="margin: 10;"><code class=' text-cat '>
C M N 
7 2 1 </code></pre>

</div>

</div>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 20%;">

<pre style="margin: 1;"><code class=' text-cat '>
<span class='text-cat'>is_u variable:</span></code></pre>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 75%;">

<pre style="margin: 10;"><code class=' text-cat '>
0 1 
9 1 </code></pre>

</div>

</div>

And save to csv:

``` r
if(!exists("dataframes")) dir.create("dataframes")
```

    ## Warning in dir.create("dataframes"): 'dataframes' already exists

``` r
saveRDS(ndGLM1, sprintf("dataframes/ndGLM1.rds"))
# saveRDS(ndGLM1, "ndGLM1.rds")
# saveRDS(nd, "nd.rds")
# rm(nd)
```
