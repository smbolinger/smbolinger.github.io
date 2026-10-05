---
title: "Nest status from strings"
author: "Sarah Bolinger"
permalink: /projects/fate_glm/nest_status/
output: 
  md_document:
    variant: gfm
    preserve_yaml: TRUE
---

``` r
library(stats)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)

sday <- function(date){ # converts dates to season-days starting at a given date
  date <- as.Date( lubridate::parse_date_time(date, c("%d-%b", "%m/%d/%Y", "%d-%b-%y"), exact = TRUE))
  return(lubridate::yday(date) - 90) # actual Julian date minus no. days from 1 Jan - 10 Apr
} 

init <- function(date, species) {
  inc = case_when( species == "WIPL" ~ 28, ## Wilson's Plover incubation time
                   species == "LETE" ~ 19, ## Least Tern incubation time
                   species == "CONI" ~ 16 ) ## Common Nighthawk incubation time
  return(date-inc) 
}

if (file.exists("output")==FALSE) dir.create("output")  ## create the output directory (if doesn't exist)
source("functions/md_other_fun.R")
```

### a. Load cleaned data:

<!-- ### Check some nest data if needed -->

### Make observations into rows

Now we will use dplyr’s “pivot_longer” to make each observation of each
nest a separate row, as is needed for many analyses. This will result in
a dataframe where each nest has as many rows as it has observations,
with format of: nest ID, nest info columns, season-day, status on that
day.

Beforehand, we will convert everything to class “character” (chr) so the
columns behave well together.

``` r
nestData <- as.data.frame(nestdata)
nestData  <- nestData |> mutate(across(where(is.numeric),as.character))

nestData  <- nestData |>  
  pivot_longer(cols=starts_with("d."), 
               names_to  ="season_day", # each season-day (col) gets its own row
               names_prefix = "d.",   # remove prefix
               names_transform = as.integer,
               values_to = "status",    # individual obs move to status column
               values_drop_na = TRUE)  |> 
  mutate(across(c(i,j,k,nest,season_day,cov_1m,cov_5m), as.integer)) |> # nest needs to be integer bc we subtract nest numbers later
  mutate(last_obs = k == season_day ) # is it the last observation of the nest?
```

<span class="text-cat">\>\> all unique values of status:</span>
<pre style="margin: 10;"><code class=' text-cat '> [1] "1E"                   "BON"                  "3E"                   "fail"                 "empty"               
 [6] "2C"                   "2E"                   "not locate"           "?"                    "not check"           
[11] "hatch"                "not see"              "0E"                   "no stick"             "not f"               
[16] "1C"                   "3C"                   "Hatch"                "H"                    "W"                   
[21] "D"                    "F"                    "0C"                   "no chicks or parents" "hatch?"              </code></pre>

## Dealing with special cases ——————————————————

We tried put the strings in an order such that these were minimized, but
there may still be the following issues:

1.  Abandoned nests often have eggs in the last observation; we need to
    make sure this isn’t picked up as “alive”
2.  Some hatched nests have ‘0E’ in the last observation; we need to
    change to ‘hatch’ so it’s not interpreted as nest failure
3.  Some chicks from hatched nests were recorded in the data sheet on
    dates subsequent to the “final observation” for nest fate - need to
    get rid of those so the nest doesn’t stay ‘alive’ past hatch

### Convert status of “lost” nests

“Not found” or similar does not necessarily mean failure. Sometimes
nests were lost & then found on later surveys.

``` r
notFound <- c("not locate", "not f", "nothing", "no activity", "not check", "not see", "not acc", "not obs", "DNL", "no stick")
nestData$uncertain <- nestData$status %in% notFound
lostnest <- nestData$nest[which(nestData$uncertain == T)]
```

### Convert status for special cases

To deal with the aforementioned special cases, we need to check the
final observations for each nest. Because we have recorded “k”, or the
day the nest was last checked, we know the day final fate was assigned.
So we make sure that:

1.  The last observation for all abandoned nests == “failed”
2.  The last observation for all hatched nests == “hatch”
3.  If observation date is past the value of k, status is NA (for the
    purpose of the nest model, which only counts up until hatch/failure)
    even if nest was checked after that.
4.  For everything else, use whatever value of status they already have.

``` r
nestData1 <- nestData |>
  filter(!is.na(k)) |>
  mutate(status = as.character(status)) |>
  mutate(status = case_when( 
    last_obs == TRUE & final_fate == "A"  ~ "fail",
    last_obs == TRUE & final_fate == "H"  ~ "hatch",
    season_day > k                        ~ NA, # should remove obs after nest reaches fate
    uncertain == TRUE & season_day < k    ~ "U",
    # TRUE                                  ~ status ))
    .default                              = status ))
# NB: none of the above will work if k==NA
```

### Convert status to active/inactive

Then we will convert all the observation strings to a simpler format:
nest is alive (TRUE or 1) or not (FALSE or 0).

We do this by specifying all the status strings that correspond to a
nest being active/“alive”, and then asking whether each status
observation is in that list or not, leaving us with a T/F for “is the
nest active that day?”

First, we need to determin which strings are classified as alive /
active.

We will save the nest status data of each nest to csv by grouping by
nest (collapsing all rows for nest X into one row) with status condensed
into a single list (since each nest is one row now) and keeping final
fate.

We also need to do something with the unknown statuses from nests that
were found again eventually (and still active) and account for
capitalization in the “camera” column (Y/N camera or not)

``` r
nestData1 <- nestData1 |> filter(!is.na(status))
alive = c('H', '3C', '2C', '1C', '3E', '2E', '1E', 'BON', 'chick behavior', 'nest behavior', 'hatch', 'poops') 

dead  = c('F', "fail", "0E", "D", "W", "empty", "0e", "no activity", "coyote", "no chicks or parents", "nothing", "no activity")

nestData1 <- nestData1 |> 
  mutate( status = ifelse(status!="U", as.integer( status %in% alive ), status)) |> 
  mutate(camera = ifelse(camera=="Y"|camera=="y", TRUE, FALSE))
```

<pre style="margin: 1;"><code class=' text-cat '>
<span class='text-cat'>>> number of nests before: 43</span></code></pre>

<span class="text-cat">\>\> status strings before: 1E / BON / 3E / fail
/ empty / 2C / 2E / not locate / ? / not check / hatch / not see / 0E /
no stick / not f / 1C / 3C / Hatch / H / W / D / F / 0C / no chicks or
parents / hatch?</span>

<span class="text-cat">\>\> number of nests after replacing uncertain:
43</span>

<span class="text-cat">\>\> status strings after replacing uncertain: 1E
/ BON / 3E / fail / hatch / 2E / U / ? / NA / 0E / no stick / W / D / F
/ hatch?</span>

<span class="text-cat">\>\> number of nests after replacing
active/inactive: 43</span>

<span class="text-cat">\>\> status strings after replacing
active/inactive: 1 / 0 / U</span>

### Check some things.

I grouped by nest to print these, since the data are still in long
format. Number of observations should be the same for each nest before
and after replacing status strings:

<span class="text-cat">\>\> observation histories of first 10 nests -
before: </span>
<pre style="margin: 10;"><code class=' text-cat '> [1] 10002: 1E|BON|3E|fail (k=39)(4 obs)                10003: 3E|3E|BON|empty (k=49)(4 obs)              
 [3] 10007: 3E|3E|3E|3E|3E|BON|3E|2C (k=45)(8 obs)      10010: 1E|BON|BON|2E|BON|BON|1E|fail (k=47)(8 obs)
 [5] 10013: 2E|not locate|not locate|?|1E (k=50)(5 obs) 10019: 1E|not check|fail (k=50)(3 obs)            
 [7] 10020: 2E|2E|fail (k=41)(3 obs)                    10033: 2E|2E|2E|fail (k=42)(4 obs)                
 [9] 10036: 1E|2E|BON|3E|BON|3E|hatch (k=58)(7 obs)     10038: 3E|not see|BON|BON|3E|0E (k=53)(6 obs)     </code></pre>

<span class="text-cat">\>\> observation histories of first 10 nests
(1=active, 0=inactive): </span>
<pre style="margin: 10;"><code class=' text-cat '> [1] 10002: 1|1|1|0 (k=39)(4 obs)                  10003: 1|1|1|1 (k=49)(4 obs)                 
 [3] 10007: 1|1|1|1|1|1|1|1 (k=45)(8 obs)          10010: 1|1|1|1|1|1|1|0 (k=47)(8 obs)         
 [5] 10013: 1|U|U|0|1 (k=50)(5 obs)                10019: 1|U|0 (k=50)(3 obs)                   
 [7] 10020: 1|1|0 (k=41)(3 obs)                    10033: 1|1|1 (k=42)(3 obs)                   
 [9] 10036: 1|1|1|1|1|1|1 (k=58)(7 obs)            10038: 1|U|1|1|1|0 (k=53)(6 obs)             </code></pre>

### Save to csv.

If all looks correct, write the final cleaned and filtered dataset
(still in long format) to csv:

``` r
# filename2 <- paste0("output/nest_data_cleaned_",now,".csv") ## if you want unique files each time you run it
filename2 <- paste0("output/nd_cleaned_long.csv")
write.csv(nestData1, filename2)
```
