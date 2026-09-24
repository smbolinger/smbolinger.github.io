---
title: "Clean & format nest data"
author: "Sarah Bolinger"
permalink: /projects/fate_glm/clean_format/
output: 
  md_document:
    variant: gfm
    preserve_yaml: TRUE
---

## R code for cleaning and formatting nest monitoring data for use in DSR analysis (logistic exposure, Program MARK, etc.)

This script walks the user through cleaning the observation data, with
steps along the way to view the data and make sure that a) the R code is
behaving the way we expect and b) everything was entered accurately into
the spreadsheet.

## Formatting:

Audubon data has observation dates as columns and nests as rows; need to
convert to one row per observation to run the models. Some of the
formatting is tedious because I have a lot of checks to make sure we are
removing/editing the right things.

First, load packages and create a function that will be used to convert
dates to “season-days”, or days since the beginning of the season (which
can be adjusted), and another to calculate initiation date (estimated
hatch date - incubation time for species).

``` r
library(stats)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)

# give all files created from this document a shared suffix that is unique to the specific run
now = format(Sys.time(), "%m%d_%H%M_")

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
```

------------------------------------------------------------------------

## Loading and cleaning the data ——————————————-

Load the data from one or more years and concatenate into a single
dataframe. I added year as an additional column to sort by/use as a
covariate, and added a year prefix to nest numbers.

Then, check the column names (here, I am printing them as “index: name”)
to see if any columns need to be removed.

``` r
fnames <- list(
  'data19' = 'nest_data/2019_sample_nests.csv', ## put commas after all but the last file name!
  'data20' = 'nest_data/2020_sample_nests.csv',
  'data21' = 'nest_data/2021_sample_nests.csv'
)

yrs <- c(2019,2020, 2021)

ndList <- lapply(seq_along(fnames), function(x) {
  ndata <- read_csv(fnames[[x]], skip=1, skip_empty_rows=TRUE, col_types=cols(.default="c"))
  ndata$year <- yrs[x]
  ndata$nest <- ndata$nest |> str_pad(width=4, side="left", pad="0" )
  ndata$nest <- paste0(x, ndata$nest)
  ndata <- ndata[,colSums(!is.na(ndata))>0] ## remove empty columns
  return(ndata)
  })

names(ndList)<- names(fnames)
allData <- as.data.frame(data.table::rbindlist(ndList, fill=TRUE))
```

<pre style="margin: 10;"><code class=' text-cat '> [1] 1: nest        2: site        3: species     4: lat         5: lon         6: prot        7: estHD      
 [8] 8: pic         9: cov_1m      10: cov_5m     11: ad_band    12: pred_type  13: i          14: j         
[15] 15: k          16: duration   17: final_obs  18: NOTES      19: camera     20: field_fate 21: cam_fate  
[22] 22: final_fate 23: fate_date  24: cam_notes  25: new_notes  26: 26-Apr     27: 29-Apr     28: 3-May     
[29] 29: 5-May      30: 6-May      31: 8-May      32: 9-May      33: 11-May     34: 12-May     35: 14-May    
[36] 36: 15-May     37: 17-May     38: 19-May     39: 20-May     40: 23-May     41: 24-May     42: 28-May    
[43] 43: year       44: j_cam      45: cam_diff   46: phot_loc   47: 13-May     48: 18-May     49: 25-May    
[50] 50: 27-May     51: 1-Jun      52: ch_band    53: k_cam      54: found      55: issue      56: 30-Apr    
[57] 57: 4-May      58: 26-May    
</code></pre>

Also extract any “notes” columns to file so you can check them later if
there are issues.

``` r
notesColumns <- c("NOTES", "cam_notes", "new_notes") ## type names of notes columns, in quotes!
notes <- subset(allData, select=notesColumns)
write.csv(notes, sprintf("output/field_notes_%s.csv", now))
```

### Remove & rearrange columns

Enter the names of the columns you know you want to remove into the
vector “rmNames” below, or the index of the column(s) into rmIndex, or
some combination of the two. I used a combination (index 8 is the column
“pic”). I renamed the columns in Excel to remove spaces, but you can use
column name repair to do this in R.

``` r
rmNames <- c( "ad_band", "ch_band", "NOTES", "found", "cam_diff", "cam_notes", "new_notes", "photo", "phot_loc","k-final?", "issue") # names of columns to be removed (in quotes!)
rmIndex <- c(8) ## or, index of columns to be removed (just as an example, I put one column here)

cols <- match(rmNames, names(allData)) # store the indices of chosen columns
cols <- c(cols[!is.na(cols)], rmIndex)

print(names(allData)[cols]) ## make sure you got the correct columns
```

<pre style="margin: 10;"><code class=' text-cat '> [1] "ad_band"   "ch_band"   "NOTES"     "found"     "cam_diff"  "cam_notes" "new_notes" "phot_loc"  "issue"    
[10] "pic"      
</code></pre>

If the printed names are correct, go ahead and remove the columns. Also,
move some of the information (non-date) columns to join the others.

``` r
mvNames <- c("year", "j_cam", "k_cam") # columns to move to beginning

nestdata <- subset(allData,select=-cols)            # remove extraneous columns
nestdata <- allData |>
  select(-all_of(cols)) |>            # remove extraneous columns
  relocate(all_of(mvNames), .after=k)         

print(names(nestdata))                               # check column names again!
```

<pre style="margin: 10;"><code class=' text-cat '> [1] "nest"       "site"       "species"    "lat"        "lon"        "prot"       "estHD"      "cov_1m"    
 [9] "cov_5m"     "pred_type"  "i"          "j"          "k"          "year"       "j_cam"      "k_cam"     
[17] "duration"   "final_obs"  "camera"     "field_fate" "cam_fate"   "final_fate" "fate_date"  "26-Apr"    
[25] "29-Apr"     "3-May"      "5-May"      "6-May"      "8-May"      "9-May"      "11-May"     "12-May"    
[33] "14-May"     "15-May"     "17-May"     "19-May"     "20-May"     "23-May"     "24-May"     "28-May"    
[41] "13-May"     "18-May"     "25-May"     "27-May"     "1-Jun"      "30-Apr"     "4-May"      "26-May"    
</code></pre>

### Format the columns

Column names in the Excel file are dates, representing days that nest
surveys occurred. The code below uses regular expressions (regex) to
extract the date strings for each column name. This gives us a list of
observation dates, and assigning the indices for these columns to
dateIndex allows us to easily target just the observation columns by
using data\[,dateIndex\].

Next, we will convert the names of the observation columns (in day-month
day-month-year format) to season-days using the “sdays” function, and
then add a character (I picked “d” for day) so that the column names are
easier for R to interpret (numeric column names can cause issues).

``` r
colNames <- names(nestdata)

dateIndex <- which( str_detect(colNames, '[0-9]{1,2}-[:LETTER:]{3}') ) ## observation columns - no year 
#dateIndex <- which( str_detect(colNames, '[0-9]{1,2}-[:LETTER:]{3}-[0-9]{2}') ) ## includes year
infoIndex <- seq(1,ncol(nestdata))[-dateIndex]  ## non-observation columns (info columns)

dates     <- names(nestdata)[dateIndex] # select names of observation columns 
julDates  <- sday(dates)                # convert the names to season-days
names(julDates)  <- dates               # also creates index of date:season-day 

newNames         <- c(names(nestdata)[infoIndex],paste0("d.", julDates))
```

<pre style="margin: 10;"><code class=' text-cat '> [1] "nest"       "site"       "species"    "lat"        "lon"        "prot"       "estHD"      "cov_1m"    
 [9] "cov_5m"     "pred_type"  "i"          "j"          "k"          "year"       "j_cam"      "k_cam"     
[17] "duration"   "final_obs"  "camera"     "field_fate" "cam_fate"   "final_fate" "fate_date"  "d.26"      
[25] "d.29"       "d.33"       "d.35"       "d.36"       "d.38"       "d.39"       "d.41"       "d.42"      
[33] "d.44"       "d.45"       "d.47"       "d.49"       "d.50"       "d.53"       "d.54"       "d.58"      
[41] "d.43"       "d.48"       "d.55"       "d.57"       "d.62"       "d.30"       "d.34"       "d.56"      
</code></pre>

If the new column names look correct, apply them here. Then, reorder the
observation columns by date.

``` r
names(nestdata) <- newNames            # apply the new column names!

dateOrder <- order(julDates)
colOrder <- c(infoIndex, dateOrder+length(infoIndex)) ## add back info column indices
colsInOrder <- names(nestdata)[colOrder]
nestdata <- nestdata[,colOrder]
```

<span class="text-cat">\>\> column names in date order:</span>
<pre style="margin: 10;"><code class=' text-cat '> [1] "nest"       "site"       "species"    "lat"        "lon"        "prot"       "estHD"      "cov_1m"    
 [9] "cov_5m"     "pred_type"  "i"          "j"          "k"          "year"       "j_cam"      "k_cam"     
[17] "duration"   "final_obs"  "camera"     "field_fate" "cam_fate"   "final_fate" "fate_date"  "d.26"      
[25] "d.29"       "d.30"       "d.33"       "d.34"       "d.35"       "d.36"       "d.38"       "d.39"      
[33] "d.41"       "d.42"       "d.43"       "d.44"       "d.45"       "d.47"       "d.48"       "d.49"      
[41] "d.50"       "d.53"       "d.54"       "d.55"       "d.56"       "d.57"       "d.58"       "d.62"      
</code></pre>

<span class="text-cat">\>\> should match rearranged columns:</span>
<pre style="margin: 10;"><code class=' text-cat '> [1] "nest"       "site"       "species"    "lat"        "lon"        "prot"       "estHD"      "cov_1m"    
 [9] "cov_5m"     "pred_type"  "i"          "j"          "k"          "year"       "j_cam"      "k_cam"     
[17] "duration"   "final_obs"  "camera"     "field_fate" "cam_fate"   "final_fate" "fate_date"  "d.26"      
[25] "d.29"       "d.30"       "d.33"       "d.34"       "d.35"       "d.36"       "d.38"       "d.39"      
[33] "d.41"       "d.42"       "d.43"       "d.44"       "d.45"       "d.47"       "d.48"       "d.49"      
[41] "d.50"       "d.53"       "d.54"       "d.55"       "d.56"       "d.57"       "d.58"       "d.62"      
</code></pre>

### Check and extract observation strings

Observations entered into the Excel sheet are not always formatted
uniformly. Generally they start with number of eggs or chicks observed,
but there is often other information as well. They are generally
strings.

To extract the relevant parts of the observations and cut out the rest,
we will use the stringr function str_detect. Use this code to view all
unique nest observation strings in the data, so you can add all relevant
observations to the string list later in the code.

To check that we extracted all observations, we can count the number of
unique strings and the total number of observations (cells with
information in them within the observation columns) and compare with the
data after extraction.

If the syntax is confusing, remember that these operations are
vectorized, so e.g. colSums is acting on ALL of the observation columns
(nestdata\[,dateIndex\]) when it sums the number of non-NA cells per
column, which produces another vector, and then sum is summing ALL of
the column sums in that 2nd vector.

In the full data I was working with, there were 485 unique strings!

Getting the strings with question marks helps separate out truly
uncertain nest fates from other uncertainties (like if observer was
unsure of nest age after floating eggs)

``` r
uniqueStr <- vector()          # vector to fill with unique strings

for (col in dateIndex) uniqueStr <- append(uniqueStr, na.omit(unique(nestdata[col]))) ## unique within columns
names(uniqueStr) <- names(nestdata[,dateIndex])
uniqueUnique <- unique(unlist(uniqueStr)) ## unique across all columns

obsPerCol <- colSums(!is.na(nestdata[,dateIndex])) # Number of observations in each observation column 
totalObs  <- sum(obsPerCol) # Total number of observations in data before extraction
question <- uniqueUnique[str_detect(uniqueUnique, "\\?")]
```

<span class="text-cat">Unique strings across all columns (72 strings
total): </span> <span class="text-cat">1E // 3E // 3E, 4d // 1E, 1d //
BON // 2E // 3E, 7d // failed, empty // cannot locate // 2E, 2-3d // 3E,
3-5d // nest not located // 2E, 2d // 3E, 3d // 3E, 4d, 1d, 1d // 3E,
cattle disturbance // high water, could not check // 3E, 8d // 2E, 3d //
2E, 4d // 1E, 4d // failed // did not see // 2E, 1d // D // 3E, BON //
BON, 3E // 3E, 5d // 2E, vertical at bottom // 3E, 1E pipped // 1E,
cracked BON active // 2C, 1E hatch day // ? // could not find // 0E,
failed // 3E, 15d, 15d, 7d // D, coyote // 2E, BON // 2E, BON.eggs
floated // F // 2E, 7-8d // 2C // 2E, ~10d // 1E, angled on bottom // 1E
washed? // 2C?, 1E // W, 0E // 0E, U // 0E, S // 2E, ~14d // 1E, hear
cheeping & little taps // 2E, eggs feel cold // 2E, starring // 2E, 12d
// 0E // 0E, W // 0C // 1E, 1C // hatch? // not found // 1C // not
checked // H; 2C // no chicks or parents // 2C, a few days old // hatch
day, 2C // Hatched // 2C, 1E // hatching, 3E // no stick, eggs, chicks
// 3C // 2C, hatch</span> <span class="text-cat">\>\> strings w/
question marks: ? // 1E washed? // 2C?, 1E // hatch?</span>

Save the strings for each column to file for easier viewing:

``` r
filename <- paste0("output/uniqueStr_perColumn_", now, ".txt") ## filename to store output
sink(filename)
md_print(uniqueStr)
sink()
```

Now, tell R which substrings to look for & extract. They can just be the
first part of what is written in the cell; what you want is to be as
general as possible so as to capture as many observations per specified
string as possible. In addition to making the observation strings more
uniform, extracting only these substrings also allows you to capture
only the essential information.

After you type in the strings, use str_extract() to extract the relevant
bits. Don’t use str_extract_all() - it will look for all matches within
a given string (“all” doesn’t refer to all the strings but all the
matches within the string). We want one match per string. The code tells
R to print the values of each observation column before and after
extraction so you can compare them.

The code should capture words regardless of capitalization (ex:
“\[Hh\]atch”). A couple have single letters instead of words (ex: “D”
could accidentally capture “DNF” or other), so look for the single
letters (“\bD\b”).

You can leave any of these vectors blank, but the order does matter.
Strings will be extracted for the first pattern they match (ex: “hatch?”
will be caught by the first pattern because it looks for question marks,
and not by the “hatch” pattern). All of the strings will be collapsed
into one giant string, and the function will extract the first match it
finds.

The leftover strings that do not match will help you determine which
substrings need to be added. For example, one cell has “hatch”
misspelled as “hatxh” so I can either fix that one cell or add a new
substring to capture “hatxh”.

``` r
## type strings here:
matchStrList <- list(
  "uncertain"   = c("^\\?","\\bU\\b","unk","Fail\\?", "failed\\?", "hatch\\?", "abandoned\\?"),
  "other"       = c( "not obs","not [Cc]heck","Didn't check"), ## you could choose to separate out "could not check" and "did not check"
  "hatch"       = c("\\bH\\b","[Hh]atch", "wet chick", "pip", "poops", "chick behavior","star","cheep"),
  "fail"        = c("[Ff]ail","\\bF\\b","\\bW\\b","\\bD\\b","Hu","[Dd]ep","coyote","[Aa]band","rotten","[Ww]ash","not viable","ants"),
  "inactive"    = c("0[A-Z]","nothing","no activity","no chicks or parents","empty"),
  "birdOnNest"  = c("nest behavior","[Bb][Oo][Nn]"),
  "missing"     = c("no stick", "not see","not f", "DNF", "DNL", "not locate"),
  "chickObs"    = "([1-9][Ee])*([1-9][Cc])([1-9][Ee])*", ## may or may not also include eggs
  "eggObs"      = "[1-9][[Ee]]"
)
matchStr <- sapply(matchStrList, function(x) paste(x, collapse="|"))

uniqueVal <- uniqueUnique
for (x in seq_along(matchStr)){ 
                # patt <- paste(matchStr[[x]], collapse="|")
                ind <- which(str_detect(uniqueVal,matchStr[x]))
                md_cat(sprintf(">> %s (pattern=\"%s\")",names(matchStr)[x],matchStr[x]), "\t>> matches:\n" )
                md_print(uniqueVal[ind])
                uniqueVal <- uniqueVal[-ind]
} 
```

<span class="text-cat">\>\> uncertain
(pattern=“^?\|unk\|Fail?\|failed?\|hatch?\|abandoned?”) \>\> matches:
</span>
<pre style="margin: 10;"><code class=' text-cat '>[1] "?"      "0E, U"  "hatch?"
</code></pre>

<span class="text-cat">\>\> other (pattern=“not obs\|not
\[Cc\]heck\|Didn’t check”) \>\> matches: </span>
<pre style="margin: 10;"><code class=' text-cat '>[1] "high water, could not check" "not checked"                
</code></pre>

<span class="text-cat">\>\> hatch (pattern=“\[Hh\]atch\|wet
chick\|pip\|poops\|chick behavior\|star\|cheep”) \>\> matches: </span>
<pre style="margin: 10;"><code class=' text-cat '>[1] "3E, 1E pipped"                   "2C, 1E hatch day"                "1E, hear cheeping & little taps"
[4] "2E, starring"                    "H; 2C"                           "hatch day, 2C"                  
[7] "Hatched"                         "hatching, 3E"                    "2C, hatch"                      
</code></pre>

<span class="text-cat">\>\> fail
(pattern=“\[Ff\]ail\|Hu\|\[Dd\]ep\|coyote\|\[Aa\]band\|rotten\|\[Ww\]ash\|not
viable\|ants”) \>\> matches: </span>
<pre style="margin: 10;"><code class=' text-cat '>[1] "failed, empty" "failed"        "D"             "0E, failed"    "D, coyote"     "F"             "1E washed?"   
[8] "W, 0E"         "0E, W"        
</code></pre>

<span class="text-cat">\>\> inactive (pattern=“0\[A-Z\]\|nothing\|no
activity\|no chicks or parents\|empty”) \>\> matches: </span>
<pre style="margin: 10;"><code class=' text-cat '>[1] "0E, S"                "0E"                   "0C"                   "no chicks or parents"
</code></pre>

<span class="text-cat">\>\> birdOnNest (pattern=“nest
behavior\|\[Bb\]\[Oo\]\[Nn\]”) \>\> matches: </span>
<pre style="margin: 10;"><code class=' text-cat '>[1] "BON"                    "3E, BON"                "BON, 3E"                "1E, cracked BON active"
[5] "2E, BON"                "2E, BON.eggs floated"  
</code></pre>

<span class="text-cat">\>\> missing (pattern=“no stick\|not see\|not
f\|DNF\|DNL\|not locate”) \>\> matches: </span>
<pre style="margin: 10;"><code class=' text-cat '>[1] "cannot locate"          "nest not located"       "did not see"            "could not find"        
[5] "not found"              "no stick, eggs, chicks"
</code></pre>

<span class="text-cat">\>\> chickObs
(pattern=“(\[1-9\]\[Ee\])*(\[1-9\]\[Cc\])(\[1-9\]\[Ee\])*”) \>\>
matches: </span>
<pre style="margin: 10;"><code class=' text-cat '>[1] "2C"                 "2C?, 1E"            "1E, 1C"             "1C"                 "2C, a few days old"
[6] "2C, 1E"             "3C"                
</code></pre>

<span class="text-cat">\>\> eggObs (pattern=“\[1-9\]\[\[Ee\]\]”) \>\>
matches: </span>
<pre style="margin: 10;"><code class=' text-cat '> [1] "1E"                     "3E"                     "3E, 4d"                 "1E, 1d"                
 [5] "2E"                     "3E, 7d"                 "2E, 2-3d"               "3E, 3-5d"              
 [9] "2E, 2d"                 "3E, 3d"                 "3E, 4d, 1d, 1d"         "3E, cattle disturbance"
[13] "3E, 8d"                 "2E, 3d"                 "2E, 4d"                 "1E, 4d"                
[17] "2E, 1d"                 "3E, 5d"                 "2E, vertical at bottom" "3E, 15d, 15d, 7d"      
[21] "2E, 7-8d"               "2E, ~10d"               "1E, angled on bottom"   "2E, ~14d"              
[25] "2E, eggs feel cold"     "2E, 12d"               
</code></pre>

``` r
strings <- paste(unlist(matchStr), sep = "|", collapse = "|") ## all strings in the order specified above
```

<span class="text-cat">\*\*\* Strings to search for (in order):</span>
<pre style="margin: 10;"><code class=' text-cat '>^\?|\bU\b|unk|Fail\?|failed\?|hatch\?|abandoned\?|not obs|not [Cc]heck|Didn't check|\bH\b|[Hh]atch|wet chick|pip|poops|chick behavior|star|cheep|[Ff]ail|\bF\b|\bW\b|\bD\b|Hu|[Dd]ep|coyote|[Aa]band|rotten|[Ww]ash|not viable|ants|0[A-Z]|nothing|no activity|no chicks or parents|empty|nest behavior|[Bb][Oo][Nn]|no stick|not see|not f|DNF|DNL|not locate|([1-9][Ee])*([1-9][Cc])([1-9][Ee])*|[1-9][[Ee]]NULL
</code></pre>

<span class="text-cat"> \>\>\>\>\>\> remaining strings with no match:
</span>

### Replace the strings in the observation columns

Now make replacement columns for all the observation columns (replace
strings with standardized values). Since we already have nest fate
stored in another column, we can replace the observations that include
fate, too. Save the old columns and replacement columns to file so you
can inspect them.

``` r
filepath1 <- paste0("output/col_repl_", now, ".txt")

replCols <- list()
notExtr <- list()
for (col in dateIndex) {
    replCols[[col]] = str_extract( nestdata[[col]], strings)
    oldCol <- nestdata[[col]]
    newCol <- replCols[[col]]
    ID     <- nestdata$nest
    Cols   <- as.data.frame(cbind(ID,oldCol, newCol))
    Cols   <- subset(Cols, !is.na(oldCol))
    
    if (any(is.na(Cols$newCol))){
      notExtr[[length(notExtr)+1]] <- paste(which(is.na(Cols$newCol)), col,sep=",")
    }
    column <- paste("\n column: ", col)
    write(column, file=filepath1, append=T)
     
    write.table(Cols, file = filepath1, 
                append = T,  sep=" || ",
                col.names=T, row.names = F) 
}
```

If the new columns look good on visual inspection, go ahead and run the
replacement command, then check which cells weren’t extracted. Then, as
a precaution, we will count the unique strings and total number of
observations in the dataframe post-extraction and compare it to the
values we got before (which we stored as obsPerCol and totalObs)

``` r
for (col in dateIndex) nestdata[[col]] = replCols[[col]] # replace old column with new

obsPCAfter  <- colSums(!is.na(nestdata[,dateIndex])) # total per column
totObsAfter <- sum(obsPCAfter)                      # total observations
diffC <- which(obsPerCol!=obsPCAfter ) # compare number of non-NA observations per column
```

<span class="text-cat">\>\> columns with obs that weren’t
replaced:</span> <span class="text-cat"> \>\> obs that weren’t extracted
(row,column):</span>

### Tidy up loose ends

Now we will remove any nests where final fate == NA, but we will keep a
tally and record which nests we removed, so we can see if any nests
needed for analysis are being removed and correct their final fate.

``` r
fateNA     = nestdata$nest[which(is.na(nestdata$final_fate) )] # store nest num
numNA_fate = length(fateNA)                                    # count them
nestdata   = nestdata[!is.na(nestdata$final_fate),]            # rm the rows
```

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
nestData  <- nestData %>% mutate(across(where(is.numeric),as.character))

nestData  <- nestData %>%  
pivot_longer(cols=starts_with("d."), 
             names_to  ="season_day", # each season-day (col) gets its own row
             names_prefix = "d.",   # remove prefix
             names_transform = as.integer,
             values_to = "status",    # individual obs move to status column
             values_drop_na = TRUE) 
```

After a visual check of the “long” dataframe, we will calculate the
observation interval (days between each nest observation). This requires
converting some numeric variables back to integers.

We also check which status observations have an obs interval of more
than 6 and which ones have no observation interval. In addition, we will
create a column of T/F whether the observation in that row is the final
observation of the nest or not. If nests do not have a value for k, this
will be NA.

``` r
# nest needs to be integer bc we subtract nest numbers later
nestData <- nestData %>% 
  mutate(across(c(i,j,k,nest,season_day,cov_1m,cov_5m), as.integer)) %>%
  mutate(across(c(i,j,k), function(x) x-10)) %>% # convert to season-days
  mutate(last_obs = k == season_day ) # is it the last observation of the nest?

# is obs of same nest as prev row? 0 = yes; 1 = no
nestData$same_ID <- c(1, diff(nestData$nest)) # diff = lagged difference
subtractPrev <- function(day) day - lag(day)  # lag = value from prev row

nestData$obs_int <- ifelse( nestData$same_ID== 0, subtractPrev(nestData$season_day), 0)

# if any obs int is greater than 6, check to make sure we didn't miss obs
longObs <- which(nestData$obs_int > 6)         # which rows?
noObsInt <- which(is.na(nestData$obs_int))
no_HD <- nestData$nest[which(is.na(nestData$estHD) & nestData$final_fate!="H")] ## missing est. hatch date
```

<span class="text-cat">\>\> nests with long observation interval: 27 64
77 159 164</span> <span class="text-cat"> \>\> nests missing observation
interval: 27 64 77 159 164</span> <span class="text-cat"> \>\> nests
missing estimated hatch date: 20009</span>

### Estimating nest age

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
nestData$fate_date <- sday(nestData$fate_date) ## convert to season-days

nestData <- nestData %>%
  mutate(init_date = init(estHD, species)) %>%  ## calculate initation date
  mutate(final_obs = ifelse(last_obs==T, status, NA)) ## T/F is obs the final obs?

nestData <- nestData %>%
  mutate(init_date = ifelse(
    final_fate=="H" & !is.na(k) & is.na(init_date), 
    init(k, species),
    init_date))

initNA   <- unique( nestData$nest[which(is.na(nestData$init_date))]  )
kNA       <- unique(nestData$nest[which(is.na(nestData$k))])
nestData$nest_age <- nestData$k - nestData$init_date
```

<span class="text-cat">\>\> nests missing initiation date (& therefore
age): 10007 20009 30902</span> <span class="text-cat"> \>\> nests
missing k value: </span>

------------------------------------------------------------------------

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
nestData1 <- nestData %>%
  filter(!is.na(k)) %>%
  mutate(status = as.character(status)) %>%
  mutate(status = case_when( 
    last_obs == TRUE & final_fate == "A"  ~ "fail",
    last_obs == TRUE & final_fate == "H"  ~ "hatch",
    season_day > k                        ~ NA,
    uncertain == TRUE & season_day < k    ~ "unknown",
    # TRUE                                  ~ status ))
    .default                              = status ))
# NB: none of the above will work if k==NA
```

<pre style="margin: 10;"><code class=' text-cat '>[1] nest 1: 1E|BON|NA|NA (4 obs)               nest 2: NA|NA|NA (3 obs)                  
[3] nest 3: 3E|3E|3E|NA|NA|NA|NA|NA (8 obs)    nest 4: 1E|BON|BON|2E|BON|NA|NA|NA (8 obs)
[5] nest 5: 2E|unknown|unknown|NA|NA (5 obs)   nest 6: NA|NA|NA (3 obs)                  
</code></pre>

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
nestData1 <- nestData1 %>% filter(!is.na(status))
alive = c('H', '3C', '2C', '1C', '3E', '2E', '1E', 'BON', 'chick behavior', 'nest behavior', 'hatch', 'poops') 

dead  = c('F', "fail", "0E", "D", "W", "empty", "0e", "no activity", "coyote", "no chicks or parents", "nothing", "no activity")

nestData1 <- nestData1 %>% 
  mutate( status = ifelse(status!="unknown", as.integer( status %in% alive ), status)) %>% 
  mutate(camera = ifelse(camera=="Y"|camera=="y", TRUE, FALSE))
```

<span class="text-cat">\>\> status strings before: 1E / BON / 3E / fail
/ 2C / 2E / not locate / ? / not check / hatch / not see / 0E / no stick
/ not f / 1C / 3C / Hatch / H / W / D / F / 0C / no chicks or parents /
hatch?</span> <span class="text-cat">\>\> number of nests before:
43</span> <span class="text-cat">\>\> status strings after replacing
uncertain: 1E / BON / NA / 3E / 2E / unknown / hatch</span>
<span class="text-cat">\>\> number of nests after replacing uncertain:
43</span> <span class="text-cat">\>\> status strings after replacing
active/inactive: 1 / unknown</span> <span class="text-cat">\>\> number
of nests after replacing active/inactive: 19</span>

### Check some things.

I grouped by nest to print these, since the data are still in long
format.

<span class="text-cat">\>\> observation histories of first 6 nests -
before: </span>
<pre style="margin: 10;"><code class=' text-cat '>[1] nest 1: 1E|BON|3E|fail (4 obs)                nest 2: 3E|BON|fail (3 obs)                  
[3] nest 3: 3E|3E|3E|3E|3E|BON|3E|2C (8 obs)      nest 4: 1E|BON|BON|2E|BON|BON|1E|fail (8 obs)
[5] nest 5: 2E|not locate|not locate|?|1E (5 obs) nest 6: 1E|not check|fail (3 obs)            
</code></pre>

<span class="text-cat">\>\> observation histories of first 6 nests
(1=active, 0=inactive): </span>
<pre style="margin: 10;"><code class=' text-cat '>[1] nest 1: 1|1 (2 obs)                           nest 2: 1|1|1 (3 obs)                        
[3] nest 3: 1|1|1|1|1 (5 obs)                     nest 4: 1|unknown|unknown (3 obs)            
[5] nest 5: 1|1|1|1 (4 obs)                       nest 6: 1|unknown|1 (3 obs)                  
</code></pre>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 15%;">

<span class="text-cat">final fates: </span>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 80%;">

<pre style="margin: 10;"><code class=' text-cat '>
 Ca   D   F   H  Hu   S   U U-H 
  1   2   1  10   1   2   1   1 
</code></pre>

</div>

</div>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 15%;">

<span class="text-cat">camera fates: </span>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 80%;">

<pre style="margin: 10;"><code class=' text-cat '>
 H Hu  S 
 2  1  1 
</code></pre>

</div>

</div>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 15%;">

<span class="text-cat">species: </span>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 80%;">

<pre style="margin: 10;"><code class=' text-cat '>
CONI LETE SNPL WIPL 
   4   10    1    4 
</code></pre>

</div>

</div>

<div class="columns"
style="display: flex; justify-content: space-between; align-items: flex-start;">

<div class="column" style="width: 15%;">

<span class="text-cat">site: </span>

</div>

<div class="column" style="width: 3% ;">

<!-- Spacer -->

</div>

<div class="column" style="width: 80%;">

<pre style="margin: 10;"><code class=' text-cat '>
HBRB ROJH RUTE RUTW 
   1    1    7   10 
</code></pre>

</div>

</div>

<span class="text-cat">nests with fate “NA”: </span>
<span class="text-cat">number of camera nests: 0</span>
<span class="text-cat">camera nests with fate “NA”: </span>

### Remove NA fates, filter and save.

Now we can filter to our focal sites and focal species (Wilson’s Plover,
Least Tern, & Common Nighthawk), and then see how many camera nests
remain:

``` r
nestData1 <- nestData1[!is.na(nestData1$final_fate),] # REMOVE NAs
nestData_fil <- nestData1 %>% 
  filter(site %in% c("RUTW", "RUTE", "HBRB")) %>%
  filter(species %in% c("CONI", "LETE", "WIPL")) 

camNests2 <- unique(nestData_fil$nest[which(nestData_fil$camera == TRUE)])
```

<span class="text-cat">\>\> number of camera nests after filtering:
3</span>

If all looks correct, write the final cleaned and filtered dataset
(still in long format) to csv:

``` r
if (file.exists("nest_data")==FALSE) dir.create("nest_data") 
filename2 <- paste0("output/nest_data_cleaned_",now,".csv")
write.csv(nestData1, filename2)
```
