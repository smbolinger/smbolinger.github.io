---
title: "Clean & format nest data"
author: "Sarah Bolinger"
date: '2026-09-21'
layout: page
always_allow_html: true
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
convert to one row per observation to run the models

Some of the formatting is tedious because I have a lot of checks to make
sure we are removing/editing the right things.

First, load packages and create a function that will be used to convert
dates to “season-days”, or days since the beginning of the season (which
can be adjusted).

------------------------------------------------------------------------

## Loading and cleaning the data ——————————————-

Load the data from one or more years and concatenate into a single
dataframe. I added year as an additional column to sort by/use as a
covariate.

One could simply sort by year to account for the fact that there are
nests with the same ID number in multiple years, but I added a prefix to
nest ID for each year to differentiate them (from earliest to latest
year: “10”, “20”, and “30”, with extra zero-padding to make all nest
numbers the same length)

``` r
fnames <- list(
  'data21' = 'nest_data/2021_nests.csv',
  'data20' = 'nest_data/2020_nests.csv',
  'data19' = 'nest_data/2019_nests.csv'
)

yrs <- c(2019, 2020, 2021)
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

### Extract the “Notes” columns to file

``` r
notes <- data.frame(allData$nest, allData$NOTES, allData$cam_notes)
# print(sapply(head(notes,4), paste))
head(notes,4)
```

    ##   allData.nest                                                                                                                                                                                allData.NOTES
    ## 1        10001 May 19: no starring seen, didn't handle eggs. no parents in the area. May 26: no eggs in nest, parents are acting very territorial. likely hatched June 6: father in area acting territorial
    ## 2        10003                                                                                                                                                                                         <NA>
    ## 3        10004                                                                                                                                                                                         <NA>
    ## 4        10005           May 16: 1 egg left in nest, parents were acting territorial. May 19: no eggs left in nest, parents territorial but no chicks located. 5/22: two chicks seen in area running around
    ##   allData.cam_notes
    ## 1              <NA>
    ## 2              <NA>
    ## 3              <NA>
    ## 4              <NA>

``` r
if(file.exists("output")==FALSE) dir.create("output") 
write.csv(notes, sprintf("output/field_notes_%s.csv", now))
```

### Format the columns

First, we will work in steps to remove extraneous columns or empty
columns accidentally introduced during import. Empty rows will be dealt
with later.

Check the column names to see which ones you want to remove (also print
as index: name):

``` r
names(allData)
```

    ##   [1] "nest"        "site"        "species"     "lat"         "lon"         "prot"        "estHD"       "pic"         "cov_1m"      "cov_5m"      "ad_band"     "ch_band"     "pred_type"   "i"           "j"          
    ##  [16] "j_cam"       "k"           "k_cam"       "duration"    "final_obs"   "NOTES"       "camera"      "found"       "cam_diff"    "field_fate"  "field_fate2" "cam_fate"    "final_fate"  "issue"       "fate_date"  
    ##  [31] "phot_loc"    "cam_notes"   "new_notes"   "25-Apr"      "26-Apr"      "27-Apr"      "29-Apr"      "30-Apr"      "1-May"       "3-May"       "4-May"       "6-May"       "7-May"       "8-May"       "11-May"     
    ##  [46] "14-May"      "15-May"      "16-May"      "19-May"      "20-May"      "22-May"      "23-May"      "26-May"      "30-May"      "31-May"      "1-Jun"       "2-Jun"       "3-Jun"       "4-Jun"       "6-Jun"      
    ##  [61] "7-Jun"       "8-Jun"       "10-Jun"      "11-Jun"      "12-Jun"      "14-Jun"      "15-Jun"      "16-Jun"      "17-Jun"      "18-Jun"      "19-Jun"      "21-Jun"      "22-Jun"      "23-Jun"      "25-Jun"     
    ##  [76] "26-Jun"      "28-Jun"      "30-Jun"      "1-Jul"       "2-Jul"       "3-Jul"       "4-Jul"       "5-Jul"       "6-Jul"       "7-Jul"       "8-Jul"       "9-Jul"       "11-Jul"      "12-Jul"      "13-Jul"     
    ##  [91] "15-Jul"      "17-Jul"      "18-Jul"      "20-Jul"      "21-Jul"      "22-Jul"      "23-Jul"      "25-Jul"      "26-Jul"      "28-Jul"      "30-Jul"      "3-Aug"       "6-Aug"       "9-Aug"       "12-Aug"     
    ## [106] "16-Aug"      "19-Aug"      "23-Aug"      "26-Aug"      "30-Aug"      "year"        "k-final?"    "24-Apr"      "9-May"       "12-May"      "13-May"      "17-May"      "18-May"      "25-May"      "27-May"     
    ## [121] "28-May"      "29-May"      "5-Jun"       "9-Jun"       "13-Jun"      "20-Jun"      "24-Jun"      "27-Jun"      "29-Jun"      "10-Jul"      "14-Jul"      "16-Jul"      "19-Jul"      "27-Jul"      "31-Jul"     
    ## [136] "photo"       "init_date"   "cam_num"     "Column1"     "20-Apr"      "21-Apr"      "22-Apr"      "28-Apr"      "2-May"       "5-May"       "21-May"      "24-May"      "24-Jul"

``` r
print(sapply(seq_along(allData), function(x) paste(x,names(allData)[x], sep=": ")), quote=FALSE)                      # check column names
```

    ##   [1] 1: nest         2: site         3: species      4: lat          5: lon          6: prot         7: estHD        8: pic          9: cov_1m       10: cov_5m      11: ad_band     12: ch_band     13: pred_type  
    ##  [14] 14: i           15: j           16: j_cam       17: k           18: k_cam       19: duration    20: final_obs   21: NOTES       22: camera      23: found       24: cam_diff    25: field_fate  26: field_fate2
    ##  [27] 27: cam_fate    28: final_fate  29: issue       30: fate_date   31: phot_loc    32: cam_notes   33: new_notes   34: 25-Apr      35: 26-Apr      36: 27-Apr      37: 29-Apr      38: 30-Apr      39: 1-May      
    ##  [40] 40: 3-May       41: 4-May       42: 6-May       43: 7-May       44: 8-May       45: 11-May      46: 14-May      47: 15-May      48: 16-May      49: 19-May      50: 20-May      51: 22-May      52: 23-May     
    ##  [53] 53: 26-May      54: 30-May      55: 31-May      56: 1-Jun       57: 2-Jun       58: 3-Jun       59: 4-Jun       60: 6-Jun       61: 7-Jun       62: 8-Jun       63: 10-Jun      64: 11-Jun      65: 12-Jun     
    ##  [66] 66: 14-Jun      67: 15-Jun      68: 16-Jun      69: 17-Jun      70: 18-Jun      71: 19-Jun      72: 21-Jun      73: 22-Jun      74: 23-Jun      75: 25-Jun      76: 26-Jun      77: 28-Jun      78: 30-Jun     
    ##  [79] 79: 1-Jul       80: 2-Jul       81: 3-Jul       82: 4-Jul       83: 5-Jul       84: 6-Jul       85: 7-Jul       86: 8-Jul       87: 9-Jul       88: 11-Jul      89: 12-Jul      90: 13-Jul      91: 15-Jul     
    ##  [92] 92: 17-Jul      93: 18-Jul      94: 20-Jul      95: 21-Jul      96: 22-Jul      97: 23-Jul      98: 25-Jul      99: 26-Jul      100: 28-Jul     101: 30-Jul     102: 3-Aug      103: 6-Aug      104: 9-Aug     
    ## [105] 105: 12-Aug     106: 16-Aug     107: 19-Aug     108: 23-Aug     109: 26-Aug     110: 30-Aug     111: year       112: k-final?   113: 24-Apr     114: 9-May      115: 12-May     116: 13-May     117: 17-May    
    ## [118] 118: 18-May     119: 25-May     120: 27-May     121: 28-May     122: 29-May     123: 5-Jun      124: 9-Jun      125: 13-Jun     126: 20-Jun     127: 24-Jun     128: 27-Jun     129: 29-Jun     130: 10-Jul    
    ## [131] 131: 14-Jul     132: 16-Jul     133: 19-Jul     134: 27-Jul     135: 31-Jul     136: photo      137: init_date  138: cam_num    139: Column1    140: 20-Apr     141: 21-Apr     142: 22-Apr     143: 28-Apr    
    ## [144] 144: 2-May      145: 5-May      146: 21-May     147: 24-May     148: 24-Jul

Enter the names of the columns you know you want to remove into the
vector “rmNames” below. Also, move the year column from the very end of
the dataframe to a spot just before the observations, so it is with the
rest of the nest info. I renamed the columns in Excel to remove spaces,
but you can use column name repair to do this in R.

``` r
rmNames <- c("pic","photo", "ad_band", "ch_band", "pred_type", "NOTES", "found", "cam_diff", "cam_notes", "new_notes", "Column1", "init_date", "k-final?") # names of columns to be removed (in quotes!)
rmIndex <- c(139) ## or, index of columns to be removed (just as an example, I put one column here)

cols <- match(rmNames, names(allData)) # store the indices of chosen columns
cols <- c(cols, rmIndex)

## make sure you got the correct columns:
names(allData)[cols]
```

    ##  [1] "pic"       "photo"     "ad_band"   "ch_band"   "pred_type" "NOTES"     "found"     "cam_diff"  "cam_notes" "new_notes" "Column1"   "init_date" "k-final?"  "Column1"

If the printed names are correct, go ahead and remove the columns:

``` r
nestdata <- subset(allData,select=-cols)            # remove extraneous columns

nestdata <- nestdata |>
  relocate(year, .after=fate_date)         # move year column

names(nestdata)                               # check column names again!
```

    ##   [1] "nest"        "site"        "species"     "lat"         "lon"         "prot"        "estHD"       "cov_1m"      "cov_5m"      "i"           "j"           "j_cam"       "k"           "k_cam"       "duration"   
    ##  [16] "final_obs"   "camera"      "field_fate"  "field_fate2" "cam_fate"    "final_fate"  "issue"       "fate_date"   "year"        "phot_loc"    "25-Apr"      "26-Apr"      "27-Apr"      "29-Apr"      "30-Apr"     
    ##  [31] "1-May"       "3-May"       "4-May"       "6-May"       "7-May"       "8-May"       "11-May"      "14-May"      "15-May"      "16-May"      "19-May"      "20-May"      "22-May"      "23-May"      "26-May"     
    ##  [46] "30-May"      "31-May"      "1-Jun"       "2-Jun"       "3-Jun"       "4-Jun"       "6-Jun"       "7-Jun"       "8-Jun"       "10-Jun"      "11-Jun"      "12-Jun"      "14-Jun"      "15-Jun"      "16-Jun"     
    ##  [61] "17-Jun"      "18-Jun"      "19-Jun"      "21-Jun"      "22-Jun"      "23-Jun"      "25-Jun"      "26-Jun"      "28-Jun"      "30-Jun"      "1-Jul"       "2-Jul"       "3-Jul"       "4-Jul"       "5-Jul"      
    ##  [76] "6-Jul"       "7-Jul"       "8-Jul"       "9-Jul"       "11-Jul"      "12-Jul"      "13-Jul"      "15-Jul"      "17-Jul"      "18-Jul"      "20-Jul"      "21-Jul"      "22-Jul"      "23-Jul"      "25-Jul"     
    ##  [91] "26-Jul"      "28-Jul"      "30-Jul"      "3-Aug"       "6-Aug"       "9-Aug"       "12-Aug"      "16-Aug"      "19-Aug"      "23-Aug"      "26-Aug"      "30-Aug"      "24-Apr"      "9-May"       "12-May"     
    ## [106] "13-May"      "17-May"      "18-May"      "25-May"      "27-May"      "28-May"      "29-May"      "5-Jun"       "9-Jun"       "13-Jun"      "20-Jun"      "24-Jun"      "27-Jun"      "29-Jun"      "10-Jul"     
    ## [121] "14-Jul"      "16-Jul"      "19-Jul"      "27-Jul"      "31-Jul"      "cam_num"     "20-Apr"      "21-Apr"      "22-Apr"      "28-Apr"      "2-May"       "5-May"       "21-May"      "24-May"      "24-Jul"

Column names in the Excel file are dates, representing days that nest
surveys occurred. The code below uses regular expressions (regex) to
extract the date strings for each column name. This gives us a list of
observation dates, and assigning the indices for these columns to
dateIndex allows us to easily target just the observation columns by
using data\[,dateIndex\]

As is, the code extracts day-month; the commented code also extracts
year.

Next, we will convert the names of the observation columns (in day-month
day-month-year format) to season-days using the “sdays” function, and
then add a character (I picked “j” for Julian even though they aren’t
true Julian dates) so that the column names are easier for R to
interpret (numeric column names can cause issues).

``` r
colNames <- names(nestdata)

## observation columns:
dateIndex <- which( str_detect(colNames, '[0-9]{1,2}-[:LETTER:]{3}') ) # no year
#dateIndex <- which( str_detect(colNames, '[0-9]{1,2}-[:LETTER:]{3}-[0-9]{2}') )

## non-observation columns (info columns):
infoIndex <- seq(1,ncol(nestdata))[-dateIndex]  

dates     <- names(nestdata)[dateIndex] # select names of observation columns 
print(dates)
```

    ##   [1] "25-Apr" "26-Apr" "27-Apr" "29-Apr" "30-Apr" "1-May"  "3-May"  "4-May"  "6-May"  "7-May"  "8-May"  "11-May" "14-May" "15-May" "16-May" "19-May" "20-May" "22-May" "23-May" "26-May" "30-May" "31-May" "1-Jun" 
    ##  [24] "2-Jun"  "3-Jun"  "4-Jun"  "6-Jun"  "7-Jun"  "8-Jun"  "10-Jun" "11-Jun" "12-Jun" "14-Jun" "15-Jun" "16-Jun" "17-Jun" "18-Jun" "19-Jun" "21-Jun" "22-Jun" "23-Jun" "25-Jun" "26-Jun" "28-Jun" "30-Jun" "1-Jul" 
    ##  [47] "2-Jul"  "3-Jul"  "4-Jul"  "5-Jul"  "6-Jul"  "7-Jul"  "8-Jul"  "9-Jul"  "11-Jul" "12-Jul" "13-Jul" "15-Jul" "17-Jul" "18-Jul" "20-Jul" "21-Jul" "22-Jul" "23-Jul" "25-Jul" "26-Jul" "28-Jul" "30-Jul" "3-Aug" 
    ##  [70] "6-Aug"  "9-Aug"  "12-Aug" "16-Aug" "19-Aug" "23-Aug" "26-Aug" "30-Aug" "24-Apr" "9-May"  "12-May" "13-May" "17-May" "18-May" "25-May" "27-May" "28-May" "29-May" "5-Jun"  "9-Jun"  "13-Jun" "20-Jun" "24-Jun"
    ##  [93] "27-Jun" "29-Jun" "10-Jul" "14-Jul" "16-Jul" "19-Jul" "27-Jul" "31-Jul" "20-Apr" "21-Apr" "22-Apr" "28-Apr" "2-May"  "5-May"  "21-May" "24-May" "24-Jul"

``` r
julDates  <- sday(dates)                # convert the names to season-days
names(julDates)  <- dates               # also creates index of date:season-day 

# create a vector of all column names by combining the nest info columns #  (cols minus dateIndex cols) and the observation columns (with the "j_" pasted in front)
newNames         <- c(names(nestdata)[-dateIndex],paste0("j_", julDates))
print(newNames)
```

    ##   [1] "nest"        "site"        "species"     "lat"         "lon"         "prot"        "estHD"       "cov_1m"      "cov_5m"      "i"           "j"           "j_cam"       "k"           "k_cam"       "duration"   
    ##  [16] "final_obs"   "camera"      "field_fate"  "field_fate2" "cam_fate"    "final_fate"  "issue"       "fate_date"   "year"        "phot_loc"    "cam_num"     "j_15"        "j_16"        "j_17"        "j_19"       
    ##  [31] "j_20"        "j_21"        "j_23"        "j_24"        "j_26"        "j_27"        "j_28"        "j_31"        "j_34"        "j_35"        "j_36"        "j_39"        "j_40"        "j_42"        "j_43"       
    ##  [46] "j_46"        "j_50"        "j_51"        "j_52"        "j_53"        "j_54"        "j_55"        "j_57"        "j_58"        "j_59"        "j_61"        "j_62"        "j_63"        "j_65"        "j_66"       
    ##  [61] "j_67"        "j_68"        "j_69"        "j_70"        "j_72"        "j_73"        "j_74"        "j_76"        "j_77"        "j_79"        "j_81"        "j_82"        "j_83"        "j_84"        "j_85"       
    ##  [76] "j_86"        "j_87"        "j_88"        "j_89"        "j_90"        "j_92"        "j_93"        "j_94"        "j_96"        "j_98"        "j_99"        "j_101"       "j_102"       "j_103"       "j_104"      
    ##  [91] "j_106"       "j_107"       "j_109"       "j_111"       "j_115"       "j_118"       "j_121"       "j_124"       "j_128"       "j_131"       "j_135"       "j_138"       "j_142"       "j_14"        "j_29"       
    ## [106] "j_32"        "j_33"        "j_37"        "j_38"        "j_45"        "j_47"        "j_48"        "j_49"        "j_56"        "j_60"        "j_64"        "j_71"        "j_75"        "j_78"        "j_80"       
    ## [121] "j_91"        "j_95"        "j_97"        "j_100"       "j_108"       "j_112"       "j_10"        "j_11"        "j_12"        "j_18"        "j_22"        "j_25"        "j_41"        "j_44"        "j_105"

If the new column names look correct, apply them here and save to csv

``` r
names(nestdata) <- newNames            # apply the new column names!
print(names(nestdata))
```

    ##   [1] "nest"        "site"        "species"     "lat"         "lon"         "prot"        "estHD"       "cov_1m"      "cov_5m"      "i"           "j"           "j_cam"       "k"           "k_cam"       "duration"   
    ##  [16] "final_obs"   "camera"      "field_fate"  "field_fate2" "cam_fate"    "final_fate"  "issue"       "fate_date"   "year"        "phot_loc"    "cam_num"     "j_15"        "j_16"        "j_17"        "j_19"       
    ##  [31] "j_20"        "j_21"        "j_23"        "j_24"        "j_26"        "j_27"        "j_28"        "j_31"        "j_34"        "j_35"        "j_36"        "j_39"        "j_40"        "j_42"        "j_43"       
    ##  [46] "j_46"        "j_50"        "j_51"        "j_52"        "j_53"        "j_54"        "j_55"        "j_57"        "j_58"        "j_59"        "j_61"        "j_62"        "j_63"        "j_65"        "j_66"       
    ##  [61] "j_67"        "j_68"        "j_69"        "j_70"        "j_72"        "j_73"        "j_74"        "j_76"        "j_77"        "j_79"        "j_81"        "j_82"        "j_83"        "j_84"        "j_85"       
    ##  [76] "j_86"        "j_87"        "j_88"        "j_89"        "j_90"        "j_92"        "j_93"        "j_94"        "j_96"        "j_98"        "j_99"        "j_101"       "j_102"       "j_103"       "j_104"      
    ##  [91] "j_106"       "j_107"       "j_109"       "j_111"       "j_115"       "j_118"       "j_121"       "j_124"       "j_128"       "j_131"       "j_135"       "j_138"       "j_142"       "j_14"        "j_29"       
    ## [106] "j_32"        "j_33"        "j_37"        "j_38"        "j_45"        "j_47"        "j_48"        "j_49"        "j_56"        "j_60"        "j_64"        "j_71"        "j_75"        "j_78"        "j_80"       
    ## [121] "j_91"        "j_95"        "j_97"        "j_100"       "j_108"       "j_112"       "j_10"        "j_11"        "j_12"        "j_18"        "j_22"        "j_25"        "j_41"        "j_44"        "j_105"

``` r
write.csv(nestdata, sprintf("output/nestdata_cols_%s.csv", now), row.names = F)
dateOrder <- order(julDates)
## add back info column indices:
colOrder <- c(infoIndex, dateOrder+length(infoIndex))
print(colOrder)
```

    ##   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25 126 127 128 129 104  27  28  29 130  30  31  32 131  33  34 132  35  36  37 105  38 106 107  39  40  41 108
    ##  [53] 109  42  43 133  44  45 134 110  46 111 112 113  47  48  49  50  51  52 114  53  54  55 115  56  57  58 116  59  60  61  62  63  64 117  65  66  67 118  68  69 119  70 120  71  72  73  74  75  76  77  78  79
    ## [105]  80 121  81  82  83 122  84 123  85  86 124  87  88  89  90 135  91  92 125  93  94 126  95  96  97  98  99 100 101 102 103

``` r
cat("\n>> column names in order:\n")
```

    ## 
    ## >> column names in order:

``` r
names(nestdata)[colOrder]
```

    ##   [1] "nest"        "site"        "species"     "lat"         "lon"         "prot"        "estHD"       "cov_1m"      "cov_5m"      "i"           "j"           "j_cam"       "k"           "k_cam"       "duration"   
    ##  [16] "final_obs"   "camera"      "field_fate"  "field_fate2" "cam_fate"    "final_fate"  "issue"       "fate_date"   "year"        "phot_loc"    "j_112"       "j_10"        "j_11"        "j_12"        "j_14"       
    ##  [31] "j_15"        "j_16"        "j_17"        "j_18"        "j_19"        "j_20"        "j_21"        "j_22"        "j_23"        "j_24"        "j_25"        "j_26"        "j_27"        "j_28"        "j_29"       
    ##  [46] "j_31"        "j_32"        "j_33"        "j_34"        "j_35"        "j_36"        "j_37"        "j_38"        "j_39"        "j_40"        "j_41"        "j_42"        "j_43"        "j_44"        "j_45"       
    ##  [61] "j_46"        "j_47"        "j_48"        "j_49"        "j_50"        "j_51"        "j_52"        "j_53"        "j_54"        "j_55"        "j_56"        "j_57"        "j_58"        "j_59"        "j_60"       
    ##  [76] "j_61"        "j_62"        "j_63"        "j_64"        "j_65"        "j_66"        "j_67"        "j_68"        "j_69"        "j_70"        "j_71"        "j_72"        "j_73"        "j_74"        "j_75"       
    ##  [91] "j_76"        "j_77"        "j_78"        "j_79"        "j_80"        "j_81"        "j_82"        "j_83"        "j_84"        "j_85"        "j_86"        "j_87"        "j_88"        "j_89"        "j_90"       
    ## [106] "j_91"        "j_92"        "j_93"        "j_94"        "j_95"        "j_96"        "j_97"        "j_98"        "j_99"        "j_100"       "j_101"       "j_102"       "j_103"       "j_104"       "j_105"      
    ## [121] "j_106"       "j_107"       "j_108"       "j_109"       "j_111"       "j_112"       "j_115"       "j_118"       "j_121"       "j_124"       "j_128"       "j_131"       "j_135"       "j_138"       "j_142"

``` r
nestdata <- nestdata[,colOrder]
cat("\n>> should match rearranged columns:\n")
```

    ## 
    ## >> should match rearranged columns:

``` r
names(nestdata)
```

    ##   [1] "nest"        "site"        "species"     "lat"         "lon"         "prot"        "estHD"       "cov_1m"      "cov_5m"      "i"           "j"           "j_cam"       "k"           "k_cam"       "duration"   
    ##  [16] "final_obs"   "camera"      "field_fate"  "field_fate2" "cam_fate"    "final_fate"  "issue"       "fate_date"   "year"        "phot_loc"    "j_112"       "j_10"        "j_11"        "j_12"        "j_14"       
    ##  [31] "j_15"        "j_16"        "j_17"        "j_18"        "j_19"        "j_20"        "j_21"        "j_22"        "j_23"        "j_24"        "j_25"        "j_26"        "j_27"        "j_28"        "j_29"       
    ##  [46] "j_31"        "j_32"        "j_33"        "j_34"        "j_35"        "j_36"        "j_37"        "j_38"        "j_39"        "j_40"        "j_41"        "j_42"        "j_43"        "j_44"        "j_45"       
    ##  [61] "j_46"        "j_47"        "j_48"        "j_49"        "j_50"        "j_51"        "j_52"        "j_53"        "j_54"        "j_55"        "j_56"        "j_57"        "j_58"        "j_59"        "j_60"       
    ##  [76] "j_61"        "j_62"        "j_63"        "j_64"        "j_65"        "j_66"        "j_67"        "j_68"        "j_69"        "j_70"        "j_71"        "j_72"        "j_73"        "j_74"        "j_75"       
    ##  [91] "j_76"        "j_77"        "j_78"        "j_79"        "j_80"        "j_81"        "j_82"        "j_83"        "j_84"        "j_85"        "j_86"        "j_87"        "j_88"        "j_89"        "j_90"       
    ## [106] "j_91"        "j_92"        "j_93"        "j_94"        "j_95"        "j_96"        "j_97"        "j_98"        "j_99"        "j_100"       "j_101"       "j_102"       "j_103"       "j_104"       "j_105"      
    ## [121] "j_106"       "j_107"       "j_108"       "j_109"       "j_111"       "j_112.1"     "j_115"       "j_118"       "j_121"       "j_124"       "j_128"       "j_131"       "j_135"       "j_138"       "j_142"

### Check and extract observation strings

Observations entered into the Excel sheet are not always formatted
uniformly, although generally they start with number of eggs or chicks
observed. They are generally strings.

To extract the relevant parts of the observations and cut out the rest,
we will use the stringr function str_extract. Use this code to view all
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

Now, tell R which strings to look for. They can just be the first part
of what is written in the cell; what you want is to be as general as
possible so as to capture as many observations per specified string as
possible. In addition to making the observation strings more uniform,
this also allows you to capture only the essential information.

After you type in the strings, use str_extract() to extract the relevant
bits. Don’t use str_extract_all() - it will look for all matches within
a given string (“all” doesn’t refer to all the strings but all the
matches within the string).

We want one match per string.

The code tells R to print the values of each observation column before
and after extraction so you can compare them.

“Not” captures the following: “could not find”, “not found”, “could not
locate”, “not located”, “did not check”, “nothing”

A couple have single letters instead of words; F, Hu,

“wet chick” bc one observation is “adult with chicks - cam”

Or maybe just make all the unique non-egg non-chick strings into a list
for extraction

D and DNL/DNF are going to conflict - it will just extract “D” for all
of them

How to tell R to look for “D” with and without other characters after
it?

which cols are OK to be NA after extraction? \[39,40,59,61,74,76,99\] I
don’t see the NAs it says are there in cols 75, 77 also don’t know where
the extra 7 obs excluded come from.

You can leave any of these vectors blank. All that matters is that all
the strings you want are in “strings”

The order that we check matters.

``` r
replCols <- list()
notExtr <- list()
for( col in dateIndex ) {
    # write only relevant cells to the file - want to ignore NAs in oldCol, in case NAs intoduced in new col
    replCols[[col]] = str_extract( nestdata[[col]], strings)
    oldCol <- nestdata[[col]]
    newCol <- replCols[[col]]
    ID     <- nestdata$nest
    # Cols   <- cbind(ID, oldCol)
    # Cols   <- as.data.frame(cbind(Cols, newCol))
    Cols   <- as.data.frame(cbind(ID,oldCol, newCol))
    Cols   <- subset(Cols, !is.na(oldCol))
    
    notExtr[[length(notExtr)+1]] <- c(col,":", which(is.na(Cols$newCol)))
    column <- paste("\n column: ", col)
    write(column, file=filepath1, append=T)
     
    write.table(Cols, file = filepath1, 
                append = T,  sep=" || ",
                col.names=T, row.names = F) 
}
    # writeLines(c(col,': oldCol = ',paste(nestdata[[col]], sep=" "),'\n'), conn)
    # writeLines(c(col,': oldCol = ',t(nestdata[[col]])), conn)
    # paste(col,': oldCol = ',paste(nestdata[[col]], sep=" "),'\n')
    #browser()
    # Cols   <- merge(oldCol, newCol)
    #browser()
    #notExtr <- append(notExtr, c(col, c(which(is.na(Cols$newCol)))))
    #notExtr[[length(notExtr)+1]] <- c(names(nestdata)[col],":", which(is.na(Cols$newCol)))
    #append(notExtr, c(col, which(is.na(newCol))))
    #if(col==30) colNA <- which(is.na(newCol))
    # colNA <- whichV(is.na(Cols[1]))
    #browser()
    # if(col == 30) colNA <- which(is.na(Cols[8,V]))
    # if(col==30){
    #   colNA <- Cols[,unname(apply(Cols[8,], 1, function(x) which(is.na(x))))]
    # }
    #tf4[,unname(apply(tf4['INTENSITY2',],1,function(x) which(x>10)))]
    #for(c in nrow(Cols)) append(notExtr, c(which(is.na(Cols[c])),col))
    # for(c in nrow(Cols)) {
    #   append(notExtr, 
    #          c(Cols[,unname(apply(Cols[c,], 1, function(x) which(is.na(x))))],
    #            col))
    # }
      # append(notExtr, c(Cols[[1]][which(is.na(Cols[[3]]))], 
      #                   names(nestdata)[col]))
      # append(notExtr, c(Cols[which(is.na(Cols[[3]])),], 
      #                   names(nestdata)[col]))
      # append(notExtr, Cols[which(is.na(Cols$newCol)),])
                        
    
    # write(paste("\n column", names(oldCol), "\n", sep=" "), 
    #       file=filepath1, append=T)
    # write( names(oldCol), file=filepath1, append=T)
    # write(cat(' column', names(oldCol), ' \n'), file=filepath1, append=T)
    
    # column <- paste("\n column: ", col, "\n nest ID - old vs. new \n")
    # avoid warnings by not printing col names
    # but with col.names = F, you get "New names:" for each df
    
    
    # writeLines(Cols)
    # writeLines(c(col,': newCol = ',paste(newCol, sep=" "),'\n\n'), conn) # compare new & old
    # writeLines(c(col,': newCol = ',t(newCol)), conn) # compare new & old
    # paste(col,': newCol = ',paste(newCol, sep=" "),'\n\n')  # compare new & old
#write(unnest(as.data.frame(notExtr), file=sprintf("not_extracted_%s.txt",now)))
```

It says j_40, row 22 was not extracted, but that was NA before the
extraction! should have been removed

If the new columns look good on visual inspection, go ahead and run the
extraction command, then check which cells weren’t extracted:

``` r
for ( col in dateIndex ) nestdata[[col]] = replCols[[col]] # replace old column with new
```

``` r
uniqueStr2 <- vector()          # vector to fill with unique strings
for(col in dateIndex) uniqueStr2 <- append(uniqueStr2, unique(nestdata[col]))
uniqueStr2 <- unlist(uniqueStr2)
uniqueUnique2 <- unique(uniqueStr2)
stringE_after <- str_detect(uniqueUnique2, "[0-9][:LETTER:]")
stringNoE_after  <- uniqueUnique2[stringE_after == F]
write(stringNoE_after, sprintf("output/unique_noE_after%s.txt", now))
# this should match to what we end up with after extraction
```

Now, as a precaution, we will count the unique strings and total number
of observations in the dataframe post-extraction and compare it to the
values we got before (which we stored as obsPerCol and totalObs)

``` r
obsPCAfter  <- colSums(!is.na(nestdata[,dateIndex])) # total per column
totObsAfter <- sum(obsPCAfter)                      # total observations

differ <- obsPerCol == obsPCAfter
# comparing befor and after, which columns differ in the number of observations that == NA?
diffC <- which(differ==F) # ooo, it has names, that is cool

# 3o obs not picked up

# uniqueStr2 <- vector()    # to store unique strings from after extraction
# for(col in colIndex) uniqueStr2 <- append(uniqueStr2, unique(nestdata[col]))
# length(uniqueStr2)        # compare to number of unique str before extraction
```

### Tidy up loose ends and write to csv

Now we will remove any nests where final fate == NA, but we will keep a
tally and record which nests we removed, so we can see if any nests
needed for analysis are being removed and correct their final fate.

``` r
fateNA     = nestdata$nest[which(is.na(nestdata$final_fate) )] # store nest num
numNA_fate = length(fateNA)                                    # count them
nestdata   = nestdata[!is.na(nestdata$final_fate),]            # rm the rows

# we save it as a new df so we can compare the two
```

And now, write your cleaned data to csv so you can skip the preceding
steps and simply load it in:

``` r
#nestdataC <- apply(nestdata, 2, as.character)
if(file.exists("nest_data")==FALSE) dir.create("nest_data") 
filename_ <- paste0("nest_data/nest_data_combined_", now, ".csv")
write.csv(nestdata, filename_) # new file

#nestDataD <- as.data.frame(nestdata)
```

------------------------------------------------------------------------

## Dealing with special cases ——————————————————

### Next up: more formatting! We need to deal with special observation

cases, such as:

1.  Abandoned nests often have eggs in the last observation; we need to
    make sure this isn’t picked up as “alive”

2.  Some hatched nests have ‘0E’ in the last observation; we need to
    change to ‘hatch’ so it’s not interpreted as nest failure

3.  Some chicks from hatched nests were recorded in the data sheet on
    dates subsequent to the “final observation” for nest fate - need to
    get rid of those so the nest doesn’t stay ‘alive’ past hatch

First, load the data from the previous section if applicable :

### Check some nest data if needed

At any point in the process (until ~line 250, when the dataframe name
changes), if you’d like to look at the row for a specific nest and
compare it to e.g. the Excel file row, simply put the nest number in
quotes in the following code:

Or view the numbers of all nests currently in the dataframe:

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

#weirdStatus <- nestData[which(nestData$status %in% c(0:4)),] # which ones don't have "E"? 
nestData  <- nestData %>%  
pivot_longer(cols=starts_with("j_"), 
             names_to  ="season_day", # each season-day (col) gets its own row
             names_prefix = "j_",   # remove prefix
             names_transform = as.integer,
             values_to = "status",    # individual obs move to status column
             values_drop_na = TRUE) 
#weirdStatus <- nestData[which(nestData$status %in% c(0:4)),] # which ones don't have "E"? 
```

After a visual check of the “long” dataframe, we will calculate the
observation interval (days between each nest observation).

This requires converting some numeric variables back to integers.

We also check which status observations have an obs interval of more
than 6 and which ones have no observation interval.

In addition, we will create a column of T/F whether the observation in
that row is the final observation of the nest or not. If nests do not
have a value for k, this will be NA.

``` r
# nest needs to be integer bc we subtract nest numbers later
nestData <- nestData %>% 
  mutate(across(c(i,j,k,nest,season_day,cov_1m,cov_5m), as.integer)) %>%
  # mutate(across(c(i,j,k), function(x) x-13)) %>% # convert to season-days
  mutate(across(c(i,j,k), function(x) x-10)) %>% # convert to season-days
  # now it's -10 since I changed the sday function
  mutate(last_obs = k == season_day ) # is it the last observation of the nest?

# is obs of same nest as prev row? 0 = yes; 1 = no
nestData$same_ID <- c(1, diff(nestData$nest)) # diff = lagged difference
subtractPrev <- function(day) day - lag(day)  # lag = value from prev row

nestData$obs_int   <- ifelse(
  nestData$same_ID== 0, subtractPrev(nestData$season_day), 0)

# if any obs int is greater than 6, check to make sure we didn't miss obs
# longObs <- unique(nestData$nest[which(nestData$obs_int > 6)]) # which nests?
longObs <- which(nestData$obs_int > 6)         # which rows?
noObsInt <- which(is.na(nestData$obs_int))
```

### Estimating nest age

Now we can calculate the nest initiation dates using either the
estimated hatch date or, if not available, the actual hatch date (for
hatched nests). Before the calculation, we will see which nests do not
have an estimated or real hatch date, and correct any if possible.

``` r
no_HD <- nestData$nest[which(is.na(nestData$estHD) & nestData$final_fate!="H")]
```

To calculate the initiation dates, we need to convert the hatch dates
and fate classification dates to season-days.

We create a function that calculates the initiation date based on the
incubation time of the species.

We will calculate initiation date for all nests with estimated hatch
dates. Then for each remaining nest that has a value for fate date (no
NAs), we will ask a) did it hatch? and b) does it not have an initiation
date? If both are true, use the actual hatch date to calculate
initiation; if not, just fill in the previously calculated value of
init_date.

Finally, use the estimated initiation date to estimate nest age.

``` r
nestData$estHD     <- sday(nestData$estHD)
nestData$fate_date <- sday(nestData$fate_date)
# nestData$fdate     <- sday(nestData$k)
#k has already been transformed
```

``` r
init <- function(date, species) {
  
  inc = case_when( species == "WIPL" ~ 28,
                   species == "LETE" ~ 19,
                   species == "CONI" ~ 16 )
  return(date-inc) 
}
```

``` r
nestData <- nestData %>%
  mutate(init_date = init(estHD, species)) %>%
  mutate(final_obs = ifelse(last_obs==T, status, NA))
initNA   <- unique( nestData$nest[which(is.na(nestData$init_date))]  )

# nestData1 <- nestData %>%
nestData <- nestData %>%
  mutate(init_date = ifelse(
    # final_fate=="H" & !is.na(fate_date) & is.na(init_date), 
    final_fate=="H" & !is.na(k) & is.na(init_date), 
    init(k, species),
    init_date))

initNA1   <- unique( nestData$nest[which(is.na(nestData$init_date))]  )
kNA       <- unique(nestData$nest[which(is.na(nestData$k))])
# fdateNA   <- unique(nestData1$nest[which(is.na(nestData1$fdate))])
# why is this one so much longer than kNA?
# table(nestData$k)

# initNA1 <- initNA1 %>% filter(camera==T)

# nestData$nest_age <- nestData$fate_date - nestData$init_date
# nestData1$nest_age <- nestData1$k - nestData1$init_date
nestData$nest_age <- nestData$k - nestData$init_date
# ageNA1   <- unique( nestData1$nest[which(is.na(nestData1$nest_age))]  )
```

### Special cases

Now we can deal with all the aforementioned special cases at once,
making sure the model doesn’t mark abandoned nests with eggs as “alive”
or hatched nests with no eggs as “failed”, and getting rid of chick
observations past the “final observation” date as specified by k:

1.  The last observation for all abandoned nests == “failed”
2.  The last observation for all hatched nests == “hatch”
3.  If observation date is past the value of k, status is NA
4.  For everything else, use whatever value of status they already have.
5.  “Not found” or similar does not necessarily mean failure. Sometimes
    nests were lost and then found on later surveys.

``` r
unique(nestData$status)
```

    ##  [1] "2E"                   "3E"                   "0E"                   "1E"                   "D"                    "not locate"           "2C"                   "H"                    "W"                   
    ## [10] "1C"                   "BON"                  "U"                    "0C"                   "F"                    "Hu"                   "hatch"                "empty"                "coyote"              
    ## [19] "fail"                 "not f"                "stick missing"        "4E"                   "4C"                   "pipping"              "3C"                   "wet chick"            "no chicks or parents"
    ## [28] "?"                    "72"                   "65"                   "76"                   "68"                   "77"                   "87"                   "nothing"              "74"                  
    ## [37] "89"                   "98"                   "Hatch"                "58"                   "not check"            "73"                   "82"                   "84"                   "83"                  
    ## [46] "79"                   "92"                   "96"                   "103"                  "100"                  "Fail"                 "108"                  "Didn't check"         "116"                 
    ## [55] "nest behavior"        "not see"              "no stick"             "unk"                  "no activity"          "88"                   "poops"                "54"                   "55"                  
    ## [64] "chick behavior"       "not obs"              "78"                   "94"                   "102"                  "5"                    "eaten"                "depredated"

``` r
notFound <- c("not locate", "not f", "nothing", "no activity", "not check", "not see", "not acc", "not obs", "DNL", "U", "unk", "?", "no stick", "stick missing")
nestData$lost <- nestData$status %in% notFound
# nestData$found <- 
lostnest <- nestData$nest[which(nestData$lost == T)]
```

``` r
lostN <- nestData %>% 
  #filter(lost == TRUE & season_day < k) %>% # only select those obs
  filter(nest %in% lostnest) %>%
  group_by(nest) %>%
  summarize(status = list(status),
            final_fate = last(final_fate),
            final_obs = max(season_day),
            k         = last(k))

# kNA <- unique(nestData$nest[which(is.na(nesVtData$k))])
```

``` r
nestData1 <- nestData %>%
  # filter(!is.na(k)) %>%
  mutate(status = as.character(status)) %>%
  mutate(status = case_when( 
    last_obs == TRUE & final_fate == "A"  ~ "fail",
    last_obs == TRUE & final_fate == "H"  ~ "hatch",
    season_day > k                        ~ NA,
    lost == TRUE     & season_day < k     ~ "unknown",
    TRUE                                  ~ status ))

# NB: none of the above will work if k==NA

statusStr <- unique(nestData$status)
write(statusStr, sprintf("output/status_strings%s.txt", now))
statusStr
```

    ##  [1] "2E"                   "3E"                   "0E"                   "1E"                   "D"                    "not locate"           "2C"                   "H"                    "W"                   
    ## [10] "1C"                   "BON"                  "U"                    "0C"                   "F"                    "Hu"                   "hatch"                "empty"                "coyote"              
    ## [19] "fail"                 "not f"                "stick missing"        "4E"                   "4C"                   "pipping"              "3C"                   "wet chick"            "no chicks or parents"
    ## [28] "?"                    "72"                   "65"                   "76"                   "68"                   "77"                   "87"                   "nothing"              "74"                  
    ## [37] "89"                   "98"                   "Hatch"                "58"                   "not check"            "73"                   "82"                   "84"                   "83"                  
    ## [46] "79"                   "92"                   "96"                   "103"                  "100"                  "Fail"                 "108"                  "Didn't check"         "116"                 
    ## [55] "nest behavior"        "not see"              "no stick"             "unk"                  "no activity"          "88"                   "poops"                "54"                   "55"                  
    ## [64] "chick behavior"       "not obs"              "78"                   "94"                   "102"                  "5"                    "eaten"                "depredated"

``` r
# actN <- nestData$nest[which(nestData$status == "unknown")]
```

### Convert observations and fates

Then we will convert all the observation strings to a simpler format:
nest is alive (TRUE) or not (FALSE).

We do this by specifying all the status strings that correspond to a
nest being active/“alive”, and then asking whether each status
observation is in that list or not, leaving us with a T/F for “is the
nest active that day?”

First, we need to determin which strings are classified as “alive”

``` r
statusStr <- unique(nestData1$status)
statusStr
```

    ##  [1] "2E"            "3E"            "hatch"         "1E"            "0E"            "D"             "unknown"       "2C"            NA              "W"             "1C"            "BON"           "F"            
    ## [14] "Hu"            "coyote"        "fail"          "stick missing" "4E"            "pipping"       "wet chick"     "72"            "65"            "76"            "68"            "77"            "87"           
    ## [27] "74"            "89"            "?"             "98"            "3C"            "Hatch"         "empty"         "58"            "73"            "82"            "84"            "83"            "79"           
    ## [40] "92"            "96"            "103"           "100"           "Fail"          "108"           "Didn't check"  "116"           "nest behavior" "not f"         "no activity"   "not check"     "88"           
    ## [53] "poops"         "54"            "55"            "78"            "94"            "102"           "eaten"         "4C"            "not locate"

``` r
write(statusStr, sprintf("output/status_strings_after%s.txt", now))

# weirdStatus <- nestData[which(nestData$status %in% c(0:4)),] # which ones don't have "E"? 
# but I don't see any evidence in the column replace file
```

We will save the nest status data of each nest to csv by grouping by
nest (collapsing all rows for nest X into one row) with status condensed
into a single list (since each nest is one row now) and keeping final
fate.

We also need to do something with the unknown statuses from nests that
were found again eventually (and still active)

``` r
nestData1 <- nestData1 %>% filter(!is.na(status))
# alive = c('H', '3C', '2C', '1C', '3E', '2E', '1E', 'BON', 'chick behavior', 'nest behavior', '[Hh]atch', 'poops') 
alive = c('H', '3C', '2C', '1C', '3E', '2E', '1E', 'BON', 'chick behavior', 'nest behavior', 'hatch', 'poops') 

dead  = c('F', "fail", "0E", "D", "W", "empty", "0e", "no activity", "coyote", "no chicks or parents", "nothing", "no activity")

nestData1 <- nestData1 %>% 
  mutate( status = ifelse(status!="unknown", 
                          as.integer( status %in% alive ),
                          status
                          ))# %>%
  # group_by(nest) %>%
  # summarize(status = list(status))
  # 
unique(nestData1$final_fate)
```

    ##  [1] "H"      "U"      "D"      "S"      "F"      "W"      "Hu"     "U-H"    "U-D"    "U-F"    "A"      "0"      "Ca"     "#NAME?"

``` r
unique(nestData1$cam_fate)
```

    ## [1] NA   "D"  "H"  "F"  "S"  "A"  "Hu" "Ca"

``` r
nestStatus <- nestData1 %>% 
  group_by(nest) %>% 
  summarize(status = list(status), final_fate = last(final_fate))
# nestStatus <- nestData %>% 
#   group_by(nest) %>% 
#   summarize(status = list(status), final_fate = last(final_fate))

filename2 <- paste0("output/nest_status_", now, ".csv")

nestStatus %>%  
  mutate_if(is.list, ~paste(unlist(.), collapse = '|')) %>%
  write.csv(filename2, row.names = FALSE)

# you could view the status lists again by nest fate to see how they changed
```

Now we recode the final fates and the camera fates so they’re numeric,
and see if any were not converted correctly and are NA.

We also account for capitalization in the “camera” column (Y/N camera or
not)

We can also see which of the NA-fate nests have cameras, and then see
which nest R has marked as having a camera (and how many) in order to
make sure all camera nests have been picked up.

For now, we’ll label U-H as its own category, and add D-A to D and F-A
to F since we generally go with the first fate

``` r
nestData1 <- nestData1 %>% 
  mutate(ffate = final_fate, # create 2 vars that remained coded as letters
         cfate = cam_fate) %>%
  mutate(final_fate = case_match(final_fate,
                                 "H"   ~ 1, 
                                 "F"   ~ 0, 
                                 "F-A" ~ 0,
                                 "D"   ~ 2,
                                 "D-A" ~ 2,
                                 "S"   ~ 3,
                                 "W"   ~ 3,
                                 "A"   ~ 4,
                                 "Hu"  ~ 5,
                                 "Ca"  ~ 6,
                                 "U"   ~ 7,
                                 "U?"  ~ 7,
                                 "U-H" ~ 8,
                                 "U-F" ~ 9,
                                 "U-A" ~ 9,
                                 "U-D" ~ 9
         ),
         
         cam_fate = case_match(cam_fate,
                               "H"  ~ 1, 
                               "F"  ~ 0, 
                               "D"  ~ 2,
                               "S"  ~ 3,
                               "A"  ~ 4,
                               "Hu" ~ 5,
                               "Ca" ~ 6,
                               "U"  ~ 7#,
                               # NA   ~ 7
         ), 
         camera = ifelse(camera=="Y"|camera=="y", TRUE, FALSE)
         
  )

unique(nestData1$final_fate)
```

    ##  [1]  1  7  2  3  0  5  8  9  4 NA  6

``` r
unique(nestData1$cam_fate)
```

    ## [1] NA  2  1  0  3  4  5  6

### Check some things.

``` r
# fate_NA <- sum(is.na(nestData$final_fate))             # how many NA fate?
fate_NA <- unique(nestData1$nest[which(is.na(nestData1$final_fate))])  # which nests?
nestData1$nest[which(nestData1$camera == T & is.na(nestData1$final_fate))]
```

    ##   [1] 30033 30033 30033 30033 30033 30048 30048 30048 30048 30048 30048 30072 30072 30072 30072 30072 30122 30122 30122 30125 30125 30125 30126 30126 30126 30126 30127 30127 30127 30127 30127 30127 30127 30206 30206
    ##  [36] 30206 30206 30206 30206 30210 30210 30210 30210 30210 30210 30210 30211 30211 30211 30211 30211 30223 30223 30223 30223 30225 30225 30225 30225 30225 30225 30225 30225 30225 30229 30229 30229 30229 30231 30231
    ##  [71] 30231 30231 30231 30233 30233 30233 30233 30233 30233 30234 30234 30234 30234 30234 30234 30234 30235 30235 30235 30235 30235 30240 30240 30241 30241 30241 30241 30241 30241 30242 30242 30242 30256 30256 30256
    ## [106] 30256 30256 30257 30257 30257 30257 30257 30258 30258 30258 30258 30288 30288 30288 30288 30288 30289 30289 30295 30295 30298 30298 30298 30299 30299 30299 30299 30299 30299 30299 30299 30299 30307 30307 30308
    ## [141] 30308 30310 30310 30310 30310 30311 30311 30318 30318 30318 30318 30319 30319 30319 30319 30322 30322 30322 30323 30323 30323 30323 30323 30323 30324 30324 30324 30325 30325 30325 30325 30325 30326 30326 30326
    ## [176] 30326 30326 30327 30327 30327 30327 30329 30329 30329 30329 30329 30329 30329 30338 30338 30338 30338 30338 30339 30339 30339 30339 30340 30353 30353 30353 30353 30353 30353 30353 30355 30355 30355 30356 30356
    ## [211] 30356 30356 30357 30357 30357 30357 30358 30358 30359 30359 30359 30360 30360 30360 30360 30371 30371 30371 33211 33211 33211 33211 33211

``` r
nestData1 <- nestData1[!is.na(nestData1$final_fate),] # REMOVE NAs
```

``` r
camNests <- unique(nestData1$nest[which(nestData1$camera == T)]) # cam nest IDs
cat("number of camera nests:", length(camNests)) # how many nests with cameras, according to R?
```

    ## number of camera nests: 105

### Filter and save.

We can filter to the relevant sites and species, filter out 2021, and
then see how many camera nests remain:

``` r
nestData_fil <- nestData1 %>% 
  filter(site %in% c("RUTW", "RUTE")) %>%
  filter(species %in% c("CONI", "LETE")) %>%
  filter(year %in% c(2019,2020))

camNests2 <- unique(nestData_fil$nest[which(nestData_fil$camera == TRUE)])
cat("camera nests after filtering:",length(camNests2)) # how many camera nests once sites/species/years excluded?
```

    ## camera nests after filtering: 97

If all looks correct, write the final cleaned and formatted datset
(still in long format) to csv:

``` r
# not going to remove 2021 yet
# nestData2 <- nestData1 %>% 
  # filter(site %in% c("RUTW", "RUTE")) %>%
  # filter(species %in% c("CONI", "LETE"))
# now = "0830_722"
if(file.exists("nest_data")==FALSE) dir.create("nest_data") 
filename2 <- paste0("nest_data/nest_data_cleaned_",now,".csv")
# write.csv(nestData2, filename2)
write.csv(nestData1, filename2)
# write.csv(nestData[-1], filename2)
```

### Load the data again if necessary.

We can also format the nest data further for use in the GLM and the
Bayesian DSR model. First, load in the data and packages if necessary.
