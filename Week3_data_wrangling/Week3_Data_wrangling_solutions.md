# Data Wrangling Task -- Solutions
Rob Chavez  
4/25/2018  

Here is my attempt at Bradley's hack. \
\
**Note: Because I am not familiar with the real data set, it is hard to know if these results are 'correct' for his substantive questions. However, the point of exercise was to follow the instructions as best as possible and get wranglin'. Hopefully everyone found this challenging and informative.** \
\




```r
library(tidyverse)
```

```
## Loading tidyverse: ggplot2
## Loading tidyverse: tibble
## Loading tidyverse: tidyr
## Loading tidyverse: readr
## Loading tidyverse: purrr
## Loading tidyverse: dplyr
```

```
## Conflicts with tidy packages ----------------------------------------------
```

```
## filter(): dplyr, stats
## lag():    dplyr, stats
```

```r
library(psych)
```

```
## 
## Attaching package: 'psych'
```

```
## The following objects are masked from 'package:ggplot2':
## 
##     %+%, alpha
```
#Wrangling Hackathon
In this hackathon you will practice data wrangling by merging data from three csv files. One of these files is data collected in the lab after a dyadic interaction and the items of interest are BFI personality perceptions of the participant's interaction partner. All of the complete cases we want in our final data frame are in this data set. We will be connecting the self-report data collected prior to the lab study from two seperate dataframes in slightly different formats. You will be using different identifiers to connect each of these self-report to the lab data. At the end the dataframe will only have calculated BFI self-report and perception scores for the big five and HP. From there we will manipulate the dataframe so that it can be analyzed as dyads in lavaan, and then make it into tidy data(long).

##Calculating BFI-2 scores
As to maximize you time cleaning and wrangling the data, I have provided code that will calculate scores for each trait and save them to a new column in the dataframe. I did this with the scoreItems function in the psych package. It provides some reliability statitistics. 

##Data
In order to maximize the reproducability of the eventual analysis, begin by importing the raw data files directly from Qualtrics. All identifiable data has been removed or altered and all of the data has been simulated. However, besides a few missing columns it is exactly what qualtrics exports.

#Cleaning and Merging
##Start with the lab data. When this data frame is cleaned it will contain all the usable dyads in so much as we have the in-lab data for them.


```r
#import data---------------------------------------------------
inlab_raw<- read.csv("https://raw.githubusercontent.com/bthda/data_sci_wrangle_hack/master/sim_wrangle_lab.csv", stringsAsFactors = FALSE)

# remove first two lines and save to file
inlab_reduced <- inlab_raw[-(1:2),]

write.csv(inlab_reduced,"inlab_reduced.csv", row.names = FALSE, quote = FALSE)

# read new file
inlab_all_df <- read.csv("inlab_reduced.csv", stringsAsFactors = FALSE) 

# remove superfluous data frames 
rm(inlab_raw)
rm(inlab_reduced)

#remove pilot data, Dyads 1-14 ---------------------------
  inlab_df <- inlab_all_df
  
  inlab_df <- inlab_df %>% filter(Dyad_ID > 14) 

# exclude participants who knew each other before participating---------------------------------
#self-identified - already_know = 1 (they know the other participant) or well_known 1 or 2 (they have only seen them or interacted with them briefly)
inlab_df <- inlab_df %>% filter(already_know == 1 |
    already_know == 0 & well_known == 1 | 
    already_know == 0 & well_known == 2)
  
  #My RAs also identified dyads that were friends although they did not admit so in the survey. Remove dyads 17 and 25. --------------
inlab_df <- inlab_df %>% filter(Dyad_ID != 17, Dyad_ID !=25)
  
  
#exclude 2 participants for bringing emotional support animals to the study, dyads 110 and 124 ------------------------
  inlab_df <- inlab_df %>% filter(Dyad_ID != 110, Dyad_ID !=124)

#fixing some data entry errors: ----------------------------------------------------------------
#participant 45 in dyad 45 should be participant 23.
inlab_df$P_ID <- ifelse(inlab_df$P_ID == 45 & inlab_df$Dyad_ID == 45, 23, inlab_df$P_ID)

#participant 58 entered their last 4 wrong, should be 4058
inlab_df$ID <- ifelse(inlab_df$P_ID == 58, 4058, inlab_df$ID)

#remove 2 columns of "know each other" variables --------------------------------------
inlab_df <- inlab_df %>% select(-already_know, -well_known)

#identify any dyads with only one person and remove them ---------------------------------------------
# find loners
loners <- inlab_df %>% group_by(Dyad_ID) %>% count() %>% filter(n < 2)
loners <- loners$Dyad_ID

# create loner filter variable
inlab_df$loner_filter <- ifelse(inlab_df$Dyad_ID %in% loners == TRUE, 0,1)

# filter and remove the filter variable
inlab_df <- inlab_df %>% filter(loner_filter != 0) %>% select(-loner_filter)

#print the head of the dataframe ------------------------------
  # Note: I am just printing truncated output for less of a mess in the output
inlab_df %>% select(1:5) %>% head(5)
```

```
##       StartDate       EndDate Status Progress Duration..in.seconds.
## 1 4/11/17 17:20 4/11/17 17:52      0      100                  1948
## 2 4/11/17 17:20 4/11/17 17:53      0      100                  1967
## 3 4/12/17 15:18 4/12/17 15:35      0      100                  1016
## 4 4/12/17 15:17 4/12/17 15:40      0      100                  1367
## 5 4/14/17 11:20 4/14/17 12:38      0      100                  4686
```

##Next calculate BFI trait scores. For ease I have used the generic data in all of these functions, so please assign your dataframe to data below.


```r
data <-  inlab_df

mitems <- cbind(data$BFII_2, data$BFII_7, data$BFII_12, data$BFII_17,
                       data$BFII_22, data$BFII_27, data$BFII_32, 
                        data$BFII_37, data$BFII_42, data$BFII_47, 
                        data$BFII_52, data$BFII_57)
mkey <- c(1 , 1, -1, -1, -1, 1, 1, -1, -1, -1, 1, 1) 
agree_per <- scoreItems(mkey, mitems, totals = FALSE, ilabels = NULL,missing=TRUE, impute="none", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$agree_per <- agree_per$scores

agree_per
```

```
## Call: scoreItems(keys = mkey, items = mitems, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "none", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Standardized) Alpha:
##         A1
## alpha 0.45
## 
## Standard errors of unstandardized Alpha:
##       [,1]
## ASE   0.05
## 
## Standardized Alpha of observed scales:
##      [,1]
## [1,] 0.45
## 
## Average item correlation:
##            [,1]
## average.r 0.063
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.54
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise 0.81
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
## 
## Note that these are the correlations of the complete scales based on the correlation matrix,
##  not the observed scales based on the raw items.
##      [,1]
## [1,] 0.45
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$agree_per)
```

```
## [1] 0.2983917
```

```r
#Extraversion
# Ext = 1, 6, 11R, 16R, 21, 26R, 31R, 36R, 41, 46, 51R, 56
mitemse <- cbind(data$BFII_1, data$BFII_6, data$BFII_11, data$BFII_16,
                 data$BFII_21, data$BFII_26, data$BFII_31, data$BFII_36, 
                 data$BFII_41, data$BFII_46, data$BFII_51, data$BFII_56)
mkeye <- c(1 , 1, -1, -1, 1, -1, -1, -1, 1, 1, -1, 1)
extra_per <- scoreItems(mkeye, mitemse, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$extra_per <- extra_per$scores

extra_per
```

```
## Call: scoreItems(keys = mkeye, items = mitemse, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "mean", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Unstandardized) Alpha:
##         A1
## alpha 0.61
## 
## Standard errors of unstandardized Alpha:
##        [,1]
## ASE   0.038
## 
## Average item correlation:
##           [,1]
## average.r 0.11
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.71
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise  1.5
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
##      [,1]
## [1,] 0.61
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$extra_per)
```

```
## [1] 0.394866
```

```r
#openness
# Open = 5R, 10, 15, 20, 25R, 30R, 35, 40, 45R, 50R, 55R, 60
mitemso <- cbind(data$BFII_5, data$BFII_10, data$BFII_15, data$BFII_20,
                 data$BFII_25,data$BFII_30, data$BFII_35, data$BFII_40,
                 data$BFII_45, data$BFII_50, data$BFII_55, 
                 data$BFII_60)
mkeyo <- c(-1 , 1, 1, 1, -1, -1, 1, 1, -1, -1, -1, 1)
open_per <- scoreItems(mkeyo, mitemso, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$open_per <- open_per$scores

open_per
```

```
## Call: scoreItems(keys = mkeyo, items = mitemso, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "mean", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Unstandardized) Alpha:
##         A1
## alpha 0.56
## 
## Standard errors of unstandardized Alpha:
##        [,1]
## ASE   0.042
## 
## Average item correlation:
##            [,1]
## average.r 0.096
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.66
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise  1.3
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
##      [,1]
## [1,] 0.56
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$open_per)
```

```
## [1] 0.3994424
```

```r
#Conscientiousness
# Consc = 3R, 8R, 13, 18, 23R, 28R, 33, 38, 43, 48R, 53, 58R
mitemsc <- cbind(data$BFII_3, data$BFII_8, data$BFII_13, data$BFII_18, 
                 data$BFII_23, data$BFII_28, data$BFII_33, data$BFII_38, 
                 data$BFII_43, data$BFII_48, data$BFII_53, data$BFII_58)
mkeyc <- c(-1 , -1, 1, 1, -1, -1, 1, 1, 1, -1, 1, -1)
consc_per <- scoreItems(mkeyc, mitemsc, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$consc_per <- consc_per$scores

consc_per
```

```
## Call: scoreItems(keys = mkeyc, items = mitemsc, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "mean", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Unstandardized) Alpha:
##        A1
## alpha 0.6
## 
## Standard errors of unstandardized Alpha:
##        [,1]
## ASE   0.039
## 
## Average item correlation:
##           [,1]
## average.r 0.11
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.67
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise  1.5
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
##      [,1]
## [1,]  0.6
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$consc_per)
```

```
## [1] 0.3573913
```

```r
#Neuroticism
# Neur = 4R, 9R, 14, 19, 24R, 29R, 34, 39, 44R, 49R, 54, 59
mitemsn <- cbind(data$BFII_4, data$BFII_9, data$BFII_14, data$BFII_19,
                 data$BFII_24, data$BFII_29, data$BFII_34, data$BFII_39, 
                 data$BFII_44, data$BFII_49, data$BFII_54, 
                 data$BFII_59)
mkeyn <- c(1 , -1, 1, 1, -1, -1, 1, 1, -1, -1, 1, 1)
neuro_per <- scoreItems(mkeyn, mitemsn, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$neuro_per<- neuro_per$scores

neuro_per
```

```
## Call: scoreItems(keys = mkeyn, items = mitemsn, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "mean", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Unstandardized) Alpha:
##         A1
## alpha 0.18
## 
## Standard errors of unstandardized Alpha:
##        [,1]
## ASE   0.068
## 
## Average item correlation:
##            [,1]
## average.r 0.018
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.37
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise 0.22
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
##      [,1]
## [1,] 0.18
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$neuro_per)
```

```
## [1] 0.279744
```

```r
# HP = HP45r HP46r HP47r HP48r HP49r HP50 HP51 HP52
mitemshp <- cbind(data$BFII_HP45, data$BFII_HP46, data$BFII_HP47,
                  data$BFII_HP48, data$BFII_HP49, data$BFII_HP50,
                  data$BFII_HP51, data$BFII_HP52)
mkeyhp <- c(-1 , -1, -1, -1, -1, 1, 1, 1)
hp_per <- scoreItems(mkeyhp, mitemshp, totals = FALSE, ilabels = NULL, missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$hp_per <- hp_per$scores

hp_per
```

```
## Call: scoreItems(keys = mkeyhp, items = mitemshp, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "mean", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Unstandardized) Alpha:
##         A1
## alpha 0.55
## 
## Standard errors of unstandardized Alpha:
##        [,1]
## ASE   0.047
## 
## Average item correlation:
##           [,1]
## average.r 0.13
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.56
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise  1.2
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
##      [,1]
## [1,] 0.55
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$hp_per)
```

```
## [1] 0.4124353
```

```r
#Now create a dataframe with only Dyad_ID, P_ID, ID, and the BFI trait scores. --------------------------
inlab_bfi <- data %>% select(Dyad_ID, 
                             P_ID, 
                             ID, 
                             agree_per, 
                             extra_per,
                             open_per,
                             consc_per,
                             neuro_per,
                             hp_per)

##Be sure to check the class of the variables and make any adjustments necessary. ------------------------------
inlab_bfi[,4:9] <- apply(inlab_bfi[,4:9],2,as.numeric)

#We will use the P_ID and ID variables to match with the other dataframes. This makes it vital that they are of the same class, and in the same format.

#print the head of the dataframe
head(inlab_bfi)
```

```
##   Dyad_ID P_ID   ID agree_per extra_per open_per consc_per neuro_per
## 1      15   29 4475  3.833333  2.500000 3.833333  4.000000  3.000000
## 2      15   30 8337  4.333333  2.666667 3.750000  4.250000  2.416667
## 3      16   31  416  3.333333  3.166667 3.333333  2.750000  3.250000
## 4      16   32 6115  4.000000  2.500000 4.000000  4.333333  3.250000
## 5      18   36  771  3.250000  3.083333 3.583333  3.500000  3.000000
## 6      18   35 2690  3.666667  3.083333 3.416667  4.000000  2.916667
##   hp_per
## 1  4.125
## 2  4.500
## 3  2.750
## 4  4.000
## 5  3.125
## 6  3.875
```

##Next, import and score data from online part 1 and prescreen. 
###Online


```r
#import online data --------------------------------------
online_raw <- read.csv("https://raw.githubusercontent.com/bthda/data_sci_wrangle_hack/master/sim_wrangle_online.csv", stringsAsFactors = FALSE)

# remove first two lines and save to file
online_reduced <- online_raw[-(1:2),]

write.csv(online_reduced,"online_reduced.csv", row.names = FALSE, quote = FALSE)

# read new file
online_all_df <- read.csv("online_reduced.csv", stringsAsFactors = FALSE) 

# remove superfluous data frames 
rm(online_raw)
rm(online_reduced)


#score BFI-------------------------------
data <- online_all_df

#Agreeableness
#alabel <- c(data$BFIS_2, data$BFIS_7, data$BFIS_12, data$BFIS_17,
 #                      data$BFIS_22, data$BFIS_27, data$BFIS_32, 
  #                      data$BFIS_37, data$BFIS_42, data$BFIS_47, 
   #                     data$BFIS_52, data$BFIS_57)
mitems <- cbind(data$BFIS_2, data$BFIS_7, data$BFIS_12, data$BFIS_17,
                       data$BFIS_22, data$BFIS_27, data$BFIS_32, 
                        data$BFIS_37, data$BFIS_42, data$BFIS_47, 
                        data$BFIS_52, data$BFIS_57)
mkey <- c(1 , 1, -1, -1, -1, 1, 1, -1, -1, -1, 1, 1)
agree_self <- scoreItems(mkey, mitems, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$agree_self <- agree_self$scores

agree_self
```

```
## Call: scoreItems(keys = mkey, items = mitems, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "mean", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Unstandardized) Alpha:
##         A1
## alpha 0.77
## 
## Standard errors of unstandardized Alpha:
##        [,1]
## ASE   0.024
## 
## Average item correlation:
##           [,1]
## average.r 0.21
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.78
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise  3.3
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
##      [,1]
## [1,] 0.77
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$agree_self)
```

```
## [1] 0.5206568
```

```r
#Extraversion
# Ext = 1, 6, 11R, 16R, 21, 26R, 31R, 36R, 41, 46, 51R, 56
mitemse <- cbind(data$BFIS_1, data$BFIS_6, data$BFIS_11, data$BFIS_16,
                 data$BFIS_21, data$BFIS_26, data$BFIS_31, data$BFIS_36, 
                 data$BFIS_41, data$BFIS_46, data$BFIS_51, 
                 data$BFIS_56)
mkeye <- c(1 , 1, -1, -1, 1, -1, -1, -1, 1, 1, -1, 1)
extra_self <- scoreItems(mkeye, mitemse, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$extra_self <- extra_self$scores

extra_self
```

```
## Call: scoreItems(keys = mkeye, items = mitemse, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "mean", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Unstandardized) Alpha:
##         A1
## alpha 0.87
## 
## Standard errors of unstandardized Alpha:
##        [,1]
## ASE   0.016
## 
## Average item correlation:
##           [,1]
## average.r 0.36
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.89
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise  6.9
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
##      [,1]
## [1,] 0.87
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$extra_self)
```

```
## [1] 0.6988051
```

```r
#openness
# Open = 5R, 10, 15, 20, 25R, 30R, 35, 40, 45R, 50R, 55R, 60
mitemso <- cbind(data$BFIS_5, data$BFIS_10, data$BFIS_15, data$BFIS_20,
                 data$BFIS_25, data$BFIS_30, data$BFIS_35, data$BFIS_40, 
                 data$BFIS_45, data$BFIS_50, data$BFIS_55, data$BFIS_60)
mkeyo <- c(-1 , 1, 1, 1, -1, -1, 1, 1, -1, -1, -1, 1)
open_self <- scoreItems(mkeyo, mitemso, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$open_self <- open_self$scores

open_self
```

```
## Call: scoreItems(keys = mkeyo, items = mitemso, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "mean", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Unstandardized) Alpha:
##         A1
## alpha 0.82
## 
## Standard errors of unstandardized Alpha:
##        [,1]
## ASE   0.019
## 
## Average item correlation:
##           [,1]
## average.r 0.28
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.84
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise  4.7
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
##      [,1]
## [1,] 0.82
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$open_self)
```

```
## [1] 0.6022504
```

```r
#Conscientiousness
# Consc = 3R, 8R, 13, 18, 23R, 28R, 33, 38, 43, 48R, 53, 58R
mitemsc <- cbind(data$BFIS_3, data$BFIS_8, data$BFIS_13, data$BFIS_18,
                 data$BFIS_23, data$BFIS_28, data$BFIS_33, data$BFIS_38, 
                 data$BFIS_43, data$BFIS_48, data$BFIS_53, 
                 data$BFIS_58)
mkeyc <- c(-1 , -1, 1, 1, -1, -1, 1, 1, 1, -1, 1, -1)
consc_self <- scoreItems(mkeyc, mitemsc, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$consc_self <- consc_self$scores

consc_self
```

```
## Call: scoreItems(keys = mkeyc, items = mitemsc, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "mean", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Unstandardized) Alpha:
##         A1
## alpha 0.83
## 
## Standard errors of unstandardized Alpha:
##        [,1]
## ASE   0.019
## 
## Average item correlation:
##           [,1]
## average.r 0.29
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.85
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise  4.8
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
##      [,1]
## [1,] 0.83
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$consc_self)
```

```
## [1] 0.5791858
```

```r
#Neuroticism
# Neur = 4R, 9R, 14, 19, 24R, 29R, 34, 39, 44R, 49R, 54, 59
mitemsn <- cbind(data$BFIS_4, data$BFIS_9, data$BFIS_14, data$BFIS_19,
                 data$BFIS_24, data$BFIS_29, data$BFIS_34, data$BFIS_39, 
                 data$BFIS_44, data$BFIS_49, data$BFIS_54, data$BFIS_59)
mkeyn <- c(1 , -1, 1, 1, -1, -1, 1, 1, -1, -1, 1, 1)
neuro_self <- scoreItems(mkeyn, mitemsn, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$neuro_self<- neuro_self$scores

neuro_self
```

```
## Call: scoreItems(keys = mkeyn, items = mitemsn, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "mean", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Unstandardized) Alpha:
##        A1
## alpha 0.8
## 
## Standard errors of unstandardized Alpha:
##       [,1]
## ASE   0.02
## 
## Average item correlation:
##           [,1]
## average.r 0.26
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.86
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise  4.1
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
##      [,1]
## [1,]  0.8
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$neuro_self)
```

```
## [1] 0.6289565
```

```r
# HP = HP45r HP46r HP47r HP48r HP49r HP50 HP51 HP52
mitemshp <- cbind(data$BFIS_HP45, data$BFIS_HP46, data$BFIS_HP47,
                  data$BFIS_HP48, data$BFIS_HP49, data$BFIS_HP50,
                  data$BFIS_HP51, data$BFIS_HP52)
mkeyhp <- c(-1 , -1, -1, -1, -1, 1, 1, 1)
hp_self <- scoreItems(mkeyhp, mitemshp, totals = FALSE, ilabels = NULL, missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$hp_self <- hp_self$scores

hp_self
```

```
## Call: scoreItems(keys = mkeyhp, items = mitemshp, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "mean", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Unstandardized) Alpha:
##         A1
## alpha 0.59
## 
## Standard errors of unstandardized Alpha:
##       [,1]
## ASE   0.04
## 
## Average item correlation:
##           [,1]
## average.r 0.15
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.59
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise  1.4
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
##      [,1]
## [1,] 0.59
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$hp_self)
```

```
## [1] 0.4953497
```

```r
#create a dataframe with only ID and scored self-report BFI, remove duplicate ID's. This means the participant took part 1 or began part 1 multiple times. --------------------------------------------------------------------

# change BFI classes to numeric
data[,143:148] <- apply(data[,143:148],2,as.numeric)

# filter
online_df <- data %>% 
  select(ID, agree_self,  extra_self, open_self, consc_self, neuro_self, hp_self)

multiples <- online_df %>% group_by(ID) %>% count() %>%  filter(n > 1)
multiples <- multiples$ID

online_df$multiple <- ifelse(online_df$ID %in% multiples ==TRUE, 0, 1)

online_df <- online_df %>% filter(multiple != 0) %>% select(-multiple)

#print the head of the dataframe
head(online_df)
```

```
##     ID agree_self extra_self open_self consc_self neuro_self hp_self
## 1 9231   3.166667   3.000000  2.500000   3.250000   2.833333   3.875
## 2 2405   3.833333   2.333333  4.083333   3.000000   3.166667   4.000
## 3 9129   2.583333   4.416667  4.166667   3.500000   4.333333   3.375
## 4 7323   5.000000   3.500000  4.916667   4.416667   2.750000   4.500
## 5 1298   3.666667   3.916667  4.250000   3.833333   2.166667   3.625
## 6 3385   3.833333   2.250000  4.583333   2.416667   3.666667   3.500
```

###Prescreen


```r
#import prescreen data
prescreen_raw <- read.csv("https://raw.githubusercontent.com/bthda/data_sci_wrangle_hack/master/sim_wrangle_pre.csv", stringsAsFactors = FALSE)

# remove first two lines and save to file
prescreen_reduced <- prescreen_raw[-(1:2),]

write.csv(prescreen_reduced,"prescreen_reduced.csv", row.names = FALSE, quote = FALSE)

# read new file
prescreen_all_df <- read.csv("prescreen_reduced.csv", stringsAsFactors = FALSE) 

# remove superfluous data frames 
rm(prescreen_raw)
rm(prescreen_reduced)


#score prescreen data
data <-  prescreen_all_df
#Agreeableness
#alabel <- c(data$BFIS_2, data$BFIS_7, data$BFIS_12, data$BFIS_17,
 #                      data$BFIS_22, data$BFIS_27, data$BFIS_32, 
  #                      data$BFIS_37, data$BFIS_42, data$BFIS_47, 
   #                     data$BFIS_52, data$BFIS_57)
mitems <- cbind(data$BFIS_2, data$BFIS_7, data$BFIS_12, data$BFIS_17,
                       data$BFIS_22, data$BFIS_27, data$BFIS_32, 
                        data$BFIS_37, data$BFIS_42, data$BFIS_47, 
                        data$BFIS_52, data$BFIS_57)
mkey <- c(1 , 1, -1, -1, -1, 1, 1, -1, -1, -1, 1, 1)
agree_self <- scoreItems(mkey, mitems, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$agree_self <- agree_self$scores

agree_self
```

```
## Call: scoreItems(keys = mkey, items = mitems, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "mean", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Unstandardized) Alpha:
##        A1
## alpha 0.7
## 
## Standard errors of unstandardized Alpha:
##        [,1]
## ASE   0.048
## 
## Average item correlation:
##           [,1]
## average.r 0.16
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.74
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise  2.3
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
##      [,1]
## [1,]  0.7
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$agree_self)
```

```
## [1] 0.4736831
```

```r
#Extraversion
# Ext = 1, 6, 11R, 16R, 21, 26R, 31R, 36R, 41, 46, 51R, 56
mitemse <- cbind(data$BFIS_1, data$BFIS_6, data$BFIS_11, data$BFIS_16,
                 data$BFIS_21, data$BFIS_26, data$BFIS_31, data$BFIS_36, 
                 data$BFIS_41, data$BFIS_46, data$BFIS_51, 
                 data$BFIS_56)
mkeye <- c(1 , 1, -1, -1, 1, -1, -1, -1, 1, 1, -1, 1)
extra_self <- scoreItems(mkeye, mitemse, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$extra_self <- extra_self$scores

extra_self
```

```
## Call: scoreItems(keys = mkeye, items = mitemse, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "mean", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Unstandardized) Alpha:
##         A1
## alpha 0.82
## 
## Standard errors of unstandardized Alpha:
##        [,1]
## ASE   0.033
## 
## Average item correlation:
##           [,1]
## average.r 0.28
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.87
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise  4.6
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
##      [,1]
## [1,] 0.82
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$extra_self)
```

```
## [1] 0.6310312
```

```r
#openness
# Open = 5R, 10, 15, 20, 25R, 30R, 35, 40, 45R, 50R, 55R, 60
mitemso <- cbind(data$BFIS_5, data$BFIS_10, data$BFIS_15, data$BFIS_20,
                 data$BFIS_25, data$BFIS_30, data$BFIS_35, data$BFIS_40, 
                 data$BFIS_45, data$BFIS_50, data$BFIS_55, data$BFIS_60)
mkeyo <- c(-1 , 1, 1, 1, -1, -1, 1, 1, -1, -1, -1, 1)
open_self <- scoreItems(mkeyo, mitemso, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$open_self <- open_self$scores

open_self
```

```
## Call: scoreItems(keys = mkeyo, items = mitemso, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "mean", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Unstandardized) Alpha:
##         A1
## alpha 0.83
## 
## Standard errors of unstandardized Alpha:
##        [,1]
## ASE   0.033
## 
## Average item correlation:
##           [,1]
## average.r 0.28
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.86
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise  4.7
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
##      [,1]
## [1,] 0.83
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$open_self)
```

```
## [1] 0.5850484
```

```r
#Conscientiousness
# Consc = 3R, 8R, 13, 18, 23R, 28R, 33, 38, 43, 48R, 53, 58R
mitemsc <- cbind(data$BFIS_3, data$BFIS_8, data$BFIS_13, data$BFIS_18,
                 data$BFIS_23, data$BFIS_28, data$BFIS_33, data$BFIS_38, 
                 data$BFIS_43, data$BFIS_48, data$BFIS_53, 
                 data$BFIS_58)
mkeyc <- c(-1 , -1, 1, 1, -1, -1, 1, 1, 1, -1, 1, -1)
consc_self <- scoreItems(mkeyc, mitemsc, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$consc_self <- consc_self$scores

consc_self
```

```
## Call: scoreItems(keys = mkeyc, items = mitemsc, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "mean", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Unstandardized) Alpha:
##         A1
## alpha 0.85
## 
## Standard errors of unstandardized Alpha:
##        [,1]
## ASE   0.029
## 
## Average item correlation:
##           [,1]
## average.r 0.33
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.88
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise  5.9
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
##      [,1]
## [1,] 0.85
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$consc_self)
```

```
## [1] 0.6442895
```

```r
#Neuroticism
# Neur = 4R, 9R, 14, 19, 24R, 29R, 34, 39, 44R, 49R, 54, 59
mitemsn <- cbind(data$BFIS_4, data$BFIS_9, data$BFIS_14, data$BFIS_19,
                 data$BFIS_24, data$BFIS_29, data$BFIS_34, data$BFIS_39, 
                 data$BFIS_44, data$BFIS_49, data$BFIS_54, data$BFIS_59)
mkeyn <- c(1 , -1, 1, 1, -1, -1, 1, 1, -1, -1, 1, 1)
neuro_self <- scoreItems(mkeyn, mitemsn, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$neuro_self<- neuro_self$scores

neuro_self
```

```
## Call: scoreItems(keys = mkeyn, items = mitemsn, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "mean", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Unstandardized) Alpha:
##         A1
## alpha 0.77
## 
## Standard errors of unstandardized Alpha:
##        [,1]
## ASE   0.038
## 
## Average item correlation:
##           [,1]
## average.r 0.22
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.84
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise  3.3
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
##      [,1]
## [1,] 0.77
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$neuro_self)
```

```
## [1] 0.598936
```

```r
# HP = HP45r HP46r HP47r HP48r HP49r HP50 HP51 HP52
mitemshp <- cbind(data$BFIS_HP45, data$BFIS_HP46, data$BFIS_HP47,
                  data$BFIS_HP48, data$BFIS_HP49, data$BFIS_HP50,
                  data$BFIS_HP51, data$BFIS_HP52)
mkeyhp <- c(-1 , -1, -1, -1, -1, 1, 1, 1)
hp_self <- scoreItems(mkeyhp, mitemshp, totals = FALSE, ilabels = NULL, missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
data$hp_self <- hp_self$scores

hp_self
```

```
## Call: scoreItems(keys = mkeyhp, items = mitemshp, totals = FALSE, ilabels = NULL, 
##     missing = TRUE, impute = "mean", delete = TRUE, min = NULL, 
##     max = NULL, digits = 4)
## 
## (Unstandardized) Alpha:
##         A1
## alpha 0.61
## 
## Standard errors of unstandardized Alpha:
##        [,1]
## ASE   0.065
## 
## Average item correlation:
##           [,1]
## average.r 0.16
## 
##  Guttman 6* reliability: 
##          [,1]
## Lambda.6 0.64
## 
## Signal/Noise based upon av.r : 
##              [,1]
## Signal/Noise  1.5
## 
## Scale intercorrelations corrected for attenuation 
##  raw correlations below the diagonal, alpha on the diagonal 
##  corrected correlations above the diagonal:
##      [,1]
## [1,] 0.61
## 
##  In order to see the item by scale loadings and frequency counts of the data
##  print with the short option = FALSE
```

```r
sd(data$hp_self)
```

```
## [1] 0.5202694
```

```r
#create dataframe with only P_ID and self-report BFI scores ---------------------------------------------
# change BFI classes to numeric
data[,144:149] <- apply(data[,144:149],2,as.numeric)


prescreen_df <- data %>% select(P_ID,agree_self, extra_self, consc_self, neuro_self,hp_self)


#print the head of the dataframe

head(prescreen_df)
```

```
##   P_ID agree_self extra_self consc_self neuro_self hp_self
## 1  203   3.833333   3.166667   3.333333   2.833333   4.000
## 2  205   3.666667   3.500000   2.666667   3.000000   3.750
## 3  209   3.000000   3.500000   4.083333   2.416667   2.500
## 4  210   3.500000   2.416667   4.000000   3.583333   3.750
## 5  215   4.750000   2.666667   3.833333   3.500000   4.375
## 6  216   4.166667   4.416667   2.500000   3.083333   3.250
```

##Merge the three data frames


```r
#Merge the lab data and the 2 self-report dataframes (online, prescreen). 
#lab_data and online common variable is "ID""
#lab_data and prescreen data common variable is "P_ID" 
#I approached this by merging each of the self-report surveys with the lab data independently and then combining those dataframes. I am sure there are other (probably better) ways.

lab_online_join <- left_join(inlab_bfi, online_df)
```

```
## Joining, by = "ID"
```

```r
all_joined <- left_join(lab_online_join, prescreen_df) 
```

```
## Joining, by = c("P_ID", "agree_self", "extra_self", "consc_self", "neuro_self", "hp_self")
```

```r
#print the head of the dataframe
head(all_joined)
```

```
##   Dyad_ID P_ID   ID agree_per extra_per open_per consc_per neuro_per
## 1      15   29 4475  3.833333  2.500000 3.833333  4.000000  3.000000
## 2      15   30 8337  4.333333  2.666667 3.750000  4.250000  2.416667
## 3      16   31  416  3.333333  3.166667 3.333333  2.750000  3.250000
## 4      16   32 6115  4.000000  2.500000 4.000000  4.333333  3.250000
## 5      18   36  771  3.250000  3.083333 3.583333  3.500000  3.000000
## 6      18   35 2690  3.666667  3.083333 3.416667  4.000000  2.916667
##   hp_per agree_self extra_self open_self consc_self neuro_self hp_self
## 1  4.125   3.500000   3.333333  3.750000   3.666667   3.916667   3.625
## 2  4.500   3.666667   2.916667  4.666667   3.750000   3.000000   3.625
## 3  2.750   4.000000   2.833333  4.750000   4.333333   3.333333   5.000
## 4  4.000         NA         NA        NA         NA         NA      NA
## 5  3.125   3.666667   2.833333  3.500000   3.666667   3.333333   4.500
## 6  3.875   3.833333   4.583333  3.833333   3.750000   2.666667   3.125
```

##remove dyads with only 1 participant and repeating rows. Some participants completed the pre-measures twice. We will let R remove duplicate responses to avoid QRPs.


```r
#First remove any duplicate participant data
df_noduplicates <- all_joined

multiples <- df_noduplicates %>% group_by(ID) %>% count() %>%  filter(n > 1)
multiples <- multiples$ID

df_noduplicates$multiple <- ifelse(df_noduplicates$ID %in% multiples ==TRUE, 0, 1)

df_noduplicates <- df_noduplicates %>% filter(multiple != 0) %>% select(-multiple)


#remove any 1 person dyads
df_noloners <- df_noduplicates

multiples <- df_noloners %>% group_by(Dyad_ID) %>% count() %>%  filter(n != 2)
multiples <- multiples$Dyad_ID

df_noloners$multiple <- ifelse(df_noloners$Dyad_ID %in% multiples ==TRUE, 0, 1)

df_noloners <- df_noloners %>% filter(multiple != 0) %>% select(-multiple)

#remove multiple responses by same participant
 # This step should have been taken care of by the left_join merging commands above. 
print("Way ahead of you.")
```

```
## [1] "Way ahead of you."
```

```r
#remove the ID column
df_final <- df_noloners %>% select(-ID)

#print the head of the dataframe
head(df_final)
```

```
##   Dyad_ID P_ID agree_per extra_per open_per consc_per neuro_per hp_per
## 1      15   29  3.833333  2.500000 3.833333  4.000000  3.000000  4.125
## 2      15   30  4.333333  2.666667 3.750000  4.250000  2.416667  4.500
## 3      18   36  3.250000  3.083333 3.583333  3.500000  3.000000  3.125
## 4      18   35  3.666667  3.083333 3.416667  4.000000  2.916667  3.875
## 5      19   37  3.500000  2.000000 2.750000  4.083333  2.916667  3.125
## 6      19   38  3.916667  3.166667 3.666667  4.333333  2.916667  4.375
##   agree_self extra_self open_self consc_self neuro_self hp_self
## 1   3.500000   3.333333  3.750000   3.666667   3.916667   3.625
## 2   3.666667   2.916667  4.666667   3.750000   3.000000   3.625
## 3   3.666667   2.833333  3.500000   3.666667   3.333333   4.500
## 4   3.833333   4.583333  3.833333   3.750000   2.666667   3.125
## 5         NA         NA        NA         NA         NA      NA
## 6   3.333333   2.916667  3.583333   2.916667   3.916667   3.500
```

##Now that we have our final dataframe let's try some more advanced data manipulation.

To analyze the data in lavaan, I need to have a dataframe per trait and a row per dyad with the following columns ("dyad","p1trait", "p1percp2", "p2trait", "p2percp1") in each dataframe.


```r
#First create 6 data frames, one for each trait.--------------------------
agree_df <- df_final %>% select(Dyad_ID, P_ID,contains('agree'))
extra_df <- df_final %>% select(Dyad_ID, P_ID,contains('extra'))
open_df <- df_final %>% select(Dyad_ID, P_ID,contains('open'))
consc_df <- df_final %>% select(Dyad_ID, P_ID,contains('consc'))
neuro_df <- df_final %>% select(Dyad_ID, P_ID,contains('neuro'))
hp_df <- df_final %>% select(Dyad_ID, P_ID,contains('hp'))

#Next, use dplyr functions (spread, gather, etc.) to create a row for each dyad (the participant IDs do not matter because they are exchangable). The column names should be Dyad_ID p1_self, p1_perp2, p2_self, p2_per_p1. ----------------------------------------------------------------

# Here is an example with the agreeableness data frame

wide_agree_df <- agree_df %>% 
  select(-P_ID) %>% 
  arrange(Dyad_ID) %>% 
  mutate(pairs = rep(c("p1","p2"), length(agree_df$Dyad_ID)/2)) %>% 
  gather(trait, score, -Dyad_ID, -pairs) %>% 
  unite(person_trait, pairs, trait) %>% 
  spread(person_trait, score) %>% 
  select(dyad = Dyad_ID, 
         p1trait = p1_agree_self,
         p1percp2 = p1_agree_per,
         p2trait = p2_agree_self,
         p2percp1 = p2_agree_per)


#print the head of the dataframe ------------------------
head(wide_agree_df)
```

```
##   dyad  p1trait p1percp2  p2trait p2percp1
## 1   15 3.500000 3.833333 3.666667 4.333333
## 2   18 3.666667 3.250000 3.833333 3.666667
## 3   19       NA 3.500000 3.333333 3.916667
## 4   20       NA 4.083333 3.583333 3.583333
## 5   21 3.666667 3.916667 4.000000 3.333333
## 6   22 4.416667 3.333333 4.333333 4.000000
```

##Usually we want our data in tidy (long) form. Use the dplyr functions to create a dataframe that contains all of theinformation, but only has the following columns: P_ID, Dyad_ID, rating_type (self, perception), and trait (agree, extra, open, neuro, consc). 


```r
# make long data frame
long_df_final <- df_final %>% gather(var, score, -P_ID,-Dyad_ID) %>% 
  separate(var,c('trait','rating_type'), sep = "_")

#print the head of the dataframe --------------------------
head(long_df_final)
```

```
##   Dyad_ID P_ID trait rating_type    score
## 1      15   29 agree         per 3.833333
## 2      15   30 agree         per 4.333333
## 3      18   36 agree         per 3.250000
## 4      18   35 agree         per 3.666667
## 5      19   37 agree         per 3.500000
## 6      19   38 agree         per 3.916667
```


















