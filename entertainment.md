Project 3
================
Kara Belknap & Cassio Monti
2022-10-29

<<<<<<< HEAD
-   <a href="#project-3" id="toc-project-3">Project 3</a>
    -   <a href="#report-for-data-channel--entertainment"
        id="toc-report-for-data-channel--entertainment">Report for Data Channel
        = entertainment</a>
    -   <a href="#monti---introduction" id="toc-monti---introduction">Monti -
        Introduction</a>
-   <a href="#relevance-of-topic-metrics-5-top-topics-according-to-lda"
    id="toc-relevance-of-topic-metrics-5-top-topics-according-to-lda">Relevance
    of topic metrics (5 top topics according to LDA).</a>
    -   <a href="#getting-started" id="toc-getting-started">Getting Started</a>
    -   <a href="#read-in-the-data" id="toc-read-in-the-data">Read in the
        Data</a>
    -   <a href="#select-data-for-appropriate-data-channel"
        id="toc-select-data-for-appropriate-data-channel">Select Data for
        Appropriate Data Channel</a>
    -   <a href="#summarizations-for-data-channel-entertainment"
        id="toc-summarizations-for-data-channel-entertainment">Summarizations
        for data channel entertainment</a>
        -   <a href="#subsetting-variables-of-interest"
            id="toc-subsetting-variables-of-interest">Subsetting Variables of
            Interest</a>
        -   <a href="#data-manipulation-for-statistics"
            id="toc-data-manipulation-for-statistics">Data manipulation for
            statistics</a>
        -   <a href="#belknap---summary-stats"
            id="toc-belknap---summary-stats">Belknap - Summary Stats</a>
        -   <a href="#monti---summary-stats" id="toc-monti---summary-stats">Monti -
            Summary Stats</a>
        -   <a href="#monti---graphs-3" id="toc-monti---graphs-3">Monti - Graphs
            (3)</a>
        -   <a href="#belknap---graphs-3" id="toc-belknap---graphs-3">Belknap -
            Graphs (3)</a>
    -   <a href="#modeling" id="toc-modeling">Modeling</a>
        -   <a href="#data-split" id="toc-data-split">Data Split</a>
        -   <a href="#belknap---linear-regression-model-explanation"
            id="toc-belknap---linear-regression-model-explanation">Belknap - Linear
            Regression Model Explanation</a>
        -   <a href="#monti---linear-regression-model"
            id="toc-monti---linear-regression-model">Monti - Linear Regression
            Model</a>
        -   <a href="#belknap---linear-regression-model"
            id="toc-belknap---linear-regression-model">Belknap - Linear Regression
            Model</a>
        -   <a href="#monti---ensemble-tree-based-model"
            id="toc-monti---ensemble-tree-based-model">Monti - Ensemble Tree-based
            Model</a>
        -   <a href="#belknap---ensemble-tree-based-model"
            id="toc-belknap---ensemble-tree-based-model">Belknap - Ensemble
            Tree-based Model</a>
        -   <a href="#belknap---random-forest-model--explanation"
            id="toc-belknap---random-forest-model--explanation">Belknap - Random
            Forest Model &amp; Explanation</a>
-   <a href="#now-rf-works" id="toc-now-rf-works">NOW RF Works</a>
    -   <a href="#monti---boosted-tree-model--explanation"
        id="toc-monti---boosted-tree-model--explanation">Monti - Boosted Tree
        Model &amp; Explanation</a>
    -   <a
        href="#comparison--conclusion---monti-or-belknap-whoever-doesnt-do-automation-of-r-markdown"
        id="toc-comparison--conclusion---monti-or-belknap-whoever-doesnt-do-automation-of-r-markdown">Comparison
        &amp; Conclusion - Monti or Belknap (whoever doesn’t do automation of R
        Markdown)</a>

=======
>>>>>>> aa58d189fd7eea466367118eb723237843afd97f
# Project 3

## Report for Data Channel = entertainment

## Monti - Introduction

<<<<<<< HEAD
The objective of this project is to analyze data

timedelta: Days between the article publication and the data set
acquisition (non-predictive)

Quality of keywords set of metrics:

kw_min_min: Worst keyword (min. shares) kw_max_min: Worst keyword (max.
shares) kw_avg_min: Worst keyword (avg. shares) kw_min_max: Best keyword
(min. shares) kw_max_max: Best keyword (max. shares) kw_avg_max: Best
keyword (avg. shares) kw_min_avg: Avg. keyword (min. shares) kw_max_avg:
Avg. keyword (max. shares) kw_avg_avg: Avg. keyword (avg. shares)

Mashable is one of the largest news websites from which the content of
all the articles published in 2013 and 2014 was extracted.

LDA means Latent Dirichlet Allocation algorithm and this algorithm was
applied to the data set to Marshable texts (known before publication) to
identify the 5 top relevant topics and then measure the closeness of the
current article to such topic.

# Relevance of topic metrics (5 top topics according to LDA).

LDA_00: Closeness to LDA topic 0 LDA_01: Closeness to LDA topic 1
LDA_02: Closeness to LDA topic 2 LDA_03: Closeness to LDA topic 3
LDA_04: Closeness to LDA topic 4

Day of week on which papers were published

weekday_is_monday: Was the article published on a Monday?
weekday_is_tuesday: Was the article published on a Tuesday?
weekday_is_wednesday: Was the article published on a Wednesday?
weekday_is_thursday: Was the article published on a Thursday?
weekday_is_friday: Was the article published on a Friday?
weekday_is_saturday: Was the article published on a Saturday?
weekday_is_sunday: Was the article published on a Sunday?

article content summary metrics:

n_tokens_title: Number of words in the title n_tokens_content: Number of
words in the content n_unique_tokens: Rate of unique words in the
content n_non_stop_words: Rate of non-stop words in the content
n_non_stop_unique_tokens: Rate of unique non-stop words in the content
num_hrefs: Number of links num_self_hrefs: Number of links to other
articles published by Mashable num_imgs: Number of images num_videos:
Number of videos average_token_length: Average length of the words in
the content num_keywords: Number of keywords in the metadata

=======
>>>>>>> aa58d189fd7eea466367118eb723237843afd97f
## Getting Started

Before we can begin our analysis, we must load in the following
packages:

``` r
library(tidyverse)
library(caret)
library(randomForest)
<<<<<<< HEAD
library(knitr)
=======
>>>>>>> aa58d189fd7eea466367118eb723237843afd97f
```

## Read in the Data

<<<<<<< HEAD
Using the data file `OnlineNewsPopularity.csv`, we will read in the data
=======
Using the datafile `OnlineNewsPopularity.csv`, we will read in the data
>>>>>>> aa58d189fd7eea466367118eb723237843afd97f
and add a new column corresponding to the type of data channel from
which the data was classified. The new variable will be called
`dataChannel`. Note that there are some rows that are unclassified
according to the six channels of interest and those are indicated by
`other`.

Once the data column is created, we can easily subset the data using the
<<<<<<< HEAD
`filter` function to create a new data set for each data channel. We
=======
`filter` function to create a new dataset for each data channel. We
>>>>>>> aa58d189fd7eea466367118eb723237843afd97f
removed the original `data_channel_is_*` columns as well as two
non-predictive columns `url` and `timedelta`.

``` r
<<<<<<< HEAD
rawData <- read_csv("../OnlineNewsPopularity.csv")

rawDataChannel <- rawData %>%
  mutate(dataChannel = ifelse(data_channel_is_lifestyle == 1, "lifestyle", 
                              ifelse(data_channel_is_entertainment == 1, "entertainment", 
                              ifelse(data_channel_is_bus == 1, "bus", 
                              ifelse(data_channel_is_socmed == 1, "socmed", 
                              ifelse(data_channel_is_tech == 1, "tech", 
                              ifelse(data_channel_is_world == 1, "world", 
                                     "other"))))))) %>%
  select(-data_channel_is_lifestyle, -data_channel_is_entertainment, 
         -data_channel_is_bus, -data_channel_is_socmed, -data_channel_is_tech,
         -data_channel_is_world, -url, -timedelta)
=======
rawData <- read_csv("OnlineNewsPopularity.csv")
```

    ## Rows: 39644 Columns: 61
    ## ── Column specification ─────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): url
    ## dbl (60): timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
rawDataChannel <- rawData %>%
  mutate(dataChannel = ifelse(data_channel_is_lifestyle == 1, "lifestyle", 
                              ifelse(data_channel_is_entertainment == 1, "entertainment", 
                                     ifelse(data_channel_is_bus == 1, "bus", 
                                            ifelse(data_channel_is_socmed == 1, "socmed", 
                                                   ifelse(data_channel_is_tech == 1, "tech", 
                                                          ifelse(data_channel_is_world == 1, "world", "other"))))))) %>%
  select(-data_channel_is_lifestyle, -data_channel_is_entertainment, -data_channel_is_bus, -data_channel_is_socmed, 
         -data_channel_is_tech, -data_channel_is_world, -url, -timedelta)
>>>>>>> aa58d189fd7eea466367118eb723237843afd97f


lifestyleData <- rawDataChannel %>%
  filter(dataChannel == "lifestyle")

entertainmentData <- rawDataChannel %>%
  filter(dataChannel == "entertainment")

busData <- rawDataChannel %>%
  filter(dataChannel == "bus")

socmedData <- rawDataChannel %>%
  filter(dataChannel == "socmed")

techData <- rawDataChannel %>%
  filter(dataChannel == "tech")

worldData <- rawDataChannel %>%
  filter(dataChannel == "world")
```

## Select Data for Appropriate Data Channel

To select the appropriate data channel based on the `params$channel`, we
<<<<<<< HEAD
created a function `selectData` which would return the appropriate data
set and assign it to the data set `activeData`. This will be the file we
will use for the remainder of the report.
=======
created a function `selectData` which would return the appropriate
dataset and assign it to the dataset `activeData`. This will be the file
we will use for the remainder of the report.
>>>>>>> aa58d189fd7eea466367118eb723237843afd97f

``` r
selectData <- function(dataChannel) { 
  if (dataChannel == "lifestyle"){
    return(lifestyleData)
  }
  if (dataChannel == "entertainment"){
    return(entertainmentData)
  }
  if (dataChannel == "bus"){
    return(busData)
  }
  if (dataChannel == "socmed"){
    return(socmedData)
  }
  if (dataChannel == "tech"){
    return(techData)
  }
  if (dataChannel == "world"){
    return(worldData)
  }
<<<<<<< HEAD
}
=======
  }
>>>>>>> aa58d189fd7eea466367118eb723237843afd97f

dataChannelSelect <- params$channel

activeData <- selectData(dataChannelSelect)
```

## Summarizations for data channel entertainment

<<<<<<< HEAD
NEW TOPICS !!!!!!!!!!!!!!!!!!!!

### Subsetting Variables of Interest

Define .

``` r
D1 = 1800

activeData$shares = as.factor(if_else(activeData$shares > D1,1,0))

activeData = activeData %>%
  select(shares, starts_with("weekday_is_"), starts_with("kw_"),
         starts_with("LDA_"),starts_with("n_"), starts_with("num_"), 
         average_token_length,is_weekend)

# looking for NAs
anyNA(activeData)
```

    ## [1] FALSE

=======
>>>>>>> aa58d189fd7eea466367118eb723237843afd97f
### Data manipulation for statistics

``` r
statsData <- activeData %>%
<<<<<<< HEAD
  mutate(Day = ifelse(weekday_is_monday == 1, "Monday", 
                      ifelse(weekday_is_tuesday == 1, "Tuesday", 
                      ifelse(weekday_is_wednesday == 1, "Wednesday", 
                      ifelse(weekday_is_thursday == 1, "Thursday", 
                      ifelse(weekday_is_friday == 1, "Friday", 
                      ifelse(weekday_is_saturday == 1, "Saturday", 
                      ifelse(weekday_is_sunday == 1, "Sunday",
                             "missingdata")))))))) %>%
  mutate(Weekend = ifelse(is_weekend == 1, "Yes", "No"))

statsData$Day <- factor(statsData$Day, 
                levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                           "Friday", "Saturday", "Sunday"))
=======
  mutate(Day = as.factor(ifelse(weekday_is_monday == 1, "Monday", 
                      ifelse(weekday_is_tuesday == 1, "Tuesday", 
                             ifelse(weekday_is_wednesday == 1, "Wednesday", 
                                    ifelse(weekday_is_thursday == 1, "Thursday", 
                                           ifelse(weekday_is_friday == 1, "Friday", 
                                                  ifelse(weekday_is_saturday == 1, "Saturday", 
                                                         ifelse(weekday_is_sunday == 1, "Sunday", "missingdata"))))))))) %>%
  mutate(Weekend = ifelse(is_weekend == 1, "Yes", "No"))

statsData$Day <- factor(statsData$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
>>>>>>> aa58d189fd7eea466367118eb723237843afd97f
```

### Belknap - Summary Stats

The following table gives us information about the summary statistics
for the number of shares for articles in the data channel entertainment.

``` r
<<<<<<< HEAD
table(activeData$shares)
```

    ## 
    ##    0    1 
    ## 5051 2006
=======
summary(activeData$shares)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      47     833    1200    2970    2100  210300
>>>>>>> aa58d189fd7eea466367118eb723237843afd97f

The following table gives us information about the average, median, and
standard deviation for the number of shares based on whether the post
was made on a weekend or a weekday.

``` r
statsData %>% 
  group_by(Weekend) %>%
  summarise(sumShares = sum(shares), avgShares = mean(shares), medShares = median(shares), sdShares = sd(shares))
```

<<<<<<< HEAD
=======
    ## # A tibble: 2 × 5
    ##   Weekend sumShares avgShares medShares sdShares
    ##   <chr>       <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 No       17621825     2870.      1100    8060.
    ## 2 Yes       3340902     3647.      1650    6307.

>>>>>>> aa58d189fd7eea466367118eb723237843afd97f
Likewise, this table gives us information about the number of shares by
the day of the week.

``` r
statsData %>% 
  group_by(Day) %>%
  arrange(Day) %>%
  summarise(sumShares = sum(shares), avgShares = mean(shares), medShares = median(shares), sdShares = sd(shares), maxShares = max(shares))
```

<<<<<<< HEAD
=======
    ## # A tibble: 7 × 6
    ##   Day       sumShares avgShares medShares sdShares maxShares
    ##   <fct>         <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
    ## 1 Monday      3980347     2931.      1100    7176.    112600
    ## 2 Tuesday     3479822     2708.      1100    6453.     98000
    ## 3 Wednesday   3696732     2855.      1100    8285.    138700
    ## 4 Thursday    3548004     2882.      1100    9316.    197600
    ## 5 Friday      2916920     3001.      1200    9068.    210300
    ## 6 Saturday    1298232     3416.      1600    6460.     68300
    ## 7 Sunday      2042670     3811.      1700    6197.     69500

>>>>>>> aa58d189fd7eea466367118eb723237843afd97f
### Monti - Summary Stats

### Monti - Graphs (3)

### Belknap - Graphs (3)

The following graph shows the number of shares compared to the number of
words in the title. The output is colored by the day of the week.

``` r
titlewordcountGraph <- ggplot(statsData, aes(x = n_tokens_title, y = shares))
titlewordcountGraph + geom_point(aes(color = Day)) + 
  ggtitle("Number of Shares vs. Number of Words in Title") +
  ylab("Number of Shares") +
  xlab("Number of Words in Title")
```

<<<<<<< HEAD
=======
![](entertainment_files/figure-gfm/titlewordcountGraph-1.png)<!-- -->

>>>>>>> aa58d189fd7eea466367118eb723237843afd97f
The following plot shows the number of shares by the rate of positive
words in the article. A positive trend would indicate that articles with
more positive words are shared more often than articles with negative
words.

``` r
positivewordrateGraph <- ggplot(statsData, aes(x = rate_positive_words, y = shares))
positivewordrateGraph + geom_point(aes(color = Day)) + 
  ggtitle("Number of Shares vs. Rate of Positive Words") +
  ylab("Number of Shares") +
  xlab("Rate of Positive Words") 
```

<<<<<<< HEAD
=======
![](entertainment_files/figure-gfm/positivewordrateGraph-1.png)<!-- -->

>>>>>>> aa58d189fd7eea466367118eb723237843afd97f
The following plot shows the total number of shares as related to the
parameter title subjectivity. A positive trend would indicate that
articles are shared more often when the title is subjective. A negative
trend would indicate that articles are shared more often when the title
is less subjective.

``` r
titleSubjectivityGraph <- ggplot(statsData, aes(x = title_subjectivity, y = shares))
titleSubjectivityGraph + geom_point(aes(color = n_tokens_title)) + 
  ggtitle("Number of Shares vs. Title Subjectivity") +
  ylab("Number of Shares") +
  xlab("Title Subjectivity") + 
  labs(color = "Word Count in Title")
```

<<<<<<< HEAD
=======
![](entertainment_files/figure-gfm/titleSubjectivityGraph-1.png)<!-- -->

>>>>>>> aa58d189fd7eea466367118eb723237843afd97f
## Modeling

### Data Split

Prior to conducting regression analysis, we split the data into a
training set (70%) and a test set (30%).

``` r
set.seed(555)

trainIndex <- createDataPartition(activeData$shares, p = 0.7, list = FALSE)
<<<<<<< HEAD

activeTrain <- activeData[trainIndex, ]

activeTest <- activeData[-trainIndex, ]
=======
activeTrain <- activeData[trainIndex, ]
activeTest <- activeData[-trainIndex, ]


activeTrain <- activeTrain %>%
  select(-dataChannel)

activeTest <- activeTest %>%
  select(-dataChannel)
>>>>>>> aa58d189fd7eea466367118eb723237843afd97f
```

### Belknap - Linear Regression Model Explanation

### Monti - Linear Regression Model

### Belknap - Linear Regression Model

### Monti - Ensemble Tree-based Model

### Belknap - Ensemble Tree-based Model

### Belknap - Random Forest Model & Explanation

NEEDS MORE WORK.

<<<<<<< HEAD
# NOW RF Works

``` r
train.control = trainControl(method = "cv", number = 5)

rfFit <- train(shares~.,
               data = activeTrain,
               method = "rf",
               trControl = train.control,
               preProcess = c("center","scale"),
               tuneGrid = data.frame(mtry = 1:10))

plot(rfFit)
```

![](entertainment_files/figure-gfm/RF-1.png)<!-- -->

``` r
rfFit$bestTune$mtry
```

    ## [1] 2

``` r
rfFit$results
```

    ##    mtry  Accuracy       Kappa  AccuracySD     KappaSD
    ## 1     1 0.7164539 0.007717204 0.002348784 0.008002811
    ## 2     2 0.7198943 0.068892149 0.006595721 0.022757075
    ## 3     3 0.7188834 0.086920833 0.006094895 0.022770507
    ## 4     4 0.7124069 0.079107425 0.006182427 0.014709795
    ## 5     5 0.7186812 0.106452150 0.005720727 0.011165193
    ## 6     6 0.7136213 0.097040112 0.004059065 0.011883482
    ## 7     7 0.7156437 0.103884805 0.003729294 0.014835290
    ## 8     8 0.7134199 0.097184826 0.005409992 0.014188855
    ## 9     9 0.7130140 0.101843809 0.005540951 0.012989990
    ## 10   10 0.7148371 0.108393205 0.008142061 0.018250203

``` r
RF_pred <- predict(rfFit, newdata = activeTest)

acc_rf = confusionMatrix(RF_pred, activeTest$shares)

acc_rf
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 1486  548
    ##          1   29   53
    ##                                           
    ##                Accuracy : 0.7273          
    ##                  95% CI : (0.7078, 0.7462)
    ##     No Information Rate : 0.716           
    ##     P-Value [Acc > NIR] : 0.1284          
    ##                                           
    ##                   Kappa : 0.0934          
    ##                                           
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 0.98086         
    ##             Specificity : 0.08819         
    ##          Pos Pred Value : 0.73058         
    ##          Neg Pred Value : 0.64634         
    ##              Prevalence : 0.71597         
    ##          Detection Rate : 0.70227         
    ##    Detection Prevalence : 0.96125         
    ##       Balanced Accuracy : 0.53452         
    ##                                           
    ##        'Positive' Class : 0               
    ## 

### Monti - Boosted Tree Model & Explanation

``` r
tunG = expand.grid(n.trees = seq(25,200,25),
                      interaction.depth = 1:4,
                      shrinkage = 0.1,
                      n.minobsinnode = 10)

gbmFit <- train(shares~.,
               data = activeTrain, 
               method = "gbm", 
               preProcess = c("center","scale"),
               trControl = train.control,
               tuneGrid = tunG,
               verbose = FALSE
               )

gbmFit$bestTune$n.trees
```

    ## [1] 75

``` r
gbmFit$bestTune$interaction.depth
```

    ## [1] 3

``` r
gbmFit$results
```

    ##    shrinkage interaction.depth n.minobsinnode n.trees  Accuracy      Kappa  AccuracySD
    ## 1        0.1                 1             10      25 0.7188814 0.03622392 0.004816651
    ## 9        0.1                 2             10      25 0.7168575 0.03789249 0.008011552
    ## 17       0.1                 3             10      25 0.7140249 0.03976854 0.007917030
    ## 25       0.1                 4             10      25 0.7154407 0.05256177 0.008630744
    ## 2        0.1                 1             10      50 0.7192858 0.04900700 0.009383916
    ## 10       0.1                 2             10      50 0.7164522 0.06395813 0.011360202
    ## 18       0.1                 3             10      50 0.7166555 0.08273410 0.010957879
    ## 26       0.1                 4             10      50 0.7178694 0.09191486 0.014690075
    ## 3        0.1                 1             10      75 0.7196897 0.06524868 0.014676090
    ## 11       0.1                 2             10      75 0.7204998 0.09751242 0.013563741
    ## 19       0.1                 3             10      75 0.7219174 0.10488802 0.011562567
    ## 27       0.1                 4             10      75 0.7170607 0.10273387 0.016073046
    ## 4        0.1                 1             10     100 0.7198925 0.07219163 0.012526376
    ## 12       0.1                 2             10     100 0.7182735 0.09919328 0.013068984
    ## 20       0.1                 3             10     100 0.7192860 0.11170160 0.010655209
    ## 28       0.1                 4             10     100 0.7162514 0.11345608 0.016906573
    ## 5        0.1                 1             10     125 0.7207016 0.08615842 0.015866580
    ## 13       0.1                 2             10     125 0.7209044 0.11473477 0.012089159
    ## 21       0.1                 3             10     125 0.7196899 0.12499858 0.011938260
    ## 29       0.1                 4             10     125 0.7164522 0.11378326 0.016225157
    ## 6        0.1                 1             10     150 0.7213095 0.09328083 0.016228411
    ## 14       0.1                 2             10     150 0.7172615 0.10817066 0.012446889
    ## 22       0.1                 3             10     150 0.7166545 0.11962106 0.012555501
    ## 30       0.1                 4             10     150 0.7160468 0.12196578 0.014134639
    ## 7        0.1                 1             10     175 0.7202972 0.09347376 0.013497701
    ## 15       0.1                 2             10     175 0.7176674 0.11412724 0.013530536
    ## 23       0.1                 3             10     175 0.7128079 0.11470555 0.015345125
    ## 31       0.1                 4             10     175 0.7172619 0.12877783 0.012538598
    ## 8        0.1                 1             10     200 0.7190826 0.09575366 0.014296086
    ## 16       0.1                 2             10     200 0.7188822 0.12221194 0.012523836
    ## 24       0.1                 3             10     200 0.7111895 0.11828322 0.012763742
    ## 32       0.1                 4             10     200 0.7148346 0.13004351 0.013117769
    ##       KappaSD
    ## 1  0.01628296
    ## 9  0.02619227
    ## 17 0.02234209
    ## 25 0.02566269
    ## 2  0.02994360
    ## 10 0.02964299
    ## 18 0.03286313
    ## 26 0.04207671
    ## 3  0.04807798
    ## 11 0.04119401
    ## 19 0.03201278
    ## 27 0.05081293
    ## 4  0.03987477
    ## 12 0.03931310
    ## 20 0.02612882
    ## 28 0.05221400
    ## 5  0.04636599
    ## 13 0.03722940
    ## 21 0.03329735
    ## 29 0.05164183
    ## 6  0.05307911
    ## 14 0.03602767
    ## 22 0.03500687
    ## 30 0.04766007
    ## 7  0.04072134
    ## 15 0.04485072
    ## 23 0.04136972
    ## 31 0.03642235
    ## 8  0.03856348
    ## 16 0.04279702
    ## 24 0.03032610
    ## 32 0.04231214

``` r
plot(gbmFit)
```

![](entertainment_files/figure-gfm/Boosting-1.png)<!-- -->

``` r
gbm_pred <- predict(gbmFit, newdata = activeTest)

acc_boosting = confusionMatrix(gbm_pred, activeTest$shares)

acc_boosting
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 1456  519
    ##          1   59   82
    ##                                           
    ##                Accuracy : 0.7268          
    ##                  95% CI : (0.7073, 0.7457)
    ##     No Information Rate : 0.716           
    ##     P-Value [Acc > NIR] : 0.1389          
    ##                                           
    ##                   Kappa : 0.1268          
    ##                                           
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 0.9611          
    ##             Specificity : 0.1364          
    ##          Pos Pred Value : 0.7372          
    ##          Neg Pred Value : 0.5816          
    ##              Prevalence : 0.7160          
    ##          Detection Rate : 0.6881          
    ##    Detection Prevalence : 0.9334          
    ##       Balanced Accuracy : 0.5487          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

## Comparison & Conclusion - Monti or Belknap (whoever doesn’t do automation of R Markdown)

``` r
tb = data.frame(RF = acc_rf$overall[[1]],
                Boosting = acc_boosting$overall[[1]])

kable(tb, caption = "Accuracy Metric by Ensemble Method on Test Set")
```
=======
``` r
str(activeTrain)

rfFit <- train(shares ~ .,
               data = activeTrain, 
               method = "rf", 
               trControl = trainControl(method = "cv", number = 5), 
               tuneGrid = data.frame(mtry = 1:3))

rfFit$results


rfPred <- predict(rfFit, newData = activeTest)
postResample(rfPred, activeTest$shares)
```

### Monti - Boosted Tree Model & Explanation

## Comparison & Conclusion - Monti or Belknap (whoever doesn’t do automation of R Markdown)
>>>>>>> aa58d189fd7eea466367118eb723237843afd97f
