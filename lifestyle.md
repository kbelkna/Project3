Project 3
================
Kara Belknap & Cassio Monti
2022-10-29

<<<<<<< HEAD
-   <a href="#project-3" id="toc-project-3">Project 3</a>
    -   <a href="#report-for-data-channel--lifestyle"
        id="toc-report-for-data-channel--lifestyle">Report for Data Channel =
        lifestyle</a>
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
    -   <a href="#summarizations-for-data-channel-lifestyle"
        id="toc-summarizations-for-data-channel-lifestyle">Summarizations for
        data channel lifestyle</a>
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

## Report for Data Channel = lifestyle

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

## Summarizations for data channel lifestyle

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
for the number of shares for articles in the data channel lifestyle.

``` r
<<<<<<< HEAD
table(activeData$shares)
```

    ## 
    ##    0    1 
    ## 1152  947
=======
summary(activeData$shares)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      28    1100    1700    3682    3250  208300
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
    ## 1 No        6193432     3628.      1600    9552.
    ## 2 Yes       1535345     3917.      2100    5044.

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
    ## 1 Monday      1399319     4346.      1600   14073.    196700
    ## 2 Tuesday     1386933     4152.      1500   13544.    208300
    ## 3 Wednesday   1231194     3173.      1600    5608.     73100
    ## 4 Thursday    1253096     3500.      1600    5821.     56000
    ## 5 Friday       922890     3026.      1500    4540.     40400
    ## 6 Saturday     739366     4062.      2100    5351.     43000
    ## 7 Sunday       795979     3790.      2100    4772.     33100

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
![](lifestyle_files/figure-gfm/titlewordcountGraph-1.png)<!-- -->

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
![](lifestyle_files/figure-gfm/positivewordrateGraph-1.png)<!-- -->

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
![](lifestyle_files/figure-gfm/titleSubjectivityGraph-1.png)<!-- -->

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

![](lifestyle_files/figure-gfm/RF-1.png)<!-- -->

``` r
rfFit$bestTune$mtry
```

    ## [1] 2

``` r
rfFit$results
```

    ##    mtry  Accuracy     Kappa AccuracySD    KappaSD
    ## 1     1 0.5993364 0.1438858 0.01001691 0.01950728
    ## 2     2 0.6047601 0.1788542 0.01550423 0.03142340
    ## 3     3 0.6013541 0.1760747 0.01367615 0.02533229
    ## 4     4 0.6040590 0.1837139 0.02346622 0.04507383
    ## 5     5 0.5993133 0.1753274 0.01488266 0.02981138
    ## 6     6 0.5965968 0.1693051 0.01679265 0.03248859
    ## 7     7 0.6027101 0.1813146 0.01649286 0.03210239
    ## 8     8 0.5992925 0.1766458 0.02161733 0.04251111
    ## 9     9 0.5986446 0.1755077 0.01402619 0.03164202
    ## 10   10 0.5986215 0.1758197 0.01730783 0.03399944

``` r
RF_pred <- predict(rfFit, newdata = activeTest)

acc_rf = confusionMatrix(RF_pred, activeTest$shares)

acc_rf
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 260 193
    ##          1  85  91
    ##                                           
    ##                Accuracy : 0.558           
    ##                  95% CI : (0.5182, 0.5973)
    ##     No Information Rate : 0.5485          
    ##     P-Value [Acc > NIR] : 0.3301          
    ##                                           
    ##                   Kappa : 0.0766          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.386e-10       
    ##                                           
    ##             Sensitivity : 0.7536          
    ##             Specificity : 0.3204          
    ##          Pos Pred Value : 0.5740          
    ##          Neg Pred Value : 0.5170          
    ##              Prevalence : 0.5485          
    ##          Detection Rate : 0.4134          
    ##    Detection Prevalence : 0.7202          
    ##       Balanced Accuracy : 0.5370          
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

    ## [1] 175

``` r
gbmFit$bestTune$interaction.depth
```

    ## [1] 1

``` r
gbmFit$results
```

    ##    shrinkage interaction.depth n.minobsinnode n.trees  Accuracy     Kappa AccuracySD
    ## 1        0.1                 1             10      25 0.5959003 0.1480242 0.02072742
    ## 9        0.1                 2             10      25 0.5972494 0.1511619 0.02774142
    ## 17       0.1                 3             10      25 0.5999357 0.1608586 0.03643958
    ## 25       0.1                 4             10      25 0.5938503 0.1551622 0.02952339
    ## 2        0.1                 1             10      50 0.5870359 0.1360506 0.02320727
    ## 10       0.1                 2             10      50 0.5918234 0.1487071 0.03051306
    ## 18       0.1                 3             10      50 0.6054080 0.1818559 0.02407276
    ## 26       0.1                 4             10      50 0.6020275 0.1802045 0.02026749
    ## 3        0.1                 1             10      75 0.5856916 0.1360831 0.01343733
    ## 11       0.1                 2             10      75 0.5992947 0.1697131 0.02528351
    ## 19       0.1                 3             10      75 0.5876907 0.1489238 0.03387597
    ## 27       0.1                 4             10      75 0.6047509 0.1886356 0.03832660
    ## 4        0.1                 1             10     100 0.5931931 0.1531336 0.01321363
    ## 12       0.1                 2             10     100 0.5931769 0.1598732 0.01908775
    ## 20       0.1                 3             10     100 0.5897640 0.1567598 0.02544266
    ## 28       0.1                 4             10     100 0.5884197 0.1578797 0.03175875
    ## 5        0.1                 1             10     125 0.5965876 0.1615992 0.02168507
    ## 13       0.1                 2             10     125 0.5938572 0.1661480 0.02973138
    ## 21       0.1                 3             10     125 0.5863696 0.1505674 0.02173191
    ## 29       0.1                 4             10     125 0.5911408 0.1668401 0.03049679
    ## 6        0.1                 1             10     150 0.6027170 0.1753798 0.03198829
    ## 14       0.1                 2             10     150 0.5890698 0.1582496 0.03672866
    ## 22       0.1                 3             10     150 0.5904558 0.1626408 0.02974010
    ## 30       0.1                 4             10     150 0.5952201 0.1756063 0.02719091
    ## 7        0.1                 1             10     175 0.6054219 0.1832143 0.02711864
    ## 15       0.1                 2             10     175 0.5884219 0.1554597 0.03445027
    ## 23       0.1                 3             10     175 0.5958911 0.1739892 0.01859031
    ## 31       0.1                 4             10     175 0.5986168 0.1806359 0.03048671
    ## 8        0.1                 1             10     200 0.5972516 0.1670408 0.02164085
    ## 16       0.1                 2             10     200 0.5890791 0.1575662 0.03293236
    ## 24       0.1                 3             10     200 0.5856823 0.1516585 0.02162427
    ## 32       0.1                 4             10     200 0.6033765 0.1924316 0.03537964
    ##       KappaSD
    ## 1  0.04336791
    ## 9  0.06110467
    ## 17 0.08146899
    ## 25 0.06443943
    ## 2  0.05236642
    ## 10 0.05974400
    ## 18 0.04958354
    ## 26 0.04496783
    ## 3  0.02950733
    ## 11 0.05332500
    ## 19 0.07298334
    ## 27 0.07951193
    ## 4  0.03364757
    ## 12 0.04281777
    ## 20 0.05302603
    ## 28 0.06954642
    ## 5  0.05177680
    ## 13 0.06352515
    ## 21 0.04381839
    ## 29 0.06603321
    ## 6  0.07093262
    ## 14 0.08051599
    ## 22 0.06065182
    ## 30 0.06072216
    ## 7  0.06173498
    ## 15 0.07491109
    ## 23 0.03763830
    ## 31 0.07005589
    ## 8  0.04551744
    ## 16 0.07045891
    ## 24 0.04390865
    ## 32 0.07851394

``` r
plot(gbmFit)
```

![](lifestyle_files/figure-gfm/Boosting-1.png)<!-- -->

``` r
gbm_pred <- predict(gbmFit, newdata = activeTest)

acc_boosting = confusionMatrix(gbm_pred, activeTest$shares)

acc_boosting
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 259 193
    ##          1  86  91
    ##                                           
    ##                Accuracy : 0.5564          
    ##                  95% CI : (0.5166, 0.5957)
    ##     No Information Rate : 0.5485          
    ##     P-Value [Acc > NIR] : 0.3597          
    ##                                           
    ##                   Kappa : 0.0736          
    ##                                           
    ##  Mcnemar's Test P-Value : 2.209e-10       
    ##                                           
    ##             Sensitivity : 0.7507          
    ##             Specificity : 0.3204          
    ##          Pos Pred Value : 0.5730          
    ##          Neg Pred Value : 0.5141          
    ##              Prevalence : 0.5485          
    ##          Detection Rate : 0.4118          
    ##    Detection Prevalence : 0.7186          
    ##       Balanced Accuracy : 0.5356          
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
