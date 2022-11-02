Project 3
================
Kara Belknap & Cassio Monti
2022-10-29

-   <a href="#project-3" id="toc-project-3">Project 3</a>
    -   <a href="#report-for-data-channel--bus"
        id="toc-report-for-data-channel--bus">Report for Data Channel = bus</a>
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
    -   <a href="#summarizations-for-data-channel-bus"
        id="toc-summarizations-for-data-channel-bus">Summarizations for data
        channel bus</a>
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

# Project 3

## Report for Data Channel = bus

## Monti - Introduction

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

## Getting Started

Before we can begin our analysis, we must load in the following
packages:

``` r
library(tidyverse)
library(caret)
library(randomForest)
library(knitr)
```

## Read in the Data

Using the data file `OnlineNewsPopularity.csv`, we will read in the data
and add a new column corresponding to the type of data channel from
which the data was classified. The new variable will be called
`dataChannel`. Note that there are some rows that are unclassified
according to the six channels of interest and those are indicated by
`other`.

Once the data column is created, we can easily subset the data using the
`filter` function to create a new data set for each data channel. We
removed the original `data_channel_is_*` columns as well as two
non-predictive columns `url` and `timedelta`.

``` r
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
created a function `selectData` which would return the appropriate data
set and assign it to the data set `activeData`. This will be the file we
will use for the remainder of the report.

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
}

dataChannelSelect <- params$channel

activeData <- selectData(dataChannelSelect)
```

## Summarizations for data channel bus

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

### Data manipulation for statistics

``` r
statsData <- activeData %>%
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
```

### Belknap - Summary Stats

The following table gives us information about the summary statistics
for the number of shares for articles in the data channel bus.

``` r
table(activeData$shares)
```

    ## 
    ##    0    1 
    ## 3962 2296

The following table gives us information about the average, median, and
standard deviation for the number of shares based on whether the post
was made on a weekend or a weekday.

``` r
statsData %>% 
  group_by(Weekend) %>%
  summarise(sumShares = sum(shares), avgShares = mean(shares), medShares = median(shares), sdShares = sd(shares))
```

Likewise, this table gives us information about the number of shares by
the day of the week.

``` r
statsData %>% 
  group_by(Day) %>%
  arrange(Day) %>%
  summarise(sumShares = sum(shares), avgShares = mean(shares), medShares = median(shares), sdShares = sd(shares), maxShares = max(shares))
```

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

## Modeling

### Data Split

Prior to conducting regression analysis, we split the data into a
training set (70%) and a test set (30%).

``` r
set.seed(555)

trainIndex <- createDataPartition(activeData$shares, p = 0.7, list = FALSE)

activeTrain <- activeData[trainIndex, ]

activeTest <- activeData[-trainIndex, ]
```

### Belknap - Linear Regression Model Explanation

### Monti - Linear Regression Model

### Belknap - Linear Regression Model

### Monti - Ensemble Tree-based Model

### Belknap - Ensemble Tree-based Model

### Belknap - Random Forest Model & Explanation

NEEDS MORE WORK.

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

![](bus_files/figure-gfm/RF-1.png)<!-- -->

``` r
rfFit$bestTune$mtry
```

    ## [1] 2

``` r
rfFit$results
```

    ##    mtry  Accuracy     Kappa  AccuracySD    KappaSD
    ## 1     1 0.6960292 0.2395460 0.004815505 0.01505890
    ## 2     2 0.7090410 0.3133877 0.014170289 0.03693640
    ## 3     3 0.7065307 0.3153910 0.018700893 0.04668481
    ## 4     4 0.7081288 0.3202149 0.011239348 0.03011537
    ## 5     5 0.7037912 0.3114048 0.010052215 0.02695926
    ## 6     6 0.7051626 0.3162310 0.016050953 0.04153628
    ## 7     7 0.7053881 0.3171041 0.011849431 0.03154020
    ## 8     8 0.7060730 0.3192202 0.017034265 0.04143094
    ## 9     9 0.7037922 0.3140874 0.015010626 0.03783056
    ## 10   10 0.7047042 0.3162807 0.017362699 0.04127057

``` r
RF_pred <- predict(rfFit, newdata = activeTest)

acc_rf = confusionMatrix(RF_pred, activeTest$shares)

acc_rf
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 1027  428
    ##          1  161  260
    ##                                          
    ##                Accuracy : 0.686          
    ##                  95% CI : (0.6645, 0.707)
    ##     No Information Rate : 0.6333         
    ##     P-Value [Acc > NIR] : 9.2e-07        
    ##                                          
    ##                   Kappa : 0.2639         
    ##                                          
    ##  Mcnemar's Test P-Value : < 2e-16        
    ##                                          
    ##             Sensitivity : 0.8645         
    ##             Specificity : 0.3779         
    ##          Pos Pred Value : 0.7058         
    ##          Neg Pred Value : 0.6176         
    ##              Prevalence : 0.6333         
    ##          Detection Rate : 0.5474         
    ##    Detection Prevalence : 0.7756         
    ##       Balanced Accuracy : 0.6212         
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

    ## [1] 4

``` r
gbmFit$results
```

    ##    shrinkage interaction.depth n.minobsinnode n.trees  Accuracy     Kappa  AccuracySD
    ## 1        0.1                 1             10      25 0.6903230 0.2354923 0.008373842
    ## 9        0.1                 2             10      25 0.6953427 0.2708052 0.008878926
    ## 17       0.1                 3             10      25 0.7060691 0.2994614 0.008830218
    ## 25       0.1                 4             10      25 0.7062998 0.3047817 0.013111349
    ## 2        0.1                 1             10      50 0.7015060 0.2872041 0.008900625
    ## 10       0.1                 2             10      50 0.7058400 0.3090311 0.007763631
    ## 18       0.1                 3             10      50 0.7047003 0.3063478 0.007226405
    ## 26       0.1                 4             10      50 0.7099501 0.3204729 0.009016010
    ## 3        0.1                 1             10      75 0.7026463 0.2977070 0.006066666
    ## 11       0.1                 2             10      75 0.7069810 0.3158287 0.008053033
    ## 19       0.1                 3             10      75 0.7051538 0.3131261 0.012491490
    ## 27       0.1                 4             10      75 0.7097192 0.3232517 0.014503314
    ## 4        0.1                 1             10     100 0.7053829 0.3062111 0.008517331
    ## 12       0.1                 2             10     100 0.7094922 0.3225932 0.010543388
    ## 20       0.1                 3             10     100 0.7067530 0.3186913 0.009650856
    ## 28       0.1                 4             10     100 0.7081213 0.3232138 0.013021341
    ## 5        0.1                 1             10     125 0.7076655 0.3150009 0.005754758
    ## 13       0.1                 2             10     125 0.7092636 0.3230604 0.009980396
    ## 21       0.1                 3             10     125 0.7076678 0.3210437 0.009246917
    ## 29       0.1                 4             10     125 0.7076665 0.3252780 0.008897402
    ## 6        0.1                 1             10     150 0.7062956 0.3142655 0.007252066
    ## 14       0.1                 2             10     150 0.7067551 0.3193683 0.008178720
    ## 22       0.1                 3             10     150 0.7069829 0.3213563 0.008599559
    ## 30       0.1                 4             10     150 0.7040140 0.3160718 0.008881527
    ## 7        0.1                 1             10     175 0.7088060 0.3226131 0.009354524
    ## 15       0.1                 2             10     175 0.7065257 0.3202492 0.013314764
    ## 23       0.1                 3             10     175 0.7076673 0.3229129 0.006244911
    ## 31       0.1                 4             10     175 0.7106298 0.3339844 0.010029917
    ## 8        0.1                 1             10     200 0.7097184 0.3240965 0.009682877
    ## 16       0.1                 2             10     200 0.7081221 0.3241391 0.009804124
    ## 24       0.1                 3             10     200 0.7078951 0.3236561 0.008142576
    ## 32       0.1                 4             10     200 0.7090345 0.3292178 0.009632902
    ##       KappaSD
    ## 1  0.02586525
    ## 9  0.02603832
    ## 17 0.02544508
    ## 25 0.03654204
    ## 2  0.02912060
    ## 10 0.02207169
    ## 18 0.01603181
    ## 26 0.02161419
    ## 3  0.01934817
    ## 11 0.01812857
    ## 19 0.02713043
    ## 27 0.03106835
    ## 4  0.02195190
    ## 12 0.02277010
    ## 20 0.02076628
    ## 28 0.02783095
    ## 5  0.01289400
    ## 13 0.02291133
    ## 21 0.01784058
    ## 29 0.02241452
    ## 6  0.01875368
    ## 14 0.02139442
    ## 22 0.02103640
    ## 30 0.02305674
    ## 7  0.01983133
    ## 15 0.03209830
    ## 23 0.01183073
    ## 31 0.02214824
    ## 8  0.02306002
    ## 16 0.02148108
    ## 24 0.02025387
    ## 32 0.02360587

``` r
plot(gbmFit)
```

![](bus_files/figure-gfm/Boosting-1.png)<!-- -->

``` r
gbm_pred <- predict(gbmFit, newdata = activeTest)

acc_boosting = confusionMatrix(gbm_pred, activeTest$shares)

acc_boosting
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 998 383
    ##          1 190 305
    ##                                           
    ##                Accuracy : 0.6946          
    ##                  95% CI : (0.6732, 0.7154)
    ##     No Information Rate : 0.6333          
    ##     P-Value [Acc > NIR] : 1.372e-08       
    ##                                           
    ##                   Kappa : 0.3012          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.050e-15       
    ##                                           
    ##             Sensitivity : 0.8401          
    ##             Specificity : 0.4433          
    ##          Pos Pred Value : 0.7227          
    ##          Neg Pred Value : 0.6162          
    ##              Prevalence : 0.6333          
    ##          Detection Rate : 0.5320          
    ##    Detection Prevalence : 0.7361          
    ##       Balanced Accuracy : 0.6417          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

## Comparison & Conclusion - Monti or Belknap (whoever doesn’t do automation of R Markdown)

``` r
tb = data.frame(RF = acc_rf$overall[[1]],
                Boosting = acc_boosting$overall[[1]])

kable(tb, caption = "Accuracy Metric by Ensemble Method on Test Set")
```
