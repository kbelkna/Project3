Project 3
================
Kara Belknap & Cassio Monti
2022-10-29

-   <a href="#project-3" id="toc-project-3">Project 3</a>
    -   <a href="#report-for-data-channel--socmed"
        id="toc-report-for-data-channel--socmed">Report for Data Channel =
        socmed</a>
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
    -   <a href="#summarizations-for-data-channel-socmed"
        id="toc-summarizations-for-data-channel-socmed">Summarizations for data
        channel socmed</a>
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

## Report for Data Channel = socmed

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

## Summarizations for data channel socmed

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
for the number of shares for articles in the data channel socmed.

``` r
table(activeData$shares)
```

    ## 
    ##    0    1 
    ##  983 1340

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

![](socmed_files/figure-gfm/RF-1.png)<!-- -->

``` r
rfFit$bestTune$mtry
```

    ## [1] 2

``` r
rfFit$results
```

    ##    mtry  Accuracy     Kappa AccuracySD    KappaSD
    ## 1     1 0.6410741 0.1951030 0.02171743 0.05040071
    ## 2     2 0.6779273 0.3155743 0.03108310 0.06669152
    ## 3     3 0.6687173 0.2989493 0.03465554 0.07395907
    ## 4     4 0.6613421 0.2852676 0.03405827 0.06984393
    ## 5     5 0.6674847 0.2994009 0.03388551 0.07223406
    ## 6     6 0.6681095 0.2985305 0.02341509 0.04722046
    ## 7     7 0.6674866 0.3009471 0.04337211 0.09045205
    ## 8     8 0.6607381 0.2857762 0.03974935 0.08251328
    ## 9     9 0.6595035 0.2850804 0.04087575 0.08536148
    ## 10   10 0.6607286 0.2873168 0.03262129 0.06884458

``` r
RF_pred <- predict(rfFit, newdata = activeTest)

acc_rf = confusionMatrix(RF_pred, activeTest$shares)

acc_rf
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 148  69
    ##          1 146 333
    ##                                           
    ##                Accuracy : 0.6911          
    ##                  95% CI : (0.6553, 0.7253)
    ##     No Information Rate : 0.5776          
    ##     P-Value [Acc > NIR] : 4.552e-10       
    ##                                           
    ##                   Kappa : 0.3439          
    ##                                           
    ##  Mcnemar's Test P-Value : 2.182e-07       
    ##                                           
    ##             Sensitivity : 0.5034          
    ##             Specificity : 0.8284          
    ##          Pos Pred Value : 0.6820          
    ##          Neg Pred Value : 0.6952          
    ##              Prevalence : 0.4224          
    ##          Detection Rate : 0.2126          
    ##    Detection Prevalence : 0.3118          
    ##       Balanced Accuracy : 0.6659          
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

    ## [1] 125

``` r
gbmFit$bestTune$interaction.depth
```

    ## [1] 2

``` r
gbmFit$results
```

    ##    shrinkage interaction.depth n.minobsinnode n.trees  Accuracy     Kappa  AccuracySD
    ## 1        0.1                 1             10      25 0.6423320 0.2187972 0.024638335
    ## 9        0.1                 2             10      25 0.6503187 0.2519672 0.021465604
    ## 17       0.1                 3             10      25 0.6693504 0.2969729 0.017492331
    ## 25       0.1                 4             10      25 0.6663094 0.2942344 0.024155851
    ## 2        0.1                 1             10      50 0.6601480 0.2747974 0.017384393
    ## 10       0.1                 2             10      50 0.6687426 0.2977018 0.019165174
    ## 18       0.1                 3             10      50 0.6675231 0.2988518 0.022344369
    ## 26       0.1                 4             10      50 0.6705831 0.3079423 0.014107186
    ## 3        0.1                 1             10      75 0.6644424 0.2872513 0.014449649
    ## 11       0.1                 2             10      75 0.6742754 0.3153573 0.018331078
    ## 19       0.1                 3             10      75 0.6656561 0.2997086 0.022842758
    ## 27       0.1                 4             10      75 0.6595079 0.2869596 0.009965241
    ## 4        0.1                 1             10     100 0.6669021 0.2961483 0.014620910
    ## 12       0.1                 2             10     100 0.6779583 0.3243689 0.015581082
    ## 20       0.1                 3             10     100 0.6595136 0.2871201 0.030319836
    ## 28       0.1                 4             10     100 0.6613522 0.2931433 0.013380886
    ## 5        0.1                 1             10     125 0.6675194 0.3017014 0.022202258
    ## 13       0.1                 2             10     125 0.6804123 0.3285413 0.018777375
    ## 21       0.1                 3             10     125 0.6582923 0.2849583 0.024448550
    ## 29       0.1                 4             10     125 0.6582431 0.2887329 0.014657721
    ## 6        0.1                 1             10     150 0.6675118 0.3008238 0.015641595
    ## 14       0.1                 2             10     150 0.6803952 0.3297999 0.020186878
    ## 22       0.1                 3             10     150 0.6595344 0.2883069 0.039893265
    ## 30       0.1                 4             10     150 0.6563988 0.2830355 0.024440112
    ## 7        0.1                 1             10     175 0.6724198 0.3122377 0.018104406
    ## 15       0.1                 2             10     175 0.6693410 0.3079856 0.013149649
    ## 23       0.1                 3             10     175 0.6558193 0.2823578 0.028144491
    ## 31       0.1                 4             10     175 0.6502695 0.2703265 0.025003879
    ## 8        0.1                 1             10     200 0.6730352 0.3130135 0.017443902
    ## 16       0.1                 2             10     200 0.6656505 0.2980459 0.016511417
    ## 24       0.1                 3             10     200 0.6582904 0.2874548 0.031231117
    ## 32       0.1                 4             10     200 0.6514927 0.2716591 0.026209371
    ##       KappaSD
    ## 1  0.05276293
    ## 9  0.04630045
    ## 17 0.03963858
    ## 25 0.05361555
    ## 2  0.03555438
    ## 10 0.03871941
    ## 18 0.04804183
    ## 26 0.02990004
    ## 3  0.02969191
    ## 11 0.03474486
    ## 19 0.05003246
    ## 27 0.02140993
    ## 4  0.02877751
    ## 12 0.02988903
    ## 20 0.06527636
    ## 28 0.02930127
    ## 5  0.04584529
    ## 13 0.03476911
    ## 21 0.05570272
    ## 29 0.03287498
    ## 6  0.03307630
    ## 14 0.03879825
    ## 22 0.08697984
    ## 30 0.05325836
    ## 7  0.03768904
    ## 15 0.02103732
    ## 23 0.06249496
    ## 31 0.05294573
    ## 8  0.03249118
    ## 16 0.02983459
    ## 24 0.06952820
    ## 32 0.05670421

``` r
plot(gbmFit)
```

![](socmed_files/figure-gfm/Boosting-1.png)<!-- -->

``` r
gbm_pred <- predict(gbmFit, newdata = activeTest)

acc_boosting = confusionMatrix(gbm_pred, activeTest$shares)

acc_boosting
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 163  82
    ##          1 131 320
    ##                                          
    ##                Accuracy : 0.694          
    ##                  95% CI : (0.6582, 0.728)
    ##     No Information Rate : 0.5776         
    ##     P-Value [Acc > NIR] : 1.649e-10      
    ##                                          
    ##                   Kappa : 0.3585         
    ##                                          
    ##  Mcnemar's Test P-Value : 0.001006       
    ##                                          
    ##             Sensitivity : 0.5544         
    ##             Specificity : 0.7960         
    ##          Pos Pred Value : 0.6653         
    ##          Neg Pred Value : 0.7095         
    ##              Prevalence : 0.4224         
    ##          Detection Rate : 0.2342         
    ##    Detection Prevalence : 0.3520         
    ##       Balanced Accuracy : 0.6752         
    ##                                          
    ##        'Positive' Class : 0              
    ## 

## Comparison & Conclusion - Monti or Belknap (whoever doesn’t do automation of R Markdown)

``` r
tb = data.frame(RF = acc_rf$overall[[1]],
                Boosting = acc_boosting$overall[[1]])

kable(tb, caption = "Accuracy Metric by Ensemble Method on Test Set")
```
