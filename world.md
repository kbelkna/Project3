Project 3
================
Kara Belknap & Cassio Monti
2022-10-29

-   <a href="#project-3" id="toc-project-3">Project 3</a>
    -   <a href="#report-for-data-channel--world"
        id="toc-report-for-data-channel--world">Report for Data Channel =
        world</a>
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
    -   <a href="#summarizations-for-data-channel-world"
        id="toc-summarizations-for-data-channel-world">Summarizations for data
        channel world</a>
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

## Report for Data Channel = world

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

## Summarizations for data channel world

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
for the number of shares for articles in the data channel world.

``` r
table(activeData$shares)
```

    ## 
    ##    0    1 
    ## 6318 2109

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

![](world_files/figure-gfm/RF-1.png)<!-- -->

``` r
rfFit$bestTune$mtry
```

    ## [1] 3

``` r
rfFit$results
```

    ##    mtry  Accuracy      Kappa  AccuracySD    KappaSD
    ## 1     1 0.7496612 0.00000000 0.000283925 0.00000000
    ## 2     2 0.7550853 0.07241295 0.001684298 0.01005569
    ## 3     3 0.7589809 0.10353689 0.004022184 0.01558088
    ## 4     4 0.7544052 0.09426115 0.003439994 0.01093727
    ## 5     5 0.7564385 0.10948852 0.004646099 0.01813435
    ## 6     6 0.7545750 0.10235813 0.004181451 0.01924839
    ## 7     7 0.7554219 0.10801252 0.005071260 0.01817425
    ## 8     8 0.7567774 0.11616416 0.004552358 0.01995695
    ## 9     9 0.7557590 0.11531135 0.007277535 0.02327993
    ## 10   10 0.7535565 0.10752670 0.006512351 0.02465051

``` r
RF_pred <- predict(rfFit, newdata = activeTest)

acc_rf = confusionMatrix(RF_pred, activeTest$shares)

acc_rf
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 1848  573
    ##          1   47   59
    ##                                           
    ##                Accuracy : 0.7546          
    ##                  95% CI : (0.7374, 0.7713)
    ##     No Information Rate : 0.7499          
    ##     P-Value [Acc > NIR] : 0.2996          
    ##                                           
    ##                   Kappa : 0.0949          
    ##                                           
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 0.97520         
    ##             Specificity : 0.09335         
    ##          Pos Pred Value : 0.76332         
    ##          Neg Pred Value : 0.55660         
    ##              Prevalence : 0.74990         
    ##          Detection Rate : 0.73130         
    ##    Detection Prevalence : 0.95805         
    ##       Balanced Accuracy : 0.53428         
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

    ## [1] 50

``` r
gbmFit$bestTune$interaction.depth
```

    ## [1] 2

``` r
gbmFit$results
```

    ##    shrinkage interaction.depth n.minobsinnode n.trees  Accuracy      Kappa  AccuracySD
    ## 1        0.1                 1             10      25 0.7510179 0.01075246 0.002523479
    ## 9        0.1                 2             10      25 0.7513561 0.02971570 0.003291651
    ## 17       0.1                 3             10      25 0.7511882 0.03818939 0.005669559
    ## 25       0.1                 4             10      25 0.7511879 0.05493519 0.004150051
    ## 2        0.1                 1             10      50 0.7542384 0.04619042 0.004568409
    ## 10       0.1                 2             10      50 0.7559330 0.07888101 0.003033331
    ## 18       0.1                 3             10      50 0.7508495 0.07614958 0.005792407
    ## 26       0.1                 4             10      50 0.7506796 0.08703960 0.004888599
    ## 3        0.1                 1             10      75 0.7547466 0.06810507 0.002948560
    ## 11       0.1                 2             10      75 0.7554248 0.09232442 0.003778040
    ## 19       0.1                 3             10      75 0.7513580 0.09594509 0.004602053
    ## 27       0.1                 4             10      75 0.7505103 0.10918844 0.006038515
    ## 4        0.1                 1             10     100 0.7547467 0.07757617 0.003835876
    ## 12       0.1                 2             10     100 0.7530522 0.09866759 0.005369117
    ## 20       0.1                 3             10     100 0.7496630 0.10169889 0.006118701
    ## 28       0.1                 4             10     100 0.7488155 0.10278806 0.007047566
    ## 5        0.1                 1             10     125 0.7533921 0.08463009 0.005332725
    ## 13       0.1                 2             10     125 0.7522049 0.10189994 0.005872607
    ## 21       0.1                 3             10     125 0.7503418 0.10907200 0.006982874
    ## 29       0.1                 4             10     125 0.7501701 0.11610104 0.009742214
    ## 6        0.1                 1             10     150 0.7533921 0.09026090 0.006087378
    ## 14       0.1                 2             10     150 0.7527128 0.10573764 0.007480268
    ## 22       0.1                 3             10     150 0.7513586 0.11950791 0.006834825
    ## 30       0.1                 4             10     150 0.7508477 0.12529138 0.007279924
    ## 7        0.1                 1             10     175 0.7523754 0.08872203 0.006835027
    ## 15       0.1                 2             10     175 0.7506793 0.10035324 0.007699776
    ## 23       0.1                 3             10     175 0.7508504 0.12121391 0.006338456
    ## 31       0.1                 4             10     175 0.7496598 0.12289706 0.006069042
    ## 8        0.1                 1             10     200 0.7525440 0.09355885 0.005876550
    ## 16       0.1                 2             10     200 0.7505091 0.10436947 0.008183187
    ## 24       0.1                 3             10     200 0.7528834 0.12672496 0.008972710
    ## 32       0.1                 4             10     200 0.7476276 0.12559883 0.006183961
    ##       KappaSD
    ## 1  0.01177962
    ## 9  0.01408814
    ## 17 0.02638726
    ## 25 0.01392142
    ## 2  0.02442813
    ## 10 0.01353654
    ## 18 0.02549585
    ## 26 0.02603701
    ## 3  0.01588904
    ## 11 0.01542266
    ## 19 0.01765235
    ## 27 0.03313035
    ## 4  0.01952899
    ## 12 0.02366239
    ## 20 0.03017802
    ## 28 0.03159677
    ## 5  0.02705924
    ## 13 0.02867078
    ## 21 0.02999794
    ## 29 0.04028685
    ## 6  0.03158426
    ## 14 0.03162613
    ## 22 0.02925693
    ## 30 0.03330917
    ## 7  0.03542567
    ## 15 0.03806722
    ## 23 0.02498595
    ## 31 0.03235699
    ## 8  0.03321944
    ## 16 0.03977735
    ## 24 0.03776496
    ## 32 0.03206415

``` r
plot(gbmFit)
```

![](world_files/figure-gfm/Boosting-1.png)<!-- -->

``` r
gbm_pred <- predict(gbmFit, newdata = activeTest)

acc_boosting = confusionMatrix(gbm_pred, activeTest$shares)

acc_boosting
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 1855  591
    ##          1   40   41
    ##                                           
    ##                Accuracy : 0.7503          
    ##                  95% CI : (0.7329, 0.7671)
    ##     No Information Rate : 0.7499          
    ##     P-Value [Acc > NIR] : 0.4924          
    ##                                           
    ##                   Kappa : 0.0617          
    ##                                           
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 0.97889         
    ##             Specificity : 0.06487         
    ##          Pos Pred Value : 0.75838         
    ##          Neg Pred Value : 0.50617         
    ##              Prevalence : 0.74990         
    ##          Detection Rate : 0.73407         
    ##    Detection Prevalence : 0.96795         
    ##       Balanced Accuracy : 0.52188         
    ##                                           
    ##        'Positive' Class : 0               
    ## 

## Comparison & Conclusion - Monti or Belknap (whoever doesn’t do automation of R Markdown)

``` r
tb = data.frame(RF = acc_rf$overall[[1]],
                Boosting = acc_boosting$overall[[1]])

kable(tb, caption = "Accuracy Metric by Ensemble Method on Test Set")
```
