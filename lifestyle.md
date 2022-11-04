Project 3
================
Kara Belknap & Cassio Monti
2022-11-5

-   <a href="#report-for-lifestyle-channel"
    id="toc-report-for-lifestyle-channel">Report for <em>lifestyle</em>
    Channel</a>
    -   <a href="#introduction" id="toc-introduction">Introduction</a>
    -   <a href="#required-packages" id="toc-required-packages">Required
        Packages</a>
    -   <a href="#read-in-the-data" id="toc-read-in-the-data">Read in the
        Data</a>
    -   <a href="#select-data-for-appropriate-data-channel"
        id="toc-select-data-for-appropriate-data-channel">Select Data for
        Appropriate Data Channel</a>
    -   <a href="#summarizations-for-data-channel-lifestyle"
        id="toc-summarizations-for-data-channel-lifestyle">Summarizations for
        data channel <em>lifestyle</em></a>
        -   <a href="#data-split" id="toc-data-split">Data Split</a>
        -   <a href="#data-manipulation-for-statistics"
            id="toc-data-manipulation-for-statistics">Data manipulation for
            statistics</a>
        -   <a href="#belknap---summary-statistics"
            id="toc-belknap---summary-statistics">Belknap - Summary Statistics</a>
        -   <a href="#monti---summary-statistics"
            id="toc-monti---summary-statistics">Monti - Summary Statistics</a>
        -   <a href="#monti---graphs-3" id="toc-monti---graphs-3">Monti - Graphs
            (3)</a>
        -   <a href="#belknap---graphs-3" id="toc-belknap---graphs-3">Belknap -
            Graphs (3)</a>
        -   <a href="#subsetting-variables-for-modeling"
            id="toc-subsetting-variables-for-modeling">Subsetting Variables for
            Modeling</a>
    -   <a href="#modeling" id="toc-modeling">Modeling</a>
        -   <a href="#belknap---linear-regression-model-explanation"
            id="toc-belknap---linear-regression-model-explanation">Belknap - Linear
            Regression Model Explanation</a>
        -   <a href="#monti---linear-regression-model-lasso-regression"
            id="toc-monti---linear-regression-model-lasso-regression">Monti - Linear
            Regression Model (LASSO Regression)</a>
        -   <a href="#belknap---linear-regression-model"
            id="toc-belknap---linear-regression-model">Belknap - Linear Regression
            Model</a>
        -   <a href="#belknap---random-forest-model--explanation"
            id="toc-belknap---random-forest-model--explanation">Belknap - Random
            Forest Model &amp; Explanation</a>
        -   <a href="#monti---boosted-tree-model--explanation"
            id="toc-monti---boosted-tree-model--explanation">Monti - Boosted Tree
            Model &amp; Explanation</a>
    -   <a href="#comparison--conclusion---monti"
        id="toc-comparison--conclusion---monti">Comparison &amp; Conclusion -
        Monti</a>

# Report for *lifestyle* Channel

This report contains Exploratory Data Analysis (EDA) about this data
channel and a modeling section applying three regression methods.

## Introduction

The objective of this analysis is to provide a comprehensive overview
about publication metrics and their relationship with the number of
shares that those publications presented during the study period. These
data have been collected from Mashable website, one of the largest news
websites from which the content of all the lifestyle channel articles
published in 2013 and 2014 was extracted. These data were originally
collected analyzed By Fernandes et al. (2015) work, in which the authors
performed classification task comparing several machine learning
algorithms. In the present study, the subset of the data used by
Fernandes et al.(2015) corresponding to the data channel lifestyle is
used for regression purposes. The response variable is the number of
`shares` that the papers presented after publication. In other words, we
will try to predict the number of shares the papers will have before
publication. To perform the regression, Random Forest, Boosting, and
Multiple Linear Regression are used. More information about the methods
will be provided in further sections.

Some metrics have been calculated based on the information obtained from
Marshable website. For instance, the Latent Dirichlet Allocation (LDA)
was applied to the data set to identify the 5 top relevant topics and
then measure the closeness of the current article to such topic. There
are 5 relevance of topic metrics according to LDA:

-   `LDA_00`: Closeness to LDA topic 0  
-   `LDA_01`: Closeness to LDA topic 1  
-   `LDA_02`: Closeness to LDA topic 2  
-   `LDA_03`: Closeness to LDA topic 3  
-   `LDA_04`: Closeness to LDA topic 4

Additionally, some quality metrics related to the keywords have been
calculated and will be used in this analysis. These metrics represent
the average number of shares for publications with worst, best, and
average keywords. The classification of keywords under these groups was
made by the authors of the original paper. The keyword metrics are shown
below.

-   `kw_avg_min`: Worst keyword (avg. shares)  
-   `kw_avg_max`: Best keyword (avg. shares)  
-   `kw_avg_avg`: Avg. keyword (avg. shares)

Article content metrics were also used in this study. These are general
metrics about the body of the publication that can influence the number
of shares of that paper. The content summary metrics are shown below.

\-`num_videos`: Number of videos  
-`n_tokens_content`: Number of words in the content  
-`n_non_stop_unique_tokens`: Rate of unique non-stop words in the
content  
-`num_hrefs`: Number of links  
-`num_self_hrefs`: Number of links to other articles published by
Mashable  
-`average_token_length`: Average length of the words in the content

These data were collected during 2013 and 2014 on daily basis. To
represent time dependent information, a binary variable indicating
whether the publication was made in a weekend or weekday, `is_weekend`
is used.

## Required Packages

Before we can begin our analysis, we must load in the following
packages:

``` r
library(tidyverse)
library(caret)
library(PerformanceAnalytics)
library(knitr)
```

`Tidyverse` is used for data management and plotting through dplyr and
ggplot packages. `Caret` package is used for data splitting and
modeling. `knitr` package is used to provide nice looking tables.
`PerformanceAnalytics` is used for nice correlation plots assisting in
the visualization.

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
# reading in the data set
rawData <- read_csv("../OnlineNewsPopularity.csv")

# creating new variable to have more comprehensive names for data channels.
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

# assigning channel data to R objects.
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

To select the appropriate data channel based on the `params$channel`, we
created a function `selectData` which would return the appropriate data
set and assign it to the data set `activeData`. This will be the file we
will use for the remainder of the report.

``` r
# function to assign automated calls for the different data channels
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

# activating corresponding data set.
dataChannelSelect <- params$channel

activeData <- selectData(dataChannelSelect)
```

## Summarizations for data channel *lifestyle*

In this section, we will perform EDA for the data channel lifestyle

### Data Split

This section splits the data set into training and test sets for the
proportion of 70/30. The data summarization will be conducted on the
training set. To split the data, the function `createDataPartition()`,
from `caret` package, was used with the argument `p=0.7` to represent
80% of the data should be in the split. The function `set.seed(555)` was
used to fix the random seed. The code below shows the creation of
training and test sets.

``` r
set.seed(555)

trainIndex <- createDataPartition(activeData$shares, p = 0.7, list = FALSE)

activeTrain <- activeData[trainIndex, ]

activeTest <- activeData[-trainIndex, ]
```

### Data manipulation for statistics

A new object is created in this section aiming to summarize publications
during weekdays and weekends and create factor levels for them to match
with `shares` variable. The functions `ifelse()` was used to vectorize
the IF-ELSE statements associated to `mutate()` which took care of
attaching the new variable to the data set. The function `factor()` was
used to explicitly coerce the days of week into levels of the newly
created categorical variable “Day”.

``` r
# IF-ELSE statements
statsData <- activeTrain %>%
  mutate(Day = ifelse(weekday_is_monday == 1, "Monday", 
                      ifelse(weekday_is_tuesday == 1, "Tuesday", 
                      ifelse(weekday_is_wednesday == 1, "Wednesday", 
                      ifelse(weekday_is_thursday == 1, "Thursday", 
                      ifelse(weekday_is_friday == 1, "Friday", 
                      ifelse(weekday_is_saturday == 1, "Saturday", 
                      ifelse(weekday_is_sunday == 1, "Sunday",
                             "missingdata")))))))) %>%
  mutate(Weekend = ifelse(is_weekend == 1, "Yes", "No"))

# Assigning factor levels
statsData$Day <- factor(statsData$Day, 
                levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                           "Friday", "Saturday", "Sunday"))
```

### Belknap - Summary Statistics

The following table gives us information about the summary statistics
for the number of shares for articles in the data channel lifestyle. The
`summary()` function was used to extract these metrics.

``` r
summary(activeTrain$shares)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      28    1100    1700    3832    3225  208300

The following table gives us information about the average, median, and
standard deviation for the number of shares based on whether the post
was made on a weekend or a weekday. The variable “weekend” was grouped,
via `grouped_by()`, and for each level the sum, average, median, and
standard deviation of shares were calculated via `sum()`, `mean()`,
`meadian()`, `sd()`, and `summarise()` functions. The summary table is
shown below.

``` r
statsData %>% 
  group_by(Weekend) %>%
  summarise(sumShares = sum(shares), avgShares = mean(shares), medShares = median(shares), sdShares = sd(shares))
```

    ## # A tibble: 2 × 5
    ##   Weekend sumShares avgShares medShares sdShares
    ##   <chr>       <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 No        4550565     3814.      1600   10812.
    ## 2 Yes       1089417     3905.      2100    5089.

Likewise, this table gives us information about the number of shares by
the day of the week. The same functions were used here, by applied to
levels of variable “Day”. Also, the quantities maximum `max()` and
minimum `min()` number of shares by levels of “Day” were calculated.

``` r
statsData %>% 
  group_by(Day) %>%
  arrange(Day) %>%
  summarise(sumShares = sum(shares), avgShares = mean(shares), medShares = median(shares), sdShares = sd(shares), maxShares = max(shares),
            minShares = min(shares))
```

    ## # A tibble: 7 × 7
    ##   Day       sumShares avgShares medShares sdShares maxShares minShares
    ##   <fct>         <dbl>     <dbl>     <dbl>    <dbl>     <dbl>     <dbl>
    ## 1 Monday      1064612     4952.      1600   16978.    196700       109
    ## 2 Tuesday      962982     4151.      1500   14810.    208300        93
    ## 3 Wednesday    905368     3257.      1500    6332.     73100        95
    ## 4 Thursday     941320     3706.      1600    6468.     56000        28
    ## 5 Friday       676283     3160.      1500    4751.     40400       127
    ## 6 Saturday     541450     4133.      2000    5800.     43000       446
    ## 7 Sunday       547967     3702.      2150    4375.     33100       613

### Monti - Summary Statistics

First, we will analyse the frequency of occurrence of publications on
each day of the week. The one-way contingency table below presents those
frequencies.

``` r
table(statsData$Day)
```

    ## 
    ##    Monday   Tuesday Wednesday  Thursday    Friday  Saturday    Sunday 
    ##       215       232       278       254       214       131       148

A second discrete analysis performed here is the two-way contingency
table related to the discretization of the response variable if we
divided `shares` into two categories. The function `cut()` was used for
the end. In this case, we count the frequency of the number of
publications in days of week with the two levels of response variable.
These levels represent smaller number of shares (on the left) and larger
number of shares (on the right). The table below shows this counting.

``` r
table(statsData$Day, cut(statsData$shares, breaks = 2))
```

    ##            
    ##             (-180,1.04e+05] (1.04e+05,2.09e+05]
    ##   Monday                213                   2
    ##   Tuesday               231                   1
    ##   Wednesday             278                   0
    ##   Thursday              254                   0
    ##   Friday                214                   0
    ##   Saturday              131                   0
    ##   Sunday                148                   0

An important EDA analysis for regression tasks is the calculation of
correlation matrix. The function `cor()` is used in this section to
return the top 10 most correlated potential predictor variables with the
response variable `shares`. The code below presents the process of
obtaining these variables and their respective correlations with the
response variable.

``` r
var_sel = select(activeTrain,starts_with("LDA_"), average_token_length,
         is_weekend, n_tokens_content, n_non_stop_unique_tokens, num_hrefs,
         num_self_hrefs, num_videos, average_token_length, kw_avg_min, 
         kw_avg_max, kw_avg_avg, is_weekend)

# correlation matrix
correlation = cor(activeTrain$shares, var_sel)

# sorting the highest correlations
p = sort(abs(correlation), decreasing = T)

# getting column ID
var_id = which(abs(correlation) %in% p[1:10])

# collecting variable names
var_cor = colnames(correlation)[var_id]

#combining names with correlations
tbcor = cbind(var_cor, correlation[var_id])

# converting to tibble
tbcor = as_tibble(tbcor)

# updating column names
colnames(tbcor)=c("Variables","Correlation")

# rounding the digits
tbcor$Correlation = round(as.numeric(tbcor$Correlation),3)

# nice printing with kable
kable(tbcor, caption = "Top 10 Response Correlated Variables")
```

| Variables            | Correlation |
|:---------------------|------------:|
| LDA_00               |       0.026 |
| LDA_02               |      -0.043 |
| LDA_03               |       0.045 |
| LDA_04               |      -0.030 |
| average_token_length |      -0.032 |
| n_tokens_content     |       0.042 |
| num_hrefs            |       0.043 |
| num_videos           |       0.110 |
| kw_avg_min           |       0.026 |
| kw_avg_avg           |       0.097 |

Top 10 Response Correlated Variables

The variables that present most correlation with the response variable
`shares` are LDA_00, LDA_02, LDA_03, LDA_04, average_token_length,
n_tokens_content, num_hrefs, num_videos, kw_avg_min, kw_avg_avg. These
variables will be studied in more depth via PCA to understand the
orientation of the most variable potential predictors. The code below
presents the PCA analysis as part of the EDA.

``` r
id = which(colnames(activeTrain) %in% var_cor)

# PCA
PC = prcomp(activeTrain[,id], center = TRUE, scale = TRUE)

pc_directions=as.data.frame(PC$rotation)

kable(pc_directions, caption="PCs for EDA", digits = 3)
```

|                      |    PC1 |    PC2 |    PC3 |    PC4 |    PC5 |    PC6 |    PC7 |    PC8 |    PC9 |   PC10 |
|:---------------------|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|
| n_tokens_content     | -0.212 |  0.117 | -0.338 |  0.340 | -0.267 |  0.524 | -0.428 | -0.428 |  0.004 | -0.003 |
| num_hrefs            | -0.365 |  0.315 | -0.431 |  0.108 | -0.044 | -0.023 | -0.040 |  0.731 | -0.177 |  0.017 |
| num_videos           | -0.130 |  0.103 |  0.190 |  0.095 | -0.423 |  0.413 |  0.757 |  0.032 |  0.058 | -0.004 |
| average_token_length |  0.036 |  0.077 | -0.474 |  0.323 | -0.191 | -0.633 |  0.325 | -0.346 |  0.039 | -0.013 |
| kw_avg_min           | -0.126 |  0.074 |  0.516 |  0.650 |  0.120 | -0.144 | -0.058 | -0.008 | -0.502 | -0.011 |
| kw_avg_avg           | -0.428 |  0.276 |  0.333 |  0.131 |  0.018 | -0.211 | -0.143 |  0.009 |  0.740 |  0.020 |
| LDA_00               | -0.328 | -0.648 | -0.132 |  0.190 |  0.224 |  0.068 |  0.114 |  0.075 |  0.129 | -0.573 |
| LDA_02               |  0.121 | -0.251 |  0.192 | -0.048 | -0.796 | -0.229 | -0.316 |  0.217 | -0.018 | -0.222 |
| LDA_03               | -0.443 |  0.324 |  0.092 | -0.500 | -0.036 | -0.135 |  0.010 | -0.314 | -0.350 | -0.447 |
| LDA_04               |  0.534 |  0.448 | -0.012 |  0.180 |  0.105 |  0.113 | -0.013 |  0.104 |  0.159 | -0.649 |

PCs for EDA

### Monti - Graphs (3)

The plot below presents histograms, scatter plots, and correlations in a
bivariate structure of the top 5 variables chosen the correlation
analysis. Notice the shape of the distributions and the values of the
correlations for the first column, which the one related to the response
variable `shares`.

``` r
# bivariate correlation plot
cor_data <- select(activeTrain,shares,var_id[1:5])
chart.Correlation(cor_data, histogram=TRUE)
```

![](lifestyle_files/figure-gfm/correlplot-1.png)<!-- -->

The biplot below presents the PC1 and PC2 from the PCA analysis. The
function `ggplot()` was used to create the plot and the segments created
via `geom_segment()` were rescaled so that we could better see the
variable names. The most variation in the data is contained in the PC1,
hence, the most important variables in the data approximately are
oriented towards the axis of PC1 and, therefore, may be good predictors
for the `shares` response. Likewise, for PC2, which contains the second
most variability in the data set, the variables that are oriented
approximately towards the axis of PC2 are the second most important
variables.

``` r
pc_df<-data.frame(PC$x)
# plotting PC1 and PC2 for the top 5 variables
# biplot(PC, cex = 1)
ggplot(pc_directions)+
  geom_point(data = pc_df, mapping = aes(x=PC1, y=PC2))+
  geom_segment(aes(x = 0, y = 0, yend = 50 * PC2, xend = 50 * PC1))+
  geom_label(mapping = aes(, x = 51 * PC1, y = 51 * PC2, label = row.names(pc_directions)))
```

![](lifestyle_files/figure-gfm/biplot-1.png)<!-- -->

The scatter plots below show the different levels of the variables
related to the LDA metrics, from 0 to 4, and graphs the relationship
with the response variable `shares`. The function `ggplot()` is used to
create the plot frame and `geom_point()`, `geom_smooth`, and
`facert_wrap()` function are used to plot the scatter points, the smooth
GAM (Generalized Additive Models) lines, and split the data by LDA type,
respectively. It is possible to see the behavior of the response
variable in relation to each LDA types.

``` r
LDA.dat = activeTrain %>%
  select(shares, starts_with("LDA")) %>%
  pivot_longer(cols = LDA_00:LDA_04, names_to = "LDA", values_to = "values")

# relationship between shares and LDA levels (facet_wrap+smooth)
ggplot(LDA.dat, aes(y = shares, x = values))+
  geom_point() + geom_smooth(method = "loess")+ facet_wrap(~LDA)+
labs(x = "LDA Values", y = "Shares", title = "Shares by LDA Types")
```

![](lifestyle_files/figure-gfm/LDAplot-1.png)<!-- -->

The scatter plots below show the different types of the variables
related to the Keyword metrics and graphs the relationship with the
response variable `shares`. The function `ggplot()` is used to create
the plot frame and `geom_point()`, `geom_smooth`, and `facert_wrap()`
function are used to plot the scatter points, the smooth GAM
(Generalized Additive Models) lines, and split the data by keyword type,
respectively. It is possible to see the behavior of the response
variable in relation to each of the 3 keyword metric types.

``` r
# relationship between shares and keyword metrics
kw.dat = activeTrain %>%
  select(shares, kw_avg_max, kw_avg_avg, kw_avg_min) %>%
  pivot_longer(cols = 2:4, names_to = "keyword", values_to = "values")

# relationship between shares and keyword metrics types (facet_wrap+smooth)
ggplot(kw.dat, aes(y = shares, x = values))+
  geom_point() + geom_smooth(method = "loess")+ facet_wrap(~keyword)+
labs(x = "Keyword Metric Values", y = "Shares", title = "Shares by Keyword Metric Types")
```

![](lifestyle_files/figure-gfm/keywordplot-1.png)<!-- -->

Finally, the scatter plots below show the different types of the
variables related to the Content metrics and graphs the relationship
with the response variable `shares`. The function `ggplot()` is used to
create the plot frame and `geom_point()`, `geom_smooth`, and
`facert_wrap()` function are used to plot the scatter points, the smooth
GAM (Generalized Additive Models) lines, and split the data by content
type, respectively. It is possible to see the behavior of the response
variable in relation to each of the 4 content metric types.

``` r
# relationship between shares and content metrics (facet_wrap+smooth)
cont.dat = activeTrain %>%
  select(shares, num_videos, n_tokens_content, n_non_stop_unique_tokens,
         num_hrefs, num_self_hrefs, average_token_length) %>%
  pivot_longer(cols = 2:7, names_to = "content", values_to = "values")

# relationship between shares and content metrics types (facet_wrap+smooth)
ggplot(cont.dat, aes(y = shares, x = values))+
  geom_point() + geom_smooth(method = "loess")+ facet_wrap(~content)+
labs(x = "Content Metric Values", y = "Shares", title = "Shares by Content Metric Types")
```

![](lifestyle_files/figure-gfm/Contentplot-1.png)<!-- -->

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

![](lifestyle_files/figure-gfm/titlewordcountGraph-1.png)<!-- -->

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

![](lifestyle_files/figure-gfm/positivewordrateGraph-1.png)<!-- -->

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

### Subsetting Variables for Modeling

The variables selected below are those described in the introduction of
this study and will be used in the modeling section. The function
`select()` was used to subset the corresponding variables from the
training and test sets and two new objects are created specially for the
modeling section, `dfTrain` and `dfTest`.

``` r
dfTrain = activeTrain %>%
  select(shares, starts_with("LDA_"), average_token_length,
         is_weekend, n_tokens_content, n_non_stop_unique_tokens, num_hrefs,
         num_self_hrefs, num_videos, average_token_length, kw_avg_min, 
         kw_avg_max, kw_avg_avg, is_weekend)

dfTest = activeTest %>%
  select(shares, starts_with("LDA_"), average_token_length,
         is_weekend, n_tokens_content, n_non_stop_unique_tokens, num_hrefs,
         num_self_hrefs, num_videos, average_token_length, kw_avg_min, 
         kw_avg_max, kw_avg_avg, is_weekend)
```

## Modeling

In this section, we will perform regression for prediction purposes for
the data channel lifestyle. All models were fitted using 5-fold
Cross-Validation via `train()` function from `caret` package.

### Belknap - Linear Regression Model Explanation

(add some thoughts here)

### Monti - Linear Regression Model (LASSO Regression)

The linear regression chosen for this next model is based on penalized
regression via LASSO method. This method has a particular advantage of
having a triangular shape of parameters search space so that it allows
the estimated coefficients to be zero. Hence, LASSO regression is also a
variable selection method. In this application, we will test the
prediction capability of LASSO regression only. It was tested a sequence
of values for the Regularization Parameter (lambda), a tuning parameter,
from 0 to 10 by 1 via `seq(0,10,1)` assigned to the `tuneGrid =`argument
in the `train()` function from `caret` package. The code below presents
the estimated coefficients for the best hyperparameter.

``` r
LASSO = train(shares~., data = dfTrain,
              method="glmnet",
              preProcess = c("center","scale"),
              tuneGrid = expand.grid(alpha = 1, lambda = seq(0,10,1)),
              trControl = trainControl(method="CV",number=5))

coef(LASSO$finalModel, LASSO$bestTune$lambda)
```

    ## 16 x 1 sparse Matrix of class "dgCMatrix"
    ##                                  s1
    ## (Intercept)              3831.50951
    ## LDA_00                    158.64273
    ## LDA_01                   -149.17998
    ## LDA_02                   -192.13433
    ## LDA_03                     28.91024
    ## LDA_04                      .      
    ## average_token_length     -836.44486
    ## is_weekend                -74.40562
    ## n_tokens_content          588.00951
    ## n_non_stop_unique_tokens  959.60449
    ## num_hrefs                 417.91111
    ## num_self_hrefs           -253.88364
    ## num_videos               1053.75502
    ## kw_avg_min               -301.12783
    ## kw_avg_max               -665.81076
    ## kw_avg_avg               1223.22709

The best lambda for this model is 10 and this value can be seen in the
table below which summarizes all the metrics for the 5-fold
cross-validation.

``` r
lasso_out = data.frame(LASSO$results)

kable(lasso_out, caption = "Output Training Metrics for LASSO",
      digits = 3)
```

| alpha | lambda |     RMSE | Rsquared |      MAE |   RMSESD | RsquaredSD |   MAESD |
|------:|-------:|---------:|---------:|---------:|---------:|-----------:|--------:|
|     1 |      0 | 9715.040 |    0.007 | 3637.904 | 3194.431 |      0.005 | 207.689 |
|     1 |      1 | 9715.040 |    0.007 | 3637.904 | 3194.431 |      0.005 | 207.689 |
|     1 |      2 | 9715.040 |    0.007 | 3637.904 | 3194.431 |      0.005 | 207.689 |
|     1 |      3 | 9714.983 |    0.007 | 3637.504 | 3194.708 |      0.005 | 207.573 |
|     1 |      4 | 9714.548 |    0.007 | 3636.681 | 3195.462 |      0.005 | 207.736 |
|     1 |      5 | 9714.116 |    0.007 | 3635.875 | 3196.215 |      0.005 | 207.893 |
|     1 |      6 | 9713.687 |    0.007 | 3635.072 | 3196.965 |      0.005 | 208.049 |
|     1 |      7 | 9713.259 |    0.007 | 3634.273 | 3197.711 |      0.005 | 208.203 |
|     1 |      8 | 9712.831 |    0.007 | 3633.477 | 3198.451 |      0.005 | 208.360 |
|     1 |      9 | 9712.403 |    0.007 | 3632.696 | 3199.184 |      0.005 | 208.524 |
|     1 |     10 | 9711.992 |    0.007 | 3631.930 | 3199.931 |      0.005 | 208.650 |

Output Training Metrics for LASSO

The plot below shows the RMSE by Regularization Parameter (lambda). It
is easy to see that RMSE is minimized when $\lambda$ = 10.

``` r
plot(LASSO)
```

![](lifestyle_files/figure-gfm/LASSOplot-1.png)<!-- -->

The validation step for LASSO regression is applied on the test set
after predicting the response variable for unseen data (test set). By
using `predict()` and `postResample()` functions, the metrics RMSE (Root
Means Squared Error), R2 (Coefficient of Determination), and MAE (Mean
Absolute Error) are calculated and displayed below.

``` r
metric_LASSO = postResample(pred = predict(LASSO, newdata = dfTest),
                            obs = dfTest$shares)

metric_LASSO
```

    ##         RMSE     Rsquared          MAE 
    ## 5.620176e+03 6.914860e-03 3.069375e+03

### Belknap - Linear Regression Model

(add some thoughts here)

``` r
lmFit = train(shares~., data = dfTrain,
              method="lm",
              preProcess = c("center","scale"),
              trControl = trainControl(method="CV",number=5))

summary(lmFit)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -29015  -2743  -1615    -28 200718 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               3831.51     257.22  14.896  < 2e-16 ***
    ## LDA_00                     168.06     270.61   0.621 0.534662    
    ## LDA_01                    -154.93     264.10  -0.587 0.557539    
    ## LDA_02                    -190.11     277.48  -0.685 0.493381    
    ## LDA_03                      39.31     311.91   0.126 0.899735    
    ## LDA_04                         NA         NA      NA       NA    
    ## average_token_length      -876.99     348.64  -2.515 0.011995 *  
    ## is_weekend                 -88.28     269.49  -0.328 0.743262    
    ## n_tokens_content           612.18     298.49   2.051 0.040454 *  
    ## n_non_stop_unique_tokens  1007.64     384.75   2.619 0.008912 ** 
    ## num_hrefs                  439.54     324.11   1.356 0.175254    
    ## num_self_hrefs            -268.76     277.97  -0.967 0.333756    
    ## num_videos                1065.76     263.95   4.038 5.68e-05 ***
    ## kw_avg_min                -331.40     305.06  -1.086 0.277512    
    ## kw_avg_max                -707.49     349.54  -2.024 0.043144 *  
    ## kw_avg_avg                1257.70     366.94   3.427 0.000626 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9869 on 1457 degrees of freedom
    ## Multiple R-squared:  0.03166,    Adjusted R-squared:  0.02235 
    ## F-statistic: 3.402 on 14 and 1457 DF,  p-value: 1.878e-05

(add some thoughts here)

``` r
lm_out = data.frame(lmFit$results)

kable(lm_out, caption = "Output Training Metrics for Linear Regression",
      digits = 3)
```

| intercept |     RMSE | Rsquared |      MAE |   RMSESD | RsquaredSD |  MAESD |
|:----------|---------:|---------:|---------:|---------:|-----------:|-------:|
| TRUE      | 9620.813 |    0.011 | 3653.139 | 3548.032 |      0.012 | 317.37 |

Output Training Metrics for Linear Regression

``` r
metric_lm = postResample(pred = predict(lmFit, newdata = dfTest), 
                         obs = dfTest$shares)

metric_lm
```

    ##         RMSE     Rsquared          MAE 
    ## 5.627622e+03 7.056420e-03 3.079557e+03

### Belknap - Random Forest Model & Explanation

(add some thoughts here about RF)

``` r
train.control = trainControl(method = "cv", number = 5)

rfFit <- train(shares~.,
               data = dfTrain,
               method = "rf",
               trControl = train.control,
               preProcess = c("center","scale"),
               tuneGrid = data.frame(mtry = 1:5))

rfFit$bestTune$mtry
```

    ## [1] 1

(add some thoughts here)

``` r
plot(rfFit)
```

![](lifestyle_files/figure-gfm/RFplot-1.png)<!-- -->

(add some thoughts here)

``` r
rf_out = data.frame(rfFit$results)

kable(rf_out, caption = "Output Training Metrics for Random Forest",
      digits = 3)
```

| mtry |      RMSE | Rsquared |      MAE |   RMSESD | RsquaredSD |   MAESD |
|-----:|----------:|---------:|---------:|---------:|-----------:|--------:|
|    1 |  9627.929 |    0.006 | 3608.038 | 3180.415 |      0.012 | 269.543 |
|    2 |  9755.406 |    0.006 | 3706.377 | 3096.203 |      0.010 | 266.618 |
|    3 |  9875.193 |    0.007 | 3773.281 | 3014.215 |      0.012 | 243.891 |
|    4 | 10011.375 |    0.007 | 3796.619 | 2880.292 |      0.012 | 215.541 |
|    5 | 10145.114 |    0.006 | 3847.374 | 2783.569 |      0.009 | 218.791 |

Output Training Metrics for Random Forest

(add some thoughts here)

``` r
RF_pred <- predict(rfFit, newdata = activeTest)

metric_rf = postResample(RF_pred, activeTest$shares)

metric_rf
```

    ##         RMSE     Rsquared          MAE 
    ## 5.662783e+03 7.056862e-03 3.120132e+03

### Monti - Boosted Tree Model & Explanation

(add some thoughts here)

``` r
tunG = expand.grid(n.trees = seq(25,200,25),
                      interaction.depth = 1:4,
                      shrinkage = 0.1,
                      n.minobsinnode = 10)

gbmFit <- train(shares~.,
               data = dfTrain,
               method = "gbm",
               preProcess = c("center","scale"),
               trControl = train.control,
               tuneGrid = tunG,
               verbose = FALSE
               )


gbmFit$bestTune$n.trees
```

    ## [1] 25

``` r
gbmFit$bestTune$interaction.depth
```

    ## [1] 3

The best n.trees and interaction.depth parameters for this model are 25
and 3, respectively. These values can be seen in the table below, which
summarizes all the metrics for the 5-fold cross-validation. It is easy
to see that these values minimize the RMSE.

``` r
gbm_out = data.frame(gbmFit$results)

kable(gbm_out, caption = "Output Training Metrics for Boosting",
      digits = 3)
```

|     | shrinkage | interaction.depth | n.minobsinnode | n.trees |      RMSE | Rsquared |      MAE |   RMSESD | RsquaredSD |   MAESD |
|:----|----------:|------------------:|---------------:|--------:|----------:|---------:|---------:|---------:|-----------:|--------:|
| 1   |       0.1 |                 1 |             10 |      25 |  9709.411 |    0.005 | 3599.301 | 3205.502 |      0.008 | 354.866 |
| 9   |       0.1 |                 2 |             10 |      25 |  9702.848 |    0.004 | 3573.750 | 3188.915 |      0.007 | 339.809 |
| 17  |       0.1 |                 3 |             10 |      25 |  9696.755 |    0.004 | 3576.332 | 3183.151 |      0.008 | 353.758 |
| 25  |       0.1 |                 4 |             10 |      25 |  9711.516 |    0.008 | 3605.779 | 3091.440 |      0.013 | 320.866 |
| 2   |       0.1 |                 1 |             10 |      50 |  9730.198 |    0.004 | 3580.114 | 3186.039 |      0.006 | 342.605 |
| 10  |       0.1 |                 2 |             10 |      50 |  9800.573 |    0.004 | 3637.807 | 3130.709 |      0.004 | 351.154 |
| 18  |       0.1 |                 3 |             10 |      50 |  9796.191 |    0.003 | 3637.085 | 3085.322 |      0.004 | 285.422 |
| 26  |       0.1 |                 4 |             10 |      50 |  9744.222 |    0.009 | 3648.396 | 3088.820 |      0.011 | 362.278 |
| 3   |       0.1 |                 1 |             10 |      75 |  9744.546 |    0.005 | 3609.206 | 3183.040 |      0.009 | 394.874 |
| 11  |       0.1 |                 2 |             10 |      75 |  9874.591 |    0.004 | 3672.218 | 3081.316 |      0.004 | 357.645 |
| 19  |       0.1 |                 3 |             10 |      75 |  9911.234 |    0.003 | 3715.022 | 2985.519 |      0.003 | 290.963 |
| 27  |       0.1 |                 4 |             10 |      75 |  9842.136 |    0.007 | 3709.145 | 3027.012 |      0.007 | 339.850 |
| 4   |       0.1 |                 1 |             10 |     100 |  9748.809 |    0.003 | 3580.823 | 3202.005 |      0.006 | 402.347 |
| 12  |       0.1 |                 2 |             10 |     100 |  9952.721 |    0.003 | 3717.463 | 3011.265 |      0.004 | 331.547 |
| 20  |       0.1 |                 3 |             10 |     100 |  9914.841 |    0.003 | 3708.340 | 2949.377 |      0.002 | 310.295 |
| 28  |       0.1 |                 4 |             10 |     100 |  9895.012 |    0.005 | 3733.758 | 2965.514 |      0.003 | 371.105 |
| 5   |       0.1 |                 1 |             10 |     125 |  9817.304 |    0.003 | 3633.494 | 3146.323 |      0.005 | 343.831 |
| 13  |       0.1 |                 2 |             10 |     125 |  9958.687 |    0.003 | 3686.618 | 3026.416 |      0.004 | 331.371 |
| 21  |       0.1 |                 3 |             10 |     125 | 10056.568 |    0.003 | 3824.889 | 2897.939 |      0.002 | 333.708 |
| 29  |       0.1 |                 4 |             10 |     125 |  9991.121 |    0.005 | 3795.252 | 2946.160 |      0.005 | 331.499 |
| 6   |       0.1 |                 1 |             10 |     150 |  9815.005 |    0.003 | 3637.852 | 3138.950 |      0.005 | 342.902 |
| 14  |       0.1 |                 2 |             10 |     150 | 10003.390 |    0.003 | 3725.484 | 2972.919 |      0.004 | 338.205 |
| 22  |       0.1 |                 3 |             10 |     150 | 10066.480 |    0.002 | 3826.256 | 2875.815 |      0.002 | 329.674 |
| 30  |       0.1 |                 4 |             10 |     150 |  9979.831 |    0.007 | 3814.978 | 2955.563 |      0.008 | 351.207 |
| 7   |       0.1 |                 1 |             10 |     175 |  9806.222 |    0.003 | 3615.825 | 3127.238 |      0.005 | 342.813 |
| 15  |       0.1 |                 2 |             10 |     175 | 10050.568 |    0.003 | 3725.447 | 2920.496 |      0.003 | 333.275 |
| 23  |       0.1 |                 3 |             10 |     175 | 10139.783 |    0.003 | 3875.355 | 2801.861 |      0.002 | 345.105 |
| 31  |       0.1 |                 4 |             10 |     175 | 10055.018 |    0.007 | 3864.496 | 2876.643 |      0.006 | 290.053 |
| 8   |       0.1 |                 1 |             10 |     200 |  9833.345 |    0.003 | 3635.456 | 3122.960 |      0.004 | 343.931 |
| 16  |       0.1 |                 2 |             10 |     200 | 10082.899 |    0.003 | 3760.501 | 2910.303 |      0.005 | 353.729 |
| 24  |       0.1 |                 3 |             10 |     200 | 10176.142 |    0.003 | 3902.815 | 2795.644 |      0.002 | 313.003 |
| 32  |       0.1 |                 4 |             10 |     200 | 10041.545 |    0.007 | 3855.447 | 2937.363 |      0.007 | 313.465 |

Output Training Metrics for Boosting

The plot below shows the RMSE by Number of Boosting Iterations and
display Max Tree Depth lines for the 5-fold CV performed. It is easy to
see that RMSE is minimized when n.trees = 25 and interaction.depth = 3.

``` r
plot(gbmFit)
```

![](lifestyle_files/figure-gfm/boostingPlot-1.png)<!-- -->

The validation step for Boosting is applied on the test set after
predicting the response variable for unseen data (test set). By using
`predict()` and `postResample()` functions, the metrics RMSE (Root Means
Squared Error), R2 (Coefficient of Determination), and MAE (Mean
Absolute Error) are calculated and displayed below.

``` r
gbm_pred <- predict(gbmFit, newdata = activeTest)

metric_boosting = postResample(gbm_pred, activeTest$shares)

metric_boosting
```

    ##         RMSE     Rsquared          MAE 
    ## 5.999861e+03 1.119842e-03 3.224908e+03

## Comparison & Conclusion - Monti

For the overall comparison among all 4 created models in previous
sections, the test set was used for predictions and some quality of fit
metrics were calculated based on these prediction on unseen data. The
code below shows the function that returns the name of the best model
based on RMSE values estimated on the test set. The code below displays
the table comparing all 4 models.

``` r
bestMethod = function(x){
  
  bestm = which.min(lapply(1:length(x), function(i) x[[i]][1]))
  
  out = switch(bestm,
                "Random Forest",
                "Boosting",
               "LASSO Regression",
               "Linear Regression")
  
  return(out)
  
}

tb = data.frame(RF = metric_rf, Boosting = metric_boosting,
                LASSO = metric_LASSO, Linear = metric_lm)

kable(tb, caption = "Accuracy Metric by Ensemble Method on Test Set",
      digits = 3)
```

|          |       RF | Boosting |    LASSO |   Linear |
|:---------|---------:|---------:|---------:|---------:|
| RMSE     | 5662.783 | 5999.861 | 5620.176 | 5627.622 |
| Rsquared |    0.007 |    0.001 |    0.007 |    0.007 |
| MAE      | 3120.132 | 3224.908 | 3069.375 | 3079.557 |

Accuracy Metric by Ensemble Method on Test Set

After comparing all the 4 models fit throughout this analysis, the best
model was chosen based on the RMSE value, such that the model with
minimum RMSE is the “winner”. Therefore, the best model is LASSO
Regression based on RMSE metric. The RMSE, R2 (coefficient of
Determination), and MAE metrics for all 4 models can be seen in the
table above.
