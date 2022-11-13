Project 3
================
Kara Belknap & Cassio Monti
2022-11-5

-   <a href="#report-for-the-lifestyle-data-channel"
    id="toc-report-for-the-lifestyle-data-channel">Report for the
    <em>lifestyle</em> Data Channel</a>
    -   <a href="#introduction" id="toc-introduction">Introduction</a>
    -   <a href="#data-import-and-manipulation"
        id="toc-data-import-and-manipulation">Data Import and Manipulation</a>
        -   <a href="#required-packages" id="toc-required-packages">Required
            Packages</a>
        -   <a href="#read-in-the-data" id="toc-read-in-the-data">Read in the
            Data</a>
        -   <a href="#select-data-for-appropriate-data-channel"
            id="toc-select-data-for-appropriate-data-channel">Select Data for
            Appropriate Data Channel</a>
    -   <a href="#summarizations-for-the-lifestyle-data-channel"
        id="toc-summarizations-for-the-lifestyle-data-channel">Summarizations
        for the <em>lifestyle</em> Data Channel</a>
        -   <a href="#data-manipulation-for-eda"
            id="toc-data-manipulation-for-eda">Data Manipulation for EDA</a>
        -   <a href="#eda-summary-statistics" id="toc-eda-summary-statistics">EDA:
            Summary Statistics</a>
        -   <a href="#eda-graphical-analysis" id="toc-eda-graphical-analysis">EDA:
            Graphical Analysis</a>
    -   <a href="#modeling" id="toc-modeling">Modeling</a>
        -   <a href="#data-manipulation-for-modeling"
            id="toc-data-manipulation-for-modeling">Data Manipulation for
            Modeling</a>
        -   <a href="#linear-regression-modeling"
            id="toc-linear-regression-modeling">Linear Regression Modeling</a>
        -   <a href="#tree-based-modeling" id="toc-tree-based-modeling">Tree-Based
            Modeling</a>
    -   <a href="#model-comparison--conclusion"
        id="toc-model-comparison--conclusion">Model Comparison &amp;
        Conclusion</a>
    -   <a href="#reference-list" id="toc-reference-list">Reference List</a>

# Report for the *lifestyle* Data Channel

This report contains Exploratory Data Analysis (EDA) about the lifestyle
data channel and a modeling section applying three regression methods
which attempt to predict trends about article sharing on the Mashable
website.

## Introduction

The objective of this analysis is to provide a comprehensive overview
about publication metrics and their relationship with the number of
shares that those publications presented during the study period. These
data have been collected from Mashable website, one of the largest news
websites from which the content of all the lifestyle channel articles
published in 2013 and 2014 was extracted. The full data description can
be found
[here](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity).
These data were originally collected and analyzed by Fernandes et
al. (2015), in which the authors performed classification task comparing
several machine learning algorithms. In the present study, a subset of
the data used by Fernandes et al.(2015) corresponding to the data
channel lifestyle is used for regression purposes. The response variable
is the number of `shares` that the papers presented after publication.
In other words, we will try to predict the number of shares that the
papers will have before publication and evaluate the prediction of each
selected model based on some common metrics, such as RMSE (Root Mean
Squared Error), $R^2$ (Coefficient of Determination), and MAE (Mean
Absolute Error) applied to the test set. To perform the regression, the
methods Random Forest, Boosting, Multiple Linear Regression, and LASSO
regression will be used. More information about the methods will be
corresponding in further sections.

Some metrics have been calculated based on the information obtained from
Mashable website. For instance, the Latent Dirichlet Allocation (LDA)
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

-   `num_videos`: Number of videos  
-   `n_tokens_content`: Number of words in the content  
-   `n_non_stop_unique_tokens`: Rate of unique non-stop words in the
    content  
-   `num_hrefs`: Number of links  
-   `num_self_hrefs`: Number of links to other articles published by
    Mashable  
-   `average_token_length`: Average length of the words in the content

These data were collected during 2013 and 2014 on daily basis. To
represent time dependent information, a binary variable indicating
whether the publication was made in a weekend or weekday, `is_weekend`
is used.

## Data Import and Manipulation

### Required Packages

Before we can begin our analysis, we must load in the following
packages:

``` r
library(tidyverse)
library(caret)
library(GGally)
library(knitr)
```

`Tidyverse` is used for data management and plotting through dplyr and
ggplot packages. `Caret` package is used for data splitting and
modeling. `GGally` is used for nice correlation and exploratory plots
assisting in the visualization. `knitr` package is used to provide nice
looking tables.

### Read in the Data

Using the data file `OnlineNewsPopularity.csv`, we will read in the data
and add a new column corresponding to the type of data channel from
which the data was classified. The new variable will be called
`dataChannel`. Note that there are some rows that are unclassified
according to the six channels of interest and those are indicated by
`other`. The data indicated by `other` was excluded from all reports
since the data had not been assigned to one of our channels of interest.

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

### Select Data for Appropriate Data Channel

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

## Summarizations for the *lifestyle* Data Channel

In this section, we will perform EDA for the data channel lifestyle.

### Data Manipulation for EDA

#### Data Split

This section splits the data set into training and test sets for the
proportion of 70/30. The data summarizing will be conducted on the
training set. To split the data, the function `createDataPartition()`,
from `caret` package, was used with the argument `p=0.7` to represent
70% of the data should be in the split. The function `set.seed(555)` was
used to fix the random seed. The code below shows the creation of
training and test sets.

``` r
set.seed(555)

trainIndex <- createDataPartition(activeData$shares, p = 0.7, list = FALSE)

activeTrain <- activeData[trainIndex, ]

activeTest <- activeData[-trainIndex, ]
```

#### Data manipulation for statistics

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

### EDA: Summary Statistics

#### Summary Statistics, Number of Articles Shared

The following table gives us information about the summary statistics
for the number of shares for articles in the data channel lifestyle. The
`summary()` function was used to extract these metrics.

``` r
summary(activeTrain$shares)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      28    1100    1700    3832    3225  208300

#### Summary Statistics, Number of Articles Shared, Weekend vs. Weekday

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
  summarise(sumShares = sum(shares), avgShares = mean(shares), medShares = median(shares), sdShares = sd(shares)) %>% 
  kable(caption = "Statistics for Shares for Weekend or Weekdays") 
```

| Weekend | sumShares | avgShares | medShares |  sdShares |
|:--------|----------:|----------:|----------:|----------:|
| No      |   4550565 |  3814.388 |      1600 | 10811.518 |
| Yes     |   1089417 |  3904.720 |      2100 |  5089.105 |

Statistics for Shares for Weekend or Weekdays

#### Summary Statistics, Articles Shared by Day of Week

Likewise, this table gives us information about the number of shares by
the day of the week. The same functions were used here, by applied to
levels of variable “Day”. Also, the quantities maximum `max()` and
minimum `min()` number of shares by levels of “Day” were calculated.

``` r
statsData %>% 
  group_by(Day) %>%
  arrange(Day) %>%
  summarise(sumShares = sum(shares), avgShares = mean(shares), medShares = median(shares), sdShares = sd(shares), maxShares = max(shares),
            minShares = min(shares)) %>% 
  kable(caption = "Statistics for Shares Across Days of Week")
```

| Day       | sumShares | avgShares | medShares |  sdShares | maxShares | minShares |
|:----------|----------:|----------:|----------:|----------:|----------:|----------:|
| Monday    |   1064612 |  4951.684 |      1600 | 16977.651 |    196700 |       109 |
| Tuesday   |    962982 |  4150.784 |      1500 | 14809.894 |    208300 |        93 |
| Wednesday |    905368 |  3256.719 |      1500 |  6331.518 |     73100 |        95 |
| Thursday  |    941320 |  3705.984 |      1600 |  6468.343 |     56000 |        28 |
| Friday    |    676283 |  3160.201 |      1500 |  4750.602 |     40400 |       127 |
| Saturday  |    541450 |  4133.206 |      2000 |  5800.116 |     43000 |       446 |
| Sunday    |    547967 |  3702.480 |      2150 |  4374.984 |     33100 |       613 |

Statistics for Shares Across Days of Week

#### Total Articles Shared by Day of Week

Next, we will analyse the frequency of occurrence of publications on
each day of the week. The one-way contingency table below presents those
frequencies.

``` r
table(statsData$Day)
```

    ## 
    ##    Monday   Tuesday Wednesday  Thursday    Friday  Saturday    Sunday 
    ##       215       232       278       254       214       131       148

#### Contingency Table

Another discrete analysis performed here is the two-way contingency
table related to the discretization of the response variable if we
divided `shares` into two categories. The function `cut()` was used for
the end. In this case, we count the frequency of the number of
publications in the weekend versus working days with the two levels of
response variable. These levels represent smaller number of shares (on
the left) and larger number of shares (on the right). The table below
shows this counting. In the table below, 0 (zero) represents working
days and 1 (one) represents weekends.

``` r
table(activeTrain$is_weekend, cut(activeTrain$shares, breaks = 2)) %>%
  kable(caption = "Frequency of Shares in Weekend vs in Working Days")
```

|     | (-180,1.04e+05\] | (1.04e+05,2.09e+05\] |
|:----|-----------------:|---------------------:|
| 0   |             1190 |                    3 |
| 1   |              279 |                    0 |

Frequency of Shares in Weekend vs in Working Days

#### Correlation Matrix

An important EDA analysis for regression tasks is the correlation
matrix. The function `cor()` is used in this section to return the top
10 most correlated potential predictor variables with the response
variable `shares`. The code below presents the process of obtaining
these variables and their respective correlations with the response
variable. The correlations are clearly small for this case, which may
difficult the modeling process and produce low quality of prediction
metrics.

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

#### Principal Components Analysis (PCA)

The variables that present most correlation with the response variable
`shares` are LDA_00, LDA_02, LDA_03, LDA_04, average_token_length,
n_tokens_content, num_hrefs, num_videos, kw_avg_min, kw_avg_avg. These
variables will be studied in more depth via PCA to understand the
orientation of the most important potential predictors. The code below
presents the PCA analysis as part of the EDA. The 10 PCs displayed in
the table below correspond to the most variable combination of the 10
predictors, which the PC1 has the most variation in the data, PC2
presents the second most variation and so on. The coefficients
associated to each variable are the loadings and they give the idea of
importance of that particular variable to the variance of the 10
predictor variables. The negative values only mean that the orientation
of the weights are opposite in the same PC. Since the first PC has the
largest variability, it is possible to say that the variables with more
weights in PC1 are the most important variables for the variance of the
predictors. This variables are supposed to present large influence on
the explanation of the variance of the response variable. The table
below show these numbers.

``` r
id = which(colnames(activeTrain) %in% var_cor)

# PCA
PC = prcomp(activeTrain[,id], center = TRUE, scale = TRUE)

pc_directions=as.data.frame(PC$rotation)

kable(pc_directions, caption="Principal Components for EDA", digits = 3)
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

Principal Components for EDA

``` r
id2 = which(abs(pc_directions$PC1) %in% 
             sort(abs(pc_directions$PC1),dec= T)[1:3])
```

It is possible to see that the three most important variables in PC1 are
kw_avg_avg, LDA_03, LDA_04 from the table above. These variables are
considered the most important variables in terms of variance of the
predictor variables. Although the metrics for prediction are expected to
be poor, these variables are expected to show the most influence to the
explanation of the variance of the response `shares`.

### EDA: Graphical Analysis

#### Correlation Plot

The plot below presents histograms, scatter plots, and correlations in a
bivariate structure of the top 5 variables chosen in the correlation
analysis. Notice the shape of the distributions and the values of the
correlations for the first column, which the one related to the response
variable `shares`.

``` r
# bivariate correlation plot
cor_data <- select(activeTrain,shares,var_id[1:5])
ggpairs(cor_data)
```

![](lifestyle_files/figure-gfm/correlplot-1.png)<!-- -->

#### PCA: Biplot

The biplot below presents the PC1 and PC2 from the PCA analysis. The
function `ggplot()` was used to create the plot and the segments created
via `geom_segment()` were re-scaled so that we could better see the
variable names. The most variation in the data is contained in the PC1,
hence, the most important variables in the data are approximately
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
  geom_label(mapping = aes(x = 51 * PC1, y = 51 * PC2, label = row.names(pc_directions)))
```

![](lifestyle_files/figure-gfm/biplot-1.png)<!-- -->

#### Scatter Plots by LDA Value

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

#### Scatter Plots by Keyword Metrics

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

#### Scatter Plots by Content Metrics

The scatter plots below show the different types of the variables
related to the Content metrics and graphs the relationship with the
response variable `shares`. The function `ggplot()` is used to create
the plot frame and `geom_point()`, `geom_smooth`, and `facert_wrap()`
function are used to plot the scatter points, the smooth GAM
(Generalized Additive Models) lines, and split the data by content type,
respectively. It is possible to see the behavior of the response
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

#### Scatter Plot of Title Words

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

#### Scatter Plot of Positive Words

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

#### Scatter Plot of Title Subjectivity

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

![](lifestyle_files/figure-gfm/titleSubjectivityGraph-1.png)<!-- -->

## Modeling

In this section, we will perform regression for prediction purposes for
the data channel lifestyle. All models were fitted using 5-fold
Cross-Validation via `train()` function from `caret` package. All
variables were scaled and centered as well.

### Data Manipulation for Modeling

#### Subsetting Variables for Modeling

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

### Linear Regression Modeling

Linear regression is a modeling technique by which one attempts to model
a response variable (in this case `shares`) with one or more explanatory
variables using a straight line. If there is only one explanatory
variable, you would call this simple linear regression. Using more than
one explanatory variable is called multiple linear regression. For the
purposes of this report, we will be looking at multiple linear
regression (MLR).

The basic formula that is used for multiple linear regression is

$$ 
Y_i = \beta_0 + \beta_{1}x_{1i} + \beta_{2}x_{2i} + ... + \beta_{n}x_{ni} + E_i
$$ Here:  
- $Yi$ is our response variable for the $i^{th}$ observation  
- $x_{i}$ is the value of our explanatory variable for the \$i^{th}
observation for each explanatory variable (1-n)  
- $\beta_0$ is the y intercept  
- $\beta_{1,...,n)$ is the regression coefficient corresponding to the
explanatory variable of interest (1-n)  
- $E_i$ is an error parameter

The goal of linear regression is to model the fit by minimizing the sum
of squared errors.

$$
SSE = \sum_{i=1}^n(y_{i} - \hat{y}_{i})^2
$$ Here:  
- $y_i$ is the actual value  
- $\hat{y}_{i}$ is the predicted value

In R, MLR is generally done with the function `lm`. There are also a
variety of other methods that fall under the umbrella of MLR, namely
LASSO regression which we will explore as part of this analysis.

#### Linear Regression Model \#1: Multiple Linear Regression Using `lm`

Here, modeling for linear regression is done with the `caret` package
using the method `lm`. The `summary` function gives us the regression
coefficients.

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

The following table shows the output training metrics for this linear
regression.

``` r
lm_out = data.frame(lmFit$results)

kable(lm_out, caption = "Output Training Metrics for Linear Regression",
      digits = 3)
```

| intercept |    RMSE | Rsquared |      MAE |   RMSESD | RsquaredSD |   MAESD |
|:----------|--------:|---------:|---------:|---------:|-----------:|--------:|
| TRUE      | 9716.31 |    0.007 | 3640.022 | 3192.437 |      0.005 | 207.165 |

Output Training Metrics for Linear Regression

The following shows the RMSE, $R^2$, and MAE values for the model as it
performed on predicting the test set.

``` r
metric_lm = postResample(pred = predict(lmFit, newdata = dfTest), 
                         obs = dfTest$shares)

metric_lm
```

    ##         RMSE     Rsquared          MAE 
    ## 5.627622e+03 7.056420e-03 3.079557e+03

#### Linear Regression Model \#2: LASSO Regression using `glmnet`

The linear regression chosen for this next model is based on penalized
regression via LASSO regression. This method has a particular advantage
of having a triangular shape of parameters search space so that it
allows the estimated coefficients to be zero. This is due to LASSO
optimization that has in the loss function the penalty associating the
sum of the absolute value of the parameters multiplied by `lambda`, the
penalty term (hyperparameter). Hence, LASSO regression is also a
variable selection method. In this application, we will test the
prediction capability of LASSO regression only. It was tested a sequence
of values for the Regularization Parameter (`lambda`), a tuning
parameter, from 0 to 10 by 1 via `seq(0,10,1)` assigned to the
`tuneGrid =`argument in the `train()` function from `caret` package. The
code below presents the estimated coefficients for the best
hyperparameter.

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

The best `lambda` for this model is 10 and this value can be seen in the
table below which summarizes all the metrics for the 5-fold
cross-validation.

``` r
lasso_out = data.frame(LASSO$results)

kable(lasso_out, caption = "Output Training Metrics for LASSO",
      digits = 3)
```

| alpha | lambda |     RMSE | Rsquared |      MAE |   RMSESD | RsquaredSD |   MAESD |
|------:|-------:|---------:|---------:|---------:|---------:|-----------:|--------:|
|     1 |      0 | 9619.544 |    0.011 | 3650.817 | 3550.207 |      0.012 | 318.474 |
|     1 |      1 | 9619.544 |    0.011 | 3650.817 | 3550.207 |      0.012 | 318.474 |
|     1 |      2 | 9619.552 |    0.011 | 3650.812 | 3550.218 |      0.012 | 318.467 |
|     1 |      3 | 9619.430 |    0.011 | 3650.173 | 3550.642 |      0.012 | 318.389 |
|     1 |      4 | 9618.976 |    0.011 | 3649.184 | 3551.509 |      0.012 | 318.729 |
|     1 |      5 | 9618.527 |    0.011 | 3648.210 | 3552.374 |      0.012 | 319.070 |
|     1 |      6 | 9618.081 |    0.011 | 3647.256 | 3553.236 |      0.012 | 319.418 |
|     1 |      7 | 9617.637 |    0.010 | 3646.317 | 3554.095 |      0.012 | 319.759 |
|     1 |      8 | 9617.189 |    0.010 | 3645.388 | 3554.947 |      0.013 | 320.086 |
|     1 |      9 | 9616.740 |    0.010 | 3644.467 | 3555.790 |      0.013 | 320.416 |
|     1 |     10 | 9616.288 |    0.010 | 3643.565 | 3556.618 |      0.013 | 320.740 |

Output Training Metrics for LASSO

The plot below shows the RMSE by Regularization Parameter (`lambda`). It
is easy to see that RMSE is minimized when `lambda` = 10.

``` r
plot(LASSO)
```

![](lifestyle_files/figure-gfm/LASSOplot-1.png)<!-- -->

The validation step for LASSO regression is applied on the test set
after predicting the response variable for unseen data (test set). By
using `predict()` and `postResample()` functions, the metrics RMSE (Root
Means Squared Error), Rsquared (Coefficient of Determination), and MAE
(Mean Absolute Error) are calculated and displayed below.

``` r
metric_LASSO = postResample(pred = predict(LASSO, newdata = dfTest),
                            obs = dfTest$shares)

metric_LASSO
```

    ##         RMSE     Rsquared          MAE 
    ## 5.620176e+03 6.914860e-03 3.069375e+03

### Tree-Based Modeling

The next two models, Random Forest and Boosted Tree, are both types of
tree-based modeling methods. Generally speaking, in a tree-based
modeling method, the predictor space is split into regions, with
different predictions for each region. In the case of regression trees
where the goal is to predict a continuous response, the mean of
observations for a given region is typically used to make the
predictions.

To make the predictions, the trees are split using recursive binary
splitting. For every possible value of each predictor, find the residual
sum of squares (RSS) and try to minimize that. The process is repeated
with each split. Often, trees are grown very large and need to be cut
back using cost complexity pruning. This ensures that the model is not
overfit and will work well on prediction of new data.

#### Random Forest Model

In this section, we attempt to model the data using a Random Forest
model, which is a type of ensemble learning which averages multiple tree
models in order to lower the variance of the final model and thus
improve our prediction.

In a random forest model, we first begin by creating multiple trees from
bootstrap samples. A random subset of predictors is used to create each
bootstrap sample. The predictors are selected randomly to prevent the
trees from being correlated. If the random subset was not used (as in
another tree based method called bagging), the trees would likely all
choose the same predictors for the first split. Choosing the splits
randomly avoids this correlation. The number of predictors is specified
by `mtry`. The maximum number of predictors for a regression model is
generally chosen to be the total number of predictors divided by 3. Once
the bootstrap sample statistics are collected, they are averaged and
used to select a final model.

Random forest models use “out of bag” error to test the data using
samples from the original data set that were not included in a
particular bootstrap data set.

For the random forest model, we will use the `train` function from the
`caret` package. We set the `mtry` to select 1-5 predictors.

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

The best `mtry` for this particular model was 1.

The following plot shows the RMSE values for each of the tune. The
objective in random forest modeling is to choose the model with the
lowest RMSE.

``` r
plot(rfFit)
```

![](lifestyle_files/figure-gfm/RFplot-1.png)<!-- -->

The following table shows training metrics for the random forest model.
Again, the best model is the one that minimizes RMSE.

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

Now we will run a prediction on our test data split that we obtained
when we split the data based on a 70/30 split. The table shows the RMSE
value for the test data set, which is an indication of how well our
model worked to predict data that was not included when training the
original model. We can compare this model against other models to find
the model with the lowest RMSE.

``` r
RF_pred <- predict(rfFit, newdata = activeTest)

metric_rf = postResample(RF_pred, activeTest$shares)

metric_rf
```

    ##         RMSE     Rsquared          MAE 
    ## 5.662783e+03 7.056862e-03 3.120132e+03

#### Boosted Tree Model

In this section the Ensemble Learning algorithm Boosting will be
trained. Boosted tree method is one of the Tree based models most used
in data science because it presents a fitting strategy improves
sequentially throughout the iterations. Boosting uses single trees
fitted (each single tree has `d` splits) on the training data and
produces predictions off of that training. The residuals of this
prediction is, then, used as response variable for the next single tree
training step. New predictions are done for this new model. This process
occurs several times during `B` iterations and the predictions are
updated during the fitting process, being driven by the shrinkage
parameter, also called growth rate, `lambda`. These training features of
Boosting make this method to produce a reduction in the variance of the
predictions as well as gains in accuracy, mainly over Random Forest,
Bagging, and single tree. The shrinkage parameter will be set as 0.1 and
n.minobsinnode set as 10. The parameters n.tree and interaction.depth
will be chosen based on 5-fold cross-validation. The former will be
chosen from a sequence from 25 to 200, counting by 25. The latter will
be chosen from a sequence from 1 to 4. The code below shows the training
and tuning procedure and prints out the resultant values of the two
considered hyperparameters.

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

gbm_out <- gbm_out %>%
  arrange(RMSE)

kable(gbm_out, caption = "Output Training Metrics for Boosting",
      digits = 3, row.names = FALSE)
```

| shrinkage | interaction.depth | n.minobsinnode | n.trees |      RMSE | Rsquared |      MAE |   RMSESD | RsquaredSD |   MAESD |
|----------:|------------------:|---------------:|--------:|----------:|---------:|---------:|---------:|-----------:|--------:|
|       0.1 |                 3 |             10 |      25 |  9696.755 |    0.004 | 3576.332 | 3183.151 |      0.008 | 353.758 |
|       0.1 |                 2 |             10 |      25 |  9702.848 |    0.004 | 3573.750 | 3188.915 |      0.007 | 339.809 |
|       0.1 |                 1 |             10 |      25 |  9709.411 |    0.005 | 3599.301 | 3205.502 |      0.008 | 354.866 |
|       0.1 |                 4 |             10 |      25 |  9711.516 |    0.008 | 3605.779 | 3091.440 |      0.013 | 320.866 |
|       0.1 |                 1 |             10 |      50 |  9730.198 |    0.004 | 3580.114 | 3186.039 |      0.006 | 342.605 |
|       0.1 |                 4 |             10 |      50 |  9744.222 |    0.009 | 3648.396 | 3088.820 |      0.011 | 362.278 |
|       0.1 |                 1 |             10 |      75 |  9744.546 |    0.005 | 3609.206 | 3183.040 |      0.009 | 394.874 |
|       0.1 |                 1 |             10 |     100 |  9748.809 |    0.003 | 3580.823 | 3202.005 |      0.006 | 402.347 |
|       0.1 |                 3 |             10 |      50 |  9796.191 |    0.003 | 3637.085 | 3085.322 |      0.004 | 285.422 |
|       0.1 |                 2 |             10 |      50 |  9800.573 |    0.004 | 3637.807 | 3130.709 |      0.004 | 351.154 |
|       0.1 |                 1 |             10 |     175 |  9806.222 |    0.003 | 3615.825 | 3127.238 |      0.005 | 342.813 |
|       0.1 |                 1 |             10 |     150 |  9815.005 |    0.003 | 3637.852 | 3138.950 |      0.005 | 342.902 |
|       0.1 |                 1 |             10 |     125 |  9817.304 |    0.003 | 3633.494 | 3146.323 |      0.005 | 343.831 |
|       0.1 |                 1 |             10 |     200 |  9833.345 |    0.003 | 3635.456 | 3122.960 |      0.004 | 343.931 |
|       0.1 |                 4 |             10 |      75 |  9842.136 |    0.007 | 3709.145 | 3027.012 |      0.007 | 339.850 |
|       0.1 |                 2 |             10 |      75 |  9874.591 |    0.004 | 3672.218 | 3081.316 |      0.004 | 357.645 |
|       0.1 |                 4 |             10 |     100 |  9895.012 |    0.005 | 3733.758 | 2965.514 |      0.003 | 371.105 |
|       0.1 |                 3 |             10 |      75 |  9911.234 |    0.003 | 3715.022 | 2985.519 |      0.003 | 290.963 |
|       0.1 |                 3 |             10 |     100 |  9914.841 |    0.003 | 3708.340 | 2949.377 |      0.002 | 310.295 |
|       0.1 |                 2 |             10 |     100 |  9952.721 |    0.003 | 3717.463 | 3011.265 |      0.004 | 331.547 |
|       0.1 |                 2 |             10 |     125 |  9958.687 |    0.003 | 3686.618 | 3026.416 |      0.004 | 331.371 |
|       0.1 |                 4 |             10 |     150 |  9979.831 |    0.007 | 3814.978 | 2955.563 |      0.008 | 351.207 |
|       0.1 |                 4 |             10 |     125 |  9991.121 |    0.005 | 3795.252 | 2946.160 |      0.005 | 331.499 |
|       0.1 |                 2 |             10 |     150 | 10003.390 |    0.003 | 3725.484 | 2972.919 |      0.004 | 338.205 |
|       0.1 |                 4 |             10 |     200 | 10041.545 |    0.007 | 3855.447 | 2937.363 |      0.007 | 313.465 |
|       0.1 |                 2 |             10 |     175 | 10050.568 |    0.003 | 3725.447 | 2920.496 |      0.003 | 333.275 |
|       0.1 |                 4 |             10 |     175 | 10055.018 |    0.007 | 3864.496 | 2876.643 |      0.006 | 290.053 |
|       0.1 |                 3 |             10 |     125 | 10056.568 |    0.003 | 3824.889 | 2897.939 |      0.002 | 333.708 |
|       0.1 |                 3 |             10 |     150 | 10066.480 |    0.002 | 3826.256 | 2875.815 |      0.002 | 329.674 |
|       0.1 |                 2 |             10 |     200 | 10082.899 |    0.003 | 3760.501 | 2910.303 |      0.005 | 353.729 |
|       0.1 |                 3 |             10 |     175 | 10139.783 |    0.003 | 3875.355 | 2801.861 |      0.002 | 345.105 |
|       0.1 |                 3 |             10 |     200 | 10176.142 |    0.003 | 3902.815 | 2795.644 |      0.002 | 313.003 |

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
Squared Error), Rsquared (Coefficient of Determination), and MAE (Mean
Absolute Error) are calculated and displayed below.

``` r
gbm_pred <- predict(gbmFit, newdata = activeTest)

metric_boosting = postResample(gbm_pred, activeTest$shares)

metric_boosting
```

    ##         RMSE     Rsquared          MAE 
    ## 5.999861e+03 1.119842e-03 3.224908e+03

## Model Comparison & Conclusion

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
                "Multiple Linear Regression")
  
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
minimum RMSE is the “winner”. Therefore, the best model is **LASSO
Regression** based on RMSE metric. The RMSE, coefficient of
determination, and MAE metrics for all 4 models can be seen in the table
above.

## Reference List

K. Fernandes, P. Vinagre and P. Cortez. A Proactive Intelligent Decision
Support System for Predicting the Popularity of Online News. Proceedings
of the 17th EPIA 2015 - Portuguese Conference on Artificial
Intelligence, September, Coimbra, Portugal.
