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
            -   <a href="#data-split" id="toc-data-split">Data Split</a>
            -   <a href="#outlier-detection-and-cleaning"
                id="toc-outlier-detection-and-cleaning">Outlier Detection and
                Cleaning</a>
            -   <a href="#data-manipulation-for-statistics"
                id="toc-data-manipulation-for-statistics">Data manipulation for
                statistics</a>
        -   <a href="#eda-summary-statistics" id="toc-eda-summary-statistics">EDA:
            Summary Statistics</a>
            -   <a href="#summary-statistics-number-of-articles-shared"
                id="toc-summary-statistics-number-of-articles-shared">Summary
                Statistics, Number of Articles Shared</a>
            -   <a
                href="#summary-statistics-number-of-articles-shared-weekend-vs-weekday"
                id="toc-summary-statistics-number-of-articles-shared-weekend-vs-weekday">Summary
                Statistics, Number of Articles Shared, Weekend vs. Weekday</a>
            -   <a href="#summary-statistics-articles-shared-by-day-of-week"
                id="toc-summary-statistics-articles-shared-by-day-of-week">Summary
                Statistics, Articles Shared by Day of Week</a>
            -   <a href="#total-articles-shared-by-day-of-week"
                id="toc-total-articles-shared-by-day-of-week">Total Articles Shared by
                Day of Week</a>
            -   <a href="#contingency-table" id="toc-contingency-table">Contingency
                Table</a>
            -   <a href="#correlation-matrix" id="toc-correlation-matrix">Correlation
                Matrix</a>
            -   <a href="#principal-components-analysis-pca"
                id="toc-principal-components-analysis-pca">Principal Components Analysis
                (PCA)</a>
        -   <a href="#eda-graphical-analysis" id="toc-eda-graphical-analysis">EDA:
            Graphical Analysis</a>
            -   <a href="#correlation-plot" id="toc-correlation-plot">Correlation
                Plot</a>
            -   <a href="#pca-biplot" id="toc-pca-biplot">PCA: Biplot</a>
            -   <a href="#scatter-plots-by-lda-value"
                id="toc-scatter-plots-by-lda-value">Scatter Plots by LDA Value</a>
            -   <a href="#scatter-plots-by-keyword-metrics"
                id="toc-scatter-plots-by-keyword-metrics">Scatter Plots by Keyword
                Metrics</a>
            -   <a href="#scatter-plots-by-content-metrics"
                id="toc-scatter-plots-by-content-metrics">Scatter Plots by Content
                Metrics</a>
            -   <a href="#box-plot-of-shares-for-data-channel-lifestyle"
                id="toc-box-plot-of-shares-for-data-channel-lifestyle">Box Plot of
                Shares for Data Channel <em>lifestyle</em></a>
            -   <a href="#scatter-plot-of-title-words"
                id="toc-scatter-plot-of-title-words">Scatter Plot of Title Words</a>
            -   <a href="#scatter-plot-of-positive-words"
                id="toc-scatter-plot-of-positive-words">Scatter Plot of Positive
                Words</a>
            -   <a href="#scatter-plot-of-title-subjectivity"
                id="toc-scatter-plot-of-title-subjectivity">Scatter Plot of Title
                Subjectivity</a>
    -   <a href="#modeling" id="toc-modeling">Modeling</a>
        -   <a href="#data-manipulation-for-modeling"
            id="toc-data-manipulation-for-modeling">Data Manipulation for
            Modeling</a>
            -   <a href="#subsetting-variables-for-modeling"
                id="toc-subsetting-variables-for-modeling">Subsetting Variables for
                Modeling</a>
        -   <a href="#linear-regression-modeling"
            id="toc-linear-regression-modeling">Linear Regression Modeling</a>
            -   <a href="#linear-regression-model-1-multiple-linear-regression-using-lm"
                id="toc-linear-regression-model-1-multiple-linear-regression-using-lm">Linear
                Regression Model #1: Multiple Linear Regression Using
                <code>lm</code></a>
            -   <a href="#linear-regression-model-2-lasso-regression-using-glmnet"
                id="toc-linear-regression-model-2-lasso-regression-using-glmnet">Linear
                Regression Model #2: LASSO Regression using <code>glmnet</code></a>
        -   <a href="#tree-based-modeling" id="toc-tree-based-modeling">Tree-Based
            Modeling</a>
            -   <a href="#random-forest-model" id="toc-random-forest-model">Random
                Forest Model</a>
            -   <a href="#boosted-tree-model" id="toc-boosted-tree-model">Boosted Tree
                Model</a>
    -   <a href="#model-comparison--conclusion"
        id="toc-model-comparison--conclusion">Model Comparison &amp;
        Conclusion</a>
    -   <a href="#reference-list" id="toc-reference-list">Reference List</a>

# Report for the *lifestyle* Data Channel

This report contains Exploratory Data Analysis (EDA) about the lifestyle
data channel and a modeling section applying different regression
methods which attempt to predict trends about article sharing on the
Mashable website.

## Introduction

The objective of this analysis is to provide a comprehensive overview
about publication metrics and their relationship with the number of
shares that those publications presented during the study period. These
data have been collected from Mashable website, one of the largest news
websites from which the content of all the **lifestyle** channel
articles published in 2013 and 2014 was extracted. The full data
description can be found
[here](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity).
These data were originally collected and analyzed by [Fernandes et
al. (2015)](#reference-list), in which the authors performed
classification task comparing several machine learning algorithms. In
the present study, a subset of the data used by Fernandes et al.(2015)
corresponding to the data channel **lifestyle** is used for regression
purposes. The response variable is the number of `shares` that the
papers presented after publication. In other words, we will try to
predict the number of shares that the papers will have before
publication and evaluate the prediction of each selected model based on
some common metrics, such as RMSE (Root Mean Squared Error), RSquared
(Coefficient of Determination), and MAE (Mean Absolute Error) applied to
the test set. To perform the regression, the methods Random Forest,
Boosting, Multiple Linear Regression, and LASSO regression will be used.
More information about the methods will be provided in the corresponded
sections.

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

#### Outlier Detection and Cleaning

In this section we will perform a very important step of EDA, the
outlier detection and cleaning. In order to accomplish this task, we
will use the studentized residuals from a linear regression using the
`rstandard()` function. Linear models can also be useful for EDA when
analyzing the residuals. This analysis is famous for looking for values
above 2 and below -2 for the standardized residuals in the student
distribution scale, which means that if a residual goes above 2 or below
-2, it is considered an outlier and it is recommended to be deleted. The
code below shows the steps to use this function and cleans the detected
outliers from the training set.

``` r
# selecting variables of importance
var_sel = select(activeTrain,starts_with("LDA_"), average_token_length,
         is_weekend, n_tokens_content, n_non_stop_unique_tokens, num_hrefs,
         num_self_hrefs, num_videos, average_token_length, kw_avg_min, 
         kw_avg_max, kw_avg_avg, is_weekend)

# fitting a MLR with all important predictors
outlier_mod = lm(activeTrain$shares~.,data=var_sel)

# finding values greater than smaller than -2
a=(1:length(rstandard(outlier_mod)))[rstandard(outlier_mod) > 2]
b=(1:length(rstandard(outlier_mod)))[rstandard(outlier_mod) < -2]

# cleaning these values in the training set.
activeTrain = activeTrain[-c(a,b),]
```

#### Data manipulation for statistics

A new created object in this section aims to summarize publications
during weekdays and weekends and create factor levels for them to match
with `shares` variable. The functions `ifelse()` was used to vectorize
the IF-ELSE statements associated to `mutate()`, which took care of
creating and appending the new variable to the data set. The function
`factor()` was used to explicitly coerce the days of week into levels of
the newly created categorical variable “Day”.

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
    ##      28    1100    1600    2907    3100   24100

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

| Weekend | sumShares | avgShares | medShares | sdShares |
|:--------|----------:|----------:|----------:|---------:|
| No      |   3237634 |  2769.576 |      1500 | 3420.077 |
| Yes     |    960117 |  3491.335 |      2100 | 3688.831 |

Statistics for Shares for Weekend or Weekdays

#### Summary Statistics, Articles Shared by Day of Week

Likewise, this table gives us information about the number of shares by
the day of the week. The same functions were used here, but applied to
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

| Day       | sumShares | avgShares | medShares | sdShares | maxShares | minShares |
|:----------|----------:|----------:|----------:|---------:|----------:|----------:|
| Monday    |    619580 |  2964.498 |      1500 | 3655.682 |     24100 |       109 |
| Tuesday   |    577182 |  2542.652 |      1500 | 2806.972 |     19800 |        93 |
| Wednesday |    729469 |  2662.296 |      1500 | 3444.118 |     22300 |        95 |
| Thursday  |    700320 |  2835.304 |      1500 | 3434.143 |     20800 |        28 |
| Friday    |    611083 |  2882.467 |      1500 | 3729.982 |     23700 |       127 |
| Saturday  |    445250 |  3478.516 |      2000 | 3748.858 |     23100 |       446 |
| Sunday    |    514867 |  3502.497 |      2100 | 3648.567 |     21000 |       613 |

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
    ##       209       227       274       247       212       128       147

#### Contingency Table

Another discrete analysis performed here is the two-way contingency
table related to the discretization of the response variable if we
divided `shares` into two categories. The function `cut()` was used for
this end. In this case, we count the frequency of the number of
publications in the weekend versus weekdays with the two levels of
response variable. These levels represent the number of shares between
the minimum and average number of shares (on the left) and between the
average and maximum number of shares (on the right). The table below
shows the frequencies. In the table below, 0 (zero) represents weekdays
and 1 (one) represents weekends.

``` r
cutoff = cut(activeTrain$shares, 
             breaks = c(min(activeTrain$shares), 
                        mean(activeTrain$shares),
                        max(activeTrain$shares)),
             labels = c(paste0("(",round(min(activeTrain$shares),2),
                              ", ",round(mean(activeTrain$shares),2),
                              "]"),
                        paste0("(",round(mean(activeTrain$shares),2),
                              ", ",round(max(activeTrain$shares),2),
                              "]"))
             )


table(activeTrain$is_weekend, cutoff) %>%
  kable(caption = "Frequency of Shares in Weekend vs in Weekdays")
```

|     | (28, 2907.03\] | (2907.03, 24100\] |
|:----|---------------:|------------------:|
| 0   |            880 |               288 |
| 1   |            179 |                96 |

Frequency of Shares in Weekend vs in Weekdays

#### Correlation Matrix

An important EDA analysis for regression tasks is the correlation
matrix. The function `cor()` is used in this section to return the top
10 most correlated potential predictor variables with the response
variable `shares` according to Pearson’s Correlation Coefficient. The
code below presents the process of obtaining these variables and their
respective correlations with the response variable. The correlations are
clearly small for this case, which may difficult the modeling process
and produce low quality of prediction metrics.

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
var_id = unlist(lapply(1:10,
                         function(i) which(abs(correlation) == p[i])))

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

| Variables                | Correlation |
|:-------------------------|------------:|
| kw_avg_avg               |       0.165 |
| LDA_03                   |       0.117 |
| average_token_length     |      -0.104 |
| num_hrefs                |       0.098 |
| n_non_stop_unique_tokens |      -0.093 |
| kw_avg_min               |       0.089 |
| is_weekend               |       0.081 |
| num_videos               |       0.080 |
| kw_avg_max               |       0.074 |
| n_tokens_content         |       0.070 |

Top 10 Response Correlated Variables

#### Principal Components Analysis (PCA)

The variables that present highest correlation with the response
variable `shares` are kw_avg_avg, LDA_03, average_token_length,
num_hrefs, n_non_stop_unique_tokens, kw_avg_min, is_weekend, num_videos,
kw_avg_max, n_tokens_content. These variables will be studied in more
depth via PCA to understand the orientation of the most important
potential predictors. The code below presents the PCA analysis as part
of the EDA. The 10 PCs displayed in the table below correspond to the
most variable combination of the 10 predictors, which the PC1 has the
most variation in the data, PC2 presents the second most variation and
so on. The coefficients associated to each variable are the loadings and
they give the idea of importance of that particular variable to the
variance of the 10 predictor variables. The negative values only mean
that the orientation of the weights are opposite in the same PC. Since
the first PC has the largest variability, it is possible to say that the
variables with more weights in PC1 might be the most important variables
that contribute more with the variance of the predictors. This variables
are expected to present large influence on the explanation of the
variance of the response variable. The table below show these numbers.

``` r
id = which(colnames(activeTrain) %in% var_cor)

# PCA
PC = prcomp(activeTrain[,id], center = TRUE, scale = TRUE)

pc_directions=as.data.frame(PC$rotation)

kable(pc_directions, caption="Principal Components for EDA", digits = 3)
```

|                          |    PC1 |    PC2 |    PC3 |    PC4 |    PC5 |    PC6 |    PC7 |    PC8 |    PC9 |   PC10 |
|:-------------------------|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|
| n_tokens_content         | -0.262 |  0.290 | -0.452 | -0.165 |  0.392 | -0.054 | -0.425 |  0.453 |  0.075 | -0.260 |
| n_non_stop_unique_tokens |  0.412 | -0.554 |  0.073 | -0.055 |  0.036 |  0.055 | -0.069 |  0.138 |  0.210 | -0.666 |
| num_hrefs                | -0.348 | -0.111 | -0.513 | -0.202 |  0.039 |  0.002 |  0.429 | -0.527 | -0.006 | -0.316 |
| num_videos               | -0.097 | -0.083 |  0.190 |  0.269 |  0.702 |  0.591 |  0.130 | -0.093 |  0.043 |  0.077 |
| average_token_length     |  0.196 | -0.579 | -0.401 | -0.263 |  0.182 | -0.042 | -0.088 |  0.094 | -0.187 |  0.557 |
| kw_avg_min               | -0.022 |  0.110 |  0.369 | -0.766 |  0.041 |  0.241 | -0.004 |  0.001 | -0.437 | -0.116 |
| kw_avg_max               | -0.387 | -0.318 |  0.232 |  0.293 |  0.144 | -0.380 | -0.331 | -0.171 | -0.527 | -0.165 |
| kw_avg_avg               | -0.448 | -0.244 |  0.331 | -0.315 |  0.040 | -0.170 | -0.161 | -0.099 |  0.657 |  0.179 |
| is_weekend               | -0.248 | -0.156 | -0.165 |  0.095 | -0.506 |  0.637 | -0.457 | -0.085 | -0.026 | -0.002 |
| LDA_03                   | -0.425 | -0.240 |  0.057 |  0.081 | -0.195 |  0.074 |  0.512 |  0.659 | -0.109 | -0.018 |

Principal Components for EDA

``` r
id2 = which(abs(pc_directions$PC1) %in% 
             sort(abs(pc_directions$PC1),dec= T)[1:3])
```

It is possible to see that the three most important variables in PC1 are
n_non_stop_unique_tokens, kw_avg_avg, LDA_03 from the table above. These
variables are considered the most important variables in terms of
variance of the predictor variables. Although the metrics for prediction
are expected to be poor, these variables are expected to show the most
influence to the explanation of the variance of the response `shares`.

### EDA: Graphical Analysis

#### Correlation Plot

The plot below presents histograms, scatter plots, and correlations in a
bivariate structure of the top 5 variables chosen in the correlation
analysis. Notice the shape of the distributions and the values of the
correlations relative to the response variable `shares`.

``` r
# bivariate correlation plot
cor_data <- cbind(select(activeTrain,shares),var_sel[,var_id[1:5]])
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
`facet_wrap()` function are used to plot the scatter points, the smooth
GAM (Generalized Additive Models) lines, and split the data by LDA type,
respectively. It is possible to see the behavior of the response
variable in relation to each LDA types. A common analysis using scatter
plots is related to the pattern shown by the smooth curve fitted to the
points. If this curve shows a flat or constant line parallel to the
predictor axis, then the predictor has little contribution to the
explanation of the variance of the response variable.

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
the plot frame and `geom_point()`, `geom_smooth`, and `facet_wrap()`
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
the plot frame and `geom_point()`, `geom_smooth`, and `facet_wrap()`
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

#### Box Plot of Shares for Data Channel *lifestyle*

The following box plot shows the distribution of shares for this data
channel. The main chunk of the data can be seen within the “box”
representing the region encompassing the first and third quartiles. For
some cases, there are possible outliers in the data that make
distortions to the box plot and this mentioned “box” looks thinner than
usual. If this happens, then it means that the possible outliers are
much larger than the main chunk of data. The outliers usually appear as
individual points in the box plot. The graph below shows this pattern
for the response variable `shares`. However, the data might not have
outliers and the highlighted data points are in fact part of the data.
This stresses the importance of knowing about the subject and data set
in order to perform statistical analysis.

``` r
boxSharesGraph <- ggplot(statsData, aes(y = shares))
boxSharesGraph + geom_boxplot() + 
  ggtitle(paste("Number of Shares for Data Channel:", dataChannelSelect)) +
  ylab("Number of Shares") +
  xlab("Data Channel") 
```

![](lifestyle_files/figure-gfm/boxsharesGraph-1.png)<!-- -->

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
variables using a straight line. A model with only one explanatory
variable is called simple linear regression (SLR). In simple linear
regression, the response variable is predicted by an intercept and a
regression coefficient multiplied by the value of your explanatory
variable. The goal of regression is to determine the intercept and the
regression coefficients. This is done by fitting a straight line across
all of the data with the goal of minimizing the residuals sum of squares
via Least Squares method. The model is fit by minimizing the sum of
squared errors (SSE).

The same concept can be applied to multiple linear regression (MLR),
which has more than one explanatory variable. In this case, the goal is
to determine the intercept and a regression coefficient corresponding to
each explanatory variable in an attempt to minimize the sum of squared
errors.

In R, MLR is generally done with the function `lm`. There are also a
variety of other methods that fall under the umbrella of MLR. One of
these methods, LASSO regression, will be explored as part of this
analysis.

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
    ##     Min      1Q  Median      3Q     Max 
    ## -5985.9 -1723.3 -1043.9   270.8 20379.0 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              2907.0298    89.5076  32.478  < 2e-16 ***
    ## LDA_00                    -33.7647    94.1045  -0.359 0.719798    
    ## LDA_01                     -0.7295    91.8764  -0.008 0.993666    
    ## LDA_02                    -43.3569    96.5121  -0.449 0.653328    
    ## LDA_03                    126.9410   108.2720   1.172 0.241221    
    ## LDA_04                          NA         NA      NA       NA    
    ## average_token_length     -454.9769   122.0528  -3.728 0.000201 ***
    ## is_weekend                161.1937    93.7802   1.719 0.085859 .  
    ## n_tokens_content          192.0149   104.0070   1.846 0.065074 .  
    ## n_non_stop_unique_tokens  188.3122   134.7045   1.398 0.162340    
    ## num_hrefs                 292.4034   112.4253   2.601 0.009395 ** 
    ## num_self_hrefs            -50.5621    96.8320  -0.522 0.601637    
    ## num_videos                227.3017    91.3995   2.487 0.012999 *  
    ## kw_avg_min                214.2312   106.6862   2.008 0.044826 *  
    ## kw_avg_max                -22.8487   122.3882  -0.187 0.851930    
    ## kw_avg_avg                348.0264   128.9820   2.698 0.007052 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3401 on 1429 degrees of freedom
    ## Multiple R-squared:  0.05567,    Adjusted R-squared:  0.04642 
    ## F-statistic: 6.018 on 14 and 1429 DF,  p-value: 1.103e-11

The following table shows the output training metrics for this linear
regression.

``` r
lm_out = data.frame(lmFit$results)

kable(lm_out, caption = "Output Training Metrics for Linear Regression",
      digits = 3)
```

| intercept |     RMSE | Rsquared |      MAE |  RMSESD | RsquaredSD |   MAESD |
|:----------|---------:|---------:|---------:|--------:|-----------:|--------:|
| TRUE      | 3445.242 |    0.034 | 2185.643 | 421.502 |       0.04 | 166.187 |

Output Training Metrics for Linear Regression

The following shows the RMSE, RSquared, and MAE values for the model as
it performed on predicting the test set.

``` r
metric_lm = postResample(pred = predict(lmFit, newdata = dfTest), 
                         obs = dfTest$shares)

metric_lm
```

    ##         RMSE     Rsquared          MAE 
    ## 5.484429e+03 1.384488e-02 2.567296e+03

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
    ##                                   s1
    ## (Intercept)              2907.029778
    ## LDA_00                    -21.989521
    ## LDA_01                      .       
    ## LDA_02                    -40.175884
    ## LDA_03                    122.220732
    ## LDA_04                      .       
    ## average_token_length     -415.136545
    ## is_weekend                153.703544
    ## n_tokens_content          166.281015
    ## n_non_stop_unique_tokens  139.376080
    ## num_hrefs                 266.978243
    ## num_self_hrefs            -32.396689
    ## num_videos                217.785951
    ## kw_avg_min                210.706914
    ## kw_avg_max                 -0.216876
    ## kw_avg_avg                333.938941

The best `lambda` for this model is 10 and this value can be seen in the
table below which summarizes all the metrics for the 5-fold
cross-validation.

``` r
lasso_out = data.frame(LASSO$results)

kable(lasso_out, caption = "Output Training Metrics for LASSO",
      digits = 3)
```

| alpha | lambda |     RMSE | Rsquared |      MAE |  RMSESD | RsquaredSD |   MAESD |
|------:|-------:|---------:|---------:|---------:|--------:|-----------:|--------:|
|     1 |      0 | 3439.824 |    0.035 | 2177.647 | 281.958 |      0.039 | 124.165 |
|     1 |      1 | 3439.824 |    0.035 | 2177.647 | 281.958 |      0.039 | 124.165 |
|     1 |      2 | 3439.652 |    0.035 | 2177.399 | 281.885 |      0.040 | 124.142 |
|     1 |      3 | 3439.454 |    0.035 | 2177.104 | 281.796 |      0.040 | 124.100 |
|     1 |      4 | 3439.276 |    0.035 | 2176.831 | 281.708 |      0.040 | 124.076 |
|     1 |      5 | 3439.097 |    0.035 | 2176.564 | 281.608 |      0.040 | 124.038 |
|     1 |      6 | 3438.920 |    0.035 | 2176.296 | 281.514 |      0.040 | 124.003 |
|     1 |      7 | 3438.688 |    0.036 | 2176.023 | 281.427 |      0.040 | 123.959 |
|     1 |      8 | 3438.421 |    0.036 | 2175.752 | 281.338 |      0.040 | 123.904 |
|     1 |      9 | 3438.174 |    0.036 | 2175.498 | 281.232 |      0.040 | 123.831 |
|     1 |     10 | 3437.909 |    0.036 | 2175.233 | 281.136 |      0.040 | 123.762 |

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
    ## 5.484450e+03 1.355832e-02 2.563645e+03

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

| mtry |     RMSE | Rsquared |      MAE |  RMSESD | RsquaredSD |  MAESD |
|-----:|---------:|---------:|---------:|--------:|-----------:|-------:|
|    1 | 3440.326 |    0.027 | 2190.962 | 336.718 |      0.020 | 82.815 |
|    2 | 3473.762 |    0.020 | 2240.135 | 329.290 |      0.011 | 91.406 |
|    3 | 3484.549 |    0.022 | 2257.626 | 316.541 |      0.014 | 81.308 |
|    4 | 3501.722 |    0.020 | 2271.472 | 315.905 |      0.012 | 84.558 |
|    5 | 3512.716 |    0.019 | 2278.493 | 320.238 |      0.011 | 90.148 |

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
    ## 5.476735e+03 1.395719e-02 2.570579e+03

#### Boosted Tree Model

In this section the Ensemble Learning algorithm Boosting will be
trained. Boosted tree method is one of the Tree based models most used
in data science because it presents a fitting strategy that improves
sequentially throughout the iterations. Boosting uses single trees
fitted (each single tree has `d` splits) on the training data and
produces predictions off of that training. The residuals of this
prediction is, then, used as response variable for the next single tree
training step. New predictions are done for this new model as well and
so on. This process occurs several times during `B` iterations and the
predictions are updated during the fitting process, being driven by the
shrinkage parameter, also called growth rate, `lambda`. These training
features of Boosting make this method to produce a reduction in the
variance of the predictions as well as gains in precision, mainly over
Random Forest, Bagging, and single tree. The shrinkage parameter will be
set as 0.1 and n.minobsinnode set as 10. The parameters n.tree and
interaction.depth will be chosen based on 5-fold cross-validation. The
former will be chosen from a sequence from 25 to 200, counting by 25.
The latter will be chosen from a sequence from 1 to 4. The code below
shows the training and tuning procedure and prints out the resultant
values of the two considered hyperparameters.

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

    ## [1] 1

The best n.trees and interaction.depth parameters for this model are 25
and 1, respectively. These values can be seen in the table below, which
summarizes all the metrics for the 5-fold cross-validation. It is easy
to see that these values minimize the RMSE.

``` r
gbm_out = data.frame(gbmFit$results)

gbm_out <- gbm_out %>%
  arrange(RMSE)

kable(gbm_out, caption = "Output Training Metrics for Boosting",
      digits = 3, row.names = FALSE)
```

| shrinkage | interaction.depth | n.minobsinnode | n.trees |     RMSE | Rsquared |      MAE |  RMSESD | RsquaredSD |   MAESD |
|----------:|------------------:|---------------:|--------:|---------:|---------:|---------:|--------:|-----------:|--------:|
|       0.1 |                 1 |             10 |      25 | 3449.933 |    0.030 | 2174.450 | 368.728 |      0.049 | 135.697 |
|       0.1 |                 2 |             10 |      25 | 3455.866 |    0.027 | 2170.931 | 376.052 |      0.051 | 137.940 |
|       0.1 |                 1 |             10 |      50 | 3457.997 |    0.027 | 2177.234 | 372.453 |      0.042 | 146.910 |
|       0.1 |                 1 |             10 |      75 | 3472.210 |    0.026 | 2199.803 | 364.873 |      0.038 | 145.933 |
|       0.1 |                 3 |             10 |      25 | 3473.521 |    0.018 | 2187.612 | 379.471 |      0.022 | 147.319 |
|       0.1 |                 1 |             10 |     125 | 3477.967 |    0.025 | 2192.704 | 362.301 |      0.034 | 142.334 |
|       0.1 |                 1 |             10 |     100 | 3482.249 |    0.023 | 2180.636 | 360.786 |      0.033 | 134.922 |
|       0.1 |                 1 |             10 |     150 | 3485.774 |    0.022 | 2199.727 | 368.116 |      0.026 | 133.880 |
|       0.1 |                 2 |             10 |      50 | 3492.024 |    0.027 | 2191.440 | 376.442 |      0.050 | 138.930 |
|       0.1 |                 3 |             10 |      50 | 3493.507 |    0.020 | 2199.520 | 352.444 |      0.023 | 121.830 |
|       0.1 |                 2 |             10 |      75 | 3495.792 |    0.023 | 2190.924 | 387.315 |      0.034 | 154.573 |
|       0.1 |                 1 |             10 |     175 | 3496.085 |    0.020 | 2196.355 | 368.271 |      0.027 | 137.891 |
|       0.1 |                 4 |             10 |      25 | 3497.465 |    0.013 | 2187.718 | 362.511 |      0.014 | 108.323 |
|       0.1 |                 3 |             10 |      75 | 3503.285 |    0.018 | 2200.925 | 343.144 |      0.023 | 128.892 |
|       0.1 |                 1 |             10 |     200 | 3505.557 |    0.016 | 2206.767 | 369.438 |      0.019 | 140.029 |
|       0.1 |                 2 |             10 |     100 | 3516.840 |    0.018 | 2201.927 | 365.095 |      0.027 | 128.877 |
|       0.1 |                 2 |             10 |     125 | 3523.975 |    0.019 | 2213.731 | 376.036 |      0.026 | 141.880 |
|       0.1 |                 3 |             10 |     100 | 3532.045 |    0.018 | 2222.869 | 351.136 |      0.024 | 127.556 |
|       0.1 |                 4 |             10 |      50 | 3534.795 |    0.013 | 2210.058 | 353.325 |      0.013 | 131.730 |
|       0.1 |                 2 |             10 |     150 | 3546.924 |    0.016 | 2235.680 | 366.506 |      0.021 | 113.751 |
|       0.1 |                 2 |             10 |     175 | 3554.888 |    0.016 | 2239.687 | 368.040 |      0.020 | 124.827 |
|       0.1 |                 3 |             10 |     125 | 3557.803 |    0.016 | 2239.807 | 353.695 |      0.022 | 132.820 |
|       0.1 |                 2 |             10 |     200 | 3562.372 |    0.015 | 2239.476 | 354.919 |      0.019 | 109.678 |
|       0.1 |                 4 |             10 |      75 | 3565.097 |    0.012 | 2221.548 | 365.663 |      0.013 | 147.885 |
|       0.1 |                 3 |             10 |     150 | 3567.004 |    0.016 | 2250.812 | 338.489 |      0.022 | 126.104 |
|       0.1 |                 4 |             10 |     100 | 3581.380 |    0.011 | 2240.138 | 340.901 |      0.011 | 130.338 |
|       0.1 |                 3 |             10 |     175 | 3606.334 |    0.012 | 2287.999 | 336.079 |      0.017 | 125.706 |
|       0.1 |                 4 |             10 |     125 | 3608.715 |    0.009 | 2260.849 | 353.990 |      0.011 | 139.472 |
|       0.1 |                 3 |             10 |     200 | 3613.193 |    0.012 | 2304.850 | 337.817 |      0.016 | 136.405 |
|       0.1 |                 4 |             10 |     150 | 3641.772 |    0.009 | 2293.797 | 344.355 |      0.012 | 135.185 |
|       0.1 |                 4 |             10 |     175 | 3652.867 |    0.009 | 2314.115 | 333.610 |      0.013 | 132.709 |
|       0.1 |                 4 |             10 |     200 | 3671.079 |    0.008 | 2330.907 | 328.781 |      0.011 | 137.399 |

Output Training Metrics for Boosting

The plot below shows the RMSE by Number of Boosting Iterations and
display Max Tree Depth lines for the 5-fold CV performed. It is easy to
see that RMSE is minimized when n.trees = 25 and interaction.depth = 1.

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
    ## 5480.7363930    0.0146463 2559.5313252

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
| RMSE     | 5476.735 | 5480.736 | 5484.450 | 5484.429 |
| Rsquared |    0.014 |    0.015 |    0.014 |    0.014 |
| MAE      | 2570.579 | 2559.531 | 2563.645 | 2567.296 |

Accuracy Metric by Ensemble Method on Test Set

After comparing all the 4 models fit throughout this analysis, the best
model was chosen based on the RMSE value, such that the model with
minimum RMSE is the “winner”. Therefore, the best model is **Random
Forest** based on RMSE metric. The RMSE, coefficient of determination,
and MAE metrics for all 4 models can be seen in the table above.

## Reference List

K. Fernandes, P. Vinagre and P. Cortez. A Proactive Intelligent Decision
Support System for Predicting the Popularity of Online News. Proceedings
of the 17th EPIA 2015 - Portuguese Conference on Artificial
Intelligence, September, Coimbra, Portugal.

[back to intro](#introduction)
