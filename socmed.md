Project 3
================
Kara Belknap & Cassio Monti
2022-11-5

-   <a href="#report-for-the-socmed-data-channel"
    id="toc-report-for-the-socmed-data-channel">Report for the
    <em>socmed</em> Data Channel</a>
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
    -   <a href="#summarizations-for-the-socmed-data-channel"
        id="toc-summarizations-for-the-socmed-data-channel">Summarizations for
        the <em>socmed</em> Data Channel</a>
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
            -   <a href="#box-plot-of-shares-for-data-channel-socmed"
                id="toc-box-plot-of-shares-for-data-channel-socmed">Box Plot of Shares
                for Data Channel <em>socmed</em></a>
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

# Report for the *socmed* Data Channel

This report contains Exploratory Data Analysis (EDA) about the socmed
data channel and a modeling section applying different regression
methods which attempt to predict trends about article sharing on the
Mashable website.

## Introduction

The objective of this analysis is to provide a comprehensive overview
about publication metrics and their relationship with the number of
shares that those publications presented during the study period. These
data have been collected from Mashable website, one of the largest news
websites from which the content of all the **socmed** channel articles
published in 2013 and 2014 was extracted. The full data description can
be found
[here](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity).
These data were originally collected and analyzed by [Fernandes et
al. (2015)](#reference-list), in which the authors performed
classification task comparing several machine learning algorithms. In
the present study, a subset of the data used by Fernandes et al.(2015)
corresponding to the data channel **socmed** is used for regression
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

## Summarizations for the *socmed* Data Channel

In this section, we will perform EDA for the data channel socmed.

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
for the number of shares for articles in the data channel socmed. The
`summary()` function was used to extract these metrics.

``` r
summary(activeTrain$shares)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       5    1400    2100    2924    3600   15200

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
| No      |   3917260 |  2867.687 |      2000 | 2421.123 |
| Yes     |    694064 |  3289.403 |      2300 | 2649.634 |

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
| Monday    |    670723 |  2891.047 |      2100 | 2297.727 |     12700 |        53 |
| Tuesday   |    795483 |  2724.257 |      1900 | 2253.029 |     12200 |       238 |
| Wednesday |    863321 |  2916.625 |      2100 | 2436.494 |     13700 |        23 |
| Thursday  |    945792 |  2928.149 |      2000 | 2576.024 |     15000 |         5 |
| Friday    |    641941 |  2878.659 |      2000 | 2518.937 |     13700 |       213 |
| Saturday  |    388672 |  3185.836 |      2300 | 2603.620 |     15200 |       837 |
| Sunday    |    305392 |  3431.371 |      2400 | 2719.827 |     13200 |       455 |

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
    ##       232       292       296       323       223       122        89

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

|     | (5, 2924.11\] | (2924.11, 15200\] |
|:----|--------------:|------------------:|
| 0   |           939 |               426 |
| 1   |           130 |                81 |

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
| LDA_00                   |       0.237 |
| kw_avg_avg               |       0.149 |
| n_non_stop_unique_tokens |      -0.145 |
| LDA_03                   |      -0.121 |
| LDA_01                   |      -0.115 |
| LDA_02                   |      -0.087 |
| is_weekend               |       0.058 |
| kw_avg_min               |       0.054 |
| kw_avg_max               |      -0.050 |
| num_hrefs                |      -0.049 |

Top 10 Response Correlated Variables

#### Principal Components Analysis (PCA)

The variables that present highest correlation with the response
variable `shares` are LDA_00, kw_avg_avg, n_non_stop_unique_tokens,
LDA_03, LDA_01, LDA_02, is_weekend, kw_avg_min, kw_avg_max, num_hrefs.
These variables will be studied in more depth via PCA to understand the
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
weights in PC1 might be the most important variables that contribute
more with the variance of the predictors. This variables are expected to
present large influence on the explanation of the variance of the
response variable. The table below show these numbers.

``` r
id = which(colnames(activeTrain) %in% var_cor)

# PCA
PC = prcomp(activeTrain[,id], center = TRUE, scale = TRUE)

pc_directions=as.data.frame(PC$rotation)

kable(pc_directions, caption="Principal Components for EDA", digits = 3)
```

|                          |    PC1 |    PC2 |    PC3 |    PC4 |    PC5 |    PC6 |    PC7 |    PC8 |    PC9 |   PC10 |
|:-------------------------|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|
| n_non_stop_unique_tokens | -0.464 |  0.022 | -0.076 | -0.410 |  0.072 |  0.109 |  0.378 | -0.665 | -0.082 |  0.040 |
| num_hrefs                |  0.475 | -0.230 |  0.142 |  0.283 | -0.021 | -0.206 | -0.244 | -0.669 | -0.263 |  0.045 |
| kw_avg_min               |  0.180 | -0.067 |  0.545 | -0.454 | -0.076 |  0.416 | -0.071 |  0.170 | -0.497 | -0.006 |
| kw_avg_max               | -0.388 | -0.150 | -0.169 |  0.594 |  0.036 |  0.305 |  0.084 |  0.092 | -0.579 | -0.007 |
| kw_avg_avg               | -0.178 | -0.249 |  0.559 |  0.325 |  0.114 |  0.394 |  0.013 | -0.129 |  0.550 |  0.033 |
| is_weekend               |  0.131 | -0.158 |  0.270 |  0.142 | -0.397 | -0.336 |  0.758 |  0.123 | -0.052 | -0.024 |
| LDA_00                   | -0.004 |  0.692 |  0.266 |  0.199 |  0.094 | -0.031 |  0.045 | -0.126 | -0.067 | -0.616 |
| LDA_01                   | -0.153 | -0.035 | -0.129 | -0.019 | -0.867 |  0.186 | -0.283 | -0.122 |  0.116 | -0.251 |
| LDA_02                   |  0.431 | -0.299 | -0.391 | -0.064 |  0.162 |  0.429 |  0.285 | -0.022 |  0.117 | -0.512 |
| LDA_03                   | -0.343 | -0.513 |  0.153 | -0.153 |  0.171 | -0.438 | -0.216 |  0.099 | -0.069 | -0.538 |

Principal Components for EDA

``` r
id2 = which(abs(pc_directions$PC1) %in% 
             sort(abs(pc_directions$PC1),dec= T)[1:3])
```

It is possible to see that the three most important variables in PC1 are
n_non_stop_unique_tokens, num_hrefs, LDA_02 from the table above. These
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

![](socmed_files/figure-gfm/correlplot-1.png)<!-- -->

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

![](socmed_files/figure-gfm/biplot-1.png)<!-- -->

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

![](socmed_files/figure-gfm/LDAplot-1.png)<!-- -->

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

![](socmed_files/figure-gfm/keywordplot-1.png)<!-- -->

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

![](socmed_files/figure-gfm/Contentplot-1.png)<!-- -->

#### Box Plot of Shares for Data Channel *socmed*

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

![](socmed_files/figure-gfm/boxsharesGraph-1.png)<!-- -->

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

![](socmed_files/figure-gfm/titlewordcountGraph-1.png)<!-- -->

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

![](socmed_files/figure-gfm/positivewordrateGraph-1.png)<!-- -->

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

![](socmed_files/figure-gfm/titleSubjectivityGraph-1.png)<!-- -->

## Modeling

In this section, we will perform regression for prediction purposes for
the data channel socmed. All models were fitted using 5-fold
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
    ## -5374.3 -1388.1  -613.1   460.1 10778.4 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               2924.11      57.99  50.424  < 2e-16 ***
    ## LDA_00                     463.76     100.01   4.637 3.83e-06 ***
    ## LDA_01                    -182.17      67.88  -2.684 0.007359 ** 
    ## LDA_02                     -27.41      89.88  -0.305 0.760462    
    ## LDA_03                    -117.38      93.49  -1.256 0.209447    
    ## LDA_04                         NA         NA      NA       NA    
    ## average_token_length        83.80      67.70   1.238 0.215959    
    ## is_weekend                 152.07      59.65   2.549 0.010884 *  
    ## n_tokens_content            40.78      91.87   0.444 0.657208    
    ## n_non_stop_unique_tokens  -475.47      82.05  -5.795 8.25e-09 ***
    ## num_hrefs                 -334.16      92.49  -3.613 0.000313 ***
    ## num_self_hrefs             -44.14      76.34  -0.578 0.563258    
    ## num_videos                 183.32      60.93   3.009 0.002664 ** 
    ## kw_avg_min                  -7.07      63.95  -0.111 0.911979    
    ## kw_avg_max                -174.91      66.23  -2.641 0.008349 ** 
    ## kw_avg_avg                 438.46      65.56   6.688 3.13e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2303 on 1562 degrees of freedom
    ## Multiple R-squared:  0.1288, Adjusted R-squared:  0.121 
    ## F-statistic: 16.49 on 14 and 1562 DF,  p-value: < 2.2e-16

The following table shows the output training metrics for this linear
regression.

``` r
lm_out = data.frame(lmFit$results)

kable(lm_out, caption = "Output Training Metrics for Linear Regression",
      digits = 3)
```

| intercept |     RMSE | Rsquared |      MAE |  RMSESD | RsquaredSD |  MAESD |
|:----------|---------:|---------:|---------:|--------:|-----------:|-------:|
| TRUE      | 2323.568 |     0.11 | 1607.924 | 175.888 |      0.027 | 99.098 |

Output Training Metrics for Linear Regression

The following shows the RMSE, RSquared, and MAE values for the model as
it performed on predicting the test set.

``` r
metric_lm = postResample(pred = predict(lmFit, newdata = dfTest), 
                         obs = dfTest$shares)

metric_lm
```

    ##         RMSE     Rsquared          MAE 
    ## 6.434185e+03 1.238591e-02 2.313189e+03

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
    ## (Intercept)              2924.11160
    ## LDA_00                    489.18864
    ## LDA_01                   -163.07978
    ## LDA_02                      .      
    ## LDA_03                    -82.75233
    ## LDA_04                     11.86160
    ## average_token_length       63.91704
    ## is_weekend                139.98841
    ## n_tokens_content           16.09874
    ## n_non_stop_unique_tokens -458.49858
    ## num_hrefs                -305.60559
    ## num_self_hrefs            -26.30855
    ## num_videos                167.38080
    ## kw_avg_min                  .      
    ## kw_avg_max               -157.23572
    ## kw_avg_avg                419.13340

The best `lambda` for this model is 10 and this value can be seen in the
table below which summarizes all the metrics for the 5-fold
cross-validation.

``` r
lasso_out = data.frame(LASSO$results)

kable(lasso_out, caption = "Output Training Metrics for LASSO",
      digits = 3)
```

| alpha | lambda |     RMSE | Rsquared |      MAE |  RMSESD | RsquaredSD |  MAESD |
|------:|-------:|---------:|---------:|---------:|--------:|-----------:|-------:|
|     1 |      0 | 2318.619 |    0.114 | 1602.497 | 135.584 |      0.058 | 75.499 |
|     1 |      1 | 2318.619 |    0.114 | 1602.497 | 135.584 |      0.058 | 75.499 |
|     1 |      2 | 2318.361 |    0.114 | 1602.276 | 135.664 |      0.058 | 75.459 |
|     1 |      3 | 2317.984 |    0.115 | 1601.988 | 135.733 |      0.058 | 75.401 |
|     1 |      4 | 2317.640 |    0.115 | 1601.721 | 135.793 |      0.058 | 75.347 |
|     1 |      5 | 2317.337 |    0.115 | 1601.499 | 135.869 |      0.058 | 75.291 |
|     1 |      6 | 2317.019 |    0.115 | 1601.292 | 135.945 |      0.058 | 75.172 |
|     1 |      7 | 2316.724 |    0.115 | 1601.108 | 136.017 |      0.057 | 75.044 |
|     1 |      8 | 2316.440 |    0.115 | 1600.936 | 136.089 |      0.057 | 74.897 |
|     1 |      9 | 2316.169 |    0.115 | 1600.780 | 136.163 |      0.057 | 74.740 |
|     1 |     10 | 2315.910 |    0.115 | 1600.655 | 136.239 |      0.057 | 74.619 |

Output Training Metrics for LASSO

The plot below shows the RMSE by Regularization Parameter (`lambda`). It
is easy to see that RMSE is minimized when `lambda` = 10.

``` r
plot(LASSO)
```

![](socmed_files/figure-gfm/LASSOplot-1.png)<!-- -->

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
    ## 6.437570e+03 1.148639e-02 2.311253e+03

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

    ## [1] 2

The best `mtry` for this particular model was 2.

The following plot shows the RMSE values for each of the tune. The
objective in random forest modeling is to choose the model with the
lowest RMSE.

``` r
plot(rfFit)
```

![](socmed_files/figure-gfm/RFplot-1.png)<!-- -->

The following table shows training metrics for the random forest model.
Again, the best model is the one that minimizes RMSE.

``` r
rf_out = data.frame(rfFit$results)

kable(rf_out, caption = "Output Training Metrics for Random Forest",
      digits = 3)
```

| mtry |     RMSE | Rsquared |      MAE | RMSESD | RsquaredSD |  MAESD |
|-----:|---------:|---------:|---------:|-------:|-----------:|-------:|
|    1 | 2323.472 |    0.111 | 1628.408 | 50.481 |      0.020 | 15.979 |
|    2 | 2320.075 |    0.109 | 1638.205 | 36.841 |      0.025 | 23.469 |
|    3 | 2328.902 |    0.103 | 1647.421 | 36.312 |      0.023 | 26.104 |
|    4 | 2329.520 |    0.103 | 1648.304 | 30.592 |      0.023 | 34.301 |
|    5 | 2339.430 |    0.098 | 1657.943 | 27.339 |      0.024 | 28.163 |

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
    ## 6.448230e+03 7.697142e-03 2.336022e+03

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

    ## [1] 50

``` r
gbmFit$bestTune$interaction.depth
```

    ## [1] 2

The best n.trees and interaction.depth parameters for this model are 50
and 2, respectively. These values can be seen in the table below, which
summarizes all the metrics for the 5-fold cross-validation. It is easy
to see that these values minimize the RMSE.

``` r
gbm_out = data.frame(gbmFit$results)

gbm_out <- gbm_out %>%
  arrange(RMSE)

kable(gbm_out, caption = "Output Training Metrics for Boosting",
      digits = 3, row.names = FALSE)
```

| shrinkage | interaction.depth | n.minobsinnode | n.trees |     RMSE | Rsquared |      MAE |  RMSESD | RsquaredSD |  MAESD |
|----------:|------------------:|---------------:|--------:|---------:|---------:|---------:|--------:|-----------:|-------:|
|       0.1 |                 2 |             10 |      50 | 2321.140 |    0.111 | 1604.475 | 147.999 |      0.051 | 73.458 |
|       0.1 |                 2 |             10 |      75 | 2323.360 |    0.109 | 1601.193 | 146.813 |      0.045 | 80.631 |
|       0.1 |                 1 |             10 |     100 | 2326.918 |    0.106 | 1606.124 | 131.940 |      0.037 | 60.057 |
|       0.1 |                 2 |             10 |     100 | 2327.222 |    0.111 | 1599.427 | 164.829 |      0.055 | 84.598 |
|       0.1 |                 1 |             10 |     125 | 2327.242 |    0.106 | 1603.637 | 130.435 |      0.036 | 61.314 |
|       0.1 |                 3 |             10 |      50 | 2327.435 |    0.107 | 1602.514 | 136.812 |      0.045 | 72.840 |
|       0.1 |                 1 |             10 |      75 | 2328.677 |    0.104 | 1614.682 | 128.881 |      0.035 | 54.501 |
|       0.1 |                 1 |             10 |     150 | 2329.176 |    0.106 | 1599.901 | 129.393 |      0.034 | 60.929 |
|       0.1 |                 1 |             10 |     175 | 2330.300 |    0.105 | 1602.385 | 127.523 |      0.034 | 66.533 |
|       0.1 |                 2 |             10 |     125 | 2331.019 |    0.110 | 1600.927 | 164.219 |      0.052 | 83.884 |
|       0.1 |                 1 |             10 |     200 | 2331.966 |    0.105 | 1601.921 | 130.718 |      0.038 | 66.680 |
|       0.1 |                 3 |             10 |      75 | 2333.760 |    0.106 | 1601.500 | 146.298 |      0.045 | 80.763 |
|       0.1 |                 2 |             10 |      25 | 2336.779 |    0.102 | 1630.944 | 123.227 |      0.035 | 51.285 |
|       0.1 |                 3 |             10 |      25 | 2337.160 |    0.100 | 1628.151 | 112.934 |      0.032 | 58.149 |
|       0.1 |                 1 |             10 |      50 | 2339.329 |    0.098 | 1626.792 | 122.621 |      0.031 | 52.697 |
|       0.1 |                 3 |             10 |     125 | 2341.795 |    0.106 | 1601.554 | 146.524 |      0.045 | 79.367 |
|       0.1 |                 3 |             10 |     100 | 2341.865 |    0.103 | 1600.485 | 137.564 |      0.042 | 78.370 |
|       0.1 |                 4 |             10 |      25 | 2341.901 |    0.096 | 1625.053 | 135.377 |      0.049 | 68.796 |
|       0.1 |                 4 |             10 |      50 | 2342.801 |    0.100 | 1614.011 | 145.565 |      0.048 | 80.888 |
|       0.1 |                 2 |             10 |     150 | 2345.573 |    0.103 | 1617.950 | 153.833 |      0.045 | 75.861 |
|       0.1 |                 4 |             10 |      75 | 2347.922 |    0.103 | 1611.151 | 148.350 |      0.048 | 74.638 |
|       0.1 |                 3 |             10 |     150 | 2353.303 |    0.102 | 1611.228 | 148.647 |      0.045 | 87.548 |
|       0.1 |                 2 |             10 |     175 | 2353.806 |    0.099 | 1621.542 | 139.916 |      0.035 | 70.085 |
|       0.1 |                 4 |             10 |     100 | 2362.716 |    0.100 | 1621.004 | 145.860 |      0.045 | 75.866 |
|       0.1 |                 2 |             10 |     200 | 2363.007 |    0.096 | 1625.422 | 137.561 |      0.033 | 75.015 |
|       0.1 |                 1 |             10 |      25 | 2366.105 |    0.085 | 1658.551 | 107.940 |      0.016 | 50.107 |
|       0.1 |                 3 |             10 |     175 | 2366.298 |    0.097 | 1623.354 | 144.872 |      0.041 | 81.366 |
|       0.1 |                 3 |             10 |     200 | 2373.479 |    0.096 | 1628.131 | 147.188 |      0.042 | 82.166 |
|       0.1 |                 4 |             10 |     125 | 2376.159 |    0.096 | 1627.224 | 150.497 |      0.046 | 81.653 |
|       0.1 |                 4 |             10 |     150 | 2392.163 |    0.090 | 1637.547 | 139.156 |      0.039 | 80.367 |
|       0.1 |                 4 |             10 |     175 | 2394.420 |    0.090 | 1631.672 | 139.442 |      0.035 | 81.828 |
|       0.1 |                 4 |             10 |     200 | 2408.156 |    0.086 | 1644.021 | 133.803 |      0.032 | 83.339 |

Output Training Metrics for Boosting

The plot below shows the RMSE by Number of Boosting Iterations and
display Max Tree Depth lines for the 5-fold CV performed. It is easy to
see that RMSE is minimized when n.trees = 50 and interaction.depth = 2.

``` r
plot(gbmFit)
```

![](socmed_files/figure-gfm/boostingPlot-1.png)<!-- -->

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
    ## 6.445422e+03 9.564159e-03 2.299485e+03

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
| RMSE     | 6448.230 | 6445.422 | 6437.570 | 6434.185 |
| Rsquared |    0.008 |    0.010 |    0.011 |    0.012 |
| MAE      | 2336.022 | 2299.485 | 2311.253 | 2313.189 |

Accuracy Metric by Ensemble Method on Test Set

After comparing all the 4 models fit throughout this analysis, the best
model was chosen based on the RMSE value, such that the model with
minimum RMSE is the “winner”. Therefore, the best model is **Multiple
Linear Regression** based on RMSE metric. The RMSE, coefficient of
determination, and MAE metrics for all 4 models can be seen in the table
above.

## Reference List

K. Fernandes, P. Vinagre and P. Cortez. A Proactive Intelligent Decision
Support System for Predicting the Popularity of Online News. Proceedings
of the 17th EPIA 2015 - Portuguese Conference on Artificial
Intelligence, September, Coimbra, Portugal.

[back to intro](#introduction)
