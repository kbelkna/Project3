Project 3
================
Kara Belknap & Cassio Monti
2022-11-5

-   <a href="#report-for-the-bus-data-channel"
    id="toc-report-for-the-bus-data-channel">Report for the <em>bus</em>
    Data Channel</a>
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
    -   <a href="#summarizations-for-the-bus-data-channel"
        id="toc-summarizations-for-the-bus-data-channel">Summarizations for the
        <em>bus</em> Data Channel</a>
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
            -   <a href="#box-plot-of-shares-for-data-channel-bus"
                id="toc-box-plot-of-shares-for-data-channel-bus">Box Plot of Shares for
                Data Channel <em>bus</em></a>
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

# Report for the *bus* Data Channel

This report contains Exploratory Data Analysis (EDA) about the bus data
channel and a modeling section applying different regression methods
which attempt to predict trends about article sharing on the Mashable
website.

## Introduction

The objective of this analysis is to provide a comprehensive overview
about publication metrics and their relationship with the number of
shares that those publications presented during the study period. These
data have been collected from Mashable website, one of the largest news
websites from which the content of all the **bus** channel articles
published in 2013 and 2014 was extracted. The full data description can
be found
[here](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity).
These data were originally collected and analyzed by [Fernandes et
al. (2015)](#reference-list), in which the authors performed
classification task comparing several machine learning algorithms. In
the present study, a subset of the data used by Fernandes et al.(2015)
corresponding to the data channel **bus** is used for regression
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

## Summarizations for the *bus* Data Channel

In this section, we will perform EDA for the data channel bus.

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
for the number of shares for articles in the data channel bus. The
`summary()` function was used to extract these metrics.

``` r
summary(activeTrain$shares)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       1     951    1400    2368    2500   45400

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
| No      |   8903311 |  2249.447 |      1300 | 3079.187 |
| Yes     |   1412140 |  3548.090 |      2400 | 3936.164 |

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
| Monday    |   2001116 |  2476.629 |      1400 | 3679.304 |     35300 |         1 |
| Tuesday   |   1897312 |  2266.800 |      1300 | 3122.731 |     30200 |       156 |
| Wednesday |   1864792 |  2126.331 |      1300 | 2522.627 |     25900 |        63 |
| Thursday  |   1857155 |  2142.047 |      1300 | 3349.641 |     45400 |        81 |
| Friday    |   1282936 |  2254.721 |      1500 | 2330.391 |     21500 |        83 |
| Saturday  |    623101 |  3870.193 |      2600 | 4391.445 |     27300 |       150 |
| Sunday    |    789039 |  3329.278 |      2200 | 3587.441 |     25300 |       692 |

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
    ##       808       837       877       867       569       161       237

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

|     | (1, 2368.1\] | (2368.1, 45400\] |
|:----|-------------:|-----------------:|
| 0   |         3006 |              951 |
| 1   |          197 |              201 |

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
| num_hrefs                |       0.183 |
| kw_avg_avg               |       0.179 |
| n_tokens_content         |       0.121 |
| is_weekend               |       0.117 |
| LDA_03                   |       0.089 |
| num_videos               |       0.082 |
| kw_avg_max               |       0.063 |
| n_non_stop_unique_tokens |      -0.057 |
| LDA_04                   |      -0.055 |
| kw_avg_min               |       0.047 |

Top 10 Response Correlated Variables

#### Principal Components Analysis (PCA)

The variables that present highest correlation with the response
variable `shares` are num_hrefs, kw_avg_avg, n_tokens_content,
is_weekend, LDA_03, num_videos, kw_avg_max, n_non_stop_unique_tokens,
LDA_04, kw_avg_min. These variables will be studied in more depth via
PCA to understand the orientation of the most important potential
predictors. The code below presents the PCA analysis as part of the EDA.
The 10 PCs displayed in the table below correspond to the most variable
combination of the 10 predictors, which the PC1 has the most variation
in the data, PC2 presents the second most variation and so on. The
coefficients associated to each variable are the loadings and they give
the idea of importance of that particular variable to the variance of
the 10 predictor variables. The negative values only mean that the
orientation of the weights are opposite in the same PC. Since the first
PC has the largest variability, it is possible to say that the variables
with more weights in PC1 might be the most important variables that
contribute more with the variance of the predictors. This variables are
expected to present large influence on the explanation of the variance
of the response variable. The table below show these numbers.

``` r
id = which(colnames(activeTrain) %in% var_cor)

# PCA
PC = prcomp(activeTrain[,id], center = TRUE, scale = TRUE)

pc_directions=as.data.frame(PC$rotation)

kable(pc_directions, caption="Principal Components for EDA", digits = 3)
```

|                          |    PC1 |    PC2 |    PC3 |    PC4 |    PC5 |    PC6 |    PC7 |    PC8 |    PC9 |   PC10 |
|:-------------------------|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|
| n_tokens_content         |  0.561 | -0.198 | -0.079 |  0.001 |  0.052 |  0.153 |  0.010 |  0.005 |  0.103 |  0.776 |
| n_non_stop_unique_tokens | -0.487 |  0.159 |  0.112 | -0.074 | -0.045 | -0.205 |  0.346 | -0.598 |  0.017 |  0.445 |
| num_hrefs                |  0.508 | -0.106 | -0.105 |  0.030 |  0.117 |  0.038 |  0.132 | -0.709 | -0.168 | -0.395 |
| num_videos               |  0.220 |  0.289 | -0.039 |  0.564 |  0.057 | -0.362 |  0.588 |  0.256 |  0.035 | -0.036 |
| kw_avg_min               |  0.097 | -0.036 |  0.852 |  0.112 | -0.005 |  0.111 |  0.010 |  0.048 | -0.483 |  0.049 |
| kw_avg_max               |  0.040 |  0.569 | -0.313 | -0.321 |  0.197 |  0.238 |  0.108 |  0.124 | -0.584 |  0.101 |
| kw_avg_avg               |  0.179 |  0.513 |  0.366 | -0.256 |  0.325 |  0.158 | -0.007 | -0.044 |  0.604 | -0.093 |
| is_weekend               |  0.246 | -0.035 |  0.090 | -0.551 | -0.064 | -0.775 | -0.052 |  0.108 | -0.088 | -0.008 |
| LDA_03                   |  0.090 |  0.459 | -0.035 |  0.403 | -0.191 | -0.244 | -0.677 | -0.209 | -0.061 |  0.127 |
| LDA_04                   | -0.175 | -0.202 | -0.035 |  0.168 |  0.891 | -0.218 | -0.208 | -0.002 | -0.098 |  0.070 |

Principal Components for EDA

``` r
id2 = which(abs(pc_directions$PC1) %in% 
             sort(abs(pc_directions$PC1),dec= T)[1:3])
```

It is possible to see that the three most important variables in PC1 are
n_tokens_content, n_non_stop_unique_tokens, num_hrefs from the table
above. These variables are considered the most important variables in
terms of variance of the predictor variables. Although the metrics for
prediction are expected to be poor, these variables are expected to show
the most influence to the explanation of the variance of the response
`shares`.

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

![](bus_files/figure-gfm/correlplot-1.png)<!-- -->

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

![](bus_files/figure-gfm/biplot-1.png)<!-- -->

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

![](bus_files/figure-gfm/LDAplot-1.png)<!-- -->

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

![](bus_files/figure-gfm/keywordplot-1.png)<!-- -->

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

![](bus_files/figure-gfm/Contentplot-1.png)<!-- -->

#### Box Plot of Shares for Data Channel *bus*

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

![](bus_files/figure-gfm/boxsharesGraph-1.png)<!-- -->

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

![](bus_files/figure-gfm/titlewordcountGraph-1.png)<!-- -->

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

![](bus_files/figure-gfm/positivewordrateGraph-1.png)<!-- -->

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

![](bus_files/figure-gfm/titleSubjectivityGraph-1.png)<!-- -->

## Modeling

In this section, we will perform regression for prediction purposes for
the data channel bus. All models were fitted using 5-fold
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
    ##    Min     1Q Median     3Q    Max 
    ## -11449  -1230   -736    119  35848 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              2368.102     46.536  50.887  < 2e-16 ***
    ## LDA_00                     63.702     63.076   1.010 0.312588    
    ## LDA_01                     37.357     52.709   0.709 0.478529    
    ## LDA_02                     48.048     55.815   0.861 0.389370    
    ## LDA_03                    193.539     54.178   3.572 0.000358 ***
    ## LDA_04                         NA         NA      NA       NA    
    ## average_token_length     -210.441     51.905  -4.054 5.12e-05 ***
    ## is_weekend                236.693     48.213   4.909 9.48e-07 ***
    ## n_tokens_content          119.172     65.965   1.807 0.070894 .  
    ## n_non_stop_unique_tokens  209.373     61.657   3.396 0.000691 ***
    ## num_hrefs                 567.593     62.581   9.070  < 2e-16 ***
    ## num_self_hrefs           -114.282     50.437  -2.266 0.023510 *  
    ## num_videos                 74.847     48.858   1.532 0.125617    
    ## kw_avg_min                 25.503     51.596   0.494 0.621136    
    ## kw_avg_max                  6.411     55.059   0.116 0.907311    
    ## kw_avg_avg                427.830     55.540   7.703 1.64e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3071 on 4341 degrees of freedom
    ## Multiple R-squared:  0.07516,    Adjusted R-squared:  0.07218 
    ## F-statistic:  25.2 on 14 and 4341 DF,  p-value: < 2.2e-16

The following table shows the output training metrics for this linear
regression.

``` r
lm_out = data.frame(lmFit$results)

kable(lm_out, caption = "Output Training Metrics for Linear Regression",
      digits = 3)
```

| intercept |     RMSE | Rsquared |      MAE |  RMSESD | RsquaredSD |  MAESD |
|:----------|---------:|---------:|---------:|--------:|-----------:|-------:|
| TRUE      | 3072.858 |    0.071 | 1594.714 | 357.835 |      0.044 | 92.097 |

Output Training Metrics for Linear Regression

The following shows the RMSE, RSquared, and MAE values for the model as
it performed on predicting the test set.

``` r
metric_lm = postResample(pred = predict(lmFit, newdata = dfTest), 
                         obs = dfTest$shares)

metric_lm
```

    ##         RMSE     Rsquared          MAE 
    ## 9.739408e+03 1.285013e-02 2.125499e+03

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
    ## (Intercept)              2368.101699
    ## LDA_00                      .       
    ## LDA_01                      .       
    ## LDA_02                      2.406499
    ## LDA_03                    156.754256
    ## LDA_04                    -45.211854
    ## average_token_length     -188.070175
    ## is_weekend                228.530337
    ## n_tokens_content           98.689735
    ## n_non_stop_unique_tokens  174.105504
    ## num_hrefs                 549.685971
    ## num_self_hrefs            -99.508052
    ## num_videos                 69.535626
    ## kw_avg_min                 15.567872
    ## kw_avg_max                  .       
    ## kw_avg_avg                428.062863

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
|     1 |      0 | 3080.436 |    0.062 | 1598.949 | 363.797 |      0.015 | 73.047 |
|     1 |      1 | 3080.436 |    0.062 | 1598.949 | 363.797 |      0.015 | 73.047 |
|     1 |      2 | 3080.316 |    0.062 | 1598.819 | 363.858 |      0.015 | 73.087 |
|     1 |      3 | 3080.127 |    0.062 | 1598.616 | 363.952 |      0.015 | 73.139 |
|     1 |      4 | 3079.943 |    0.062 | 1598.422 | 364.042 |      0.015 | 73.196 |
|     1 |      5 | 3079.758 |    0.062 | 1598.235 | 364.129 |      0.015 | 73.246 |
|     1 |      6 | 3079.574 |    0.062 | 1598.054 | 364.215 |      0.015 | 73.286 |
|     1 |      7 | 3079.392 |    0.062 | 1597.873 | 364.304 |      0.015 | 73.336 |
|     1 |      8 | 3079.216 |    0.062 | 1597.697 | 364.394 |      0.015 | 73.389 |
|     1 |      9 | 3079.046 |    0.062 | 1597.524 | 364.483 |      0.015 | 73.442 |
|     1 |     10 | 3078.883 |    0.062 | 1597.363 | 364.571 |      0.015 | 73.494 |

Output Training Metrics for LASSO

The plot below shows the RMSE by Regularization Parameter (`lambda`). It
is easy to see that RMSE is minimized when `lambda` = 10.

``` r
plot(LASSO)
```

![](bus_files/figure-gfm/LASSOplot-1.png)<!-- -->

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
    ## 9.739828e+03 1.292212e-02 2.125110e+03

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

![](bus_files/figure-gfm/RFplot-1.png)<!-- -->

The following table shows training metrics for the random forest model.
Again, the best model is the one that minimizes RMSE.

``` r
rf_out = data.frame(rfFit$results)

kable(rf_out, caption = "Output Training Metrics for Random Forest",
      digits = 3)
```

| mtry |     RMSE | Rsquared |      MAE |  RMSESD | RsquaredSD |  MAESD |
|-----:|---------:|---------:|---------:|--------:|-----------:|-------:|
|    1 | 3049.162 |    0.082 | 1580.075 | 311.340 |      0.012 | 57.235 |
|    2 | 3050.785 |    0.085 | 1594.070 | 286.709 |      0.016 | 46.430 |
|    3 | 3068.642 |    0.081 | 1611.581 | 278.874 |      0.017 | 45.038 |
|    4 | 3089.376 |    0.076 | 1625.251 | 279.519 |      0.017 | 51.346 |
|    5 | 3094.680 |    0.077 | 1626.642 | 270.902 |      0.018 | 49.589 |

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
    ## 9.733627e+03 1.411698e-02 2.120321e+03

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

    ## [1] 3

The best n.trees and interaction.depth parameters for this model are 50
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

| shrinkage | interaction.depth | n.minobsinnode | n.trees |     RMSE | Rsquared |      MAE |  RMSESD | RsquaredSD |  MAESD |
|----------:|------------------:|---------------:|--------:|---------:|---------:|---------:|--------:|-----------:|-------:|
|       0.1 |                 3 |             10 |      50 | 3044.279 |    0.086 | 1568.225 | 271.034 |      0.026 | 66.984 |
|       0.1 |                 1 |             10 |     150 | 3044.282 |    0.084 | 1564.030 | 256.959 |      0.022 | 57.645 |
|       0.1 |                 1 |             10 |     100 | 3044.760 |    0.083 | 1558.256 | 258.544 |      0.023 | 62.682 |
|       0.1 |                 1 |             10 |     125 | 3045.337 |    0.083 | 1562.325 | 255.422 |      0.023 | 57.734 |
|       0.1 |                 1 |             10 |     175 | 3047.269 |    0.083 | 1564.574 | 258.984 |      0.020 | 62.411 |
|       0.1 |                 1 |             10 |      75 | 3048.274 |    0.081 | 1561.849 | 256.559 |      0.025 | 52.611 |
|       0.1 |                 1 |             10 |     200 | 3048.502 |    0.083 | 1563.099 | 260.537 |      0.019 | 61.694 |
|       0.1 |                 3 |             10 |      75 | 3049.188 |    0.086 | 1566.877 | 267.000 |      0.029 | 68.784 |
|       0.1 |                 3 |             10 |      25 | 3050.476 |    0.082 | 1570.848 | 271.026 |      0.029 | 62.000 |
|       0.1 |                 2 |             10 |      75 | 3050.669 |    0.086 | 1571.957 | 249.012 |      0.035 | 64.973 |
|       0.1 |                 1 |             10 |      50 | 3051.141 |    0.080 | 1567.234 | 259.972 |      0.025 | 54.809 |
|       0.1 |                 3 |             10 |     100 | 3054.910 |    0.085 | 1578.657 | 265.404 |      0.027 | 71.740 |
|       0.1 |                 2 |             10 |      25 | 3055.670 |    0.078 | 1581.749 | 262.762 |      0.032 | 59.637 |
|       0.1 |                 2 |             10 |      50 | 3055.989 |    0.081 | 1569.498 | 249.382 |      0.040 | 61.914 |
|       0.1 |                 2 |             10 |     100 | 3057.774 |    0.081 | 1571.931 | 258.635 |      0.032 | 70.482 |
|       0.1 |                 4 |             10 |      25 | 3060.860 |    0.075 | 1573.266 | 255.858 |      0.023 | 64.021 |
|       0.1 |                 1 |             10 |      25 | 3064.944 |    0.076 | 1581.891 | 275.479 |      0.021 | 58.533 |
|       0.1 |                 3 |             10 |     125 | 3065.801 |    0.083 | 1587.001 | 270.757 |      0.019 | 73.377 |
|       0.1 |                 2 |             10 |     125 | 3067.682 |    0.079 | 1579.301 | 264.803 |      0.034 | 77.961 |
|       0.1 |                 4 |             10 |      50 | 3069.913 |    0.075 | 1575.191 | 254.553 |      0.021 | 67.835 |
|       0.1 |                 3 |             10 |     150 | 3075.404 |    0.080 | 1592.991 | 270.834 |      0.020 | 75.295 |
|       0.1 |                 2 |             10 |     175 | 3078.926 |    0.075 | 1582.748 | 270.117 |      0.027 | 77.738 |
|       0.1 |                 4 |             10 |      75 | 3081.431 |    0.073 | 1587.301 | 255.092 |      0.022 | 71.004 |
|       0.1 |                 2 |             10 |     150 | 3082.560 |    0.074 | 1583.345 | 268.237 |      0.030 | 77.568 |
|       0.1 |                 3 |             10 |     175 | 3083.039 |    0.078 | 1595.080 | 272.840 |      0.018 | 73.866 |
|       0.1 |                 2 |             10 |     200 | 3084.357 |    0.074 | 1583.073 | 268.780 |      0.028 | 79.765 |
|       0.1 |                 3 |             10 |     200 | 3091.072 |    0.077 | 1598.058 | 277.501 |      0.020 | 78.780 |
|       0.1 |                 4 |             10 |     100 | 3096.484 |    0.068 | 1589.904 | 270.602 |      0.013 | 77.052 |
|       0.1 |                 4 |             10 |     125 | 3106.346 |    0.067 | 1603.069 | 278.478 |      0.013 | 84.360 |
|       0.1 |                 4 |             10 |     150 | 3119.097 |    0.065 | 1614.585 | 283.004 |      0.014 | 84.937 |
|       0.1 |                 4 |             10 |     175 | 3126.928 |    0.064 | 1616.571 | 297.276 |      0.014 | 89.755 |
|       0.1 |                 4 |             10 |     200 | 3128.502 |    0.066 | 1625.585 | 299.120 |      0.015 | 92.948 |

Output Training Metrics for Boosting

The plot below shows the RMSE by Number of Boosting Iterations and
display Max Tree Depth lines for the 5-fold CV performed. It is easy to
see that RMSE is minimized when n.trees = 50 and interaction.depth = 3.

``` r
plot(gbmFit)
```

![](bus_files/figure-gfm/boostingPlot-1.png)<!-- -->

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
    ## 9.768783e+03 6.562282e-03 2.122103e+03

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
| RMSE     | 9733.627 | 9768.783 | 9739.828 | 9739.408 |
| Rsquared |    0.014 |    0.007 |    0.013 |    0.013 |
| MAE      | 2120.321 | 2122.103 | 2125.110 | 2125.499 |

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
