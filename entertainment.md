Project 3
================
Kara Belknap & Cassio Monti
2022-11-5

-   <a href="#report-for-entertainment-channel"
    id="toc-report-for-entertainment-channel">Report for
    <em>entertainment</em> Channel</a>
    -   <a href="#introduction" id="toc-introduction">Introduction</a>
    -   <a href="#required-packages" id="toc-required-packages">Required
        Packages</a>
    -   <a href="#read-in-the-data" id="toc-read-in-the-data">Read in the
        Data</a>
    -   <a href="#select-data-for-appropriate-data-channel"
        id="toc-select-data-for-appropriate-data-channel">Select Data for
        Appropriate Data Channel</a>
    -   <a href="#summarizations-for-the-data-channel-entertainment"
        id="toc-summarizations-for-the-data-channel-entertainment">Summarizations
        for the Data Channel <em>entertainment</em></a>
        -   <a href="#data-split" id="toc-data-split">Data Split</a>
        -   <a href="#data-manipulation-for-statistics"
            id="toc-data-manipulation-for-statistics">Data manipulation for
            statistics</a>
        -   <a href="#belknap---summary-statistics"
            id="toc-belknap---summary-statistics">Belknap - Summary Statistics</a>
        -   <a href="#monti---summary-statistics"
            id="toc-monti---summary-statistics">Monti - Summary Statistics</a>
        -   <a href="#monti---graphs-5" id="toc-monti---graphs-5">Monti - Graphs
            (5)</a>
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
    -   <a href="#reference-list" id="toc-reference-list">Reference List</a>

# Report for *entertainment* Channel

This report contains Exploratory Data Analysis (EDA) about this data
channel and a modeling section applying three regression methods.

## Introduction

The objective of this analysis is to provide a comprehensive overview
about publication metrics and their relationship with the number of
shares that those publications presented during the study period. These
data have been collected from Mashable website, one of the largest news
websites from which the content of all the entertainment channel
articles published in 2013 and 2014 was extracted. The full data
description can be found
[here](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity).
These data were originally collected and analyzed By Fernandes et
al. (2015), in which the authors performed classification task comparing
several machine learning algorithms. In the present study, a subset of
the data used by Fernandes et al.(2015) corresponding to the data
channel entertainment is used for regression purposes. The response
variable is the number of `shares` that the papers presented after
publication. In other words, we will try to predict the number of shares
that the papers will have before publication and evaluate the prediction
of each selected model based on some common metrics, such as RMSE (Root
Mean Squared Error), Rsquared (Coefficient of Determination), and MAE
(Mean Absolute Error) applied to the test set. To perform the
regression, the methods: Random Forest, Boosting, Multiple Linear
Regression, and LASSO regression will be used. More information about
the methods will be corresponding in further sections.

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
library(GGally)
library(knitr)
```

`Tidyverse` is used for data management and plotting through dplyr and
ggplot packages. `Caret` package is used for data splitting and
modeling. `knitr` package is used to provide nice looking tables.
`GGally` is used for nice correlation and exploratory plots assisting in
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

## Summarizations for the Data Channel *entertainment*

In this section, we will perform EDA for the data channel entertainment

### Data Split

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
for the number of shares for articles in the data channel entertainment.
The `summary()` function was used to extract these metrics.

``` r
summary(activeTrain$shares)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      47     833    1200    2994    2100  197600

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
| No      |  12476151 |  2883.326 |      1100 | 8132.781 |
| Yes     |   2315889 |  3771.806 |      1700 | 6189.207 |

Statistics for Shares for Weekend or Weekdays

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
| Monday    |   2859993 |  2927.321 |      1100 |  7383.656 |    112600 |        59 |
| Tuesday   |   2388631 |  2739.256 |      1100 |  5950.416 |     63300 |        47 |
| Wednesday |   2653011 |  2909.003 |      1100 |  9163.614 |    138700 |        49 |
| Thursday  |   2696500 |  3081.714 |      1100 | 10686.908 |    197600 |        57 |
| Friday    |   1878016 |  2717.823 |      1200 |  6101.696 |     82200 |        82 |
| Saturday  |    862980 |  3397.559 |      1600 |  5791.844 |     57500 |       183 |
| Sunday    |   1452909 |  4035.858 |      1800 |  6449.736 |     69500 |       171 |

Statistics for Shares Across Days of Week

### Monti - Summary Statistics

First, we will analyse the frequency of occurrence of publications on
each day of the week. The one-way contingency table below presents those
frequencies.

``` r
table(statsData$Day)
```

    ## 
    ##    Monday   Tuesday Wednesday  Thursday    Friday  Saturday    Sunday 
    ##       977       872       912       875       691       254       360

A second discrete analysis performed here is the two-way contingency
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

|     | (-151,9.88e+04\] | (9.88e+04,1.98e+05\] |
|:----|-----------------:|---------------------:|
| 0   |             4321 |                    6 |
| 1   |              614 |                    0 |

Frequency of Shares in Weekend vs in Working Days

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

| Variables  | Correlation |
|:-----------|------------:|
| LDA_00     |      -0.031 |
| LDA_01     |      -0.025 |
| LDA_02     |      -0.027 |
| LDA_03     |       0.047 |
| LDA_04     |      -0.017 |
| is_weekend |       0.037 |
| num_hrefs  |       0.053 |
| kw_avg_min |       0.198 |
| kw_avg_max |       0.055 |
| kw_avg_avg |       0.235 |

Top 10 Response Correlated Variables

The variables that present most correlation with the response variable
`shares` are LDA_00, LDA_01, LDA_02, LDA_03, LDA_04, is_weekend,
num_hrefs, kw_avg_min, kw_avg_max, kw_avg_avg. These variables will be
studied in more depth via PCA to understand the orientation of the most
important potential predictors. The code below presents the PCA analysis
as part of the EDA. The 10 PCs displayed in the table below correspond
to the most variable combination of the 10 predictors, which the PC1 has
the most variation in the data, PC2 presents the second most variation
and so on. The coefficients associated to each variable are the loadings
and they give the idea of importance of that particular variable to the
variance of the 10 predictor variables. The negative values only mean
that the orientation of the weights are opposite in the same PC. Since
the first PC has the largest variability, it is possible to say that the
variables with more weights in PC1 are the most important variables for
the variance of the predictors. This variables are supposed to present
large influence on the explanation of the variance of the response
variable. The table below show these numbers.

``` r
id = which(colnames(activeTrain) %in% var_cor)

# PCA
PC = prcomp(activeTrain[,id], center = TRUE, scale = TRUE)

pc_directions=as.data.frame(PC$rotation)

kable(pc_directions, caption="Principal Components for EDA", digits = 3)
```

|            |    PC1 |    PC2 |    PC3 |    PC4 |    PC5 |    PC6 |    PC7 |    PC8 |    PC9 |  PC10 |
|:-----------|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|------:|
| num_hrefs  | -0.084 | -0.171 | -0.071 | -0.520 | -0.002 |  0.340 | -0.566 | -0.500 | -0.053 | 0.000 |
| kw_avg_min | -0.071 |  0.618 | -0.293 | -0.347 | -0.166 |  0.138 |  0.325 | -0.017 | -0.504 | 0.000 |
| kw_avg_max | -0.391 |  0.072 |  0.525 |  0.118 |  0.097 | -0.076 | -0.385 |  0.286 | -0.554 | 0.001 |
| kw_avg_avg | -0.355 |  0.600 |  0.121 | -0.126 |  0.014 | -0.023 | -0.221 |  0.135 |  0.645 | 0.000 |
| is_weekend | -0.071 | -0.066 | -0.013 | -0.328 | -0.179 | -0.899 | -0.025 | -0.200 | -0.034 | 0.000 |
| LDA_00     |  0.041 | -0.139 | -0.317 | -0.397 |  0.651 | -0.084 | -0.055 |  0.503 | -0.008 | 0.185 |
| LDA_01     |  0.560 |  0.330 |  0.241 |  0.088 |  0.147 | -0.084 | -0.170 | -0.194 | -0.050 | 0.643 |
| LDA_02     |  0.055 | -0.246 |  0.472 | -0.502 | -0.395 |  0.187 |  0.299 |  0.308 |  0.126 | 0.266 |
| LDA_03     | -0.607 | -0.172 | -0.213 |  0.188 | -0.009 |  0.035 |  0.187 | -0.193 |  0.009 | 0.669 |
| LDA_04     |  0.129 | -0.032 | -0.438 |  0.136 | -0.574 | -0.020 | -0.470 |  0.433 | -0.030 | 0.181 |

Principal Components for EDA

``` r
id2 = which(abs(pc_directions$PC1) %in% 
             sort(abs(pc_directions$PC1),dec= T)[1:3])
```

It is possible to see that the three most important variables in PC1 are
kw_avg_max, LDA_01, LDA_03 from the table above. These variables are
considered the most important variables in terms of variance of the
predictor variables. Although the metrics for prediction are expected to
be poor, these variables are expected to show the most influence to the
explanation of the variance of the response `shares`.

### Monti - Graphs (5)

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

![](entertainment_files/figure-gfm/correlplot-1.png)<!-- -->

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

![](entertainment_files/figure-gfm/biplot-1.png)<!-- -->

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

![](entertainment_files/figure-gfm/LDAplot-1.png)<!-- -->

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

![](entertainment_files/figure-gfm/keywordplot-1.png)<!-- -->

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

![](entertainment_files/figure-gfm/Contentplot-1.png)<!-- -->

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

![](entertainment_files/figure-gfm/titlewordcountGraph-1.png)<!-- -->

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

![](entertainment_files/figure-gfm/positivewordrateGraph-1.png)<!-- -->

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
the data channel entertainment. All models were fitted using 5-fold
Cross-Validation via `train()` function from `caret` package. All
variables were scaled and centered as well.

### Belknap - Linear Regression Model Explanation

(add some thoughts here)

### Monti - Linear Regression Model (LASSO Regression)

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
    ## (Intercept)              2993.734062
    ## LDA_00                   -174.729364
    ## LDA_01                      .       
    ## LDA_02                    -27.171852
    ## LDA_03                     39.223989
    ## LDA_04                    -37.106664
    ## average_token_length      183.870933
    ## is_weekend                231.903617
    ## n_tokens_content         -123.916563
    ## n_non_stop_unique_tokens   43.097159
    ## num_hrefs                 492.265720
    ## num_self_hrefs           -140.231848
    ## num_videos               -132.431516
    ## kw_avg_min                978.594409
    ## kw_avg_max                  9.577283
    ## kw_avg_avg               1429.643984

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
|     1 |      0 | 12105.64 |    0.023 | 3111.719 | 8557.167 |      0.031 | 283.069 |
|     1 |      1 | 12105.64 |    0.023 | 3111.719 | 8557.167 |      0.031 | 283.069 |
|     1 |      2 | 12105.60 |    0.023 | 3111.710 | 8557.179 |      0.031 | 283.068 |
|     1 |      3 | 12105.42 |    0.023 | 3111.675 | 8557.226 |      0.031 | 283.065 |
|     1 |      4 | 11985.41 |    0.023 | 3107.624 | 8292.088 |      0.032 | 274.751 |
|     1 |      5 | 11853.25 |    0.023 | 3103.072 | 8000.574 |      0.032 | 265.704 |
|     1 |      6 | 11721.31 |    0.023 | 3098.448 | 7710.100 |      0.032 | 256.807 |
|     1 |      7 | 11589.90 |    0.023 | 3093.841 | 7421.046 |      0.032 | 247.985 |
|     1 |      8 | 11459.50 |    0.024 | 3089.260 | 7134.521 |      0.033 | 239.279 |
|     1 |      9 | 11330.44 |    0.024 | 3084.716 | 6851.259 |      0.033 | 230.718 |
|     1 |     10 | 11203.84 |    0.024 | 3080.252 | 6573.763 |      0.033 | 222.389 |

Output Training Metrics for LASSO

The plot below shows the RMSE by Regularization Parameter (`lambda`). It
is easy to see that RMSE is minimized when `lambda` = 10.

``` r
plot(LASSO)
```

![](entertainment_files/figure-gfm/LASSOplot-1.png)<!-- -->

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
    ## 8.132572e+03 1.963513e-03 2.995271e+03

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
    ## -46080  -2109  -1177   -163 183526 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 2993.73     108.62  27.561  < 2e-16 ***
    ## LDA_00                    -81874.75   66641.11  -1.229 0.219283    
    ## LDA_01                   -283189.51  231022.55  -1.226 0.220329    
    ## LDA_02                   -117426.61   95763.97  -1.226 0.220179    
    ## LDA_03                   -294585.23  240354.93  -1.226 0.220398    
    ## LDA_04                    -79979.24   65208.03  -1.227 0.220060    
    ## average_token_length         350.16     169.16   2.070 0.038506 *  
    ## is_weekend                   234.99     109.39   2.148 0.031752 *  
    ## n_tokens_content            -209.31     137.93  -1.517 0.129208    
    ## n_non_stop_unique_tokens  -12474.67   10220.64  -1.221 0.222320    
    ## num_hrefs                    484.21     128.66   3.764 0.000169 ***
    ## num_self_hrefs              -155.95     125.13  -1.246 0.212728    
    ## num_videos                  -136.58     116.77  -1.170 0.242213    
    ## kw_avg_min                  1000.37     130.40   7.672 2.03e-14 ***
    ## kw_avg_max                    39.06     138.88   0.281 0.778549    
    ## kw_avg_avg                  1413.23     144.96   9.749  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7635 on 4925 degrees of freedom
    ## Multiple R-squared:  0.07393,    Adjusted R-squared:  0.07111 
    ## F-statistic: 26.21 on 15 and 4925 DF,  p-value: < 2.2e-16

(add some thoughts here)

``` r
lm_out = data.frame(lmFit$results)

kable(lm_out, caption = "Output Training Metrics for Linear Regression",
      digits = 3)
```

| intercept |     RMSE | Rsquared |      MAE |   RMSESD | RsquaredSD |   MAESD |
|:----------|---------:|---------:|---------:|---------:|-----------:|--------:|
| TRUE      | 14424.55 |    0.031 | 3215.092 | 15603.02 |      0.031 | 780.901 |

Output Training Metrics for Linear Regression

``` r
metric_lm = postResample(pred = predict(lmFit, newdata = dfTest), 
                         obs = dfTest$shares)

metric_lm
```

    ##         RMSE     Rsquared          MAE 
    ## 8.145986e+03 1.841703e-03 3.003641e+03

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

![](entertainment_files/figure-gfm/RFplot-1.png)<!-- -->

(add some thoughts here)

``` r
rf_out = data.frame(rfFit$results)

kable(rf_out, caption = "Output Training Metrics for Random Forest",
      digits = 3)
```

| mtry |     RMSE | Rsquared |      MAE |   RMSESD | RsquaredSD |   MAESD |
|-----:|---------:|---------:|---------:|---------:|-----------:|--------:|
|    1 | 7669.397 |    0.039 | 2925.218 | 1450.280 |      0.014 | 157.677 |
|    2 | 7694.337 |    0.036 | 2985.984 | 1427.870 |      0.012 | 146.450 |
|    3 | 7740.403 |    0.031 | 3023.858 | 1420.714 |      0.010 | 147.939 |
|    4 | 7773.487 |    0.029 | 3058.597 | 1425.617 |      0.010 | 150.266 |
|    5 | 7782.004 |    0.029 | 3069.008 | 1423.413 |      0.009 | 153.200 |

Output Training Metrics for Random Forest

(add some thoughts here)

``` r
RF_pred <- predict(rfFit, newdata = activeTest)

metric_rf = postResample(RF_pred, activeTest$shares)

metric_rf
```

    ##         RMSE     Rsquared          MAE 
    ## 7.717221e+03 9.487408e-03 2.890411e+03

### Monti - Boosted Tree Model & Explanation

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

    ## [1] 2

The best n.trees and interaction.depth parameters for this model are 25
and 2, respectively. These values can be seen in the table below, which
summarizes all the metrics for the 5-fold cross-validation. It is easy
to see that these values minimize the RMSE.

``` r
gbm_out = data.frame(gbmFit$results)

kable(gbm_out, caption = "Output Training Metrics for Boosting",
      digits = 3)
```

|     | shrinkage | interaction.depth | n.minobsinnode | n.trees |     RMSE | Rsquared |      MAE |   RMSESD | RsquaredSD |   MAESD |
|:----|----------:|------------------:|---------------:|--------:|---------:|---------:|---------:|---------:|-----------:|--------:|
| 1   |       0.1 |                 1 |             10 |      25 | 7627.321 |    0.017 | 2940.157 | 2266.312 |      0.011 | 279.220 |
| 9   |       0.1 |                 2 |             10 |      25 | 7612.836 |    0.025 | 2923.521 | 2282.253 |      0.023 | 274.734 |
| 17  |       0.1 |                 3 |             10 |      25 | 7634.108 |    0.024 | 2935.683 | 2281.707 |      0.021 | 282.344 |
| 25  |       0.1 |                 4 |             10 |      25 | 7629.453 |    0.023 | 2944.692 | 2219.317 |      0.016 | 253.147 |
| 2   |       0.1 |                 1 |             10 |      50 | 7641.893 |    0.020 | 2931.292 | 2250.356 |      0.011 | 271.381 |
| 10  |       0.1 |                 2 |             10 |      50 | 7699.685 |    0.017 | 2941.947 | 2213.514 |      0.013 | 252.678 |
| 18  |       0.1 |                 3 |             10 |      50 | 7719.324 |    0.018 | 2958.641 | 2253.515 |      0.017 | 266.424 |
| 26  |       0.1 |                 4 |             10 |      50 | 7694.950 |    0.018 | 2961.496 | 2191.356 |      0.009 | 261.821 |
| 3   |       0.1 |                 1 |             10 |      75 | 7630.396 |    0.022 | 2933.653 | 2233.263 |      0.012 | 267.458 |
| 11  |       0.1 |                 2 |             10 |      75 | 7659.354 |    0.018 | 2926.283 | 2255.440 |      0.010 | 292.028 |
| 19  |       0.1 |                 3 |             10 |      75 | 7715.623 |    0.017 | 2957.584 | 2258.915 |      0.014 | 258.062 |
| 27  |       0.1 |                 4 |             10 |      75 | 7689.068 |    0.019 | 2964.142 | 2186.925 |      0.010 | 256.264 |
| 4   |       0.1 |                 1 |             10 |     100 | 7670.472 |    0.018 | 2942.695 | 2233.817 |      0.014 | 276.680 |
| 12  |       0.1 |                 2 |             10 |     100 | 7687.333 |    0.017 | 2934.244 | 2247.676 |      0.009 | 286.599 |
| 20  |       0.1 |                 3 |             10 |     100 | 7757.027 |    0.017 | 2969.908 | 2239.025 |      0.013 | 240.443 |
| 28  |       0.1 |                 4 |             10 |     100 | 7737.471 |    0.016 | 2976.002 | 2182.769 |      0.006 | 234.904 |
| 5   |       0.1 |                 1 |             10 |     125 | 7703.005 |    0.017 | 2948.912 | 2209.250 |      0.010 | 269.817 |
| 13  |       0.1 |                 2 |             10 |     125 | 7710.277 |    0.015 | 2946.095 | 2252.090 |      0.006 | 280.308 |
| 21  |       0.1 |                 3 |             10 |     125 | 7755.999 |    0.015 | 2950.721 | 2236.074 |      0.011 | 238.041 |
| 29  |       0.1 |                 4 |             10 |     125 | 7787.275 |    0.013 | 2997.522 | 2174.428 |      0.005 | 224.633 |
| 6   |       0.1 |                 1 |             10 |     150 | 7674.110 |    0.016 | 2941.241 | 2236.821 |      0.010 | 256.043 |
| 14  |       0.1 |                 2 |             10 |     150 | 7724.438 |    0.014 | 2947.755 | 2260.144 |      0.006 | 280.871 |
| 22  |       0.1 |                 3 |             10 |     150 | 7784.909 |    0.015 | 2976.413 | 2252.740 |      0.014 | 232.476 |
| 30  |       0.1 |                 4 |             10 |     150 | 7815.351 |    0.013 | 3023.499 | 2134.093 |      0.006 | 198.823 |
| 7   |       0.1 |                 1 |             10 |     175 | 7651.694 |    0.018 | 2932.876 | 2250.650 |      0.011 | 273.088 |
| 15  |       0.1 |                 2 |             10 |     175 | 7736.414 |    0.013 | 2942.394 | 2265.570 |      0.007 | 271.450 |
| 23  |       0.1 |                 3 |             10 |     175 | 7781.968 |    0.016 | 2989.460 | 2238.291 |      0.013 | 225.759 |
| 31  |       0.1 |                 4 |             10 |     175 | 7836.925 |    0.014 | 3035.403 | 2121.848 |      0.007 | 198.714 |
| 8   |       0.1 |                 1 |             10 |     200 | 7704.383 |    0.015 | 2950.401 | 2232.777 |      0.012 | 259.266 |
| 16  |       0.1 |                 2 |             10 |     200 | 7750.639 |    0.013 | 2959.693 | 2264.129 |      0.006 | 275.415 |
| 24  |       0.1 |                 3 |             10 |     200 | 7826.612 |    0.014 | 3006.627 | 2203.422 |      0.011 | 203.335 |
| 32  |       0.1 |                 4 |             10 |     200 | 7840.197 |    0.015 | 3055.517 | 2130.631 |      0.008 | 221.225 |

Output Training Metrics for Boosting

The plot below shows the RMSE by Number of Boosting Iterations and
display Max Tree Depth lines for the 5-fold CV performed. It is easy to
see that RMSE is minimized when n.trees = 25 and interaction.depth = 2.

``` r
plot(gbmFit)
```

![](entertainment_files/figure-gfm/boostingPlot-1.png)<!-- -->

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
    ## 7.780011e+03 4.996867e-03 2.901337e+03

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
| RMSE     | 7717.221 | 7780.011 | 8132.572 | 8145.986 |
| Rsquared |    0.009 |    0.005 |    0.002 |    0.002 |
| MAE      | 2890.411 | 2901.337 | 2995.271 | 3003.641 |

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
