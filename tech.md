Project 3
================
Kara Belknap & Cassio Monti
2022-11-5

-   <a href="#report-for-tech-channel"
    id="toc-report-for-tech-channel">Report for <em>tech</em> Channel</a>
    -   <a href="#introduction" id="toc-introduction">Introduction</a>
    -   <a href="#required-packages" id="toc-required-packages">Required
        Packages</a>
    -   <a href="#read-in-the-data" id="toc-read-in-the-data">Read in the
        Data</a>
    -   <a href="#select-data-for-appropriate-data-channel"
        id="toc-select-data-for-appropriate-data-channel">Select Data for
        Appropriate Data Channel</a>
    -   <a href="#summarizations-for-the-data-channel-tech"
        id="toc-summarizations-for-the-data-channel-tech">Summarizations for the
        Data Channel <em>tech</em></a>
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

# Report for *tech* Channel

This report contains Exploratory Data Analysis (EDA) about this data
channel and a modeling section applying three regression methods.

## Introduction

The objective of this analysis is to provide a comprehensive overview
about publication metrics and their relationship with the number of
shares that those publications presented during the study period. These
data have been collected from Mashable website, one of the largest news
websites from which the content of all the tech channel articles
published in 2013 and 2014 was extracted. The full data description can
be found
[here](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity).
These data were originally collected and analyzed By Fernandes et
al. (2015), in which the authors performed classification task comparing
several machine learning algorithms. In the present study, a subset of
the data used by Fernandes et al.(2015) corresponding to the data
channel tech is used for regression purposes. The response variable is
the number of `shares` that the papers presented after publication. In
other words, we will try to predict the number of shares that the papers
will have before publication and evaluate the prediction of each
selected model based on some common metrics, such as RMSE (Root Mean
Squared Error), Rsquared (Coefficient of Determination), and MAE (Mean
Absolute Error) applied to the test set. To perform the regression, the
methods: Random Forest, Boosting, Multiple Linear Regression, and LASSO
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

## Summarizations for the Data Channel *tech*

In this section, we will perform EDA for the data channel tech

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
for the number of shares for articles in the data channel tech. The
`summary()` function was used to extract these metrics.

``` r
summary(activeTrain$shares)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      82    1100    1700    3084    3000  663600

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
| No      |  13430362 |  2989.840 |      1600 | 10811.232 |
| Yes     |   2436020 |  3730.505 |      2300 |  4973.371 |

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
| Monday    |   2442551 |  2830.302 |      1600 |  3982.259 |     51000 |       192 |
| Tuesday   |   2955083 |  2811.687 |      1500 |  4334.198 |     67800 |       104 |
| Wednesday |   3391555 |  3471.397 |      1600 | 21550.475 |    663600 |       181 |
| Thursday  |   2451320 |  2735.848 |      1550 |  4060.243 |     52600 |        86 |
| Friday    |   2189853 |  3106.174 |      1800 |  5733.835 |    104100 |        82 |
| Saturday  |   1288201 |  3435.203 |      2200 |  3533.531 |     26900 |       343 |
| Sunday    |   1147819 |  4128.845 |      2500 |  6409.696 |     83300 |       206 |

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
    ##       863      1051       977       896       705       375       278

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

|     | (-582,3.32e+05\] | (3.32e+05,6.64e+05\] |
|:----|-----------------:|---------------------:|
| 0   |             4491 |                    1 |
| 1   |              653 |                    0 |

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

| Variables                | Correlation |
|:-------------------------|------------:|
| LDA_00                   |       0.031 |
| LDA_04                   |      -0.016 |
| is_weekend               |       0.024 |
| n_tokens_content         |       0.073 |
| n_non_stop_unique_tokens |      -0.044 |
| num_hrefs                |       0.054 |
| num_self_hrefs           |      -0.012 |
| num_videos               |       0.025 |
| kw_avg_max               |      -0.009 |
| kw_avg_avg               |       0.044 |

Top 10 Response Correlated Variables

The variables that present most correlation with the response variable
`shares` are LDA_00, LDA_04, is_weekend, n_tokens_content,
n_non_stop_unique_tokens, num_hrefs, num_self_hrefs, num_videos,
kw_avg_max, kw_avg_avg. These variables will be studied in more depth
via PCA to understand the orientation of the most important potential
predictors. The code below presents the PCA analysis as part of the EDA.
The 10 PCs displayed in the table below correspond to the most variable
combination of the 10 predictors, which the PC1 has the most variation
in the data, PC2 presents the second most variation and so on. The
coefficients associated to each variable are the loadings and they give
the idea of importance of that particular variable to the variance of
the 10 predictor variables. The negative values only mean that the
orientation of the weights are opposite in the same PC. Since the first
PC has the largest variability, it is possible to say that the variables
with more weights in PC1 are the most important variables for the
variance of the predictors. This variables are supposed to present large
influence on the explanation of the variance of the response variable.
The table below show these numbers.

``` r
id = which(colnames(activeTrain) %in% var_cor)

# PCA
PC = prcomp(activeTrain[,id], center = TRUE, scale = TRUE)

pc_directions=as.data.frame(PC$rotation)

kable(pc_directions, caption="Principal Components for EDA", digits = 3)
```

|                          |    PC1 |    PC2 |    PC3 |    PC4 |    PC5 |    PC6 |    PC7 |    PC8 |    PC9 |   PC10 |
|:-------------------------|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|
| n_tokens_content         | -0.530 |  0.026 | -0.002 |  0.152 | -0.016 |  0.314 | -0.101 |  0.021 |  0.389 | -0.658 |
| n_non_stop_unique_tokens |  0.464 |  0.051 |  0.023 | -0.213 |  0.122 | -0.576 | -0.012 |  0.103 |  0.435 | -0.436 |
| num_hrefs                | -0.510 |  0.137 | -0.008 | -0.120 | -0.005 | -0.356 | -0.147 |  0.191 |  0.486 |  0.535 |
| num_self_hrefs           | -0.468 |  0.032 |  0.034 | -0.099 | -0.075 | -0.561 |  0.287 | -0.019 | -0.540 | -0.274 |
| num_videos               | -0.063 |  0.065 | -0.162 |  0.401 |  0.882 | -0.096 |  0.047 | -0.105 | -0.041 |  0.043 |
| kw_avg_max               |  0.055 |  0.204 | -0.659 |  0.068 | -0.154 |  0.097 |  0.639 |  0.228 |  0.148 |  0.028 |
| kw_avg_avg               | -0.001 |  0.235 | -0.657 | -0.161 | -0.061 | -0.066 | -0.638 | -0.141 | -0.210 | -0.086 |
| is_weekend               | -0.101 |  0.056 |  0.019 | -0.841 |  0.398 |  0.306 |  0.158 | -0.001 | -0.033 | -0.018 |
| LDA_00                   |  0.031 |  0.675 |  0.190 |  0.021 | -0.107 |  0.002 |  0.161 | -0.674 |  0.123 |  0.037 |
| LDA_04                   | -0.082 | -0.646 | -0.262 | -0.089 | -0.055 | -0.109 |  0.133 | -0.645 |  0.218 |  0.058 |

Principal Components for EDA

``` r
id2 = which(abs(pc_directions$PC1) %in% 
             sort(abs(pc_directions$PC1),dec= T)[1:3])
```

It is possible to see that the three most important variables in PC1 are
n_tokens_content, num_hrefs, num_self_hrefs from the table above. These
variables are considered the most important variables in terms of
variance of the predictor variables. Although the metrics for prediction
are expected to be poor, these variables are expected to show the most
influence to the explanation of the variance of the response `shares`.

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

![](tech_files/figure-gfm/correlplot-1.png)<!-- -->

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

![](tech_files/figure-gfm/biplot-1.png)<!-- -->

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

![](tech_files/figure-gfm/LDAplot-1.png)<!-- -->

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

![](tech_files/figure-gfm/keywordplot-1.png)<!-- -->

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

![](tech_files/figure-gfm/Contentplot-1.png)<!-- -->

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

![](tech_files/figure-gfm/titlewordcountGraph-1.png)<!-- -->

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

![](tech_files/figure-gfm/positivewordrateGraph-1.png)<!-- -->

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
the data channel tech. All models were fitted using 5-fold
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
    ## (Intercept)              3083.844898
    ## LDA_00                    314.324802
    ## LDA_01                      6.688812
    ## LDA_02                      .       
    ## LDA_03                    -13.025763
    ## LDA_04                     -3.721726
    ## average_token_length      -40.883991
    ## is_weekend                145.680028
    ## n_tokens_content          708.587666
    ## n_non_stop_unique_tokens  -77.196606
    ## num_hrefs                 461.087525
    ## num_self_hrefs           -720.167094
    ## num_videos                171.315649
    ## kw_avg_min               -325.203118
    ## kw_avg_max               -362.792300
    ## kw_avg_avg                578.856254

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
|     1 |      0 | 7699.557 |    0.012 | 2387.851 | 7616.828 |      0.004 | 313.672 |
|     1 |      1 | 7699.562 |    0.012 | 2387.847 | 7616.837 |      0.004 | 313.662 |
|     1 |      2 | 7699.610 |    0.012 | 2387.779 | 7616.965 |      0.004 | 313.539 |
|     1 |      3 | 7698.992 |    0.012 | 2387.257 | 7617.501 |      0.004 | 313.724 |
|     1 |      4 | 7698.325 |    0.012 | 2386.707 | 7618.068 |      0.004 | 313.953 |
|     1 |      5 | 7697.660 |    0.012 | 2386.163 | 7618.626 |      0.004 | 314.181 |
|     1 |      6 | 7696.983 |    0.012 | 2385.626 | 7619.146 |      0.004 | 314.423 |
|     1 |      7 | 7696.315 |    0.011 | 2385.093 | 7619.674 |      0.004 | 314.664 |
|     1 |      8 | 7695.650 |    0.011 | 2384.572 | 7620.196 |      0.004 | 314.918 |
|     1 |      9 | 7694.993 |    0.011 | 2384.055 | 7620.718 |      0.004 | 315.167 |
|     1 |     10 | 7694.345 |    0.011 | 2383.556 | 7621.234 |      0.004 | 315.401 |

Output Training Metrics for LASSO

The plot below shows the RMSE by Regularization Parameter (`lambda`). It
is easy to see that RMSE is minimized when `lambda` = 10.

``` r
plot(LASSO)
```

![](tech_files/figure-gfm/LASSOplot-1.png)<!-- -->

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
    ## 5.020706e+03 2.780156e-02 2.301849e+03

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
    ## -14211  -1828  -1025    136 654984 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               3083.84     142.23  21.682  < 2e-16 ***
    ## LDA_00                     328.63     144.19   2.279 0.022702 *  
    ## LDA_01                      20.73     145.75   0.142 0.886916    
    ## LDA_02                      11.57     148.64   0.078 0.937973    
    ## LDA_03                     -23.45     145.81  -0.161 0.872256    
    ## LDA_04                         NA         NA      NA       NA    
    ## average_token_length       -53.73     157.50  -0.341 0.733017    
    ## is_weekend                 153.01     143.87   1.064 0.287600    
    ## n_tokens_content           713.38     203.78   3.501 0.000468 ***
    ## n_non_stop_unique_tokens   -83.36     197.81  -0.421 0.673481    
    ## num_hrefs                  478.82     198.85   2.408 0.016075 *  
    ## num_self_hrefs            -745.43     180.81  -4.123  3.8e-05 ***
    ## num_videos                 181.33     144.43   1.255 0.209361    
    ## kw_avg_min                -351.38     163.85  -2.144 0.032043 *  
    ## kw_avg_max                -389.03     169.99  -2.289 0.022148 *  
    ## kw_avg_avg                 605.54     172.24   3.516 0.000442 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10200 on 5130 degrees of freedom
    ## Multiple R-squared:  0.0137, Adjusted R-squared:  0.01101 
    ## F-statistic: 5.091 on 14 and 5130 DF,  p-value: 1.341e-09

(add some thoughts here)

``` r
lm_out = data.frame(lmFit$results)

kable(lm_out, caption = "Output Training Metrics for Linear Regression",
      digits = 3)
```

| intercept |     RMSE | Rsquared |      MAE |   RMSESD | RsquaredSD |   MAESD |
|:----------|---------:|---------:|---------:|---------:|-----------:|--------:|
| TRUE      | 7890.096 |    0.014 | 2379.451 | 7311.616 |      0.005 | 219.246 |

Output Training Metrics for Linear Regression

``` r
metric_lm = postResample(pred = predict(lmFit, newdata = dfTest), 
                         obs = dfTest$shares)

metric_lm
```

    ##         RMSE     Rsquared          MAE 
    ## 5.022856e+03 2.789813e-02 2.305239e+03

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

![](tech_files/figure-gfm/RFplot-1.png)<!-- -->

(add some thoughts here)

``` r
rf_out = data.frame(rfFit$results)

kable(rf_out, caption = "Output Training Metrics for Random Forest",
      digits = 3)
```

| mtry |     RMSE | Rsquared |      MAE |   RMSESD | RsquaredSD |   MAESD |
|-----:|---------:|---------:|---------:|---------:|-----------:|--------:|
|    1 | 7721.228 |    0.020 | 2361.521 | 7515.532 |      0.012 | 225.146 |
|    2 | 7837.004 |    0.019 | 2420.412 | 7445.496 |      0.015 | 215.576 |
|    3 | 7939.945 |    0.019 | 2456.664 | 7382.846 |      0.015 | 205.319 |
|    4 | 8034.132 |    0.018 | 2487.590 | 7334.475 |      0.015 | 201.763 |
|    5 | 8106.802 |    0.017 | 2508.357 | 7302.253 |      0.014 | 199.586 |

Output Training Metrics for Random Forest

(add some thoughts here)

``` r
RF_pred <- predict(rfFit, newdata = activeTest)

metric_rf = postResample(RF_pred, activeTest$shares)

metric_rf
```

    ##         RMSE     Rsquared          MAE 
    ## 4.997559e+03 3.310394e-02 2.281586e+03

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

|     | shrinkage | interaction.depth | n.minobsinnode | n.trees |     RMSE | Rsquared |      MAE |   RMSESD | RsquaredSD |   MAESD |
|:----|----------:|------------------:|---------------:|--------:|---------:|---------:|---------:|---------:|-----------:|--------:|
| 1   |       0.1 |                 1 |             10 |      25 | 7956.357 |    0.002 | 2425.226 | 7423.697 |      0.002 | 259.564 |
| 9   |       0.1 |                 2 |             10 |      25 | 7903.066 |    0.004 | 2404.118 | 7466.232 |      0.004 | 278.320 |
| 17  |       0.1 |                 3 |             10 |      25 | 7781.121 |    0.009 | 2367.006 | 7540.197 |      0.009 | 308.609 |
| 25  |       0.1 |                 4 |             10 |      25 | 7889.387 |    0.007 | 2398.751 | 7458.385 |      0.004 | 292.259 |
| 2   |       0.1 |                 1 |             10 |      50 | 8115.858 |    0.002 | 2465.009 | 7345.725 |      0.002 | 251.112 |
| 10  |       0.1 |                 2 |             10 |      50 | 7922.699 |    0.004 | 2384.219 | 7445.965 |      0.005 | 281.575 |
| 18  |       0.1 |                 3 |             10 |      50 | 7842.065 |    0.009 | 2354.877 | 7502.027 |      0.008 | 298.730 |
| 26  |       0.1 |                 4 |             10 |      50 | 8016.149 |    0.009 | 2412.625 | 7356.704 |      0.006 | 285.951 |
| 3   |       0.1 |                 1 |             10 |      75 | 8140.139 |    0.002 | 2471.242 | 7328.453 |      0.002 | 241.395 |
| 11  |       0.1 |                 2 |             10 |      75 | 8078.490 |    0.003 | 2425.173 | 7361.139 |      0.004 | 260.212 |
| 19  |       0.1 |                 3 |             10 |      75 | 8049.652 |    0.006 | 2408.768 | 7387.756 |      0.005 | 284.380 |
| 27  |       0.1 |                 4 |             10 |      75 | 8127.909 |    0.009 | 2423.260 | 7284.274 |      0.006 | 265.491 |
| 4   |       0.1 |                 1 |             10 |     100 | 8093.321 |    0.002 | 2453.474 | 7347.791 |      0.002 | 246.464 |
| 12  |       0.1 |                 2 |             10 |     100 | 8151.695 |    0.003 | 2409.053 | 7321.957 |      0.004 | 265.141 |
| 20  |       0.1 |                 3 |             10 |     100 | 8200.849 |    0.005 | 2407.076 | 7296.466 |      0.006 | 275.155 |
| 28  |       0.1 |                 4 |             10 |     100 | 8208.934 |    0.009 | 2412.124 | 7256.674 |      0.005 | 284.935 |
| 5   |       0.1 |                 1 |             10 |     125 | 8137.816 |    0.002 | 2472.195 | 7328.578 |      0.002 | 243.306 |
| 13  |       0.1 |                 2 |             10 |     125 | 8173.975 |    0.003 | 2388.696 | 7308.672 |      0.004 | 271.387 |
| 21  |       0.1 |                 3 |             10 |     125 | 8303.653 |    0.005 | 2419.833 | 7233.908 |      0.005 | 274.831 |
| 29  |       0.1 |                 4 |             10 |     125 | 8348.632 |    0.008 | 2437.226 | 7194.273 |      0.004 | 278.918 |
| 6   |       0.1 |                 1 |             10 |     150 | 8165.638 |    0.002 | 2464.797 | 7315.437 |      0.002 | 238.736 |
| 14  |       0.1 |                 2 |             10 |     150 | 8440.137 |    0.003 | 2456.488 | 7176.236 |      0.004 | 248.873 |
| 22  |       0.1 |                 3 |             10 |     150 | 8381.909 |    0.005 | 2438.718 | 7196.526 |      0.004 | 267.755 |
| 30  |       0.1 |                 4 |             10 |     150 | 8494.328 |    0.007 | 2452.447 | 7108.918 |      0.004 | 278.287 |
| 7   |       0.1 |                 1 |             10 |     175 | 8180.005 |    0.002 | 2467.862 | 7304.202 |      0.002 | 234.835 |
| 15  |       0.1 |                 2 |             10 |     175 | 8522.373 |    0.003 | 2452.516 | 7127.556 |      0.003 | 253.328 |
| 23  |       0.1 |                 3 |             10 |     175 | 8508.550 |    0.005 | 2440.627 | 7113.122 |      0.004 | 269.052 |
| 31  |       0.1 |                 4 |             10 |     175 | 8606.834 |    0.007 | 2480.048 | 7030.530 |      0.005 | 258.990 |
| 8   |       0.1 |                 1 |             10 |     200 | 8171.212 |    0.002 | 2453.452 | 7304.630 |      0.002 | 239.592 |
| 16  |       0.1 |                 2 |             10 |     200 | 8681.192 |    0.003 | 2467.318 | 7027.770 |      0.002 | 233.629 |
| 24  |       0.1 |                 3 |             10 |     200 | 8706.250 |    0.006 | 2486.308 | 6978.420 |      0.005 | 260.256 |
| 32  |       0.1 |                 4 |             10 |     200 | 8655.479 |    0.007 | 2477.656 | 7008.296 |      0.005 | 268.740 |

Output Training Metrics for Boosting

The plot below shows the RMSE by Number of Boosting Iterations and
display Max Tree Depth lines for the 5-fold CV performed. It is easy to
see that RMSE is minimized when n.trees = 25 and interaction.depth = 3.

``` r
plot(gbmFit)
```

![](tech_files/figure-gfm/boostingPlot-1.png)<!-- -->

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
    ## 5058.7387897    0.0140672 2260.9025186

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
| RMSE     | 4997.559 | 5058.739 | 5020.706 | 5022.856 |
| Rsquared |    0.033 |    0.014 |    0.028 |    0.028 |
| MAE      | 2281.586 | 2260.903 | 2301.849 | 2305.239 |

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
