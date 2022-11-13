Project 3
================
Kara Belknap & Cassio Monti
2022-11-5

-   <a href="#report-for-the-world-data-channel"
    id="toc-report-for-the-world-data-channel">Report for the <em>world</em>
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
    -   <a href="#summarizations-for-the-world-data-channel"
        id="toc-summarizations-for-the-world-data-channel">Summarizations for
        the <em>world</em> Data Channel</a>
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

# Report for the *world* Data Channel

This report contains Exploratory Data Analysis (EDA) about the world
data channel and a modeling section applying three regression methods
which attempt to predict trends about article sharing on the Mashable
website.

## Introduction

The objective of this analysis is to provide a comprehensive overview
about publication metrics and their relationship with the number of
shares that those publications presented during the study period. These
data have been collected from Mashable website, one of the largest news
websites from which the content of all the world channel articles
published in 2013 and 2014 was extracted. The full data description can
be found
[here](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity).
These data were originally collected and analyzed by Fernandes et
al. (2015), in which the authors performed classification task comparing
several machine learning algorithms. In the present study, a subset of
the data used by Fernandes et al.(2015) corresponding to the data
channel world is used for regression purposes. The response variable is
the number of `shares` that the papers presented after publication. In
other words, we will try to predict the number of shares that the papers
will have before publication and evaluate the prediction of each
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

## Summarizations for the *world* Data Channel

In this section, we will perform EDA for the data channel world.

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
for the number of shares for articles in the data channel world. The
`summary()` function was used to extract these metrics.

``` r
summary(activeTrain$shares)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      42     827    1100    2287    1900  284700

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
| No      |  11487592 |  2228.005 |      1100 | 6461.435 |
| Yes     |   2008315 |  2699.348 |      1500 | 4690.582 |

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

| Day       | sumShares | avgShares | medShares | sdShares | maxShares | minShares |
|:----------|----------:|----------:|----------:|---------:|----------:|----------:|
| Monday    |   2436420 |  2551.225 |      1100 | 6566.419 |    108400 |        43 |
| Tuesday   |   2358495 |  2144.086 |      1050 | 5478.894 |    115700 |        42 |
| Wednesday |   1995054 |  1825.301 |      1100 | 3024.098 |     53500 |        48 |
| Thursday  |   2683933 |  2444.383 |      1100 | 9861.884 |    284700 |        42 |
| Friday    |   2013690 |  2212.846 |      1100 | 5147.319 |    111300 |        70 |
| Saturday  |   1007706 |  2806.981 |      1500 | 5220.524 |     75500 |        43 |
| Sunday    |   1000609 |  2598.984 |      1400 | 4140.198 |     52600 |        91 |

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
    ##       955      1100      1093      1098       910       359       385

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

|     | (-243,1.42e+05\] | (1.42e+05,2.85e+05\] |
|:----|-----------------:|---------------------:|
| 0   |             5155 |                    1 |
| 1   |              744 |                    0 |

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

| Variables                | Correlation |
|:-------------------------|------------:|
| LDA_00                   |       0.026 |
| LDA_01                   |       0.030 |
| LDA_02                   |      -0.108 |
| LDA_03                   |       0.076 |
| LDA_04                   |       0.052 |
| average_token_length     |      -0.063 |
| is_weekend               |       0.025 |
| n_non_stop_unique_tokens |      -0.029 |
| num_videos               |       0.036 |
| kw_avg_avg               |       0.068 |

Top 10 Response Correlated Variables

#### Principal Components Analysis (PCA)

The variables that present most correlation with the response variable
`shares` are LDA_00, LDA_01, LDA_02, LDA_03, LDA_04,
average_token_length, is_weekend, n_non_stop_unique_tokens, num_videos,
kw_avg_avg. These variables will be studied in more depth via PCA to
understand the orientation of the most important potential predictors.
The code below presents the PCA analysis as part of the EDA. The 10 PCs
displayed in the table below correspond to the most variable combination
of the 10 predictors, which the PC1 has the most variation in the data,
PC2 presents the second most variation and so on. The coefficients
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

|                          |    PC1 |    PC2 |    PC3 |    PC4 |    PC5 |    PC6 |    PC7 |    PC8 |    PC9 |  PC10 |
|:-------------------------|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|------:|
| n_non_stop_unique_tokens | -0.258 |  0.641 | -0.138 | -0.026 | -0.003 | -0.023 |  0.064 | -0.043 |  0.705 | 0.000 |
| num_videos               |  0.145 |  0.062 | -0.394 | -0.360 | -0.092 | -0.077 | -0.769 |  0.292 | -0.008 | 0.000 |
| average_token_length     | -0.325 |  0.607 | -0.118 | -0.048 |  0.028 | -0.070 |  0.081 |  0.000 | -0.706 | 0.000 |
| kw_avg_avg               |  0.349 |  0.052 | -0.324 | -0.050 |  0.206 | -0.040 |  0.500 |  0.688 |  0.011 | 0.000 |
| is_weekend               |  0.042 | -0.044 |  0.001 |  0.176 |  0.002 | -0.980 | -0.004 | -0.059 |  0.027 | 0.000 |
| LDA_00                   |  0.192 |  0.169 |  0.021 |  0.647 |  0.542 |  0.117 | -0.329 |  0.030 | -0.018 | 0.316 |
| LDA_01                   |  0.111 |  0.089 | -0.088 |  0.478 | -0.803 |  0.074 |  0.004 |  0.173 | -0.031 | 0.248 |
| LDA_02                   | -0.611 | -0.334 | -0.189 | -0.087 |  0.082 | -0.036 |  0.054 |  0.173 |  0.044 | 0.656 |
| LDA_03                   |  0.407 |  0.033 | -0.513 | -0.173 |  0.002 |  0.018 |  0.193 | -0.602 | -0.041 | 0.371 |
| LDA_04                   |  0.310 |  0.252 |  0.632 | -0.386 | -0.051 | -0.073 | -0.008 |  0.111 | -0.001 | 0.521 |

Principal Components for EDA

``` r
id2 = which(abs(pc_directions$PC1) %in% 
             sort(abs(pc_directions$PC1),dec= T)[1:3])
```

It is possible to see that the three most important variables in PC1 are
kw_avg_avg, LDA_02, LDA_03 from the table above. These variables are
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

![](world_files/figure-gfm/correlplot-1.png)<!-- -->

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

![](world_files/figure-gfm/biplot-1.png)<!-- -->

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

![](world_files/figure-gfm/LDAplot-1.png)<!-- -->

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

![](world_files/figure-gfm/keywordplot-1.png)<!-- -->

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

![](world_files/figure-gfm/Contentplot-1.png)<!-- -->

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

![](world_files/figure-gfm/titlewordcountGraph-1.png)<!-- -->

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

![](world_files/figure-gfm/positivewordrateGraph-1.png)<!-- -->

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

![](world_files/figure-gfm/titleSubjectivityGraph-1.png)<!-- -->

## Modeling

In this section, we will perform regression for prediction purposes for
the data channel world. All models were fitted using 5-fold
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
    ##  -7760  -1446   -803   -163 280822 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               2287.44      80.87  28.287  < 2e-16 ***
    ## LDA_00                     -60.61      91.25  -0.664 0.506556    
    ## LDA_01                      39.24      86.25   0.455 0.649200    
    ## LDA_02                    -534.85     106.56  -5.019 5.34e-07 ***
    ## LDA_03                      88.85      98.53   0.902 0.367239    
    ## LDA_04                         NA         NA      NA       NA    
    ## average_token_length      -611.09     161.21  -3.791 0.000152 ***
    ## is_weekend                 134.63      81.15   1.659 0.097153 .  
    ## n_tokens_content           -60.27     103.07  -0.585 0.558782    
    ## n_non_stop_unique_tokens   283.93     156.63   1.813 0.069916 .  
    ## num_hrefs                  262.01      94.92   2.760 0.005795 ** 
    ## num_self_hrefs              45.24      85.14   0.531 0.595175    
    ## num_videos                 120.23      83.25   1.444 0.148706    
    ## kw_avg_min                 -39.45      98.38  -0.401 0.688395    
    ## kw_avg_max                 -36.07      92.28  -0.391 0.695872    
    ## kw_avg_avg                 266.71     106.59   2.502 0.012367 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6211 on 5885 degrees of freedom
    ## Multiple R-squared:  0.02009,    Adjusted R-squared:  0.01776 
    ## F-statistic: 8.619 on 14 and 5885 DF,  p-value: < 2.2e-16

The following table shows the output training metrics for this linear
regression.

``` r
lm_out = data.frame(lmFit$results)

kable(lm_out, caption = "Output Training Metrics for Linear Regression",
      digits = 3)
```

| intercept |     RMSE | Rsquared |      MAE |  RMSESD | RsquaredSD |   MAESD |
|:----------|---------:|---------:|---------:|--------:|-----------:|--------:|
| TRUE      | 5719.998 |    0.022 | 1898.918 | 2752.46 |      0.011 | 177.292 |

Output Training Metrics for Linear Regression

The following shows the RMSE, $R^2$, and MAE values for the model as it
performed on predicting the test set.

``` r
metric_lm = postResample(pred = predict(lmFit, newdata = dfTest), 
                         obs = dfTest$shares)

metric_lm
```

    ##         RMSE     Rsquared          MAE 
    ## 5.590949e+03 2.193465e-02 1.912026e+03

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
    ## (Intercept)              2287.44186
    ## LDA_00                    -44.80817
    ## LDA_01                     33.56468
    ## LDA_02                   -531.36419
    ## LDA_03                     91.20521
    ## LDA_04                      .      
    ## average_token_length     -546.09730
    ## is_weekend                124.73387
    ## n_tokens_content          -63.14016
    ## n_non_stop_unique_tokens  221.89737
    ## num_hrefs                 239.73473
    ## num_self_hrefs             36.01440
    ## num_videos                114.63860
    ## kw_avg_min                -13.99598
    ## kw_avg_max                -12.86262
    ## kw_avg_avg                239.88462

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
|     1 |      0 | 5958.783 |    0.022 | 1899.295 | 2018.624 |      0.018 | 108.504 |
|     1 |      1 | 5958.779 |    0.022 | 1899.295 | 2018.616 |      0.018 | 108.505 |
|     1 |      2 | 5958.651 |    0.022 | 1899.078 | 2018.601 |      0.018 | 108.586 |
|     1 |      3 | 5958.511 |    0.022 | 1898.819 | 2018.626 |      0.018 | 108.694 |
|     1 |      4 | 5958.383 |    0.022 | 1898.573 | 2018.663 |      0.018 | 108.811 |
|     1 |      5 | 5958.256 |    0.022 | 1898.337 | 2018.696 |      0.018 | 108.929 |
|     1 |      6 | 5958.130 |    0.022 | 1898.114 | 2018.732 |      0.018 | 109.046 |
|     1 |      7 | 5958.010 |    0.022 | 1897.899 | 2018.764 |      0.018 | 109.164 |
|     1 |      8 | 5957.887 |    0.022 | 1897.685 | 2018.792 |      0.018 | 109.259 |
|     1 |      9 | 5957.772 |    0.022 | 1897.481 | 2018.817 |      0.018 | 109.373 |
|     1 |     10 | 5957.656 |    0.022 | 1897.286 | 2018.842 |      0.018 | 109.471 |

Output Training Metrics for LASSO

The plot below shows the RMSE by Regularization Parameter (`lambda`). It
is easy to see that RMSE is minimized when `lambda` = 10.

``` r
plot(LASSO)
```

![](world_files/figure-gfm/LASSOplot-1.png)<!-- -->

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
    ## 5592.1974953    0.0214552 1911.6124566

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

![](world_files/figure-gfm/RFplot-1.png)<!-- -->

The following table shows training metrics for the random forest model.
Again, the best model is the one that minimizes RMSE.

``` r
rf_out = data.frame(rfFit$results)

kable(rf_out, caption = "Output Training Metrics for Random Forest",
      digits = 3)
```

| mtry |     RMSE | Rsquared |      MAE |   RMSESD | RsquaredSD |  MAESD |
|-----:|---------:|---------:|---------:|---------:|-----------:|-------:|
|    1 | 5924.467 |    0.031 | 1901.671 | 2059.352 |      0.021 | 77.365 |
|    2 | 5957.473 |    0.031 | 1968.691 | 2037.479 |      0.023 | 79.222 |
|    3 | 5986.748 |    0.029 | 1993.732 | 2022.912 |      0.023 | 78.023 |
|    4 | 6037.058 |    0.023 | 2020.985 | 2000.370 |      0.018 | 77.870 |
|    5 | 6047.035 |    0.023 | 2034.542 | 2000.876 |      0.019 | 84.271 |

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
    ## 5.609240e+03 1.884229e-02 1.914800e+03

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

    ## [1] 50

``` r
gbmFit$bestTune$interaction.depth
```

    ## [1] 1

The best n.trees and interaction.depth parameters for this model are 50
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

| shrinkage | interaction.depth | n.minobsinnode | n.trees |     RMSE | Rsquared |      MAE |   RMSESD | RsquaredSD |   MAESD |
|----------:|------------------:|---------------:|--------:|---------:|---------:|---------:|---------:|-----------:|--------:|
|       0.1 |                 1 |             10 |      50 | 5922.555 |    0.020 | 1890.946 | 2144.877 |      0.012 | 141.274 |
|       0.1 |                 1 |             10 |      75 | 5926.596 |    0.021 | 1897.683 | 2129.165 |      0.010 | 137.555 |
|       0.1 |                 1 |             10 |     100 | 5929.549 |    0.021 | 1892.995 | 2123.829 |      0.011 | 134.851 |
|       0.1 |                 1 |             10 |      25 | 5930.263 |    0.017 | 1901.898 | 2139.557 |      0.009 | 115.264 |
|       0.1 |                 1 |             10 |     125 | 5937.281 |    0.021 | 1892.990 | 2119.611 |      0.011 | 139.881 |
|       0.1 |                 1 |             10 |     175 | 5941.435 |    0.021 | 1905.679 | 2117.782 |      0.011 | 137.811 |
|       0.1 |                 1 |             10 |     150 | 5942.691 |    0.021 | 1907.586 | 2121.412 |      0.011 | 146.382 |
|       0.1 |                 1 |             10 |     200 | 5952.384 |    0.021 | 1914.756 | 2114.138 |      0.011 | 141.292 |
|       0.1 |                 2 |             10 |      25 | 5988.171 |    0.008 | 1908.203 | 2135.962 |      0.005 | 150.336 |
|       0.1 |                 4 |             10 |      25 | 5995.669 |    0.011 | 1924.932 | 2100.124 |      0.006 | 128.050 |
|       0.1 |                 3 |             10 |      25 | 6017.479 |    0.008 | 1928.832 | 2111.260 |      0.006 | 140.921 |
|       0.1 |                 2 |             10 |      50 | 6033.059 |    0.008 | 1926.491 | 2099.807 |      0.006 | 136.019 |
|       0.1 |                 2 |             10 |      75 | 6041.165 |    0.011 | 1918.392 | 2093.733 |      0.007 | 131.851 |
|       0.1 |                 3 |             10 |      50 | 6041.801 |    0.010 | 1928.331 | 2060.120 |      0.008 | 117.904 |
|       0.1 |                 4 |             10 |      50 | 6053.406 |    0.011 | 1934.304 | 2061.091 |      0.008 | 144.904 |
|       0.1 |                 3 |             10 |      75 | 6062.561 |    0.014 | 1940.124 | 2032.523 |      0.012 | 135.532 |
|       0.1 |                 4 |             10 |      75 | 6074.667 |    0.012 | 1949.936 | 2037.812 |      0.009 | 130.347 |
|       0.1 |                 2 |             10 |     100 | 6075.301 |    0.009 | 1941.849 | 2069.240 |      0.006 | 145.329 |
|       0.1 |                 2 |             10 |     125 | 6083.872 |    0.011 | 1943.796 | 2037.246 |      0.008 | 134.132 |
|       0.1 |                 3 |             10 |     125 | 6085.509 |    0.015 | 1962.768 | 2034.601 |      0.011 | 131.988 |
|       0.1 |                 3 |             10 |     100 | 6091.573 |    0.013 | 1962.230 | 2023.939 |      0.011 | 129.551 |
|       0.1 |                 4 |             10 |     100 | 6098.251 |    0.012 | 1963.983 | 2011.254 |      0.010 | 124.179 |
|       0.1 |                 2 |             10 |     150 | 6101.087 |    0.010 | 1953.717 | 2029.080 |      0.007 | 133.392 |
|       0.1 |                 3 |             10 |     150 | 6119.054 |    0.013 | 1969.543 | 2039.186 |      0.010 | 137.318 |
|       0.1 |                 2 |             10 |     175 | 6127.731 |    0.010 | 1966.483 | 2026.405 |      0.007 | 149.073 |
|       0.1 |                 4 |             10 |     125 | 6137.851 |    0.012 | 1985.482 | 1995.688 |      0.010 | 133.389 |
|       0.1 |                 3 |             10 |     175 | 6150.570 |    0.011 | 1991.238 | 2029.819 |      0.009 | 144.555 |
|       0.1 |                 2 |             10 |     200 | 6161.907 |    0.011 | 1990.307 | 1993.795 |      0.008 | 152.115 |
|       0.1 |                 3 |             10 |     200 | 6165.717 |    0.011 | 1995.045 | 2020.388 |      0.008 | 149.840 |
|       0.1 |                 4 |             10 |     150 | 6170.178 |    0.010 | 2007.476 | 1976.356 |      0.010 | 134.963 |
|       0.1 |                 4 |             10 |     175 | 6191.225 |    0.011 | 2025.572 | 1957.481 |      0.010 | 125.463 |
|       0.1 |                 4 |             10 |     200 | 6222.400 |    0.010 | 2038.512 | 1946.889 |      0.009 | 137.958 |

Output Training Metrics for Boosting

The plot below shows the RMSE by Number of Boosting Iterations and
display Max Tree Depth lines for the 5-fold CV performed. It is easy to
see that RMSE is minimized when n.trees = 50 and interaction.depth = 1.

``` r
plot(gbmFit)
```

![](world_files/figure-gfm/boostingPlot-1.png)<!-- -->

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
    ## 5.615487e+03 1.539181e-02 1.890319e+03

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
| RMSE     | 5609.240 | 5615.487 | 5592.197 | 5590.949 |
| Rsquared |    0.019 |    0.015 |    0.021 |    0.022 |
| MAE      | 1914.800 | 1890.319 | 1911.612 | 1912.026 |

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
