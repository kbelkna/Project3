# Project 3
================    
  Kara Belknap & Cassio Monti   
  2022-11-11

## Purpose of the Repository

The purpose of this repository is to provide EDA and modeling for different data channels obtained between 2013 and 2014 in Marshable website. The goal is to perform prediction of the number of shares that the papers presented after publication by using some variables obtained before publication. In other words, the idea is to predict the number of shares a publication may have before it is published.

## List of Packages

The packages listed in this section are used throughout the analysis. `Tidyverse` is used for data management and plotting through `dplyr` and `ggplot` packages. `Caret` and `gbm` packages are used for data splitting and modeling. `Knitr` package is used for nice printing of tables. `GGally` is used for nice correlation and exploratory plots assisting in the visualization.

```{r}
library(tidyverse)
library(caret)
library(gbm)
library(knitr)
library(GGally)
```
## Links to the Analysis for Data Channels

In this section you can access the reports generated for each data channel considered in this analysis.

The analysis for [Lifestyle articles is available here](lifestyle.html).

The analysis for [Entertainment articles is available here](entertainment.html).

The analysis for [Business articles is available here](bus.html).

The analysis for [SocMed articles is available here](socmed.html).

The analysis for [Tech articles is available here](tech.html).

The analysis for [World articles is available here](world.html).

## Code to Run Analysis for All Data Channels

This section shows the R code used to run all analysis listed above.

```{r}
rmarkdown::render("Belknap_Monti_project3_ST558.Rmd", output_file = "lifestyle.md", params = list(channel = "lifestyle"))

rmarkdown::render("Belknap_Monti_project3_ST558.Rmd", output_file = "entertainment.md", params = list(channel = "entertainment"))

rmarkdown::render("Belknap_Monti_project3_ST558.Rmd", output_file = "bus.md", params = list(channel = "bus"))

rmarkdown::render("Belknap_Monti_project3_ST558.Rmd", output_file = "socmed.md", params = list(channel = "socmed"))

rmarkdown::render("Belknap_Monti_project3_ST558.Rmd", output_file = "tech.md", params = list(channel = "tech"))

rmarkdown::render("Belknap_Monti_project3_ST558.Rmd", output_file = "world.md", params = list(channel = "world"))
```



