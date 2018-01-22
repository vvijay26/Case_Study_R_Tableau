# Case_Study_R_Tableau
Data cleaning, analysis and inferences using data from crunchbase.com. Tools used - R, Tableau and Excel(for data verification).

## Getting Started
The 2 files that have been extracted from crunchbase.com are :- rounds2.csv and companies.txt

### Prerequisites

R/RStudio/Tableau and Excel

#### Objective
The objective of this case study is to analyze funding data of startups from crunchbase.com using R, Tableau and Excel and come up with an investment strategy for a hypothetical Asset management company. The main constraints of the investment company are -
1.	It wants to invest between 5 to 15 million USD per round of investment
2.	It wants to invest only in English-speaking countries because of the ease of communication with the companies it would invest in. English speaking means English is one of the official languages in that country
3. It wants to invest where other companies are investing. This pattern is often observed among startup investors.

#### Data cleaning

The first step is to clean the raw data. The permalink column identifies a company uniquely in companies.txt and rounds2.csv. The first step is to remove the working environment variables and set a working directory and load the 2 files into 2 R dataframes. As illustrated below (complete R code is available in Case_study_scripts.R)

```
rm(list=ls())
setwd("C:/<Project location>")
```



