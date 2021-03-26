library(mgcv) # gams
library(dplyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(ggfortify)
library(reshape)
library(knitr)
library(tibble)
library(ggpmisc)
library(tidyr)
library(readr)
library(glue)

### dataframes from 2000 to 2019 by country

# master: all existing WHO data (TB incidence per 100,000 people)
# master used in 001_TB_incidence_trends and 002_TB_projections
master <- read.csv("WHO-TB-2021-03-18.csv")

# pop: total population (estimated and projected from World Bank)
# process population for each of the 40 countries for years 2000-2035
# store in dataframe pop_df
# pop used in 002_TB_projections
pop <- read.csv("WB-Population-2021-03-20.csv")
keycol <- 'year'
valuecol <- 'population'
gathercols <- colnames(pop)[2:37]
pop <- gather_(pop, keycol, valuecol, gathercols)
# get rid of the "X" (change X2001 to 2001)
pop$year <- as.numeric(substr(pop$year, 2, 5))

# arv: HIV antiretroviral coverage (in %)
# arv used in 003_HIV_TB
master.arv <- read.csv("WB-ARVCoverage-upd2021-02-17.csv")
# tidy format
keycol <- 'year'
valuecol <- 'hiv_coverage'
gathercols <- colnames(master.arv)[2:21]
arv <- gather_(master.arv, keycol, valuecol, gathercols)
# get rid of the "X" (change X2001 to 2001)
arv$year <- as.numeric(substr(arv$year, 2, 5))

# prev: HIV prevalence (in %)
# prev used in 003_HIV_TB
master.prev <- read.csv("WB-HIVPrevalence-upd2021-02-17.csv")
# tidy format
keycol <- 'year'
valuecol <- 'hiv_prevalence'
gathercols <- colnames(master.prev)[2:21]
prev <- gather_(master.prev, keycol, valuecol, gathercols)
# get rid of the "X" (change X2001 to 2001)
prev$year <- as.numeric(substr(prev$year, 2, 5))

### all_countries: list of the 40 high-burden countries of interest
all_countries <- c("Angola","Bangladesh","Brazil","Botswana","Cambodia","Cameroon",
                   "Central African Republic","Chad","China","Congo",
                   "Democratic People's Republic of Korea",
                   "Democratic Republic of the Congo","Eswatini","Ethiopia","Ghana",
                   "Guinea-Bissau","India","Indonesia","Kenya",
                   "Laos","Lesotho","Liberia","Malawi",
                   "Mozambique","Myanmar","Namibia", "Nigeria", "Pakistan",
                   "Papua New Guinea","Philippines","Republic of Korea",
                   "Russian Federation","Sierra Leone","South Africa","Thailand",
                   "Uganda","United Republic of Tanzania","Vietnam", "Zambia","Zimbabwe")

### hiv_15: list of 15 countries analyzed for HIV
hiv_15 <- c("Angola", "Botswana", "Cameroon", "Congo", "Eswatini", "Kenya", "Lesotho", "Malawi", "Namibia", "Sierra Leone", "South Africa", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe")

### countries grouped by shape
### --> will need to reconsider grouping with GAM
# used in 001_TB_incidence_trends
# 14 linear decrease (Figure 1 from 001)
linear_decrease <- c("Botswana", "Cambodia", "Cameroon", "Chad", "China", "Ethiopia", "Ghana", "India", "Indonesia", "Laos", "Thailand", "Vietnam", "Zambia", "Zimbabwe")
# 4 increasing (Figure S1 from 001)
increasing <- c("Angola", "Guinea-Bissau", "Liberia", "Mozambique")
# 5 flat (Figure S2 from 001)
flat <- c("Bangladesh", "Central African Republic", "Nigeria", "Democratic People's Republic of Korea", "Papua New Guinea")
# 12 peak in the 2000s (Figure 2 from 001)
peak_in_00s <- c('Angola', 'Cameroon', 'Congo', 'Eswatini', 'Kenya', 'Lesotho', 'Malawi', 'Namibia', 'Sierra Leone', 'South Africa', 'United Republic of Tanzania', 'Zimbabwe')
# 8 "other" countries (Figures S4 and S5 from 001)
other <- c("Brazil", "Democratic Republic of the Congo", "Myanmar", "Pakistan", "Philippines", "Republic of Korea", "Russian Federation", "Uganda")

### countries grouped by whether or not they are projected to meet targets
### --> will need to modify with GAM
# will meet
projected_to_meet_target <- c("Botswana", "Eswatini", "Ethiopia", "Kenya", "Laos", "Lesotho", "Myanmar", "Namibia", "Republic of Korea", "Russian Federation", "South Africa", "United Republic of Tanzania", "Zambia", "Zimbabwe")

# wont meet
projected_to_miss_target <- c("Angola", "Bangladesh", "Brazil", "Cambodia", "Cameroon", "Central African Republic", "Chad", "China", "Congo", "Democratic People's Republic of Korea", "Democratic Republic of the Congo", "Ghana", "Guinea-Bissau", "India", "Indonesia", "Liberia", "Malawi", "Mozambique", "Nigeria", "Pakistan", "Papua New Guinea", "Philippines", "Sierra Leone", "Thailand", "Uganda", "Vietnam")

### throw warning if input country is not in the 40 countries of interest 
throwWarning <- function(country_name)
{
  if (country_name %in% all_countries == FALSE) warning('The input country is not among the 40 countries of interest')
}

### shorten country name to fit in graph; use vernacular names
# DRC, Central African Republic, Tanzania, Russia, South Korea, North Korea
shorten_country_name <- function(country_name){
  if (country_name == "Democratic Republic of the Congo"){
    country_name = "DR Congo"
  }
  if (country_name == "Central African Republic"){
    country_name = "Central Af. Republic"
  }
  if (country_name == "United Republic of Tanzania"){
    country_name = "Tanzania"
  }
  if (country_name == "Russian Federation"){
    country_name = "Russia"
  }
  if (country_name == "Republic of Korea"){
    country_name = "South Korea"
  }
  if (country_name == "Democratic People's Republic of Korea"){
    country_name = "North Korea"
  }
  return(country_name)
}
