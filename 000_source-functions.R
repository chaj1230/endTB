library(dplyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(ggfortify)
library(reshape)
library(tidyverse)
library(knitr)
library(tibble)
library(ggpmisc)
library(tidyr)

### dataframes from 2000 to 2018 by country

# master: all existing WHO data (includes TB incidence per 100,000 people)
# master used in 001_TB_incidence_trends and 002_TB_projections
master <- read.csv("who_ALL.csv")

# pop: total population
# process population for each of the 40 countries for years 2000-2035
# store in dataframe pop_df
# pop used in 002_TB_projections
pop <- read.csv("population_ALL.csv")
keycol <- 'year'
valuecol <- 'population'
gathercols <- colnames(pop)[2:37]
pop <- gather_(pop, keycol, valuecol, gathercols)
# get rid of the "X" (change X2001 to 2001)
pop$year <- as.numeric(substr(pop$year, 2, 5))

# arv: HIV antiretroviral coverage (in %)
# arv used in 003_HIV_TB
master.arv <- read.csv("hiv_arv_coverage.csv")
# tidy format
keycol <- 'year'
valuecol <- 'hiv_coverage'
gathercols <- colnames(master.arv)[2:20]
arv <- gather_(master.arv, keycol, valuecol, gathercols)
# get rid of the "X" (change X2001 to 2001)
arv$year <- as.numeric(substr(arv$year, 2, 5))

# prev: HIV prevalence (in %)
# prev used in 003_HIV_TB
master.prev <- read.csv("hiv_prev_ALL.csv")
# tidy format
keycol <- 'year'
valuecol <- 'hiv_prevalence'
gathercols <- colnames(master.prev)[2:20]
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

### years: list of years used to project TB in 002_TB_projections
# each of the 40 countries has a corresponding year
years <- c(2011, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2010, 2000, 2012, 2015, 2000, 2000, 2005, 2000, 2000, 2010, 2000, 2007, 2013, 2015, 2012, 2009, 2011, 2000, 2012, 2000, 2016, 2011, 2009, 2009, 2016, 2000, 2012, 2006, 2000, 2000, 2000)
names(years) <- c("Angola", "Bangladesh", "Botswana", "Brazil", "Cambodia", "Cameroon", "Central African Republic", "Chad", "China", "Congo", "Democratic People's Republic of Korea", "Democratic Republic of the Congo", "Eswatini", "Ethiopia", "Ghana", "Guinea-Bissau", "India", "Indonesia", "Kenya", "Laos", "Lesotho", "Liberia", "Malawi", "Mozambique", "Myanmar", "Namibia", "Nigeria", "Pakistan", "Papua New Guinea", "Philippines", "Republic of Korea", "Russian Federation", "Sierra Leone", "South Africa", "Thailand", "Uganda", "United Republic of Tanzania", "Vietnam", "Zambia", "Zimbabwe")

### countries grouped by shape
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