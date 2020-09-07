# obtain TB incidence (per 100k) and TB number of cases by HIV status

# call source file
source("000_source-functions.R")

######################### Data Processing #########################
######### WHO incidence data + WorldBank population data ##########

# select country, total TB incidence, and HIV+ TB incidence by year
master <- master %>% select(country, year, e_inc_100k, e_inc_tbhiv_100k) 
# change column names for clarity
master <- master %>% dplyr::rename(total_inc = e_inc_100k, hiv_inc = e_inc_tbhiv_100k)

# master.hiv with only the 15 countries of interest
master.hiv <- master %>% filter(country %in% hiv_15)

# population data to be used for calculating number of cases
# pop.hiv with only the 15 countries of interest
pop.hiv <- pop %>% filter(country %in% hiv_15)

## check that there are 15 countries
## stopifnot will error if the expression is not true
stopifnot(length(unique(master.hiv$country)) == 15)
stopifnot(length(unique(pop.hiv$country)) == 15)


######################### TB INCIDENCE #########################
################ columns A-D in toy file from PA #################

# display df for an individual country only through 2035
get_one_country_df <- function(country_name){
  
  df_onecountry <- master.hiv %>% filter(country == country_name) %>% select(year, total_inc, hiv_inc)
  
  # add column for HIV negative (nohiv) TB incidence
  df_onecountry <- df_onecountry %>% mutate(nohiv_inc = total_inc - hiv_inc)
  
  # returns df with 4 columns (year, total TB, HIV+ TB, HIV- TB) and 36 rows (2000-2035)
  return(df_onecountry)
}

# display tb incidence from 2000 to 2035 by hiv status
# from 2000 to 2018 is WHO data; from 2019 to 2035 is predicted data
# predicted data is calculated from fit from year_start to 2018
predict_inc <- function(country_name){
  
  # get df of just that country (2000-2018 WHO reported data)
  df_actual <- get_one_country_df(country_name)
  
  # extract start year for projection, same year as for overall projection (in 002)
  year_start <- years[country_name]
  
  
  # fit models from year_start to 2018
  # row is the row number corresponding to year start
  row <- as.numeric(rownames(df_actual[df_actual$year == year_start,]))
  # end is the row number corresponding to year 2018
  end <- 19
  
  # for total TB incidence
  fit_total <- lm(total_inc ~ year, data = df_actual[row:end,])
  
  # for HIV+ TB incidence
  fit_hiv <- lm(hiv_inc ~ year, data = df_actual[row:end,])
  
  # for HIV- (nohiv) TB incidence
  fit_nohiv <- lm(nohiv_inc ~ year, data = df_actual[row:end,])
  
  
  # create empty df with same columns from years 2019 to 2035
  df_preds <- data.frame(year = 2019:2035, total_inc = 0, hiv_inc = 0, nohiv_inc = 0)
  
  # populate df_preds from 2019 to 2035
  df_preds$total_inc <- as.numeric(predict(fit_total, df_preds, type = "response"))
  df_preds$hiv_inc <- as.numeric(predict(fit_hiv, df_preds, type = "response"))
  df_preds$nohiv_inc <- as.numeric(predict(fit_nohiv, df_preds, type = "response"))
  
  # minimum incidence set to 10 per 100,000
  # replace all predicted values less than 10 with 10
  df_preds[] <- lapply(df_preds, function(x) ifelse(x<10, 10, x))
  
  
  # bind df_actual (2000-2018) together with df_preds (2019-2035)
  df_incidence <- rbind(df_actual, df_preds)
  
  return(df_incidence)
}

### validate to make sure predict_by_hiv numbers match with 002 YES!
### angola <- predict_inc("Angola")
### angola[nrow(angola), ] # returns 284, consistent with Table 2

### malawi <- predict_by_hiv("Malawi")
### malawi[nrow(malawi), ] # returns 48, consistent with Table 2


######################### TB NUMBER OF CASES #########################
################### columns E-G in toy file from PA ####################


# now that we know incidence of TB by HIV status 2000-2035, 
# we want the actual NUMBER OF TB CASES by HIV status 2000-2035.
# to do this, use population data
predict_numb <- function(country_name){
  
  # get incidence predictions of that country from predict_inc, 2000-2035
  inc_country <- predict_inc(country_name)
  
  # get population of that country, 2000-2035 (worldbank data)
  pop_country <- pop.hiv %>% filter(country == country_name) %>% select(year, population)
  
  # create new empty df 2000-2035
  df_number <- data.frame(year = 2000:2035, total_numb = 0, hiv_numb = 0, nohiv_numb = 0)
  
  df_number <- df_number %>%
    mutate(
      
      total_numb = as.numeric(inc_country$total_inc/100000)*as.numeric(pop_country$population),
      hiv_numb = as.numeric(inc_country$hiv_inc/100000)*as.numeric(pop_country$population),
      nohiv_numb = as.numeric(inc_country$nohiv_inc/100000)*as.numeric(pop_country$population)
      
    ) %>% 
    # don't want two columns for "year" in final, so select all but year
    select(total_numb, hiv_numb, nohiv_numb)
  
  # tried using helper function instead (below)
  # df_number <- df_number %>% 
  #   mutate(
  #     total_numb = get_numb_from_inc(inc_country, pop_country, total_inc)
  #   )
  
  return(df_number)
  
}

# tried doing helper function to embed into predict_numb but not working
# get_numb_from_inc <- function(inc_df, pop_df, inc_hiv_status){
#   numb_hiv_status = as.numeric(inc_df$inc_hiv_status/100000)*as.numeric(pop_df$population)
#   return(numb_hiv_status)
# }


################### COMBINE INCIDENCE AND NUMBER, 2000-2035 ###################
####################### ALL COLUMNS in toy file from PA #######################


# now, combine incidence and number into one table
# this generates ONE table that looks like the toy example sent by PA
table_main <- function(country_name){
  inc <- predict_inc(country_name)
  numb <- predict_numb(country_name)
  country_df <- cbind(inc, numb)
  country_df <- country_df %>% add_column(country_name = country_name, .before = 0)
  return(country_df)
}

# depending on which countries we decide to do this for:
# below code applies table_main to all 15 countries
# (may need to do one-by-one to keep track of which country is associated with which)
# the order is in 000, and also below:
# hiv_15 <- c("Angola", 
#             "Botswana", 
#             "Cameroon", 
#             "Congo", 
#             "Eswatini", 
#             "Kenya", 
#             "Lesotho", 
#             "Malawi", 
#             "Namibia", 
#             "Sierra Leone", 
#             "South Africa", 
#             "Uganda", 
#             "United Republic of Tanzania", 
#             "Zambia", 
#             "Zimbabwe")
lapply(hiv_15, table_main)

# just for Angola
table_main("Angola")
