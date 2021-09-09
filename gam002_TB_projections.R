# 2021-04-20 JC

source("gam000_source-functions.R")

# predict tb incidence to 2035
predict_tb_inc <- function(fit){
  
  # insert predicted values to df using gam, given gam fit as input
  # Jaeyoon - why was the increment 0.01?
  predicted_tb_inc <- data.frame(year = seq(2000, 2035, 1), predict_value = 0)
  predicted_tb_inc$predict_value <- as.numeric(predict.gam(fit, predicted_tb_inc, year=predicted_tb_inc$year))
  
  # minimum incidence set to 10 per 100,000
  # replace all predicted values less than 10 with 10
  predicted_tb_inc[] <- lapply(predicted_tb_inc, function(x) ifelse(x<10, 10, x))
  return(predicted_tb_inc)
}

# model the target decline with End TB goal with benchmarks to 2035
calc_target <- function(df_country, one_country){
  # extract reported incidence value for 2015 as the baseline
  num_2015 <- as.integer(df_country$e_inc_100k[one_country$year == 2015])
  # calculate benchmark targets based on the 2015 value through 2035
  # according to the WHO End TB goals
  df_target <- data.frame("year" = c(2015, 2020, 2025, 2030, 2035), 
                          "num" = c(num_2015, 0.80*num_2015, 
                                    0.50*num_2015, 0.20*num_2015, 0.10*num_2015))
  
  # minimum incidence set to 10 per 100,000
  df_target[] <- lapply(df_target, function(x) ifelse(x<10, 10, x))
  
  # polynomial regression for target decline
  fit.target <- lm(num ~ poly(year, 5, raw = TRUE), data = df_target)
  o <- list(df_target = df_target, fit.target = fit.target)
  return(o)
}

get_one_country_df <- function(df_country){
  to_2035 <- data.frame(year=2020:2035, e_inc_100k= replicate(16, "NA"))
  # need to do this as rbind with e_inc_100k_lo, e_inc_100k_hi does not work
  tmp_df_country <- df_country %>% select(year, e_inc_100k)
  one_country <- rbind(tmp_df_country, to_2035)
  return(one_country)
}

# will need to change CI
calc_ci_ratios <- function(df_country){
  ## add new columns with the ratio of the estimate to the high and low bounds
  df_country_ci <- df_country %>% mutate(lb_ratio = e_inc_100k_lo / e_inc_100k) %>% mutate(hb_ratio = e_inc_100k_hi / e_inc_100k)
  ## take averge of the last 5 years to obtain ratios 
  lo <- mean(tail(df_country_ci$lb_ratio, 5))
  hi <- mean(tail(df_country_ci$hb_ratio, 5))
  ## df with low and high bound ratios
  ci_ratios <- data.frame("lo" = lo, "hi" = hi)
  return(ci_ratios)
}

# will need to change CI
add_ci_to_predicted <- function(predicted_tb_inc, df_country){
  ci_ratios <- calc_ci_ratios(df_country)
  low_bound_ratio <- as.numeric(ci_ratios$lo)
  high_bound_ratio <- as.numeric(ci_ratios$hi)
  ## only want values for the period for which we rely on projections i.e. > 2019
  ## so, filter, add the projected CIs
  tmp_predicted_tb_inc <- predicted_tb_inc %>% filter(year > 2019)
  tmp_predicted_tb_inc <- tmp_predicted_tb_inc %>% mutate(proj_ci_lo = predict_value * low_bound_ratio) %>% mutate(proj_ci_hi = predict_value * high_bound_ratio)
  
  ## then combine back with predicted_tb_inc using a join
  predicted_tb_inc <- left_join(predicted_tb_inc, tmp_predicted_tb_inc)
  
  return(predicted_tb_inc)
}

# project TB incidence through 2035
### keep knots default, and instead of "bs = re," do cubic splines "bs = cr"
model_main <- function(country_name){
  
  # throw warning if input country is not one of the 40 of interest
  throwWarning(country_name)
  
  # obtain data for just that country: year, TB incidence, low bound, high bound
  df_country <- master %>% filter(country == country_name) %>% 
    select(year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi)
  
  # !!! fit gam, cr, default knots
  fit <- gam(e_inc_100k ~ s(year), data = df_country, bs="cr")
  
  # predict tb inc to 2035 based on gam
  predicted_tb_inc <- predict_tb_inc(fit)
  
  # will need to change CI
  ## add a new function which adds error bars on prediction
  ## it needs to take in 1) predicted_tb_inc 2) tb_inc and confidence intervals
  ## for the last 5 years, get the ratio of the estimate to the high and the low CI, take the average of this over 5 years
  ## then, make new columns, predicting the bounds based on the average.
  predicted_tb_inc <- add_ci_to_predicted(predicted_tb_inc, df_country)
  ## get df for one country
  one_country <- get_one_country_df(df_country)
  
  ## calculate targets
  target_output  <- calc_target(df_country, one_country)
  df_target <- target_output$df_target
  print(country_name)
  print(df_target)
  fit.target <- target_output$fit.target
  
  # add predicted incidence based on gam through year 2035
  pred1 <- one_country %>% 
    mutate(pred_num_100k = predict(fit, newdata = one_country, type = "response"))
  pred1[] <- lapply(pred1, function(x) ifelse(x<10, 10, x))
  
  # add target TB incidence for each year
  # keep in mind: only relevant for 2015 and onwards
  pred2 <- pred1 %>% 
    mutate(target_100k = predict(fit.target, pred1))
  
  # population
  pop_country <- pop %>% filter(country == country_name)
  
  pred3 <- pred2 %>% 
    mutate(pop = pop_country$population)
  
  # calculate the number of cases each year under real data model
  # this is the per 100,000 rate multiplied by the population
  pred_num_cases_df <- pred3 %>% mutate(pred_cases = (as.numeric(pred3$pred_num_100k)/100000)*as.numeric(pred3$pop))
  
  # do the same for the target model
  target_num_cases_df <- pred_num_cases_df %>% 
    mutate(target_num_cases_dfses = (pred_num_cases_df$target_100k/100000)*as.numeric(pred_num_cases_df$pop))
  
  target_num_cases_df <- target_num_cases_df %>% add_column(country_name = country_name, .before = 0)
  # there was a problem with target_num_cases_df$pred_num_100k being a named number datatype, so converted to normal numeric
  target_num_cases_df$pred_num_100k <- as.numeric(target_num_cases_df$pred_num_100k)
  write_tsv(target_num_cases_df, paste('target_cases', country_name))
  
  # take the difference to find number of extra cases
  with_diff_df <- target_num_cases_df %>% mutate(diff = as.numeric(target_num_cases_df$pred_cases) - as.numeric(target_num_cases_df$target_num_cases))
  
  year_diffs <- with_diff_df %>% select(year, diff) %>% filter(year > 2019)
  
  # calculate the extra number of cases from 2020 to 2035:
  extra_cases <- year_diffs %>% 
    summarize(extra_cases = as.integer(sum(diff)))
  
  ## get the number of extra cases out of the script
  write_tsv(data.frame(c(country_name, extra_cases)), paste('extra_cases', country_name))
  # return(list(trend, tot_extra_num_cases_2020_2035))
  
  # max incidence value
  max_inc_100k = max(df_country$e_inc_100k, na.rm = TRUE)
  # min incidence value
  min_inc_100k = min(df_country$e_inc_100k, na.rm = TRUE)
  # difference
  range_inc = max_inc_100k - min_inc_100k  
  country_name <- shorten_country_name(country_name)
  
  # shorten country name to fit in graph/use vernacular names
  shorten_country_name(country_name)
  
  # graph modified linear model, include the dataframe showing
  # extra number of cases in the graph
  trend <- ggplot() + geom_point(data = df_country, aes(x = year, y = e_inc_100k)) +
    geom_point(data = df_target, aes(x = year, y = num), colour = "red", size = 2) +
    geom_line(aes(x = year, y = predict_value), data = predicted_tb_inc, colour = "blue", size = 1.2) +
    geom_ribbon(data = df_country, aes(x = year, ymin=e_inc_100k_lo, ymax = e_inc_100k_hi), alpha = 0.3) +
    geom_ribbon(data = predicted_tb_inc, aes(x = year, ymin=proj_ci_lo, ymax = proj_ci_hi), fill = "blue", alpha = 0.3) +
    theme(axis.title = element_blank()) +
    ggtitle(country_name) 
  #annotation_custom(tableGrob(extra_cases, cols = NULL, rows = NULL, theme = ttheme_minimal(base_size = 8)), xmin = 2022, ymin = (max_inc_100k - (0.2*range_inc)))
  
  # retrieve confidence interval
  write_tsv(predicted_tb_inc, paste('confidence_interval', country_name))
  
  trend
  
}

# process master to contain only columns for country, year, and TB incidence values
master <- master %>% select(country, year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi)
master <- master %>% filter(country %in% all_countries)

## check -- should be 40 countries
## stopifnot will error if the expression is not true
stopifnot(length(unique(master$country)) == 40)

## axes labels are common to both
y.grob <- textGrob("Incidence per 100k people", gp = gpar(col="black", fontsize=15), rot = 90)
x.grob <- textGrob("Year", gp = gpar(fontface="bold", col="black", fontsize=15))

# model for low HIV missing target
low_hiv_meeting <- c('Cambodia', 'Ethiopia', 'Russian Federation', 'Republic of Korea')
model_main('Cambodia')

run_bootstraps <- function(country_name, df_bootstrap) {
  throwWarning(country_name)
  
  df_country <- df_bootstrap %>% filter(country == country_name) %>% filter(bootstrap == 2) %>%
    select(year, bs_inc)
  View(df_country)
  # !!! fit gam, cr, default knots
  fit <- gam(bs_inc ~ s(year), data = df_country, bs="cr")
  
  # predict tb inc to 2035 based on gam
  predicted_tb_inc <- predict_tb_inc(fit)
  View(predicted_tb_inc)
  
  trend <- ggplot() + geom_point(data = df_country, aes(x = year, y = bs_inc)) +
    geom_line(aes(x = year, y = predict_value), data = predicted_tb_inc, colour = "blue", size = 1.2) +
    
    theme(axis.title = element_blank()) +
    ggtitle(country_name) 
  trend
  
}

bootstraps <- read.csv("../../scripts_gam/datasets/bootstraps.csv")

run_bootstraps('Cambodia', bootstraps)


low_hiv_hit_target_countries_projection <- lapply(low_hiv_meeting, model_main)
hit_plots <- do.call(grid.arrange, low_hiv_hit_target_countries_projection)
plot_hit <- plot_grid(hit_plots, vjust = 1, scale = 1, ncol = 1, align = 'v', axis = 't')
# create tiff file figure 3
tiff(filename = "Fig3_002_noHIV_hit_target.tiff", width = 6.75, height = 8, units = "in", res = 300)
grid.arrange(arrangeGrob(plot_hit, left = y.grob, bottom = x.grob))
dev.off()


## model for low HIV missing target
low_hiv_missing <- c('Chad', 'China', 'Democratic Republic of the Congo', 
                     'Ghana', 'Guinea-Bissau', 'India', 'Indonesia', 'Laos', 'Liberia', 
                     'Mozambique', 'Myanmar',  'Pakistan', 'Philippines', 'Thailand', 'Vietnam')
low_hiv_miss_target_countries_projection <- lapply(low_hiv_missing, model_main)
miss_plots <- do.call(grid.arrange, low_hiv_miss_target_countries_projection)
plot_miss <- plot_grid(miss_plots, vjust = 1, scale = 1, ncol = 1, align = 'v', axis = 't')

# create tiff file for figure 4
tiff(filename = "Fig4_002_noHIV_miss_target.tiff", width = 6.75*1.5, height = 8.75*1.5, units = "in", res = 300)
grid.arrange(arrangeGrob(plot_miss, left = y.grob, bottom = x.grob))
dev.off()


######## make new supplementary figure of the six excluded
low_hiv_exclude <- c('Bangladesh', 'Brazil', 'Central African Republic', 
                     'Nigeria',"Democratic People's Republic of Korea",
                     'Papua New Guinea')
low_hiv_exclude_target_countries_projection <- lapply(low_hiv_exclude, model_main)
exclude_plots <- do.call(grid.arrange, low_hiv_exclude_target_countries_projection)
plot_exclude <- plot_grid(exclude_plots, vjust = 1, scale = 1, ncol = 1, align = 'v', axis = 't')

# create new supplementary figure of the six excluded
tiff(filename = "FigSupp_002_noHIV_6EXCLUDED.tiff", width = 6.75*1.5, height = 8.75*1.5, units = "in", res = 300)
grid.arrange(arrangeGrob(plot_exclude, left = y.grob, bottom = x.grob))
dev.off()





## for all countries
all_countries_projection <- lapply(all_countries, model_main)
all_plots <- do.call(grid.arrange, all_countries_projection)
plot_all <- plot_grid(all_plots, vjust = 1, scale = 1, ncol = 1, align = 'v', axis = 't')
# output
tiff(filename = "gam_ALLprojections.tiff", width = 6.75*1.5, height = 8.75*1.5, units = "in", res = 300)
grid.arrange(arrangeGrob(plot_all, left = y.grob, bottom = x.grob))
dev.off()

