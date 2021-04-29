source("gam000_source-functions.R")

# process master to contain only columns for country, year, and TB incidence values
master <- master %>% select(country, year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi)

## get_r2: calculate the r2
## output a one-row tibble with columns country name and r2
get_r2 <- function(country_name){
  
  # throw error if country_name not in master
  throwWarning(country_name)
  
  # fit gam to 2000-2019 available data
  one_country <- master %>% filter(country == country_name)
  gam_model <- gam(e_inc_100k ~ s(year), data = df_country, bs="cr")
  
  ## tryCatch: run summary on the linear model
  ## and if summary returns a warning about the essentially perfect fit so summary
  ## unreliable, then r_sq is NaN

    r_sq <- tryCatch(
    {
      r_sq <- summary(gam_model)$r.sq
    },
    warning = function(w){
      return(NaN)
    }
  )
  
  # no coeff anymore with GAM
  # coefficient <- tryCatch(
  #   {
  #     coef <- summary(gam_model)$p.coeff[2]
  #   },
  #   warning = function(w){
  #     return(NaN)
  #   }
  # )
  
  # shorten country name to fit in graph/use vernacular names
  country_name <- shorten_country_name(country_name)
  
  o <- tibble(country = country_name, r2 = r_sq)
  return(o)
}

plot_trend <- function(country_name, r2_res){
  one_country <- master %>% filter(country == country_name) %>% select(year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi) 
  # edit country name to be the vernacular name/shorter form
  country_name <- shorten_country_name(country_name)
  
  ## extract the r2 for that country from the r2_res tibble
  r_sq <- r2_res %>% filter(country == country_name)
  r_sq <- format(round(r_sq$r2, 2), nsmall = 2)
  my.formula <- y ~ x
  
  # plot incidence, as is, 2000-2019
  g <- ggplot(data = one_country, mapping = aes(x = year, y = e_inc_100k)) + 
    # no longer: geom_smooth(method = "lm", se = TRUE, formula = my.formula, fill = "blue") +
    geom_point(color='black') +
    geom_line(color='blue', data = one_country, aes(x=year, y=e_inc_100k)) +
    annotate("text", label = paste("R-squared = ", r_sq), x = Inf, y = Inf, vjust = 1, hjust = 1) +
    geom_ribbon(data = one_country, aes(x = year, ymin=e_inc_100k_lo, ymax = e_inc_100k_hi), alpha = 0.3) +
    ggtitle(country_name) +
    #xlab("Year") + ylab("Incidence \nper 100k people")
    theme(axis.title = element_blank()) +
    ylim(0, NA)
  g 
}

# plot without confidence interval band for visualization
plot_trend_no_ci <- function(country_name, r2_res){
  one_country <- master %>% filter(country == country_name) %>% select(year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi) 
  # edit country name to be the vernacular name/shorter form
  country_name <- shorten_country_name(country_name)
  
  ## extract the r2 for that country from the r2_res tibble
  r_sq <- r2_res %>% filter(country == country_name)
  r_sq <- format(round(r_sq$r2, 2), nsmall = 2)
  my.formula <- y ~ x
  
  g <- ggplot(data = one_country, mapping = aes(x = year, y = e_inc_100k)) + 
    geom_point(color='black') +
    geom_line(color='blue', data = one_country, aes(x=year, y = e_inc_100k)) +
    annotate("text", label = paste("R-squared = ", r_sq), x = Inf, y = Inf, vjust = 1, hjust = 1) +
    ggtitle(country_name) +
    #xlab("Year") + ylab("Incidence \nper 100k people")
    theme(axis.title = element_blank())
  g 
}

## here,run get_r2_and_coef on everything in all_countries
## this outputs a list of tibbles, which we then merge with bind_rows
r2_res <- bind_rows(lapply(all_countries, get_r2))

## kable from knitr library
kable(r2_res)
write_tsv(r2_res, '2021.03.gams_countries_r2.tsv')

## common axis titles
y.grob <- textGrob("Incidence per 100k people", gp = gpar(col="black", fontsize=15), rot = 90)
x.grob <- textGrob("Year", gp = gpar(fontface="bold", col="black", fontsize=15))

## all 
all_plots <- lapply(all_countries, plot_trend, r2_res = r2_res)
all_plots_g <- do.call(grid.arrange, all_plots)
plot <- plot_grid(all_plots_g, vjust = 1, scale = 1, ncol = 1, align = 'v', axis = 't')
grid.arrange(arrangeGrob(plot, left = y.grob, bottom = x.grob))

###### generating figures

# may wish to change figure classifications based on WHO incidence estimate methods
# Linear decrease
linear_decrease_plots <- lapply(linear_decrease, plot_trend, r2_res = r2_res)
linear_decrease_plots_g <- do.call(grid.arrange, linear_decrease_plots)
linear_decrease_plot <- plot_grid(linear_decrease_plots_g, vjust = 1, scale = 1, ncol = 1, align = 'v', axis = 't')
# create tiff file figure 1
tiff(filename = "Fig1_001_linear_trend.tiff", width = 6.75*1.2, height = 8*1.2, units = "in", res = 300)
grid.arrange(arrangeGrob(linear_decrease_plot, left = y.grob, bottom = x.grob))
dev.off()

# Increasing
increasing_plots <- lapply(increasing, plot_trend, r2_res = r2_res)
increasing_plots_g <- do.call(grid.arrange, c(increasing_plots, nrow=2))
plot <- plot_grid(increasing_plots_g, vjust = 1, scale = 1, align = 'v', axis = 't')
# create tiff file supplementary figure 1
tiff(filename = "FigS1_001_increasing_trend.tiff", width = 6.75, height = 8, units = "in", res = 300)
grid.arrange(arrangeGrob(plot, left = y.grob, bottom = x.grob))
dev.off()

# Flat (no change)
flat_plots <- lapply(flat, plot_trend, r2_res = r2_res)
flat_plots_g <- do.call(grid.arrange, flat_plots)
plot <- plot_grid(flat_plots_g, vjust = 1, scale = 1, ncol = 1, align = 'v', axis = 't')
# create tiff file supplementary figure 2
tiff(filename = "FigS2_001_flat_trend.tiff", width = 6.75, height = 8, units = "in", res = 300)
grid.arrange(arrangeGrob(plot, left = y.grob, bottom = x.grob))
dev.off()

# Peak in 2000s
peak_in_00s_plots <- lapply(peak_in_00s, plot_trend, r2_res = r2_res)
peak_in_00s_plots_g <- do.call(grid.arrange, peak_in_00s_plots)
plot <- plot_grid(peak_in_00s_plots_g, vjust = 1, scale = 1, ncol = 1, align = 'v', axis = 't')
# create tiff file figure 2
tiff(filename = "Fig2_001_peak00s_trend.tiff", width = 6.75*1.1, height = 8*1.1, units = "in", res = 300)
grid.arrange(arrangeGrob(plot, left = y.grob, bottom = x.grob))
dev.off()

# Other (8 countries)
other_plots <- lapply(other, plot_trend, r2_res = r2_res)
other_plots_g <- do.call(grid.arrange, other_plots)
plot <- plot_grid(other_plots_g, vjust = 1, scale = 1, ncol = 1, align = 'v', axis = 't')
# create tiff file supplementary figure 4
tiff(filename = "FigS4_001_other_trend.tiff", width = 6.75, height = 8, units = "in", res = 300)
grid.arrange(arrangeGrob(plot, left = y.grob, bottom = x.grob))
dev.off()

# Other -- no CI (8 countries)
other_plots_no_ci <- lapply(other, plot_trend_no_ci, r2_res = r2_res)
other_plots_no_ci_g <- do.call(grid.arrange, other_plots_no_ci)
plot <- plot_grid(other_plots_no_ci_g, vjust = 1, scale = 1, ncol = 1, align = 'v', axis = 't')

# create tiff file supplementary figure 5
tiff(filename = "FigS5_001_other_trend-noCI.tiff", width = 6.75, height = 8, units = "in", res = 300)
grid.arrange(arrangeGrob(plot, left = y.grob, bottom = x.grob))
dev.off()

# All (40 countries)
all_plots <- lapply(all_countries, plot_trend, r2_res = r2_res)
all_plots_g <- do.call(grid.arrange, all_plots)
plot <- plot_grid(all_plots_g, vjust = 1, scale = 1, ncol = 1, align = 'v', axis = 't')
# create tiff file supplementary figure
tiff(filename = "FigS6_001_ALL_trends.tiff", width = 6.75*1.6, height = 8.5*1.5, units = "in", res = 300)
grid.arrange(arrangeGrob(plot, left = y.grob, bottom = x.grob))
dev.off()