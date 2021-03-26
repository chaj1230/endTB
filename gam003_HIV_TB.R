source("000_source-functions.R")

# arv: HIV antiretroviral coverage (in %) by country from 2000 to 2019
# prev: HIV prevalence (in %) by country from 2000 to 2019

# calculating prevalence of uncontrolled HIV (%, unc_hiv) per year

prev_uncontrolled_hiv <- function(country_name){
  
  # throw warning if country_name not in master
  throwWarning(country_name)
  
  country_arv <- arv %>% filter(country == country_name)
  hiv_prevalence <- (prev %>% filter(country == country_name))$hiv_prevalence
  
  arv.prev <- cbind(country_arv, hiv_prevalence)
  
  arv.prev <- arv.prev %>% mutate(unc_hiv = ((100 - hiv_coverage)/100) * hiv_prevalence)
  
  # shorten country name to fit in graph/use vernacular names
  country_name <- shorten_country_name(country_name)
  
  # display graph with the legend:
  p <- ggplot(arv.prev, aes(year, y = value, colour = variable)) +
    geom_point(aes(y = hiv_prevalence, col = "Total")) +
    geom_point(aes(y = unc_hiv, col = "Uncontrolled")) +
    ggtitle(country_name) +
    #xlab("Year") + ylab("% Population") + 
    theme(axis.title = element_blank()) +
    # make legend horizontal in one row
    guides(colour = guide_legend(ncol = 1)) +
    # modify legend title
    labs(colour = "HIV Prevalence (%)") +
    ylim(0, NA)
  
  p
  
}

# without legend
prev_uncontrolled_hiv_no_legend <- function(country_name){
  prev_uncontrolled_hiv(country_name) + theme(legend.position = "none")
}

# obtain legend
trAngola <- prev_uncontrolled_hiv("Angola")
legend <- get_legend(trAngola)

# axes labels
y.grob <- textGrob("HIV prevalence (% adult population)", gp = gpar(col="black", fontsize=15), rot = 90)
x.grob <- textGrob("Year", gp = gpar(fontface="bold", col="black", fontsize=15))

# generate graphs for the 15 countries of interest using lapply
hiv_plots <- lapply(hiv_15, prev_uncontrolled_hiv_no_legend)
hiv_plots_g <- do.call(grid.arrange, hiv_plots)
plot <- plot_grid(hiv_plots_g, vjust = 1, scale = 1, ncol = 1, align = 'v', axis = 't')

# create tiff file supplementary figure 3
tiff(filename = "updS3_hiv_prev.tiff", width = 6.75, height = 8, units = "in", res = 300)
grid.arrange(arrangeGrob(plot, left = y.grob, bottom = x.grob))
dev.off()

# generate the common legend
grid.arrange(legend)
g_legend <- plot_grid(legend, rel_heights = c(1, .1), ncol = 1, 
                      align = 'v', axis = 'c')
g_legend