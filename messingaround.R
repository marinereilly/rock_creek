#####load packages#####
library(tidyverse)
library(broom)
library(plyr)

#####load data#####
rc_flux <- read.csv("C:/Users/Erin/Desktop/rock_creek/erin_test.csv", stringsAsFactors=FALSE)
View(rc_flux)
#####Make the plotting function#####
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste(names(fit$model)[2], "~", y = names(fit$model)[1], "     ",
                       "Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

#####goals 1. make plot for each site/parameter 2. put the slopes, R2, p values for lm on plot#####
rc_flux2<-rc_flux %>% 
  select(unique_ID, time_sum_min, NH4_mgNL, NO2_mgNL, PO4_mgPL, NO23_mgNL, DO_mgl)
rc_flux2<-rc_flux2 %>% 
  group_by(unique_ID, time_sum_min) %>% 
  gather(., key = parameter, value = measurement, NH4_mgNL, NO2_mgNL, PO4_mgPL, NO23_mgNL, DO_mgl)
rc_flux2$plotid<-paste0(rc_flux2$unique_ID, "_",rc_flux2$parameter)

rc_nest<-rc_flux2 %>% 
  group_by(plotid) %>% 
  nest()

pull_slope<-function(mod){
  coefficients(mod)["time_sum_min","Estimate"]
}

rc_fit<-rc_nest %>%  
  mutate(lm_model = map(data, ~ lm(measurement ~ time_sum_min, data = .)),
         results=map(lm_model, glance)) %>% 
  unnest(results) %>% 
  select(plotid, data, lm_model, r.squared, p.value) %>% 
  mutate(plot = map(lm_model, ~ggplotRegression(.)))

rc_fit$plot[[1]]
