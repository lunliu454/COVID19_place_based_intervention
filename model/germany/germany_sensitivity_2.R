library(dplyr)
library(stringr)
source("intervention_status_code.R")
source("did_multiplegt_v2.R")

input <- read.csv("data/germany_input.csv",stringsAsFactors = F)
input <- input %>% 
  mutate(log_win7_Rt_estimate = log(win7_Rt_estimate),
         #remove observations whose rt estimate is unreliable (cv>0.3)
         log_win7_Rt_estimate = ifelse(cv <= 0.3, log_win7_Rt_estimate, NA)) %>%
  filter(!is.na(win7_stay_at_home))
control <- c('avg_temp',
             'win7_stay_at_home',
             'win7_school_close',
             'win7_childcare_close',
             'win7_shop_close',
             'win7_restaurant_close',
             'win7_entertainment_close',
             'win7_sports_indoor_close',
             'win7_sports_outdoor_close',
             'win7_gathering_outside_10lower',
             'win7_gathering_inside_10over',
             'win7_gathering_inside_10lower',
             'win7_gathering_outside_10over')
cities <- unique(input$city_eng_name)
#code intervention status to 0, 0.5 or 1
for (p in control[2:13]){
  for (i in cities){
    input[[p]][which(input$city_eng_name == i)] <- input[[p]][which(input$city_eng_name == i)] %>%
                                                   intervention_status_code()
  }
}
periods <- list(c("2020-03-27", "2020-05-02"),
                c("2020-03-29", "2020-04-27"),
                c("2020-03-24", "2020-05-12"),
                c("2020-03-22", "2020-05-20"),
                c("2020-03-21", "2020-06-03"))
cut_off_value <- c("09", "08", "07", "06", "05")
for (i in 1:5){
  for (p in control[2:13]){
    model <- p %>% str_sub(6, -1)
    model <- paste("ger", cut_off_value[i], model, sep = "_")
    m <- try(did(input, "log_win7_Rt_estimate", "city_eng_name", "date", p,
                 controls = control[control != p],
                 placebo = 0, dynamic = 20, cluster = "date",
                 brep = 100, covariance = TRUE, average_effect = "simple", 
                 parallel = TRUE, 
                 direction = "both", 
                 controlby = "period", period = periods[[i]],
                 controlT = FALSE))
    assign(model,m)
    save.image("sensitivity_2_ger.RData")
  }
}



