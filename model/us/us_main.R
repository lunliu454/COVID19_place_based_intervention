library(dplyr)
library(stringr)
source("intervention_status_code.R")

input <- read.csv("data/us_input.csv",stringsAsFactors = F)
input <- input %>% 
  mutate(log_win7_Rt_estimate = log(win7_Rt_estimate),
         log_win7_Rt_estimate = ifelse(cv <= 0.3, log_win7_Rt_estimate, NA)) %>%
  filter(!is.na(win7_stay_at_home))
control <- c('avg_temp',
             'win7_stay_at_home',
             'win7_school_close',
             'win7_childcare_close',
             'win7_shop_close',
             'win7_restaurant_close',
             'win7_bar_close',
             'win7_entertainment_close',
             "win7_cultural_close",
             "win7_worship_close",
             'win7_sports_indoor_close',
             'win7_sports_outdoor_close',
             'win7_gathering_outside_10lower',
             'win7_gathering_outside_10over')
cities <- unique(input$city_eng_name)
#code intervention status to 0, 0.5 or 1
for (p in control[2:14]){
  for (i in cities){
    input[[p]][which(input$city_eng_name == i)] <- input[[p]][which(input$city_eng_name == i)] %>%
                                                   intervention_status_code()
  }
}
for (p in control[2:14]){
  model <- p %>% str_sub(6, -1)
  model <- paste0("us_", model)
  m <- try(
    did(input, "log_win7_Rt_estimate", "city_eng_name", "date", p,
        controls = control[control != p],
        placebo = 0, dynamic = 20, cluster = "date",
        brep = 100, covariance = TRUE, average_effect = "simple", 
        parallel = TRUE, 
        direction = "both", 
        controlby = "period", period=c("2020-03-30","2020-05-25")),
    controlT = FALSE)
  assign(model,m)
  print(paste("finish", p))
  save.image("output.RData")
}
