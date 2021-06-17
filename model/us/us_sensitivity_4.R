library(dplyr)
library(stringr)
intervention_status_code <- function(x, value){
  y <- x
  for (w in 1:length(x)){
    if (x[w] < 0.25){
      y[w] <- 0
    }
    if (abs(x[w] - 0.28) < 0.01) {
      if (w > 1) {
        if (abs(x[w + 1] - 0.35) < 0.01 | abs(x[w - 1] - 0.35) < 0.01){
          y[w] <- value
        } else {
          y[w] <- 0
        }
      } else {
        if (abs(x[w + 1] - 0.35) < 0.01){
          y[w] <- value
        } else {
          y[w] <- 0
        }
      }
    }
    if (abs(x[w] - 0.35) < 0.01){
      y[w] <- value
    }
    if (abs(x[w] - 0.42) < 0.01){
      if (w > 1){
        if (abs(x[w + 1] - 0.5) < 0.01 | abs(x[w - 1] - 0.5) < 0.01){
          y[w] <- value
        } else {
          y[w] <- 0
        }
      } else {
        if(abs(x[w + 1] - 0.5) < 0.01){
          y[w] <- value
        } else {
          y[w] <- 0
        }
      }
    }
    if (abs(x[w] - 0.57) < 0.01){
      if (w > 1){
        if ((abs(x[w + 1] - 0.64) < 0.01) | (abs(x[w - 1] - 0.64) < 0.01)){
          y[w] <- value
        } else {
          y[w] <- 1
        }
      } else {
        if (abs(x[w + 1] - 0.64) < 0.01){
          y[w] <- value
        } else {
          y[w] <- 1
        }
      }
    }
    if (abs(x[w] - 0.64) < 0.01){y[w] <- value}
    if (abs(x[w] - 0.71) < 0.01){
      if (w > 1){
        if (abs(x[w + 1] - 0.78) < 0.01 | abs(x[w - 1] - 0.78) < 0.01){
          y[w] <- value
        } else {
          y[w] <- 1
        }
      } else {
        if (abs(x[w + 1] - 0.78) < 0.01){
          y[w] <- value
        } else {
          y[w] <- 1
        }
      }
    }
    if (x[w] > 0.75){y[w] <- 1}
  }
  y
}

input <- read.csv("data/us_input.csv",stringsAsFactors = F)
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
             'win7_bar_close',
             'win7_entertainment_close',
             "win7_cultural_close",
             "win7_worship_close",
             'win7_sports_indoor_close',
             'win7_sports_outdoor_close',
             'win7_gathering_outside_10lower',
             'win7_gathering_outside_10over')
control_recode <- c('avg_temp',
                    'win7_stay_at_home_2',
                    'win7_school_close_2',
                    'win7_childcare_close_2',
                    'win7_shop_close_2',
                    'win7_restaurant_close_2',
                    'win7_bar_close_2',
                    'win7_entertainment_close_2',
                    'win7_cultural_close_2',
                    'win7_worship_close_2',
                    'win7_sports_indoor_close_2',
                    'win7_sports_outdoor_close_2',
                    'win7_gathering_outside_10lower_2',
                    'win7_gathering_outside_10over_2')
values <- c(0.3,0.4,0.5,0.6,0.7)
cities <- unique(input$city_eng_name)
#code intervention status to 0, 0.5 or 1
for (k in 1:100){
  for (p in control[2:14]){
    value <- sample(values, 1)
    p2 <- paste(p, 2, sep="_")
    input[[p2]] <- 0
    for (i in cities){
      input[[p2]][which(input$city_eng_name == i)] <- input[[p]][which(input$city_eng_name == i) ]%>%
                                                      intervention_status_code(value)
    }
  }
  for (p in control[2:14]){
    model <- p %>% str_sub(6, -1)
    model <- paste("us", k, model, sep="_")
    p2 <- paste(p, 2, sep="_")
    m <- try(did(input, "log_win7_Rt_estimate", "city_eng_name", "date", p2,
                 controls = control_recode[control_recode != p2],
                 placebo = 0, dynamic = 20, cluster = "date",
                 brep = 0, covariance = TRUE, average_effect = "simple", 
                 parallel = TRUE, 
                 direction = "both", 
                 controlby = "period", period = c("2020-03-30","2020-05-25"),
                 controlT = FALSE))
    assign(model,m)
  }
  save.image("sensitivity_4_us.RData")
}


