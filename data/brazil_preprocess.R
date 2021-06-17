library(dplyr)
library(tidyr)

input <- read.csv("data/brazil_input.csv", stringsAsFactors = F)
# fill in missing temperature data
# find cities with no temperature data
a <- sapply(unique(input$city_eng_name), function(x){
  b <- filter(input, city_eng_name == x)
  mean(b$avg_temp, na.rm = TRUE) %>% is.na()
}) 
b <- unique(input$city_eng_name)[a == TRUE]
input <- input %>% 
  filter(!city_eng_name %in% b) %>%
  group_by(city_eng_name) %>%
  tidyr::fill(avg_temp, .direction = "down") %>%
  ungroup() %>%
  mutate(cv = win7_Rt_estimate_std / win7_Rt_estimate)
write.csv(input, "model/brazil/brazil_input.csv")
