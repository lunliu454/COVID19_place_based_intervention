source("result_process/japan/japan_effect.R")
source("result_process/southkorea/korea_effect.R")
source("result_process/germany/germany_effect.R")
source("result_process/uk/uk_effect.R")
source("result_process/us/us_effect.R")
source("result_process/brazil/brazil_effect.R")


jp <- read.csv("plot/fig_2/effect_jp.csv") %>%
  mutate(prob_jp = pt(effect_average / se_effect_average, lower.tail = F, 20)) %>%
  select(intervention, prob_jp)
kor <- read.csv("plot/fig_2/effect_kor.csv") %>%
  mutate(prob_kor = pt(effect_average / se_effect_average, lower.tail = F, 20)) %>%
  select(intervention, prob_kor)
ger <- read.csv("plot/fig_2/effect_ger.csv") %>%
  mutate(prob_ger = pt(effect_average / se_effect_average, lower.tail = F, 20)) %>%
  select(intervention, prob_ger)
uk <- read.csv("plot/fig_2/effect_uk.csv") %>%
  mutate(prob_uk = pt(effect_average / se_effect_average, lower.tail = F, 20)) %>%
  select(intervention, prob_uk)
us <- read.csv("plot/fig_2/effect_us.csv") %>%
  mutate(prob_us = pt(effect_average / se_effect_average, lower.tail = F, 20)) %>%
  select(intervention, prob_us)
bra <- read.csv("plot/fig_2/effect_bra.csv") %>%
  mutate(prob_bra = pt(effect_average / se_effect_average, lower.tail = F, 20)) %>%
  select(intervention, prob_bra)
t <- Reduce(function(x,y) merge(x = x, y = y, by = "intervention", all = T), 
            list(jp, kor, ger, uk, us, bra))
t <- t[c(11,15,2,19,18,7,10,1,4,3,9,5,8,13,14,6,12,16,17),]
t$Mean <- rowMeans(t[,2:7], na.rm = T)
colnames(t)[2:7] <- c("Japan", "South Korea", "Germany", "UK", "US", "Brazil")
write.csv(t, "plot/fig_3/prob_all.csv", row.names = F)
