load("result_process/sensitivity_3.RData")
intervention <- c("stay_at_home", "school_close", "childcare_close", 'office_close',
                  'shop_close', 'restaurant_close', 'bar_close', "entertainment_close",
                  "cultural_close", "worship_close", "sports_indoor_close", "sports_outdoor_close",
                  "gathering_outside_10lower", "gathering_outside_10over",
                  "gathering_inside_10lower", "gathering_inside_10over")
a <- data.frame(intervention = intervention, effect = 0, se_effect = 0)
b<-grep(ls(), pattern = "bra_", value = TRUE)
for (i in b){
  m <- substr(i, 5, nchar(i))
  if (is.list(get(i))){
    f <- 0
    g <- c()
    dynamic <- grep("weight", names(get(i))) %>% length() - 1
    while (f <= dynamic){#
      g <- c(g, min(get(i)[[4 * f + 4]], get(i)[[4 * f + 3]] - get(i)[[4 * f + 4]]))
      f <- f + 1
    }
    if (length(g[g <= 5]) > 0){
      if (min(which(g <= 5)) > 1){
        dynamic <- min(which(g <= 5)) - 2
        start = 1
        end = 1 + dynamic
        v <- get(i) %>% unlist()
        effect_average = mean(v[seq(1, 4 * dynamic + 1, 4)], na.rm = TRUE)
        var_effect_average = sum((1 / (dynamic + 1) * v[seq(2, 4 * dynamic + 2, 4)]) ^ 2)
        for (ii in 1:dynamic) {
          if (ii >= dynamic) { break }
          for (jj in (ii + 1):dynamic) {
            name = paste("cov_effect", toString(ii), toString(jj), sep = "_")
            var_effect_average = var_effect_average + (1 / (dynamic + 1)) ^ 2 * 2 * unlist(get(i)[name])
          }
        }
        a[which(a$intervention == m), "effect"] <- effect_average
        a[which(a$intervention == m), "se_effect"] <- sqrt(var_effect_average)
      }else{
        a[which(a$intervention == m), "effect"] <- NA
        a[which(a$intervention == m), "se_effect"] <- NA
      }
    }else{
      a[which(a$intervention == m), "effect"] <- get(i)[["effect_average"]]
      a[which(a$intervention == m), "se_effect"] <- get(i)[["se_effect_average"]]
    }
  }
}
a <- a[-c(3, 15, 16), ]
result_default <- read.csv("plot/fig_2/effect_bra.csv")
a <- cbind(result_default[,c(2,3,4)], a[,2:3])
colnames(a)[2:5] <- c("effect_08","se_effect_08","effect_add","se_effect_add")
a <- a %>% mutate(ci_08_0.95=se_effect_08*1.96*2,ci_08_0.5=se_effect_08*0.66*2,
                  ci_add_0.95=se_effect_add*1.96*2,ci_add_0.5=se_effect_add*0.66*2) %>%
  select(intervention,effect_08,se_effect_08,ci_08_0.95,ci_08_0.5,
         effect_add,se_effect_add,ci_add_0.95,ci_add_0.5)
write.csv(a, "plot/fig_s7/sensitivity_3_bra.csv")

