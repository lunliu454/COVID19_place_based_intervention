load("result_process/sensitivity_1_ger.RData")
intervention <- c("stay_at_home", "school_close", "childcare_close", 'office_close',
                  'shop_close', 'restaurant_close', 'bar_close', "entertainment_close",
                  "cultural_close", "worship_close", "sports_indoor_close", "sports_outdoor_close",
                  "gathering_outside_10lower", "gathering_outside_10over",
                  "gathering_inside_10lower", "gathering_inside_10over")
a <- list()
for (i in intervention){
  b <- grep(ls(), pattern = paste0("ger_", i) , value = TRUE)
  effect <- c()
  for (j in b){
    if (is.list(get(j))){
      dynamic <- grep("N_dynamic", names(get(j))) %>% length()
      f <- 0
      g <- c()
      while (f <= dynamic){
        g <- c(g, min(get(j)[[3 * f + 3]], get(j)[[3 * f + 2]] - get(j)[[3 * f + 3]]))
        f <- f + 1
      }
      if (length(g[g <= 5]) > 0){
        if (min(which(g <= 5)) > 1){
          dynamic <- min(which(g <= 5)) - 2
          start = 1
          end = 1 + dynamic
          v <- get(j) %>% unlist()
          effect <- c(effect, mean(v[seq(1, 3 * dynamic + 1, 3)], na.rm = TRUE))
        }else{
          effect <- c(effect, NA)
        }
      }else{
        v <- get(j) %>% unlist()
        effect <- c(effect, mean(v[seq(1, 3 * dynamic + 1, 3)], na.rm = TRUE))
      }
    }
  }
  a<-append(a, list(effect))
}
names(a) <- intervention
result <- as.data.frame(matrix(nrow = 12, ncol = length(a[[2]]) + 1))
colnames(result) <- c("intervention", 1:length(a[[1]]))
r = 0
for (i in 1:length(a)){
  if(names(a[i]) %in% c("stay_at_home", "school_close",
                        "childcare_close", 'shop_close',
                        'restaurant_close', "entertainment_close", 
                        "sports_indoor_close", "sports_outdoor_close", 
                        "gathering_outside_10lower", "gathering_outside_10over",
                        "gathering_inside_10lower", "gathering_inside_10over")){
    r = r + 1
    result$intervention[r] <- names(a[i])
    for (j in 1:length(a[[3]])){
      result[r, j + 1] <- a[[i]][j]
    }
  }
}
result$intervention <- c("Stay-at-home","School closure","Childcare closure",
                         "Non-essential retail closure","Restaurant closure","Entertainment venue closure",
                         "Indoor sports closure",
                         "Outdoor sports closure","No outdoor gathering <=10","No outdoor gathering >10",
                         "No indoor gathering <=10","No indoor gathering >10")
result_default <- read.csv("plot/fig_2/effect_ger.csv")
result$default <- result_default$effect_average
write.csv(result,"plot/fig_s5/sensitivity_1_ger.csv")
