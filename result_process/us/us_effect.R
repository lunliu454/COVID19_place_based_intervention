load("result_process/effect.RData")
intervention <- c("stay_at_home", "school_close", "childcare_close", 'office_close',
                  'shop_close', 'restaurant_close', 'bar_close', "entertainment_close",
                  "cultural_close", "worship_close", "sports_indoor_close", "sports_outdoor_close",
                  "gathering_outside_10lower", "gathering_outside_10over",
                  "gathering_inside_10lower", "gathering_inside_10over")
result <- data.frame(intervention = intervention,
                effect_average = 0,
                se_effect_average = 0,
                size = 0, 
                stringsAsFactors = F)
b<-grep(ls(),pattern="us_",value=TRUE)

for (i in b){
  int <- substr(i,4,nchar(i))
  c <- get(i)
  if (is.list(c)){
    d <- which(names(c) == "effect")
    dynamic <- grep("weight", names(c)) %>% length()
    if (dynamic > 0){
      total <- dynamic * 3 + 19
      dynamic <- dynamic - 1
      f <- 0
      g <- c()
      while (f <= dynamic){
        g <- c(g, min(c[[4 * f + 3 + d]], get(i)[[4 * f + d + 2]] - get(i)[[4 * f + d + 3]]))
        f <- f + 1
      }
      if (length(g[g <= 5]) > 0){
        if (min(which(g <= 5)) > 1){
          dynamic <- min(which(g <= 5)) - 2
          start = 1
          end = 1 + dynamic
          v <- c %>% unlist()
          effect_average = mean(v[seq(d, 4 * dynamic + d, 4)], na.rm=TRUE)
          var_effect_average = sum((1 / (dynamic + 1) * v[seq(d + 1, 4 * dynamic + 1 + d, 4)]) ^ 2)
          for (ii in 1:dynamic) {
            if (ii >= dynamic) { break }
            for (jj in (ii + 1):dynamic) {
              name = paste("cov_effect", toString(ii), toString(jj), sep="_")
              var_effect_average = var_effect_average + (1 / (dynamic + 1)) ^ 2 * 2 * unlist(c[name])
            }
          }
          result[which(result$intervention==int), 2] <- effect_average
          result[which(result$intervention==int), 3] <- sqrt(var_effect_average)
          result[which(result$intervention==int), 4] <- sum(g[which(g > 5)])
        }else{
          result[which(result$intervention==int), 2] <- NA
          result[which(result$intervention==int), 3] <- NA
        }
      }else{
        result[which(result$intervention==int), 2] <- c[["effect_average"]]
        result[which(result$intervention==int), 3] <- c[["se_effect_average"]]
        result[which(result$intervention==int), 4] <- sum(g)
      }
    }else{
      result[which(result$intervention==int), 2] <- c[["effect"]]
      result[which(result$intervention==int), 3] <- c[["se_effect"]]
      result[which(result$intervention==int), 4] <- min(c[["N_switchers_effect"]], c[["N_effect"]] - c[["N_switchers_effect"]])
      result[which(result$intervention==int), 5] <- c[["effect_average"]]
      result[which(result$intervention==int), 6] <- c[["se_effect_average"]]
    }
  }
}
result <- filter(result, effect_average != 0)
result$intervention <- c("Stay-at-home","School closure","Childcare closure",
                   "Non-essential retail closure","Restaurant closure","Bar closure",
                   "Entertainment venue closure",
                   "Cultural venue closure","Religious place closure",
                   "Indoor sports closure",
                   "Outdoor sports closure",
                   "No outdoor gathering <=10","No outdoor gathering >10")
write.csv(result,"plot/fig_2/effect_us.csv")

