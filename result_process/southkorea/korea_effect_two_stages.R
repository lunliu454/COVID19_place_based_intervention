load("effect_two_stages.RData")
intervention <- c("stay_at_home", "school_close", "childcare_close", 'office_close',
                  'shop_close', 'restaurant_close', 'bar_close', "entertainment_close","cultural_close",
                  "worship_close", "sports_indoor_close", "sports_outdoor_close",
                  "gathering_outside", "gathering_inside")
a <- sapply(c(0:20), function(x){paste0("day_", x , '=0,day_', x , '_se=0,day_', x ,"_size=0")}) %>%
  paste(collapse = ",")
a <- paste0("data.frame(intervention=rep(intervention,2),phase=c(rep('down',14),rep('lift',14)),effect_average=0,se_effect_average=0,",
              a,",stringsAsFactors=F)")
result <- eval(parse(text=a))
b <- grep(ls(), pattern = "kor_", value = TRUE)
b <- grep(b, pattern = "down", value=TRUE)
c <- grep(ls(), pattern = "kor_", value=TRUE)
c <- grep(c, pattern = "lift", value = TRUE)
b <- c(b, c)
for (i in 1:nrow(result)){
  c <- NA
  for (j in 1:length(b)){
    if (grepl(result$policy[i], b[j]) == T & grepl(result$phase[i], b[j]) == T){
      c <- get(b[j])
    }
  }
  if (is.list(c)){
    d <- which(names(c) == "effect")
    dynamic <- grep("weight",names(c)) %>% length()
    if (dynamic>0){
      total <- dynamic * 3 + 4
      dynamic <- dynamic - 1
      result[i, 5:total] <- lapply(0:dynamic, function(x){
        c(c[[x * 4 + d]], c[[x * 4 + d + 1]],
          min(c[[x * 4 + d + 3]], c[[x * 4 + d + 2]] - c[[x * 4 + d + 3]]))
        }) %>% unlist()
      result[i, 3:4] <- c(c[["effect_average"]], c[["se_effect_average"]])
    }else{
      result[i, 3] <- c[["effect"]]
      result[i, 4] <- c[["se_effect"]]
      result[i, 5] <- c[["effect_average"]]
      result[i, 6] <- c[["se_effect_average"]]
    }
    #lift signs not reversed
  }
}
# calculate 5-day average effect and standard error——--
result_5d <- data.frame(intervention = intervention,phase = result$phase,
                        effect_average = result$effect_average,
                        se_effect_average = result$se_effect_average,
                        day_0_5 = 0, day_0_5_se = 0, day_0_5_size = 0,
                        day_6_10 = 0, day_6_10_se= 0, day_6_10_size = 0,
                        day_11_15 = 0, day_11_15_se = 0, day_11_15_size = 0,
                        day_16_20 = 0, day_16_20_se = 0, day_16_20_size = 0)
for (i in 1:nrow(result)){
  a <- result$intervention[i]
  b <- result$phase[i]
  if (!is.na(result$day_0[i]) & result$day_0[i] != 0){
    a <- paste0("kor_", a, "_", b) 
    c <- get(a)
    v <- c %>% unlist()
    size_cols <- sapply(0:20,function(x){paste0("day_", x, "_size")})
    d <- result[i, size_cols] %>% unlist()
    dynamic <- max(which(d > 0)) - 1
    e <- ceiling(dynamic / 5)
    f <- min(5, dynamic)
    cols <- sapply(0:f, function(x){paste0("day_", x)})
    result_5d[i, "day_0_5"] <- result[i, cols] %>% unlist() %>% mean()
    cols <- sapply(0:f, function(x){paste0("day_", x, "_size")})
    result_5d[i, "day_0_5_size"] <- result[i, cols] %>% unlist() %>% mean()
    cols <- sapply(0:f, function(x){paste0("day_", x, "_se")})
    var_effect_average = (1 / (f + 1) * result[i, cols]) ^ 2 %>% sum()
    if (f > 0) {
      for (ii in 0:(f - 1)) {
        for (jj in (ii + 1):f) {
          name = paste("cov_effect", toString(ii), toString(jj), sep = "_")
          var_effect_average = var_effect_average + 1 / (f + 1) ^ 2 * 2 * unlist(c[name])
        }
      }
    }
    result_5d[i, "day_0_5_se"] <- sqrt(var_effect_average)
    if (e>1){
      f <- min(5, dynamic - 5) + 5
      cols <- sapply(6:f, function(x){paste0("day_", x)})
      result_5d[i, "day_6_10"] <- result[i, cols] %>% unlist() %>% mean()
      cols <- sapply(6:f, function(x){paste0("day_", x, "_size")})
      result_5d[i, "day_6_10_size"] <- result[i, cols] %>% unlist() %>% mean()
      cols <- sapply(6:f, function(x){paste0("day_", x, "_se")})
      var_effect_average = (1 / (f - 5) * result[i, cols]) ^ 2 %>% sum()
      if (f > 6){
        for (ii in 6:(f - 1)) {
          for (jj in (ii + 1):f) {
            name = paste("cov_effect", toString(ii), toString(jj), sep="_")
            var_effect_average = var_effect_average + 1 / (f - 5) ^ 2 * 2 * unlist(c[name])
          }
        }
      }
      result_5d[i, "day_6_10_se"] <- sqrt(var_effect_average)
    }
    if (e>2){
      f <- min(5, dynamic - 10) + 10
      cols <- sapply(11:f, function(x){paste0("day_", x)})
      result_5d[i, "day_11_15"] <- result[i, cols] %>% unlist() %>% mean()
      cols <- sapply(11:f, function(x){paste0("day_", x, "_size")})
      result_5d[i, "day_11_15_size"] <- result[i, cols] %>% unlist() %>% mean()
      cols <- sapply(11:f, function(x){paste0("day_", x, "_se")})
      var_effect_average = (1 / (f - 10) * result[i, cols]) ^ 2 %>% sum()
      if (f > 11){
        for (ii in 11:(f - 1)) {
          for (jj in (ii + 1):f) {
            name = paste("cov_effect", toString(ii), toString(jj), sep = "_")
            var_effect_average = var_effect_average + 1 / (f - 10) ^ 2 * 2 * unlist(c[name])
          }
        }
      }
      result_5d[i, "day_11_15_se"] <- sqrt(var_effect_average)
    }
    if (e > 3){
      f <- min(5, dynamic - 15) + 15
      cols <- sapply(16:f, function(x){paste0("day_", x)})
      result_5d[i, "day_16_20"] <- result[i, cols] %>% unlist() %>% mean()
      cols <- sapply(16:f, function(x){paste0("day_", x, "_size")})
      result_5d[i, "day_16_20_size"] <- result[i, cols] %>% unlist() %>% mean()
      cols <- sapply(16:f, function(x){paste0("day_", x, "_se")})
      var_effect_average = (1 / (f - 15) * result[i, cols]) ^ 2 %>% sum()
      if (f > 16) {
        for (ii in 16:(f - 1)) {
          for (jj in (ii + 1):f) {
            name = paste("cov_effect", toString(ii), toString(jj), sep="_")
            var_effect_average = var_effect_average + 1 / (f - 15) ^ 2 * 2 * unlist(c[name])
          }
        }
      }
      result_5d[i, "day_16_20_se"] <- sqrt(var_effect_average)
    }
  }
}
for (i in 1:nrow(result_5d)){
  if (result_5d$phase[i] == "lift"){
    result_5d[i, c(3, 5, 8, 11, 14)] <- -result_5d[i, c(3, 5, 8, 11, 14)]
  }
}

result_5d_1 <- filter(result_5d, phase == "down" & 
                        intervention %in% c("stay_at_home", "shop_close", "office_close", "worship_close"))
result_5d_2 <- filter(result_5d,phase == "lift" &
                        intervention %in% c("stay_at_home", "school_close", "office_close", "shop_close",
                                            "restaurant_close", "bar_close", "entertainment_close", "cultural_close",
                                            "worship_close", "sports_indoor_close", "sports_outdoor_close",
                                            "gathering_outside_10lower", "gathering_outside_10over"))
result_5d <- rbind(result_5d_1, result_5d_2)
result_5d$intervention_label <- c("Stay-at-home", "Office", "Shop+Restaurant+Bar+Entertainment+Sports (in)+Culture", 
                            "Religion", "Stay-at-home", "School+Childcare", "Office", 
                            "Shop", "Restaurant", "Bar", "Entertainment", "Culture", 
                            "Religion", "Sports (in)", "Sports (out)", "Gather<=10", 
                            "Gather>10")
result_5d$intervention <- c("Stay-at-home", "Office", "Shop", 
                      "Religion", "Stay-at-home", "School", "Office", 
                      "Shop", "Restaurant", "Bar", "Entertainment", "Culture", 
                      "Religion", "Sports (in)", "Sports (out)", "Gather<=10 (out)", 
                      "Gather>10 (out)")
