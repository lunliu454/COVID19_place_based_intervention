input <- read.csv("data/germany_input.csv",stringsAsFactors = F)
input <- input %>% 
  mutate(date = as.Date(date))
cities <- unique(input$city_eng_name)
intervention <- c("stay_at_home",
                  "school_close",
                  "childcare_close",
                  "shop_close",
                  "restaurant_close",
                  "entertainment_close",
                  "sports_indoor_close",
                  "sports_outdoor_close",
                  "gathering_outside_10lower",
                  "gathering_outside_10over",
                  "gathering_inside_10lower",
                  "gathering_inside_10over")
#identity the dates when interventions changed status in each city
intervention_change_dates_down <- function(y){
  k <- 1
  b <- matrix(ncol = 2)
  for (j in min(x$date):(max(x$date) - 1)){
    j <- j - 18330
    if (!is.na(x[j, y]) & !is.na(x[j + 1, y])){
      if ((x[j, y] == 0 & x[j + 1, y] > 0) | (x[j, y] == 0.5 & x[j + 1, y] > 0.5)){
        if (k == 1){
          b[k, 2] <- j + 1
          b[k, 1] <- cities[i]
        } else {
          b <- rbind(b, c(cities[i], j + 1))
        }
        k <- k + 1
      }
    }
  }
  return(b)
}
intervention_change_dates_lift <- function(y){
  k <- 1
  b <- matrix(ncol = 2)
  for (j in min(x$date):(max(x$date) - 1)){
    j <- j - 18330
    if (!is.na(x[j, y]) & !is.na(x[j + 1, y])){
      if ((x[j, y] == 1 & x[j + 1, y] < 1) | (x[j, y] == 0.5 & x[j + 1, y] < 0.5)){
        if (k == 1){
          b[k, 2] <- j + 1
          b[k, 1] <- cities[i]
        } else {
          b <- rbind(b, c(cities[i], j + 1))
        }
        k <- k + 1
      }
    }
  }
  return(b)
}
table_1_colnames <- sapply(intervention, function(x){paste0(x,"=0")}) %>% paste(collapse = ",")
table_1_colnames <- paste0("data.frame(city=0,",table_1_colnames,")")
for (j in c("down", "lift")){
  table_1 <- paste("table_1", j, sep="_")
  tmp <- eval(parse(text = table_1_colnames))
  for (i in 1:length(cities)){
    x <- filter(input,city_eng_name == cities[i])
    use_function <- paste("intervention_change_dates", j, sep = "_")
    a <- lapply(intervention, get(use_function))
    b <- lapply(a, function(x){as.data.frame(t(x), stringsAsFactors = F)}) %>% 
      purrr::reduce(plyr::rbind.fill)
    b <- b[seq(2, 24, 2),] %>%
      t() %>%
      as.data.frame(stringsAsFactors = F)
    b$city <- cities[i]
    colnames(b)[1:12] <- colnames(tmp)[2:13]
    tmp <- rbind(tmp, b)
  }
  tmp <- tmp[-1, ]
  #identify interventions that didn't change in each stage
  unchanged <- apply(tmp[, 2:13], 2, function(x){F %in% is.na(x)})
  assign(paste("unchanged", j, sep = "_"), intervention[!unchanged])
  
  tmp[is.na(tmp)] <- 0
  assign(table_1, tmp)
}

#the simultaneity between interventions
for (k in c("down", "lift")){
  table_2 <- paste("table_2", k, sep="_")
  tmp <- as.data.frame(matrix(nrow = 16, ncol = 16), row.names = intervention)
  colnames(tmp) <- row.names(tmp)
  tmp_2 <- tmp
  table_1 <- paste("table_1", k, sep = "_") %>% get()
  for (i in 1:12){
    for (j in 1:12){
      a <- row.names(tmp)[i]
      b <- colnames(tmp)[j]
      unchanged <- paste("unchanged", k, sep = "_") %>% get()
      if (a %in% unchanged | b %in% unchanged){
        tmp[i, j] <- NA
        tmp_2[i, j] <- NA
      } else {
        a_maj <- table_1[, a] %>% table() %>% which.max() %>% names()
        a <- sym(a)
        b_maj <- table_1[, b] %>% table() %>% which.max() %>% names()
        b <- sym(b)
        table_1 <- mutate(table_1,
                          col1 = ifelse((!!a == !!b) & (!!a != "0"), 1, 0),
                          col2 = ifelse(!!a != "0" | !!b != "0", 1, 0),
                          col3 = ifelse(!!a != a_maj & !!a != !!b, 1, 0),
                          col4 = ifelse(!!b != b_maj & !!a != !!b, 1, 0),
                          col5 = ifelse(!!a != a_maj, 1, 0),
                          col6 = ifelse(!!b != b_maj, 1, 0))
        tmp[i,j] <- sum(table_1$col1) / sum(table_1$col2)
        tmp_2[i,j] <- (sum(table_1$col3) / sum(table_1$col5) + sum(table_1$col4) / sum(table_1$col6)) / 2
      }
    }
  }
  assign(table_2, tmp)
}
write.csv(table_2_down,"corr_ger_down.csv")
write.csv(table_2_lift,"corr_ger_lift.csv")
