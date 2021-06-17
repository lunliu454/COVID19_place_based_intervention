intervention_status_code <- function(x){
  y <- x
  for (w in 1:length(x)){
    if (x[w] < 0.25){
      y[w] <- 0
    }
    if (abs(x[w] - 0.28) < 0.01) {
      if (w > 1) {
        if (abs(x[w + 1] - 0.35) < 0.01 | abs(x[w - 1] - 0.35) < 0.01){
          y[w] <- 0.5
        } else {
          y[w] <- 0
        }
      } else {
        if (abs(x[w + 1] - 0.35) < 0.01){
          y[w] <- 0.5
        } else {
          y[w] <- 0
        }
      }
    }
    if (abs(x[w] - 0.35) < 0.01){
      y[w] <- 0.5
    }
    if (abs(x[w] - 0.42) < 0.01){
      if (w > 1){
        if (abs(x[w + 1] - 0.5) < 0.01 | abs(x[w - 1] - 0.5) < 0.01){
          y[w] <- 0.5
        } else {
          y[w] <- 0
        }
      } else {
        if(abs(x[w + 1] - 0.5) < 0.01){
          y[w] <- 0.5
        } else {
          y[w] <- 0
        }
      }
    }
    if (abs(x[w] - 0.57) < 0.01){
      if (w > 1){
        if ((abs(x[w + 1] - 0.64) < 0.01) | (abs(x[w - 1] - 0.64) < 0.01)){
          y[w] <- 0.5
        } else {
          y[w] <- 1
        }
      } else {
        if (abs(x[w + 1] - 0.64) < 0.01){
          y[w] <- 0.5
        } else {
          y[w] <- 1
        }
      }
    }
    if (abs(x[w] - 0.64) < 0.01){y[w] <- 0.5}
    if (abs(x[w] - 0.71) < 0.01){
      if (w > 1){
        if (abs(x[w + 1] - 0.78) < 0.01 | abs(x[w - 1] - 0.78) < 0.01){
          y[w] <- 0.5
        } else {
          y[w] <- 1
        }
      } else {
        if (abs(x[w + 1] - 0.78) < 0.01){
          y[w] <- 0.5
        } else {
          y[w] <- 1
        }
      }
    }
    if (x[w] > 0.75){y[w] <- 1}
  }
  y
}