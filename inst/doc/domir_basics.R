## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup_intro--------------------------------------------------------------
library(domir)
library(datasets)

domin(mpg ~ am + cyl + carb, 
      lm, 
      list(summary, "r.squared"), 
      data = mtcars)

## ----capture_r2s--------------------------------------------------------------
lm_capture <- 
  function(x, ...) {
    count <<- count + 1
    y <- lm(x, ...)
    results[count, "fmla"] <<- deparse(x)
    results[count, "r2"] <<- summary(y)[["r.squared"]]
    return(y)
  }

count <- 0

results <- data.frame(fmla = rep("", times = 2^3-1), 
                      r2 = rep(NA, times = 2^3-1) )

lm_da <- domin(mpg ~ am + cyl + carb, lm_capture, list(summary, "r.squared"), data = mtcars)

results

## ----lm_complete--------------------------------------------------------------
lm_da$Complete_Dominance

## ----lm_conditional-----------------------------------------------------------
lm_da$Conditional_Dominance

## ----lm_general---------------------------------------------------------------
lm_da$General_Dominance

