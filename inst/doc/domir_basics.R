## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup_lm-----------------------------------------------------------------
library(datasets)
library(tidyverse)

lm_cars <- 
  lm(mpg ~ am + cyl + carb, data = mtcars)

summary(lm_cars)

## ----setup_domir--------------------------------------------------------------
library(domir)

domin(mpg ~ am + cyl + carb, 
      lm, 
      list(summary, "r.squared"), 
      data = mtcars)

## ----capture_r2s--------------------------------------------------------------
lm_capture <- 
  function(formula, ...) { # wrapper program that accepts formula and ellipsis arguments
    count <<- count + 1 # increment counter in enclosing environment
    lm_obj <- lm(formula, ...) # estimate 'lm' model and save object
    DA_results[count, "formula"] <<- 
      deparse(formula) # record string version of formula passed in 'DA_results' in enclosing environment
    DA_results[count, "R^2"] <<- 
      summary(lm_obj)[["r.squared"]] # record R^2 in 'DA_results' in enclosing environment
    return(lm_obj) # return 'lm' class-ed object
  }

count <- 0 # initialize the count indicating the row in which the results will fill-in

DA_results <- # container data frame in which to record results
  data.frame(formula = rep("", times = 2^3-1), 
             `R^2` = rep(NA, times = 2^3-1), 
             check.names = FALSE)

lm_da <- domin(mpg ~ am + cyl + carb, # implement the DA with the wrapper
               lm_capture, 
               list(summary, "r.squared"), 
               data = mtcars)

DA_results

## ----cpt_am_cyl, echo=FALSE---------------------------------------------------
knitr::kable(
  cbind(DA_results[grepl("am", DA_results$formula) & !grepl("cyl", DA_results$formula) ,],
        DA_results[!grepl("am", DA_results$formula) & grepl("cyl", DA_results$formula) ,]), 
  row.names = FALSE, caption = "Complete Dominance Comparisons: `am` versus `cyl` ", digits = 3)

## ----cpt_am_carb, echo=FALSE--------------------------------------------------
knitr::kable(cbind(DA_results[grepl("am", DA_results$formula) & !grepl("carb", DA_results$formula) ,], DA_results[!grepl("am", DA_results$formula) & grepl("carb", DA_results$formula) ,]), row.names = FALSE, caption = "Complete Dominance Comparisons: `am` versus `carb` ", digits = 3)

## ----cpt_cyl_carb, echo=FALSE-------------------------------------------------
knitr::kable(cbind(DA_results[grepl("cyl", DA_results$formula) & !grepl("carb", DA_results$formula) ,], DA_results[!grepl("cyl", DA_results$formula) & grepl("carb", DA_results$formula) ,]), row.names = FALSE, caption = "Complete Dominance Comparisons: `cyl` versus `carb` ", digits = 3)

## ----lm_complete--------------------------------------------------------------
lm_da$Complete_Dominance

## ----cdl_am, echo=FALSE-------------------------------------------------------
first_order <- 
  cbind(DA_results[DA_results$formula == "mpg ~ am",], data.frame(`formula subtrahend` = "mpg ~ 1", `R^2 subtrahend` = 0, difference = DA_results[DA_results$formula == "mpg ~ am", "R^2"], check.names = FALSE))

names(first_order)[1:2] <- c("formula minuend", "R^2 minuend")

second_order <- 
  cbind(DA_results[grepl("am", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==3),], 
        DA_results[!grepl("am", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==2),], 
        data.frame(difference = 
                     DA_results[grepl("am", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==3),"R^2"] - DA_results[!grepl("am", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==2),"R^2"]))

names(second_order)[1:4] <- c("formula minuend", "R^2 minuend", "formula subtrahend", "R^2 subtrahend")

third_order <- 
  cbind(DA_results[sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==4),],
        DA_results[!grepl("am", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==3),],
        data.frame(difference = 
                     DA_results[sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==4),"R^2"] - DA_results[!grepl("am", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==3),"R^2"]))

names(third_order)[1:4] <- c("formula minuend", "R^2 minuend", "formula subtrahend", "R^2 subtrahend")

knitr::kable(first_order, row.names = FALSE, caption = "Conditional Dominance Computations: `am` with One IV/Alone", digits = 3)

knitr::kable(second_order, row.names = FALSE, caption = "Conditional Dominance Computations: `am` with Two IVs", digits = 3)

knitr::kable(third_order, row.names = FALSE, caption = "Conditional Dominance Computations: `am` with Three IVs/Full Model", digits = 3)

## ----cdl_cyl, echo=FALSE------------------------------------------------------
first_order <- 
  cbind(DA_results[DA_results$formula == "mpg ~ cyl",], data.frame(`formula subtrahend` = "mpg ~ 1", `R^2 subtrahend` = 0, difference = DA_results[DA_results$formula == "mpg ~ cyl", "R^2"], check.names = FALSE))

names(first_order)[1:2] <- c("formula minuend", "R^2 minuend")

second_order <- 
  cbind(DA_results[grepl("cyl", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==3),], 
        DA_results[!grepl("cyl", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==2),], 
        data.frame(difference = 
                     DA_results[grepl("cyl", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==3),"R^2"] - DA_results[!grepl("cyl", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==2),"R^2"]))

names(second_order)[1:4] <- c("formula minuend", "R^2 minuend", "formula subtrahend", "R^2 subtrahend")

third_order <- 
  cbind(DA_results[sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==4),],
        DA_results[!grepl("cyl", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==3),],
        data.frame(difference = 
                     DA_results[sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==4),"R^2"] - DA_results[!grepl("cyl", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==3),"R^2"]))

names(third_order)[1:4] <- c("formula minuend", "R^2 minuend", "formula subtrahend", "R^2 subtrahend")

knitr::kable(first_order, row.names = FALSE, caption = "Conditional Dominance Computations: `cyl` with One IV/Alone", digits = 3)

knitr::kable(second_order, row.names = FALSE, caption = "Conditional Dominance Computations: `cyl` with Two IVs", digits = 3)

knitr::kable(third_order, row.names = FALSE, caption = "Conditional Dominance Computations: `cyl` with Three IVs/Full Model", digits = 3)

## ----cdl_carb, echo=FALSE-----------------------------------------------------
first_order <- 
  cbind(DA_results[DA_results$formula == "mpg ~ carb",], data.frame(`formula subtrahend` = "mpg ~ 1", `R^2 subtrahend` = 0, difference = DA_results[DA_results$formula == "mpg ~ carb", "R^2"], check.names = FALSE))

names(first_order)[1:2] <- c("formula minuend", "R^2 minuend")

second_order <- 
  cbind(DA_results[grepl("carb", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==3),], 
        DA_results[!grepl("carb", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==2),], 
        data.frame(difference = 
                     DA_results[grepl("carb", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==3),"R^2"] - DA_results[!grepl("carb", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==2),"R^2"]))

names(second_order)[1:4] <- c("formula minuend", "R^2 minuend", "formula subtrahend", "R^2 subtrahend")

third_order <- 
  cbind(DA_results[sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==4),],
        DA_results[!grepl("carb", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==3),],
        data.frame(difference = 
                     DA_results[sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==4),"R^2"] - DA_results[!grepl("carb", DA_results$formula) & sapply(DA_results$formula, function(x) length(all.vars(as.formula(x)))==3),"R^2"]))

names(third_order)[1:4] <- c("formula minuend", "R^2 minuend", "formula subtrahend", "R^2 subtrahend")

knitr::kable(first_order, row.names = FALSE, caption = "Conditional Dominance Computations: `carb` with One IV/Alone", digits = 3)

knitr::kable(second_order, row.names = FALSE, caption = "Conditional Dominance Computations: `carb` with Two IVs", digits = 3)

knitr::kable(third_order, row.names = FALSE, caption = "Conditional Dominance Computations: `carb` with Three IVs/Full Model", digits = 3)

## ----lm_conditional-----------------------------------------------------------
lm_da$Conditional_Dominance

## ----cdl_am_cyk, echo=FALSE---------------------------------------------------
knitr::kable(data.frame(t(lm_da$Conditional_Dominance[c("am", "cyl"),]), comparison= lm_da$Conditional_Dominance["am",] > lm_da$Conditional_Dominance["cyl",]), caption = "Conditional Dominance Designation: `am` Compared to `cyl`", digits = 3)

## ----cdl_am_carb, echo=FALSE--------------------------------------------------
knitr::kable(data.frame(t(lm_da$Conditional_Dominance[c("am", "carb"),]), comparison= lm_da$Conditional_Dominance["am",] > lm_da$Conditional_Dominance["carb",]), caption = "Conditional Dominance Designation: `am` Compared to `carb`", digits = 3)

## ----cdl_cyl_carb, echo=FALSE-------------------------------------------------
knitr::kable(data.frame(t(lm_da$Conditional_Dominance[c("cyl", "carb"),]), comparison= lm_da$Conditional_Dominance["cyl",] > lm_da$Conditional_Dominance["carb",]), caption = "Conditional Dominance Designation: `cyl` Compared to `carb`", digits = 3)

## ----condit_gph, echo=FALSE---------------------------------------------------
lm_da |> pluck("Conditional_Dominance") |> as_tibble(rownames = "pred") |> pivot_longer(names_to = "ivs", values_to = "stat", cols = starts_with("IV")) |> mutate(ivs = fct_relabel(ivs, ~ str_replace(., "_", ": "))) |> ggplot(aes(x = ivs, y = stat, group = pred, color= pred)) + geom_line() + ylab("Conditional Dominance Statistic Value") + xlab("Number of Independent Variables") + labs(color = "Independent\nVariable") + theme_linedraw() + scale_color_viridis_d() 

## ----gen_am, echo=FALSE-------------------------------------------------------
knitr::kable(data.frame(t(as.data.frame(lm_da$Conditional_Dominance["am",])), `general dominance` = lm_da$General_Dominance[["am"]], check.names = FALSE), row.names = FALSE, caption = "General Dominance Computations: `am`", digits = 3)

## ----gen_cyl, echo=FALSE------------------------------------------------------
knitr::kable(data.frame(t(as.data.frame(lm_da$Conditional_Dominance["cyl",])), `general dominance` = lm_da$General_Dominance[["cyl"]], check.names = FALSE), row.names = FALSE, caption = "General Dominance Computations: `cyl`", digits = 3)

## ----gen_carb, echo=FALSE-----------------------------------------------------
knitr::kable(data.frame(t(as.data.frame(lm_da$Conditional_Dominance["carb",])), `general dominance` = lm_da$General_Dominance[["carb"]], check.names = FALSE), row.names = FALSE, caption = "General Dominance Computations: `carb`", digits = 3)

## ----lm_general---------------------------------------------------------------
lm_da$General_Dominance

## ----gen_rank, echo = FALSE---------------------------------------------------
knitr::kable(data.frame(IV = names(lm_da$General_Dominance), `general dominance` = lm_da$General_Dominance, ranks = lm_da$Ranks, check.names = FALSE), row.names = FALSE, caption = "General Dominance Designations", digits = 3)

## ----lm_strongest-------------------------------------------------------------
summary(lm_da)$Strongest_Dominance

