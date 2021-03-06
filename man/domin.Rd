% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/domin.r
\name{domin}
\alias{domin}
\title{Dominance analysis supporting \code{formula}-based functions}
\usage{
domin(
  formula_overall,
  reg,
  fitstat,
  sets = NULL,
  all = NULL,
  complete = TRUE,
  ...
)
}
\arguments{
\item{formula_overall}{An object of class \code{\link{formula}} or that can be coerced to class \code{formula} for use in the function in \code{reg}.
The \code{formula} object must have the form \code{response ~ terms} with the \code{terms} separated by \code{+}.}

\item{reg}{The function implementing the predictive (or "reg"ression) model called. Uses \code{\link{do.call}} and accepts any function call
\code{do.call} would accept.}

\item{fitstat}{List of specifications to call a fit statistic extracting function (see details). Like \code{reg}, uses \code{do.call} and
accepts any function call \code{do.call} would accept.}

\item{sets}{An optional list containing vectors of variable or factor names.  Each \link{vector} in the list is used as a set and always
included together in the \code{formula}.}

\item{all}{An optional vector of variable or factor names to be built into the \code{formula} and included in all model subsets.}

\item{complete}{Logical.  If \code{FALSE} then complete dominance matrix is not computed.}

\item{...}{Additional arguments passed to the function in \code{reg}.}
}
\value{
Returns an object of \code{\link{class}} "domin".
An object of class "domin" is a list composed of the following elements:
\describe{
\item{\code{General_Dominance}}{Vector of general dominance statistics.}
\item{\code{Standardized}}{Vector of general dominance statistics normalized to be out of 100.}
\item{\code{Ranks}}{Vector of ranks applied to the general dominance statistics.}
\item{\code{Conditional_Dominance}}{Matrix of conditional dominance statistics.}
\item{\code{Complete_Dominance}}{Matrix of complete dominance designations.}
\item{\code{Fit_Statistic_Overall}}{Value of fit statistic across all IVs.}
\item{\code{Fit_Statistic_All_Subsets}}{Value of fit statistic associated with IVs in \code{all}.}
\item{\code{Call}}{The matched call.}
\item{\code{Subset_Details}}{List containing full model and descriptions of IVs in model by source.}
}
}
\description{
Computes dominance statistics for predictive modeling functions that accept a "standard" \code{formula}.
}
\details{
Dominance analysis focuses on computing the contribution of independent variables/IVs or \code{terms} to a predictive model's fit to the data.
\code{domin} automates the process whereby combinations of IVs are created and concatenated in a formula to be submitted to the model in \code{reg}.
\code{domin} creates all the concatenated IVs from the entries on the right hand side of \code{formula_overall} and the entries in \code{sets}.

Each entry in the right of \code{formula_overall} is processed and each individual entry is included as a separate IV.
\code{formula_overall} must contain the dependent variable/DV or \code{response} on the left hand side and any individual IVs separated by \code{+}.
\code{domin} applies only the formula processing that is available in the \code{stats} package.

The elements of the list entries in \code{sets} are each considered a separate IV and must be submitted as a list.
Each entry in \code{sets} must be a vector of IVs.  Individual vector elements within a single set are concatenated using \code{+} automatically.
It is possible to use a \code{domin} with only sets (i.e., no IVs in \code{formula_overall}; see examples below).

The IV's in \code{all} must also be submitted as a vector, are concatenated with \code{+} automatically, and are also included in the model.
These "all subsets" IVs are removed from the fit statistic and all subsequent dominance statistics.

The entry to \code{fitstat} must be list and follow a specific structure: (\code{fit_function}, \code{element_name}, \code{...})
\describe{
\item{\code{fit_function}}{First element and function to be applied to \code{reg}}
\item{\code{element_name}}{Second element and name of the element from the object returned by \code{fit_function}}
\item{\code{...}}{Subsequent elements and are additional arguments passed to \code{fit_function}}
}
}
\examples{
## Basic linear model with r-square
domin(mpg ~ am + vs + cyl, "lm", list("summary", "r.squared"), data=mtcars)

## Including sets
domin(mpg ~ am + vs + cyl, "lm", list("summary", "r.squared"), 
 data=mtcars, sets=list(c("carb", "gear"), c("disp", "wt")))

## Multivariate linear model with custom multivariate r-square function and all subsets variable
Rxy <- function(obj, names, data) {
   return(list("r2" = cancor(predict(obj), 
       as.data.frame(mget(names,as.environment(data))))[["cor"]][1]^2)) }
domin(cbind(wt, mpg) ~ vs + cyl + am, "lm", list("Rxy", "r2", c("mpg", "wt"), mtcars), 
 data = mtcars, all=c("carb"))

## Sets only
domin(mpg ~ 1, "lm", list("summary", "r.squared"), 
 data=mtcars, sets=list(c("am", "vs"), c("cyl", "disp"), c("qsec", "carb")))
}
\keyword{multivariate}
\keyword{utilities}
