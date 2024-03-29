% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dominance_internals.r
\name{dominance_scalar}
\alias{dominance_scalar}
\title{Scalar-returning internal dominance analysis meta-function}
\usage{
dominance_scalar(
  function2call,
  args_list,
  adj_model_args,
  value_w_all_names,
  do_cdl,
  do_cpt,
  reverse
)
}
\description{
Internal dominance analysis computation function assuming scalar
or vector of length 1 returned value.

Not intended to be called by the user.
}
\keyword{internal}
