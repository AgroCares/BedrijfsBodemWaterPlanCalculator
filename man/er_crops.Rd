% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bbwp_tables.R
\docType{data}
\name{er_crops}
\alias{er_crops}
\title{An overview of crop lists used in Ecoregelingen}
\format{
A data.table with 463 rows and 6 columns:
\describe{
  \item{B_LU_BRP}{The Dutch crop code from the BRP}
  \item{B_LU_NAME}{The Dutch crop name}
  \item{B_LU_BBWP}{The Dutch BBWP category used for allocation of measures to BBWP crop categories}
  \item{bouwland}{(boolean) indication whether the crop falls within the ER category "arable"}
  \item{productive}{(boolean) indication whether the crop falls within the ER category "productive"}
  \item{beteelbaar}{(boolean) indication whether the crop falls within the ER category "cultivated"}
}
}
\usage{
er_crops
}
\description{
This table contains the crop ids (B_LU_BRP) with applicability on ER categories arable, productive and cultivated
}
\keyword{datasets}
