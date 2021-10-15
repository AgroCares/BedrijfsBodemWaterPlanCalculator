#' Calculate the chance that soil property is lower than the measured value given the variation in a catchment
#'
#' assuming a normal distributed density function. The chance can be used as a relative ranking position of the field given the variation inside the catchment.
#'
#' @param smean (numeric) The mean value for a numeric soil property in a given catchment
#' @param ssd (numeric) The variation (standard deviation) among a numeric soil property in a given catchment
#' @param svalue (numeric) The measured value of a soil property of a field inside a catchment
#' 
#'
#' @export
# calculate the ranking position of one or multiple field given a normal distributed soil property in a single catchment
cdf_rank <- function(smean,ssd,svalue){
    
    # set lower and maximum limit for range
    range.lower <- min(svalue,smean - 3 * ssd)
    range.upper <- max(svalue,smean + 3 * ssd)
    
    # set range
    range <- sort(c(seq(range.lower,range.upper,length.out = 1000),svalue))
    
    # make cumulative density function with pnorm
    cdf <- pnorm(range, smean, ssd)
    
    # exctract the probability that a value is below given svalue
    out <- cdf[range %in% svalue]
    
    # return likelyhood
    return(out)
  }
  