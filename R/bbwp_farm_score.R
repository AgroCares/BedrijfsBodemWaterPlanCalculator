#' Calculate the total score of five opportunity indicators for all fields in the Netherlands
#'
#' Estimate the potential to contribute to agronomic and environmental challenges in a region for a farm
#'
#' @param D_OPI_TOT (numeric) the score for the integrative opportunity index (risk times impact) for each field
#' @param D_OPI_NGW (numeric) the scoring index for lowering N emission to ground water (risk times impact) for each field
#' @param D_OPI_NSW (numeric) the scoring index for lowering N emission to surface water (risk times impact) for each field
#' @param D_OPI_PSW (numeric) the scoring index for lowering P emission to surface water (risk times impact) for each field
#' @param D_OPI_NUE (numeric) the scoring index to use N and P inputs efficiently for each field
#' @param D_OPI_WB (numeric) the scoring index to buffer and store water and efficiently use water for plant growth for each field
#' @param D_AREA (numeric) the area of the field (\ m2 or \ ha) 
#'   
#' @import data.table
#'
#' @export
# calculate the opportunities for a set of fields
bbwp_farm_score <- function(D_OPI_TOT,D_OPI_NGW,D_OPI_NSW,D_OPI_PSW,D_OPI_NUE,D_OPI_WB, D_AREA){
  
  # check length of the inputs
  arg.length <- max(length(D_OPI_TOT),length(D_OPI_NGW),length(D_OPI_NSW),length(D_OPI_PSW),
                    length(D_OPI_NUE),length(D_OPI_WB))
  
  # check inputs
  checkmate::assert_numeric(D_OPI_TOT, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(D_OPI_NGW, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(D_OPI_NSW, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(D_OPI_PSW, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(D_OPI_NUE, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(D_OPI_WB, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(D_AREA, lower = 0, upper = 50000)
  
  # collect data in one data.table
  dt <- data.table(
    id = 1:arg.length,
    D_OPI_NGW = D_OPI_NGW,
    D_OPI_NSW = D_OPI_NSW,
    D_OPI_PSW = D_OPI_PSW,
    D_OPI_NUE = D_OPI_NUE,
    D_OPI_WB = D_OPI_WB,
    D_OPI_TOT = D_OPI_TOT,
    D_AREA = D_AREA
  )
  
  # columns with the score of the opportunity indexes
  cols <- c('D_OPI_TOT','D_OPI_NGW','D_OPI_NSW','D_OPI_PSW','D_OPI_NUE','D_OPI_WB')
  
  # calculate area weigthed sum of the field indices
  dt <- dt[,lapply(.SD ,weighted.mean, w = D_AREA), .SDcols = cols]
  
  # Round the values
  dt<- dt[, lapply(.SD, round, digits = 0)]
  
  # add scores from measures taken???
  
  
  # return output
  return(dt)
}
