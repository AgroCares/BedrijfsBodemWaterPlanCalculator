#' Calculate the total score of five opportunity indicators for all fields in the Netherlands
#'
#' Estimate the potential to contribute to agronomic and environmental challenges in a region for a farm
#'
#' @param S_BBWP_TOT (numeric) the score for the integrative opportunity index (risk times impact) for each field
#' @param S_BBWP_NGW (numeric) the scoring index for lowering N emission to ground water (risk times impact) for each field
#' @param S_BBWP_NSW (numeric) the scoring index for lowering N emission to surface water (risk times impact) for each field
#' @param S_BBWP_PSW (numeric) the scoring index for lowering P emission to surface water (risk times impact) for each field
#' @param S_BBWP_NUE (numeric) the scoring index to use N and P inputs efficiently for each field
#' @param S_BBWP_WB (numeric) the scoring index to buffer and store water and efficiently use water for plant growth for each field
#' @param B_AREA (numeric) the area of the field (m2) 
#' @param S_BBWP_GW (numeric) the scoring index to maintain ample groundwater
#'   
#' @import data.table
#'
#' @export
# calculate the opportunities for a set of fields
bbwp_farm_score <- function(S_BBWP_TOT,S_BBWP_NGW,S_BBWP_NSW,S_BBWP_PSW,S_BBWP_NUE,S_BBWP_WB, B_AREA, S_BBWP_GW){

  code = value_min = value_max = NULL
  
  # Load bbwp_parms
  bbwp_parms <- BBWPC::bbwp_parms
  
  # check length of the inputs
  arg.length <- max(length(S_BBWP_TOT),length(S_BBWP_NGW),length(S_BBWP_NSW),length(S_BBWP_PSW),
                    length(S_BBWP_NUE),length(S_BBWP_WB),length(B_AREA), length(S_BBWP_GW))
  
  # check inputs
  checkmate::assert_numeric(S_BBWP_TOT, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_BBWP_NGW, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_BBWP_NSW, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_BBWP_PSW, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_BBWP_NUE, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_BBWP_WB, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_BBWP_GW, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(B_AREA, lower = bbwp_parms[code == "B_AREA", value_min], upper = bbwp_parms[code == "B_AREA", value_max], len = arg.length)
  
  
  # collect data in one data.table
  dt <- data.table(id = 1:arg.length,
                   S_BBWP_NGW = S_BBWP_NGW,
                   S_BBWP_NSW = S_BBWP_NSW,
                   S_BBWP_PSW = S_BBWP_PSW,
                   S_BBWP_NUE = S_BBWP_NUE,
                   S_BBWP_WB = S_BBWP_WB,
                   S_BBWP_GW = S_BBWP_GW,
                   S_BBWP_TOT = S_BBWP_TOT,
                   B_AREA = B_AREA
                  )
  
  # columns with the score of the opportunity indexes
  cols <- c('S_BBWP_TOT','S_BBWP_NGW','S_BBWP_NSW','S_BBWP_PSW','S_BBWP_NUE','S_BBWP_WB', 'S_BBWP_GW')
  
  # calculate area weigthed sum of the field indices
  dt <- dt[,lapply(.SD, stats::weighted.mean, w = B_AREA), .SDcols = cols]
  
  # Round the values
  dt<- dt[, lapply(.SD, round, digits = 0)]
  
  # return output
  return(dt)
}
