#' Calculate the total score of five opportunity indicators conform Ecoregelingen Scoring
#'
#' Estimate the potential to contribute to agronomic and environmental challenges in a region given aims for soil quality, water quality, climate, biodiversity and landscape
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BRP (integer)
#' @param B_LU_BBWP (numeric) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_CT_SOIL (numeric) the target value for soil quality conform Ecoregeling scoring
#' @param B_CT_WATER (numeric) the target value for water quality conform Ecoregeling scoring
#' @param B_CT_CLIMATE (numeric) the target value for climate conform Ecoregeling scoring
#' @param B_CT_BIO (numeric) the target value for biodiversity conform Ecoregeling scoring
#' @param B_CT_LANDSCAPE (numeric) the target value for landscape quality conform Ecoregeling scoring
#' @param measures (list) the measures planned / done per fields (measurement nr)
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'melkveehouderij','akkerbouw','vollegrondsgroente','boomteelt','bollen','veehouderij','overig')
#'    
#' @import data.table
#'
#' @export
# calculate the opportunities for a set of fields
er_field_scores <- function(B_SOILTYPE_AGR, B_LU_BRP, B_LU_BBWP,
                            B_CT_SOIL, B_CT_WATER,B_CT_CLIMATE,B_CT_BIO,B_CT_LANDSCAPE, 
                            measures, sector){
  
  # add visual bindings
 
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(B_LU_BRP),length(B_LU_BBWP))
  
  # check inputs
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = c('duinzand','dekzand','zeeklei','rivierklei','maasklei',
                                                       'dalgrond','moerige_klei','veen','loess'))
  checkmate::assert_integerish(B_LU_BRP, lower = 0, len = arg.length)
  checkmate::assert_integerish(B_LU_BBWP, lower = 0, len = arg.length)
  checkmate::assert_numeric(B_CT_SOIL, lower = 0, upper = 1000, min.len = 1)
  checkmate::assert_numeric(B_CT_WATER, lower = 0, upper = 1000, min.len = 1)
  checkmate::assert_numeric(B_CT_CLIMATE, lower = 0, upper = 1000,min.len = 1)
  checkmate::assert_numeric(B_CT_BIO, lower = 0, upper = 1000, min.len = 1)
  checkmate::assert_numeric(B_CT_LANDSCAPE, lower = 0, upper = 1000,min.len = 1)
  checkmate::assert_list(measures)
  
  # collect data in one data.table
  dt <- data.table(
    id = 1:arg.length,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    B_LU_BRP = B_LU_BRP,
    B_LU_BBWP = B_LU_BBWP,
    B_CT_SOIL = B_CT_SOIL, 
    B_CT_WATER = B_CT_WATER,
    B_CT_CLIMATE = B_CT_CLIMATE,
    B_CT_BIO = B_CT_BIO,
    B_CT_LANDSCAPE = B_CT_LANDSCAPE,
  )
  
  # what is the opportunity to contribute to environmental challenges
  dt[, D_OPI_SOIL := 1]
  dt[, D_OPI_WATER := 1]
  dt[, D_OPI_CLIMATE := 1]
  dt[, D_OPI_BIO := 1]
  dt[, D_OPI_LANDSCAPE := 1]
  
  # add the generic farm score as baseline
  
  
  # add list of measures
  
  # calculate the change in opportunity indexes given the measures taken
  
  # column names for impact of measures on the five indexes (do not change order)
  mcols <- c('D_MEAS_NGW', 'D_MEAS_NSW', 'D_MEAS_PSW', 'D_MEAS_NUE', 'D_MEAS_WB', 'D_MEAS_TOT')
  
  # estimate these indexes
  nr.measures <- length(Filter(function(x) dim(x)[1] > 0, measures))
  if (nr.measures > 0 ) {
    dt[,c(mcols) := bbwp_meas_score(
      B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR,
      B_GWL_CLASS = dt$B_GWL_CLASS,
      A_P_SG = dt$A_P_SG,
      B_SLOPE = dt$B_SLOPE,
      B_LU_BRP = dt$B_LU_BRP,
      M_DRAIN = dt$M_DRAIN,
      D_WP = dt$D_WP,
      D_OPI_NGW = dt$D_OPI_NGW,
      D_OPI_NSW = dt$D_OPI_NSW,
      D_OPI_PSW = dt$D_OPI_PSW,
      D_OPI_NUE = dt$D_OPI_NUE,
      D_OPI_WB = dt$D_OPI_WB,
      measures = measures,
      sector = sector
    )]
  } else {
    dt[, c('D_MEAS_NGW','D_MEAS_NSW','D_MEAS_PSW','D_MEAS_NUE','D_MEAS_WB','D_MEAS_TOT') := 0]
  }
  
  
  # update the field score with measures
  dt[,D_OPI_NGW := 1 - pmax(0, D_OPI_NGW - D_MEAS_NGW)]
  dt[,D_OPI_NSW := 1 - pmax(0, D_OPI_NSW - D_MEAS_NSW)]
  dt[,D_OPI_PSW := 1 - pmax(0, D_OPI_PSW - D_MEAS_PSW)]
  dt[,D_OPI_NUE := 1 - pmax(0, D_OPI_NUE - D_MEAS_NUE)]
  dt[,D_OPI_WB :=  1 - pmax(0, D_OPI_WB - D_MEAS_WB)]
  
  # Convert form 0-1 to 0-100
  dt[,D_OPI_NGW := 100 * D_OPI_NGW]
  dt[,D_OPI_NSW := 100 * D_OPI_NSW]
  dt[,D_OPI_PSW := 100 * D_OPI_PSW]
  dt[,D_OPI_NUE := 100 * D_OPI_NUE]
  dt[,D_OPI_WB :=  100 * D_OPI_WB]
  
  # calculate the integrative opportunity index (risk times impact)
  dt[,D_OPI_TOT := (D_OPI_NGW * wf(D_OPI_NGW, type="score") + D_OPI_NSW * wf(D_OPI_NSW, type="score") + D_OPI_PSW * wf(D_OPI_PSW, type="score") + D_OPI_NUE * wf(D_OPI_NUE, type="score") + D_OPI_WB * wf(D_OPI_WB, type="score")) /
       (wf(D_OPI_NGW, type="score") + wf(D_OPI_NSW, type="score") +  wf(D_OPI_PSW, type="score") +  wf(D_OPI_NUE, type="score") +  wf(D_OPI_WB, type="score"))]
  
  # order the fields
  setorder(dt, id)
  
  # extract value
  value <- dt[,mget(c('D_OPI_NGW','D_OPI_NSW','D_OPI_PSW','D_OPI_NUE','D_OPI_WB','D_OPI_TOT'))]
  
  # Round the values
  value <- value[, lapply(.SD, round, digits = 0)]
  
  # return value
  return(value)
}
