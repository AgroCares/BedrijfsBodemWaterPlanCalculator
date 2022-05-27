#' Rank the suitability of agronomic measures to improve soil and water management for a given field
#'
#' Estimate the score for agronomic measures to improve soil and water management on agricultural farms. 
#' And send an ordered list back of the most suitable measures.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param A_P_SG (numeric) 
#' @param B_SLOPE (boolean)
#' @param B_LU_BRP (integer)
#' @param M_DRAIN (boolean) is there tube drainage present in the field
#' @param D_WP (numeric) The fraction of the parcel that is surrounded by surface water
#' @param D_OPI_NGW (numeric) the opportunity index (risk x impact) for nitrate leaching to groundwater given field properties
#' @param D_OPI_NSW (numeric) the opportunity index (risk x impact) for nitrate leaching and runoff to surface water given field properties
#' @param D_OPI_PSW (numeric) the opportunity index (risk x impact) for phosphorus leaching and runoff to surface water given field properties
#' @param D_OPI_NUE (numeric) the opportunity index (risk x impact) to improve the efficiency of nitrogen and phosphorus fertilizer use given field properties
#' @param D_OPI_WB (numeric) the opportunity index (risk x impact) to improve the potential to buffer and store water and efficiently use water for plant growth given field properties
#' @param available_measures (data.table) table with the properties of the available measures
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'diary', 'arable', 'tree_nursery', 'bulbs')
#' 
#'   
#' @import data.table
#'
#' @export
# rank the measures given their effectiveness to improve the sustainability of the farm
bbwp_meas_rank <- function(B_SOILTYPE_AGR, B_GWL_CLASS,  A_P_SG, B_SLOPE, B_LU_BRP, M_DRAIN, D_WP,
                           D_OPI_NGW, D_OPI_NSW, D_OPI_PSW, D_OPI_NUE, D_OPI_WB,
                           available_measures, sector){
  
  effect_psw = psw_psg_medium = psw_psg_high = effect_nsw = nsw_drains = nsw_gwl_low = nsw_gwl_high = psw_noslope = effect_ngw = NULL
  ngw_grassland = psw_bulbs = D_MEAS_NGW = D_MEAS_NSW = D_MEAS_NUE = effect_nue = D_MEAS_WB = effect_wb = diary = arable = tree_nursery = bulbs = NULL
  clay = sand= peat = loess = D_MEAS_TOT = effect_costs = id = D_MEAS_PSW = NULL
  
  # check length of the inputs
  arg.length <- max(length(D_OPI_NGW), length(D_OPI_NSW), length(D_OPI_PSW), length(D_OPI_NUE),
                    length(D_OPI_WB), length(B_SOILTYPE_AGR), length(B_GWL_CLASS), length(M_DRAIN),
                    length(A_P_SG), length(B_SLOPE), length(B_LU_BRP),
                    length(D_WP))
  
  # check inputs
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = c('duinzand','dekzand','zeeklei','rivierklei','maasklei',
                                                       'dalgrond','moerige_klei','veen','loess'))
  checkmate::assert_subset(B_GWL_CLASS, choices = c('-', 'GtI','GtII','GtII','GtIII','GtIII','GtIV', 'GtV','GtVI','GtVII','GtVIII'))
  checkmate::assert_logical(M_DRAIN)
  checkmate::assert_numeric(A_P_SG, lower = 0, upper = 120)
  checkmate::assert_logical(B_SLOPE)
  checkmate::assert_integerish(B_LU_BRP, lower = 0)
  checkmate::assert_numeric(D_WP, lower = 0, upper = 100)
  checkmate::assert_numeric(D_OPI_NGW, lower = 0, upper = 100)
  checkmate::assert_numeric(D_OPI_NSW, lower = 0, upper = 100)
  checkmate::assert_numeric(D_OPI_PSW, lower = 0, upper = 100)
  checkmate::assert_numeric(D_OPI_NUE, lower = 0, upper = 100)
  checkmate::assert_numeric(D_OPI_WB, lower = 0, upper = 100)
  checkmate::assert_data_table(available_measures)
  checkmate::assert_subset(sector, choices = c('diary', 'arable', 'tree_nursery', 'bulbs'))
  
  # collect data in one data.table
  dt <- data.table(
    id = 1:arg.length,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    B_GWL_CLASS = B_GWL_CLASS,
    A_P_SG = A_P_SG,
    B_SLOPE = B_SLOPE,
    B_LU_BRP = B_LU_BRP,
    M_DRAIN = M_DRAIN,
    D_WP = D_WP,
    D_OPI_NGW = D_OPI_NGW,
    D_OPI_NSW = D_OPI_NSW,
    D_OPI_PSW = D_OPI_PSW,
    D_OPI_NUE = D_OPI_NUE,
    D_OPI_WB = D_OPI_WB,
    value = NA_real_
  )
  dt <- as.data.table(merge.data.frame(dt, available_measures, all = TRUE))
  
  # Add bonus points
  dt[A_P_SG >= 50 & A_P_SG < 75, effect_psw := effect_psw + psw_psg_medium]
  dt[A_P_SG >= 75, effect_psw := effect_psw + psw_psg_high]
  dt[M_DRAIN == TRUE, effect_nsw := effect_nsw + nsw_drains]
  dt[B_GWL_CLASS %in% c('GtVII','GtVIII'), effect_nsw := effect_nsw + nsw_gwl_low]
  dt[! B_GWL_CLASS %in% c('GtVII','GtVIII'), effect_nsw := effect_nsw + nsw_gwl_high]
  dt[B_SLOPE == FALSE, effect_psw := effect_psw + psw_noslope]
  dt[B_LU_BRP %in% c(265, 266, 331, 332, 336,383), effect_ngw := effect_ngw + ngw_grassland]
  dt[B_LU_BRP %in% c(176, 964, 965, 967, 968, 970,
                     971, 973, 976, 979, 982, 983,
                     985, 986, 997, 998, 999, 1000,
                     1001, 1002, 1003, 1004, 1005,
                     1006, 1007, 1012, 1015, 1027,
                     1051, 1052), effect_psw := effect_psw + psw_bulbs]
  
  # Change back opi from oppurtunity to risk
  dt[, D_MEAS_NGW := (100 - D_OPI_NGW) * effect_ngw]
  dt[, D_MEAS_NSW := (100 - D_OPI_NSW) * effect_nsw]
  dt[, D_MEAS_PSW := (100 - D_OPI_PSW) * effect_psw]
  dt[, D_MEAS_NUE := (100 - D_OPI_NUE) * effect_nue]
  dt[, D_MEAS_WB := (100 - D_OPI_WB) * effect_wb]
  
  # columns to be adapted given applicability
  scols <- c('D_MEAS_NGW','D_MEAS_NSW','D_MEAS_PSW','D_MEAS_NUE','D_MEAS_WB')
  
  # rank is zero when measures are not applicable given the farm type
  if('diary' %in% sector) {
    dt[diary == FALSE, c(scols) := lapply(.SD,function(x) x * 0.1), .SDcols = scols]
  }
  if('arable' %in% sector) {
    dt[arable == FALSE, c(scols) := lapply(.SD,function(x) x * 0.1), .SDcols = scols]
  }
  #if('vollegrondsgroente' %in% sector){dt[tp_vgg == 0,c(scols) := lapply(.SD,function(x) x * 0.1),.SDcols = scols]}
  if('tree_nursery' %in% sector) {
    dt[tree_nursery == FALSE, c(scols) := lapply(.SD,function(x) x * 0.1), .SDcols = scols]
  }
  if('bulbs' %in% sector) {
    dt[bulbs == FALSE, c(scols) := lapply(.SD,function(x) x * 0.1), .SDcols = scols]
  }
  
  # rank is zero when measure is not applicable depending on soil type
  dt[grepl('klei', B_SOILTYPE_AGR) & clay == FALSE , c(scols) := 0]
  dt[grepl('zand|dal', B_SOILTYPE_AGR) & sand == FALSE , c(scols) := 0]
  dt[grepl('veen', B_SOILTYPE_AGR) & peat == FALSE , c(scols) := 0]
  dt[grepl('loess', B_SOILTYPE_AGR) & loess == FALSE , c(scols) := 0]
  
  # Calculate total measure score
  dt[, D_MEAS_TOT := (D_MEAS_NGW + D_MEAS_NSW + D_MEAS_PSW + D_MEAS_NUE + D_MEAS_WB ) /  5 - effect_costs * 0.01]
  
  # Loop through each field
  setorder(dt, id)
  list.measures <- list()
  for (i in 1:arg.length) {
    
    list.field <- list()
    
    # Get the overall top measures
    this.dt.tot <- dt[id == i & D_MEAS_TOT > 0, ]
    top.tot <- this.dt.tot[order(-D_MEAS_TOT)]$bbwp_id[1:5]
    list.field$top <- na.omit(top.tot)
    
    # Get the top nsw measures
    this.dt.ngw <- dt[id == i & D_MEAS_NGW > 0, ]
    top.ngw <- this.dt.ngw[order(-D_MEAS_NGW)]$bbwp_id[1:5]
    list.field$top_ngw <- na.omit(top.ngw)
    
    # Get the top nsw measures
    this.dt.nsw <- dt[id == i & D_MEAS_NSW > 0, ]
    top.nsw <- this.dt.nsw[order(-D_MEAS_NSW)]$bbwp_id[1:5]
    list.field$top_nsw <- na.omit(top.nsw)
    
    # Get the top psw measures
    this.dt.psw <- dt[id == i & D_MEAS_PSW > 0, ]
    top.psw <- this.dt.psw[order(-D_MEAS_PSW)]$bbwp_id[1:5]
    list.field$top_psw <- na.omit(top.psw)
    
    # Get the top nue measures
    this.dt.nue <- dt[id == i & D_MEAS_NUE > 0, ]
    top.nue <- this.dt.nue[order(-D_MEAS_NUE)]$bbwp_id[1:5]
    list.field$top_nue <- na.omit(top.nue)
    
    # Get the top wb measures
    this.dt.wb <- dt[id == i & D_MEAS_WB > 0, ]
    top.wb <- this.dt.psw[order(-D_MEAS_WB)]$bbwp_id[1:5]
    list.field$top_wb <- na.omit(top.wb)
    
    list.measures[[i]] <- list.field
  }
  
  # return value
  return(list.measures)
}