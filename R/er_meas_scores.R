#' Rank the suitability of agronomic measures to improve soil, climate, biodiversity and water management for a given field
#'
#' Estimate the Ecoregelingen score for agronomic measures to improve soil and water management on agricultural farms. 
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
er_meas_rank <- function(B_SOILTYPE_AGR, B_GWL_CLASS,  A_P_SG, B_SLOPE, B_LU_BRP, M_DRAIN, D_WP,
                           D_OPI_NGW, D_OPI_NSW, D_OPI_PSW, D_OPI_NUE, D_OPI_WB,
                           available_measures, sector){
  
  # add visual bindings
  
  # check length of the inputs
  arg.length <- 2
  
  # check inputs
  
  # collect data in one data.table
  dt <- data.table(
    id = 1:arg.length,
    B_SOILTYPE_AGR = 'dekzand',
    B_GWL_CLASS = 'GtIII',
    A_P_SG = 0.15,
    B_SLOPE = 1.5,
    B_LU_BRP = c(265,2005),
    B_LU_BBWP = c(1,4),
    M_DRAIN = TRUE
  )
  
  #dt.measures <- as.data.table(readxl::read_xlsx('D:/ROSG/1845.N.21. opschaling bbwp/02 data/220517 measures total versie 4.xlsx'))
  dt.measures <- 5
  
  # merge all measures to the given fields
  dt <- as.data.table(merge.data.frame(dt, dt.measures, all = TRUE))
  
  # # Add bonus points
  # dt[A_P_SG >= 50 & A_P_SG < 75, effect_psw := effect_psw + psw_psg_medium]
  # dt[A_P_SG >= 75, effect_psw := effect_psw + psw_psg_high]
  # dt[M_DRAIN == TRUE, effect_nsw := effect_nsw + nsw_drains]
  # dt[B_GWL_CLASS %in% c('GtVII','GtVIII'), effect_nsw := effect_nsw + nsw_gwl_low]
  # dt[! B_GWL_CLASS %in% c('GtVII','GtVIII'), effect_nsw := effect_nsw + nsw_gwl_high]
  # dt[B_SLOPE == FALSE, effect_psw := effect_psw + psw_noslope]
  # dt[B_LU_BRP %in% c(265, 266, 331, 332, 336,383), effect_ngw := effect_ngw + ngw_grassland]
  # dt[B_LU_BRP %in% c(176, 964, 965, 967, 968, 970,
  #                    971, 973, 976, 979, 982, 983,
  #                    985, 986, 997, 998, 999, 1000,
  #                    1001, 1002, 1003, 1004, 1005,
  #                    1006, 1007, 1012, 1015, 1027,
  #                    1051, 1052), effect_psw := effect_psw + psw_bulbs]
  # 
  # # Change back opi from oppurtunity to risk
  # dt[, D_MEAS_NGW := (100 - D_OPI_NGW) * effect_ngw]
  # dt[, D_MEAS_NSW := (100 - D_OPI_NSW) * effect_nsw]
  # dt[, D_MEAS_PSW := (100 - D_OPI_PSW) * effect_psw]
  # dt[, D_MEAS_NUE := (100 - D_OPI_NUE) * effect_nue]
  # dt[, D_MEAS_WB := (100 - D_OPI_WB) * effect_wb]
  # 
  # # columns to be adapted given applicability
  # scols <- c('D_MEAS_NGW','D_MEAS_NSW','D_MEAS_PSW','D_MEAS_NUE','D_MEAS_WB')
  # 
  # # rank is zero when measures are not applicable given the farm type
  # if('diary' %in% sector) {
  #   dt[diary == FALSE, c(scols) := lapply(.SD,function(x) x * 0.1), .SDcols = scols]
  # }
  # if('arable' %in% sector) {
  #   dt[arable == FALSE, c(scols) := lapply(.SD,function(x) x * 0.1), .SDcols = scols]
  # }
  # #if('vollegrondsgroente' %in% sector){dt[tp_vgg == 0,c(scols) := lapply(.SD,function(x) x * 0.1),.SDcols = scols]}
  # if('tree_nursery' %in% sector) {
  #   dt[tree_nursery == FALSE, c(scols) := lapply(.SD,function(x) x * 0.1), .SDcols = scols]
  # }
  # if('bulbs' %in% sector) {
  #   dt[bulbs == FALSE, c(scols) := lapply(.SD,function(x) x * 0.1), .SDcols = scols]
  # }
  # 
  # # rank is zero when measure is not applicable depending on soil type
  # dt[grepl('klei', B_SOILTYPE_AGR) & clay == FALSE , c(scols) := 0]
  # dt[grepl('zand|dal', B_SOILTYPE_AGR) & sand == FALSE , c(scols) := 0]
  # dt[grepl('veen', B_SOILTYPE_AGR) & peat == FALSE , c(scols) := 0]
  # dt[grepl('loess', B_SOILTYPE_AGR) & loess == FALSE , c(scols) := 0]
  # 
  # # Calculate total measure score
  # dt[, D_MEAS_TOT := (D_MEAS_NGW + D_MEAS_NSW + D_MEAS_PSW + D_MEAS_NUE + D_MEAS_WB ) /  5 - effect_costs * 0.01]
  # 
  # # Loop through each field
  # list.measures <- list()
  # for (i in 1:arg.length) {
  #   
  #   list.field <- list()
  #   
  #   # Get the overall top measures
  #   this.dt.tot <- dt[id == i & D_MEAS_TOT > 0, ]
  #   top.tot <- this.dt.tot[order(-D_MEAS_TOT)]$bbwp_id[1:5]
  #   list.field$total <- data.table(
  #     top = 'total',
  #     measure = top.tot,
  #     rank = 1:length(top.tot)
  #   )
  #   
  #   # Get the top nsw measures
  #   this.dt.ngw <- dt[id == i & D_MEAS_NGW > 0, ]
  #   top.ngw <- this.dt.ngw[order(-D_MEAS_NGW)]$bbwp_id[1:5]
  #   list.field$ngw <- data.table(
  #     top = 'ngw',
  #     measure = top.ngw,
  #     rank = 1:length(top.ngw)
  #   )
  #   
  #   # Get the top nsw measures
  #   this.dt.nsw <- dt[id == i & D_MEAS_NSW > 0, ]
  #   top.nsw <- this.dt.nsw[order(-D_MEAS_NSW)]$bbwp_id[1:5]
  #   list.field$nsw <- data.table(
  #     top = 'nsw',
  #     measure = top.nsw,
  #     rank = 1:length(top.nsw)
  #   )
  #   
  #   # Get the top psw measures
  #   this.dt.psw <- dt[id == i & D_MEAS_PSW > 0, ]
  #   top.psw <- this.dt.psw[order(-D_MEAS_PSW)]$bbwp_id[1:5]
  #   list.field$psw <- data.table(
  #     top = 'psw',
  #     measure = top.psw,
  #     rank = 1:length(top.psw)
  #   )
  #   
  #   # Get the top nue measures
  #   this.dt.nue <- dt[id == i & D_MEAS_NUE > 0, ]
  #   top.nue <- this.dt.nue[order(-D_MEAS_NUE)]$bbwp_id[1:5]
  #   list.field$nue <- data.table(
  #     top = 'nue',
  #     measure = top.nue,
  #     rank = 1:length(top.nue)
  #   )
  #   
  #   # Get the top wb measures
  #   this.dt.wb <- dt[id == i & D_MEAS_WB > 0, ]
  #   top.wb <- this.dt.psw[order(-D_MEAS_WB)]$bbwp_id[1:5]
  #   list.field$wb <- data.table(
  #     top = 'wb',
  #     measure = top.wb,
  #     rank = 1:length(top.wb)
  #   )
  #   
  #   list.measures[[i]] <- stats::na.omit(rbindlist(list.field))
  # }
  # 
  # # return value
  # dt.measures <- data.table::rbindlist(list.measures)
  # return(dt.measures)
}

#' Evaluate the contribution of agronomic measures to improve soil mand water management
#'
#' Estimate the Ecoregeling score for agronomic measures taken to improve soil and water management on agricultural farms.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BRP (numeric) The crop type (conform BRP coding, preferable the most frequent crop on the field)
#' @param B_LU_BBWP (numeric) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param M_DRAIN (boolean) is there tube drainage present in the field
#' @param A_P_SG (numeric) 
#' @param B_SLOPE (boolean)
#' @param B_LU_BRP (integer)
#' @param D_WP (numeric) The fraction of the parcel that is surrounded by surface water
#' @param D_OPI_NGW (numeric) the opportunity index (risk x impact) for nitrate leaching to groundwater given field properties
#' @param D_OPI_NSW (numeric) the opportunity index (risk x impact) for nitrate leaching and runoff to surface water given field properties
#' @param D_OPI_PSW (numeric) the opportunity index (risk x impact) for phosphorus leaching and runoff to surface water given field properties
#' @param D_OPI_NUE (numeric) the opportunity index (risk x impact) to improve the efficiency of nitrogen and phosphorus fertilizer use given field properties
#' @param D_OPI_WB (numeric) the opportunity index (risk x impact) to improve the potential to buffer and store water and efficiently use water for plant growth given field properties
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'diary', 'arable', 'tree_nursery', 'bulbs')
#' @param measures (list) the measures planned / done per fields
#'   
#' @import data.table
#'
#' @export
# calculate the score for a list of measures for one or multiple fields
er_meas_score <- function(B_SOILTYPE_AGR, B_GWL_CLASS,  A_P_SG, B_SLOPE, 
                          B_LU_BRP, B_LU_BBWP, M_DRAIN, D_WP,
                            D_OPI_NGW, D_OPI_NSW, D_OPI_PSW, D_OPI_NUE, D_OPI_WB,
                            measures, sector){
  
  # add visual bindings
 
  # check length of the inputs
  arg.length <- 1
  
  # check inputs
  
  
  # collect data in one data.table
  dt <- data.table(
    id = 1:arg.length,
    B_SOILTYPE_AGR = 'dekzand',
    B_GWL_CLASS = 'GtIII',
    A_P_SG = 0.15,
    B_SLOPE = 1.5,
    B_LU_BRP = 265,
    B_LU_BBWP = 1,
    M_DRAIN = TRUE
  )
  
  #dt.measures <- as.data.table(readxl::read_xlsx('D:/ROSG/1845.N.21. opschaling bbwp/02 data/220517 measures total versie 4.xlsx'))
  
  # # merge measures to the field
  # dt <- merge(dt, dt.measures, by = 'id', all = TRUE)
  # 
  # # Add bonus points
  # dt[A_P_SG >= 50 & A_P_SG < 75, effect_psw := effect_psw + psw_psg_medium]
  # dt[A_P_SG >= 75, effect_psw := effect_psw + psw_psg_high]
  # dt[M_DRAIN == TRUE, effect_nsw := effect_nsw + nsw_drains]
  # dt[B_GWL_CLASS %in% c('GtVII','GtVIII'), effect_nsw := effect_nsw + nsw_gwl_low]
  # dt[! B_GWL_CLASS %in% c('GtVII','GtVIII'), effect_nsw := effect_nsw + nsw_gwl_high]
  # dt[B_SLOPE == FALSE, effect_psw := effect_psw + psw_noslope]
  # dt[B_LU_BRP %in% c(265, 266, 331, 332, 336,383), effect_ngw := effect_ngw + ngw_grassland]
  # dt[B_LU_BRP %in% c(176, 964, 965, 967, 968, 970,
  #                    971, 973, 976, 979, 982, 983,
  #                    985, 986, 997, 998, 999, 1000,
  #                    1001, 1002, 1003, 1004, 1005,
  #                    1006, 1007, 1012, 1015, 1027,
  #                    1051, 1052), effect_psw := effect_psw + psw_bulbs]
  # 
  # # add impact score for measure per opportunity index
  # dt[, D_MEAS_NGW := D_OPI_NGW * effect_ngw]
  # dt[, D_MEAS_NSW := D_OPI_NSW * effect_nsw]
  # dt[, D_MEAS_PSW := D_OPI_PSW * effect_psw]
  # dt[, D_MEAS_NUE := D_OPI_NUE * effect_nue]
  # dt[, D_MEAS_WB := D_OPI_WB * effect_wb]
  # 
  # # columns to be adapted given applicability
  # scols <- c('D_MEAS_NGW','D_MEAS_NSW','D_MEAS_PSW','D_MEAS_NUE','D_MEAS_WB','D_MEAS_TOT')
  # 
  # # rank is zero when measures are not applicable given the farm type
  # if('diary' %in% sector) {
  #   dt[diary == FALSE, c(scols) := lapply(.SD,function(x) x * 0.1), .SDcols = scols]
  # }
  # if('arable' %in% sector) {
  #   dt[arable == FALSE, c(scols) := lapply(.SD,function(x) x * 0.1), .SDcols = scols]
  # }
  # #if('vollegrondsgroente' %in% sector){dt[tp_vgg == 0,c(scols) := lapply(.SD,function(x) x * 0.1),.SDcols = scols]}
  # if('tree_nursery' %in% sector) {
  #   dt[tree_nursery == FALSE, c(scols) := lapply(.SD,function(x) x * 0.1), .SDcols = scols]
  # }
  # if('bulbs' %in% sector) {
  #   dt[bulbs == FALSE, c(scols) := lapply(.SD,function(x) x * 0.1), .SDcols = scols]
  # }
  # 
  # # rank is zero when measure is not applicable depending on soil type
  # dt[grepl('klei', B_SOILTYPE_AGR) & clay == FALSE , c(scols) := 0]
  # dt[grepl('zand|dal', B_SOILTYPE_AGR) & sand == FALSE , c(scols) := 0]
  # dt[grepl('veen', B_SOILTYPE_AGR) & peat == FALSE , c(scols) := 0]
  # dt[grepl('loess', B_SOILTYPE_AGR) & loess == FALSE , c(scols) := 0]
  # 
  # # Calculate total measure score
  # dt[, D_MEAS_TOT := (D_MEAS_NGW + D_MEAS_NSW + D_MEAS_PSW + D_MEAS_NUE + D_MEAS_WB ) /  5]
  # 
  # # calculate the total impact of measures on the five opportunity indexes (in units of effectiveness, from -2 to +2 per measure)
  # dt.meas <- dt[ ,lapply(.SD, sum), .SDcols = scols, by = 'id']
  # 
  # # replace NA values (when no measures are taken) by 0
  # dt.meas[, c(scols) := lapply(.SD,function(x) fifelse(is.na(x), 0, x)), .SDcols = scols]
  # 
  # # it should be possible to fill all requirements with two very effective measures per field, equal to +6 points per field
  # # so the change in score can increase from 0 to 1 when 6 effectiveness unit points via measures are collected
  # # so each effectiveness unit point is equal to 1/6 score units
  # dt.meas[ ,c(scols) := lapply(.SD,function(x) x * (1 / 4)), .SDcols = scols]
  # 
  # # order
  # setorder(dt.meas, id)
  # 
  # # extract value
  # out <- dt.meas[, mget(scols)]
  out = 5
  # return value
  return(out)
}
