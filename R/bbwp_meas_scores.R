#' Evaluate the contribution of agronomic measures to improve soil mand water management
#'
#' Estimate the score for agronomic measures taken to improve soil and water management on agricultural farms.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param M_DRAIN (boolean) is there tube drainage present in the field
#' @param A_P_SG (numeric) 
#' @param B_SLOPE (numeric)
#' @param B_LU_BRP (integer)
#' @param B_LU_BBWP (numeric) The BBWP category used for allocation of measures to BBWP crop categories
#' @param D_WP (numeric) The fraction of the parcel that is surrounded by surface water
#' @param D_OPI_NGW (numeric) the opportunity index (risk x impact) for nitrate leaching to groundwater given field properties
#' @param D_OPI_NSW (numeric) the opportunity index (risk x impact) for nitrate leaching and runoff to surface water given field properties
#' @param D_OPI_PSW (numeric) the opportunity index (risk x impact) for phosphorus leaching and runoff to surface water given field properties
#' @param D_OPI_NUE (numeric) the opportunity index (risk x impact) to improve the efficiency of nitrogen and phosphorus fertilizer use given field properties
#' @param D_OPI_WB (numeric) the opportunity index (risk x impact) to improve the potential to buffer and store water and efficiently use water for plant growth given field properties
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'diary', 'arable', 'tree_nursery', 'bulbs')
#' @param measures (list) the measures planned / done per fields
#' 
#'   
#' @import data.table
#'
#' @export
# calculate the score for a list of measures for one or multiple fields
bbwp_meas_score <- function(B_SOILTYPE_AGR, B_GWL_CLASS,  A_P_SG, B_SLOPE, B_LU_BRP, B_LU_BBWP, M_DRAIN, D_WP,
                            D_OPI_NGW, D_OPI_NSW, D_OPI_PSW, D_OPI_NUE, D_OPI_WB,
                            measures = NULL, sector){
  
  effect_psw = psw_psg_medium = psw_psg_high = effect_nsw = nsw_drains = nsw_gwl_low = nsw_gwl_high = psw_noslope = NULL
  effect_ngw = ngw_grassland = psw_bulbs = D_MEAs_NGW = D_MEAS_NSW = D_MEAS_NUE = effect_nue = D_MEAS_WB = effect_Wb = diary = NULL
  arable = tree_nursery = bulbs = clay = sand = peat = loess = D_MEAS_TOT = id = NULL
  D_MEAS_PSW = D_MEAS_NGW = D_MEAS_PSW = effect_wb = NULL
  
  # check length of the inputs
  arg.length <- max(length(D_OPI_NGW), length(D_OPI_NSW), length(D_OPI_PSW), length(D_OPI_NUE),
                    length(D_OPI_WB),length(B_SOILTYPE_AGR), length(B_GWL_CLASS), length(M_DRAIN),
                    length(A_P_SG), length(B_SLOPE), length(B_LU_BRP),length(B_LU_BBWP),
                    length(D_WP))
  
  # check inputs
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = c('duinzand','dekzand','zeeklei','rivierklei','maasklei',
                                                       'dalgrond','moerige_klei','veen','loess'))
  checkmate::assert_subset(B_GWL_CLASS, choices = c('-', 'GtI','GtII','GtII','GtIII','GtIII','GtIV','GtV','GtV','GtVI','GtVII','GtVIII'))
  checkmate::assert_logical(M_DRAIN,len = arg.length)
  checkmate::assert_numeric(A_P_SG, lower = 0, upper = 120, len = arg.length)
  checkmate::assert_numeric(B_SLOPE, len = arg.length)
  checkmate::assert_integerish(B_LU_BRP, lower = 0, len = arg.length)
  checkmate::assert_integerish(B_LU_BBWP, lower = 0, upper = 9,len = arg.length)
  checkmate::assert_numeric(D_WP, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_OPI_NGW, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_OPI_NSW, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_OPI_PSW, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_OPI_NUE, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_OPI_WB, lower = 0, upper = 1, len = arg.length)
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
    D_MEAS_NGW = NA_real_,
    D_MEAS_NSW = NA_real_,
    D_MEAS_PSE = NA_real_,
    D_MEAS_NUE = NA_real_,
    D_MEAS_WB = NA_real_,
    D_MEAS_TOT = NA_real_
  )
  
  # load, check and update the measures database
  dt.measures <- bbwp_check_meas(measures,eco = FALSE,score = TRUE)
  
  # merge with input
  dt <- merge(dt, dt.measures, by = 'id', all = TRUE)
  
    # Add bonus points for psw
    dt[A_P_SG >= 50 & A_P_SG < 75, effect_psw := effect_psw + psw_psg_medium]
    dt[A_P_SG >= 75, effect_psw := effect_psw + psw_psg_high]
    dt[B_SLOPE <= 2, effect_psw := effect_psw + psw_noslope]
    dt[B_LU_BRP %in% c(176, 964, 965, 967, 968, 970,
                       971, 973, 976, 979, 982, 983,
                       985, 986, 997, 998, 999, 1000,
                       1001, 1002, 1003, 1004, 1005,
                       1006, 1007, 1012, 1015, 1027,
                       1051, 1052), effect_psw := effect_psw + psw_bulbs]
    
    # Add bonus points for nsw
    dt[M_DRAIN == TRUE, effect_nsw := effect_nsw + nsw_drains]
    dt[B_GWL_CLASS %in% c('GtVII','GtVIII'), effect_nsw := effect_nsw + nsw_gwl_low]
    dt[! B_GWL_CLASS %in% c('GtVII','GtVIII'), effect_nsw := effect_nsw + nsw_gwl_high]
    
    # Add bonus points for grassland
    dt[B_LU_BRP %in% c(265, 266, 331, 332, 336,383), effect_ngw := effect_ngw + ngw_grassland]
 
  # set scores to zero when measures are not applicable given the crop type
  
    # columns with the Ecoregelingen ranks
    cols <- c('effect_psw','effect_nsw', 'effect_ngw','effect_wb','effect_nue')
  
    # set first all missing data impacts to 0
    dt[,c(cols) := lapply(.SD, function(x) fifelse(is.na(x),0,x)), .SDcols = cols]
  
    # set the score to zero when not applicable for given crop category
    dt[B_LU_BBWP == 1 & crop_cat1 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 2 & crop_cat2 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 3 & crop_cat3 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 4 & crop_cat4 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 5 & crop_cat5 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 6 & crop_cat6 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 7 & crop_cat7 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 8 & crop_cat8 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 9 & crop_cat9 <= 0, c(cols) := 0]
  
    # set the score to zero when the measure is not applicable
  
      # add columns for the sector to which the farms belong
      fs0 <- c('fdairy','farable','ftree_nursery','fbulbs')
      fs1 <- paste0('f',sector)
      fs2 <- fs0[!fs0 %in% fs1]
      dt[,c(fs1) := 1]
      dt[,c(fs2) := 0]
  
      # estimate whether sector allows applicability
      dt[, fsector := fdairy * dairy + farable * arable + ftree_nursery * tree_nursery + fbulbs * bulbs]
  
      # adapt the score when measure is not applicable
      dt[fsector == 0, c(cols) := 0]
  
      # adapt the score when the soil type limits the applicability of measures
      dt[grepl('klei', B_SOILTYPE_AGR) & clay == FALSE , c(cols) := 0]
      dt[grepl('zand|dal', B_SOILTYPE_AGR) & sand == FALSE , c(cols) := 0]
      dt[grepl('veen', B_SOILTYPE_AGR) & peat == FALSE , c(cols) := 0]
      dt[grepl('loess', B_SOILTYPE_AGR) & loess == FALSE , c(cols) := 0]
  
  
  # add impact score for measure per opportunity index
  dt[, D_MEAS_NGW := D_OPI_NGW * effect_ngw]
  dt[, D_MEAS_NSW := D_OPI_NSW * effect_nsw]
  dt[, D_MEAS_PSW := D_OPI_PSW * effect_psw]
  dt[, D_MEAS_NUE := D_OPI_NUE * effect_nue]
  dt[, D_MEAS_WB := D_OPI_WB * effect_wb]
  
  # columns to be adapted given applicability
  scols <- c('D_MEAS_NGW','D_MEAS_NSW','D_MEAS_PSW','D_MEAS_NUE','D_MEAS_WB','D_MEAS_TOT')
  
  # Calculate total measure score
  dt[, D_MEAS_TOT := (D_MEAS_NGW + D_MEAS_NSW + D_MEAS_PSW + D_MEAS_NUE + D_MEAS_WB ) /  5]
  
  # calculate the total impact of measures on the five opportunity indexes (in units of effectiveness, from -2 to +2 per measure)
  dt.meas <- dt[ ,lapply(.SD, sum), .SDcols = scols, by = 'id']
  
  # replace NA values (when no measures are taken) by 0
  dt.meas[, c(scols) := lapply(.SD,function(x) fifelse(is.na(x), 0, x)), .SDcols = scols]
  
  # it should be possible to fill all requirements with two very effective measures per field, equal to +6 points per field
  # so the change in score can increase from 0 to 1 when 6 effectiveness unit points via measures are collected
  # so each effectiveness unit point is equal to 1/6 score units
  dt.meas[ ,c(scols) := lapply(.SD,function(x) x * (1 / 4)), .SDcols = scols]
  
  # order
  setorder(dt.meas, id)
  
  # extract value
  out <- dt.meas[, mget(scols)]
  
  # return value
  return(out)
}



