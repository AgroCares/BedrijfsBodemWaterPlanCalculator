#' Evaluate the contribution of agronomic measures to improve soil mand water management
#'
#' Estimate the score for agronomic measures taken to improve soil and water management on agricultural farms.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param M_DRAIN (boolean) is there tube drainage present in the field
#' @param A_P_CC (numeric) The plant available P content, measured via 0.01M CaCl2 extraction (mg / kg)
#' @param A_P_AL (numeric) The plant extractable P content, measured via ammonium lactate extraction (mg / kg)
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#' @param B_LU_BBWP (numeric) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param D_SA_W (numeric) The wet perimeter index of the field, fraction that field is surrounded by water
#' @param D_OPI_NGW (numeric) the opportunity index (risk x impact) for nitrate leaching to groundwater given field properties
#' @param D_OPI_NSW (numeric) the opportunity index (risk x impact) for nitrate leaching and runoff to surface water given field properties
#' @param D_OPI_PSW (numeric) the opportunity index (risk x impact) for phosphorus leaching and runoff to surface water given field properties
#' @param D_OPI_NUE (numeric) the opportunity index (risk x impact) to improve the efficiency of nitrogen and phosphorus fertilizer use given field properties
#' @param D_OPI_WB (numeric) the opportunity index (risk x impact) to improve the potential to buffer and store water and efficiently use water for plant growth given field properties
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'dairy', 'arable', 'tree_nursery', 'bulbs')
#' @param measures (data.table) the measures planned / done per fields
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'dairy', 'arable', 'tree_nursery', 'bulbs')
#' @param B_LS_HYDROCAT (character) Landscape category for differentiating effect of measures on water buffering.
#' (options: "hoge_gronden", "flanken", "beekdalen", "lokale_laagtes", "polders")
#' 
#'   
#' @import data.table
#'
#' @export
# calculate the score for a list of measures for one or multiple fields
bbwp_meas_score <- function(B_SOILTYPE_AGR, B_GWL_CLASS,  A_P_CC,A_P_AL, B_SLOPE_DEGREE, B_LU_BBWP, B_AER_CBS,
                            M_DRAIN, D_SA_W,
                            D_OPI_NGW, D_OPI_NSW, D_OPI_PSW, D_OPI_NUE, D_OPI_WB,
                            measures = NULL, sector, B_LS_HYDROCAT){
  
  # add visual bindings
  effect_psw = psw_psg_medium = psw_psg_high = effect_nsw = nsw_drains = nsw_gwl_low = nsw_gwl_high = psw_noslope = NULL
  effect_ngw = ngw_grassland = psw_bulbs = D_MEAs_NGW = D_MEAS_NSW = D_MEAS_NUE = effect_nue = D_MEAS_WB = effect_Wb = diary = nodrains = NULL
  arable = tree_nursery = bulbs = clay = sand = peat = loess = D_MEAS_TOT = id = NULL
  D_MEAS_PSW = D_MEAS_NGW = D_MEAS_PSW = effect_wb = NULL
  nc1 = nc2 = nc3 = nc4 = nc5 = nc6 = nc7 = nc8 = nc9 = nc10 = nc11 = nc12 = NULL
  fsector = fdairy = dairy = farable = arable = ftree_nursery = tree_nursery = fbulbs = bulbs = NULL
  oid = bbwp_id = psw_psg_low = NULL
  code = value_min = value_max =  choices = NULL
  hoge_gronden = flanken = beekdalen = lokale_laagtes = polders = NULL
  
  # Load bbwp_parms
  bbwp_parms <- BBWPC::bbwp_parms
  
  # check length of the inputs
  arg.length <- max(length(D_OPI_NGW), length(D_OPI_NSW), length(D_OPI_PSW), length(D_OPI_NUE),
                    length(D_OPI_WB),length(B_SOILTYPE_AGR), length(B_GWL_CLASS), length(B_AER_CBS),length(M_DRAIN),
                    length(A_P_CC),length(A_P_AL), length(B_SLOPE_DEGREE), length(B_LU_BBWP),
                    length(D_SA_W))
  
  # reformat B_AER_CBS
  B_AER_CBS <- bbwp_format_aer(B_AER_CBS)
  
  # check inputs
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unlist(bbwp_parms[code == "B_SOILTYPE_AGR", choices]))
  checkmate::assert_subset(B_GWL_CLASS, choices = unlist(bbwp_parms[code == "B_GWL_CLASS", choices]))
  checkmate::assert_subset(B_LU_BBWP, choices = unlist(bbwp_parms[code == "B_LU_BBWP", choices]))
  checkmate::assert_character(B_LU_BBWP, len = arg.length)
  checkmate::assert_logical(M_DRAIN,len = arg.length)
  checkmate::assert_numeric(A_P_CC, lower = bbwp_parms[code == "A_P_CC", value_min], upper = bbwp_parms[code == "A_P_CC", value_max],len = arg.length)
  checkmate::assert_numeric(A_P_AL, lower = bbwp_parms[code == "A_P_AL", value_min], upper = bbwp_parms[code == "A_P_AL", value_max],len = arg.length)
  checkmate::assert_numeric(B_SLOPE_DEGREE, lower = bbwp_parms[code == "B_SLOPE_DEGREE", value_min], upper = bbwp_parms[code == "B_SLOPE_DEGREE", value_max],len = arg.length)
  checkmate::assert_numeric(D_SA_W, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_OPI_NGW, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_OPI_NSW, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_OPI_PSW, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_OPI_NUE, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_OPI_WB, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_subset(sector, choices = c('dairy', 'arable', 'tree_nursery', 'bulbs'))

  # collect data in one data.table
  dt <- data.table(id = 1:arg.length,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_GWL_CLASS = B_GWL_CLASS,
                   A_P_CC = A_P_CC,
                   A_P_AL = A_P_AL,
                   B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                   B_LU_BBWP = B_LU_BBWP,
                   B_AER_CBS = B_AER_CBS,
                   M_DRAIN = M_DRAIN,
                   D_SA_W = D_SA_W,
                   D_OPI_NGW = D_OPI_NGW,
                   D_OPI_NSW = D_OPI_NSW,
                   D_OPI_PSW = D_OPI_PSW,
                   D_OPI_NUE = D_OPI_NUE,
                   D_OPI_WB = D_OPI_WB,
                   B_LS_HYDROCAT = B_LS_HYDROCAT,
                   D_MEAS_NGW = NA_real_,
                   D_MEAS_NSW = NA_real_,
                   D_MEAS_PSW = NA_real_,
                   D_MEAS_NUE = NA_real_,
                   D_MEAS_WB = NA_real_,
                   D_MEAS_TOT = NA_real_
                  )
  
  # add sector for regional studies
  if(length(sector)==nrow(dt)){dt[,sector := sector]}
  
  # do check op groundwater class
  checkmate::assert_subset(B_GWL_CLASS, choices = unlist(bbwp_parms[code == 'B_GWL_CLASS', choices]))
  
  # load, check and update the measures database
  dt.measures <- bbwp_check_meas(measures,eco = FALSE,score = TRUE)
  
  # merge with input
  dt <- merge(dt, dt.measures, by = 'id', all = TRUE)
  
    ## Adjust effect scores
  
    # Add bonus points for measures on (slightly) P saturated soils
    dt[(A_P_AL > 18 & A_P_AL <= 50 & A_P_CC > 1.5 & A_P_CC <= 3.4), effect_psw := effect_psw + psw_psg_medium]
    dt[(A_P_AL > 50 | A_P_CC > 3.4), effect_psw := effect_psw + psw_psg_high]
    dt[A_P_AL <= 18 | A_P_CC <= 1.5, effect_psw := effect_psw + psw_psg_low]
    
    # Add bonus points for other aspects psw
    dt[B_SLOPE_DEGREE <= 2, effect_psw := effect_psw + psw_noslope]
    dt[grepl('bollen',B_LU_BBWP), effect_psw := effect_psw + psw_bulbs]
    dt[M_DRAIN == TRUE, effect_psw := effect_psw + nsw_drains]
    
    # Add bonus points for nsw
    dt[M_DRAIN == TRUE, effect_nsw := effect_nsw + nsw_drains]
    dt[B_GWL_CLASS %in% c('VII', 'VIIo', 'VIId','VIII', 'VIIIo', 'VIIId'), effect_nsw := effect_nsw + nsw_gwl_low]
    dt[! B_GWL_CLASS %in% c('VII', 'VIIo', 'VIId','VIII', 'VIIIo', 'VIIId'), effect_nsw := effect_nsw + nsw_gwl_high]
    
    # Add bonus points for grassland for ngw
    dt[B_LU_BBWP %in% c('gras_permanent','gras_tijdelijk'), effect_ngw := effect_ngw + ngw_grassland]
    
    # adjust effect scores for water buffering with landscape-category-specific weighing factor
    dt[B_LS_HYDROCAT == "hoge_gronden", effect_wb := effect_wb * hoge_gronden]
    dt[B_LS_HYDROCAT == "flanken", effect_wb := effect_wb * flanken]
    dt[B_LS_HYDROCAT == "beekdalen", effect_wb := effect_wb * beekdalen]
    dt[B_LS_HYDROCAT == "lokale_laagtes", effect_wb := effect_wb * lokale_laagtes]
    dt[B_LS_HYDROCAT == "polders", effect_wb := effect_wb * polders]
 
  # set scores to zero when measures are not applicable given the crop type
  
    # columns with the Ecoregelingen ranks
    cols <- c('effect_psw','effect_nsw', 'effect_ngw','effect_wb','effect_nue')
  
    # set first all missing data impacts to 0
    dt[,c(cols) := lapply(.SD, function(x) fifelse(is.na(x),0,x)), .SDcols = cols]
  
    # set the score to zero when not applicable for given crop category
    dt[B_LU_BBWP == 'gras_permanent' & nc1 == 0, c(cols) := 0]
    dt[B_LU_BBWP == 'gras_tijdelijk' & nc2 == 0, c(cols) := 0]
    dt[B_LU_BBWP == 'rustgewas' & nc3 == 0, c(cols) := 0]
    dt[B_LU_BBWP == 'rooivrucht' & nc4 == 0, c(cols) := 0]
    dt[B_LU_BBWP == 'groenten' & nc5 == 0, c(cols) := 0]
    dt[B_LU_BBWP == 'bollensierteelt' & nc6 == 0, c(cols) := 0]
    dt[B_LU_BBWP == 'boomfruitteelt' & nc7 == 0, c(cols) := 0]
    dt[B_LU_BBWP == 'natuur' & nc8 == 0, c(cols) := 0]
    dt[B_LU_BBWP == 'mais' & nc9 == 0, c(cols) := 0]
    dt[B_LU_BBWP == 'randensloot' & nc10 == 0, c(cols) := 0]
    dt[B_LU_BBWP == 'vanggewas' & nc11 == 0, c(cols) := 0]
    dt[B_LU_BBWP == 'eiwitgewas' & nc12 == 0, c(cols) := 0]
  
    # set the score to zero when the measure is not applicable
    if('sector' %in% colnames(dt)){
      
      # sector correction for desk studies where sector is available / added per field
      dt[,c('fdairy','farable','ftree_nursery','fbulbs') := 1]
      dt[sector == 'dairy', c('ftree_nursery','farable','fbulbs') := 0]
      dt[sector == 'arable', c('ftree_nursery','fdairy','fbulbs') := 0]
      dt[sector == 'bulbs', c('ftree_nursery','fdairy','farable') := 0]
      dt[sector == 'tree_nursery', c('fbulbs','fdairy','farable') := 0]
      
    } else {
      
      fs0 <- c('fdairy','farable','ftree_nursery','fbulbs')
      fs1 <- paste0('f',sector)
      fs2 <- fs0[!fs0 %in% fs1]
      dt[,c(fs1) := 1]
      if(length(fs2) >= 1){ dt[,c(fs2) := 0] }
    }
  
      # estimate whether sector allows applicability
      dt[, fsector := fdairy * dairy + farable * arable + ftree_nursery * tree_nursery + fbulbs * bulbs]
  
      # adapt the score when measure is not applicable
      dt[fsector == 0, c(cols) := 0]
  
      # adapt the score when the soil type limits the applicability of measures
      dt[grepl('klei', B_SOILTYPE_AGR) & clay == FALSE , c(cols) := 0]
      dt[grepl('zand|dal', B_SOILTYPE_AGR) & sand == FALSE , c(cols) := 0]
      dt[grepl('veen', B_SOILTYPE_AGR) & peat == FALSE , c(cols) := 0]
      dt[grepl('loess', B_SOILTYPE_AGR) & loess == FALSE , c(cols) := 0]
  
      # adapt the score for slope dependent
      dt[B_SLOPE_DEGREE <= 2 & bbwp_id == 'G21',c(cols) := 0]
      
      # zuiveren drainage alleen als er ook drains zijn
      dt[M_DRAIN == FALSE & nodrains == TRUE, c(cols) := 0]
      
  # add impact score for measure per opportunity index => measures are more effective when risks are high
  dt[, D_MEAS_NGW := D_OPI_NGW * effect_ngw]
  dt[, D_MEAS_NSW := D_OPI_NSW * effect_nsw]
  dt[, D_MEAS_PSW := D_OPI_PSW * effect_psw]
  dt[, D_MEAS_NUE := D_OPI_NUE * effect_nue]
  dt[, D_MEAS_WB := D_OPI_WB * effect_wb]
  
  # columns to be adapted given applicability
  scols <- c('D_MEAS_NGW','D_MEAS_NSW','D_MEAS_PSW','D_MEAS_NUE','D_MEAS_WB','D_MEAS_TOT')
  
  # Calculate total measure score
  dt[, D_MEAS_TOT := (D_MEAS_NGW + D_MEAS_NSW + D_MEAS_PSW + D_MEAS_NUE + D_MEAS_WB ) /  5]
  
  # set impact of conflict measures to the highest score of those that are selected
  
    # sort conflicting measures based on total integrative impact
    dt[, oid := frank(-D_MEAS_TOT, ties.method = 'first',na.last = 'keep'), by = c('id','bbwp_conflict')]
  
    # remove the measures that are duplicated
    dt <- dt[oid==1 | is.na(oid)]
  
  # calculate the total impact of measures on the five opportunity indexes (in units of effectiveness, from -2 to +2 per measure)
  dt.meas <- dt[ ,lapply(.SD, sum), .SDcols = scols, by = 'id']
  
  # replace NA values (when no measures are taken) by 0
  dt.meas[, c(scols) := lapply(.SD,function(x) fifelse(is.na(x), 0, x)), .SDcols = scols]
  
  # it should be possible to fill all requirements with two very effective measures per field, equal to +4 points per field
  # so the change in score can increase from 0 to 1 when 4 effectiveness unit points via measures are collected
  # so each effectiveness unit point is equal to 1/4 score units
  dt.meas[ ,c(scols) := lapply(.SD,function(x) x * (1 / 4)), .SDcols = scols]
  
  # order
  setorder(dt.meas, id)
  
  # extract value
  out <- dt.meas[, mget(c('id',scols))]
  
  # return value
  return(out)
}



