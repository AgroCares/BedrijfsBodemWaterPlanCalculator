#' Rank the suitability of agronomic measures to improve soil and water management for a given field
#'
#' Estimate the score for agronomic measures to improve soil and water management on agricultural farms. 
#' And send an ordered list back of the most suitable measures.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param A_P_SG (numeric) 
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#' @param B_LU_BBWP (character) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param M_DRAIN (boolean) is there tube drainage present in the field
#' @param D_SA_W (numeric) The wet perimeter index of the field, fraction that field is surrounded by water
#' @param D_OPI_NGW (numeric) the opportunity index (risk x impact) for nitrate leaching to groundwater given field properties
#' @param D_OPI_NSW (numeric) the opportunity index (risk x impact) for nitrate leaching and runoff to surface water given field properties
#' @param D_OPI_PSW (numeric) the opportunity index (risk x impact) for phosphorus leaching and runoff to surface water given field properties
#' @param D_OPI_NUE (numeric) the opportunity index (risk x impact) to improve the efficiency of nitrogen and phosphorus fertilizer use given field properties
#' @param D_OPI_WB (numeric) the opportunity index (risk x impact) to improve the potential to buffer and store water and efficiently use water for plant growth given field properties
#' @param measures (data.table) table with the properties of the available measures
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'diary', 'arable', 'tree_nursery', 'bulbs')
#' 
#'   
#' @import data.table
#'
#' @export
# rank the measures given their effectiveness to improve the sustainability of the farm
bbwp_meas_rank <- function(B_SOILTYPE_AGR, B_GWL_CLASS,  A_P_SG, B_SLOPE_DEGREE, B_LU_BBWP,B_AER_CBS,
                           M_DRAIN, D_SA_W,
                           D_OPI_NGW, D_OPI_NSW, D_OPI_PSW, D_OPI_NUE, D_OPI_WB,
                           measures, sector){
  
  # add visual bindings
  effect_psw = psw_psg_medium = psw_psg_high = effect_nsw = nsw_drains = nsw_gwl_low = nsw_gwl_high = psw_noslope = effect_ngw = NULL
  ngw_grassland = psw_bulbs = D_MEAS_NGW = D_MEAS_NSW = D_MEAS_NUE = effect_nue = D_MEAS_WB = effect_wb = diary = arable = tree_nursery = bulbs = NULL
  clay = sand= peat = loess = D_MEAS_TOT = effect_costs = id = D_MEAS_PSW = NULL
  fsector = fdairy = dairy = farable = arable = ftree_nursery = tree_nursery = fbulbs = bulbs = NULL
  bbwp_id = oid = level = NULL
  nc1 = nc2 = nc3 = nc4 = nc5 = nc6 = nc7 = nc8 = nc9 = nc10 = nc11 = nc12 = NULL
  code = value_min = value_max = choices = NULL
  
  # Load bbwp_parms
  bbwp_parms <- BBWPC::bbwp_parms
  
  # check length of the inputs
  arg.length <- max(length(D_OPI_NGW), length(D_OPI_NSW), length(D_OPI_PSW), length(D_OPI_NUE),
                    length(D_OPI_WB), length(B_SOILTYPE_AGR), length(B_GWL_CLASS), length(M_DRAIN),
                    length(A_P_SG), length(B_SLOPE_DEGREE), length(B_LU_BBWP),length(B_AER_CBS),
                    length(D_SA_W))
  
  # reformat B_AER_CBS
  B_AER_CBS <- bbwp_format_aer(B_AER_CBS)
  
  # check inputs
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unlist(bbwp_parms[code == "B_SOILTYPE_AGR", choices]))
  checkmate::assert_subset(B_LU_BBWP, choices = unlist(bbwp_parms[code == "B_LU_BBWP", choices]))
  checkmate::assert_character(B_LU_BBWP, len = arg.length)
  checkmate::assert_logical(M_DRAIN)
  checkmate::assert_numeric(A_P_SG, lower = bbwp_parms[code == "A_P_SG", value_min], upper = bbwp_parms[code == "A_P_SG", value_max],len = arg.length)
  checkmate::assert_numeric(B_SLOPE_DEGREE,lower = bbwp_parms[code == "B_SLOPE_DEGREE", value_min], upper = bbwp_parms[code == "B_SLOPE_DEGREE", value_max],len = arg.length)
  checkmate::assert_numeric(D_SA_W, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(D_OPI_NGW, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(D_OPI_NSW, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(D_OPI_PSW, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(D_OPI_NUE, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(D_OPI_WB, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_subset(sector, choices = c('dairy', 'arable', 'tree_nursery', 'bulbs'))
  
  # load, check and update the measures database
  dt.measures <- bbwp_check_meas(measures,eco = FALSE,score = FALSE)
  
  # collect data in one data.table
  dt <- data.table(
    id = 1:arg.length,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    B_GWL_CLASS = B_GWL_CLASS,
    A_P_SG = A_P_SG,
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
    value = NA_real_
  )
  
  # add sector for regional studies
  if(length(sector)==nrow(dt)){dt[,sector := sector]}
  
  # do check op Gt
  dt[,B_GWL_CLASS := bbwp_check_gt(B_GWL_CLASS, B_AER_CBS = B_AER_CBS)]
  
  # merge inputs with data.table measures
  dt <- as.data.table(merge.data.frame(dt, dt.measures, all = TRUE))
  
    # only select measures at field level
    dt <- dt[level == 'field']
  
    # Add bonus points for psw
    dt[A_P_SG >= 50 & A_P_SG < 75, effect_psw := effect_psw + psw_psg_medium]
    dt[A_P_SG >= 75, effect_psw := effect_psw + psw_psg_high]
    dt[B_SLOPE_DEGREE <= 2, effect_psw := effect_psw + psw_noslope]
    dt[B_LU_BBWP == 6, effect_psw := effect_psw + psw_bulbs]
    
    # Add bonus points for nsw
    dt[M_DRAIN == TRUE, effect_nsw := effect_nsw + nsw_drains]
    dt[B_GWL_CLASS %in% c('GtVII','GtVIII'), effect_nsw := effect_nsw + nsw_gwl_low]
    dt[! B_GWL_CLASS %in% c('GtVII','GtVIII'), effect_nsw := effect_nsw + nsw_gwl_high]
    
    # Add bonus points for grassland
    dt[B_LU_BBWP %in% c(1,2), effect_ngw := effect_ngw + ngw_grassland]
  
  # set scores to zero when measures are not applicable given the crop type
  
    # columns with the Ecoregelingen ranks
    cols <- c('effect_psw','effect_nsw', 'effect_ngw','effect_wb','effect_nue')
    
    # set first all missing data impacts to 0
    dt[,c(cols) := lapply(.SD, function(x) fifelse(is.na(x),0,x)), .SDcols = cols]
    
    # set the score to zero when not applicable for given crop category
    dt[B_LU_BBWP == 'gras_permanent' & nc1 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'gras_tijdelijk' & nc2 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'rustgewas' & nc3 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'rooivrucht' & nc4 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'groenten' & nc5 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'bollensierteelt' & nc6 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'boomfruitteelt' & nc7 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'natuur' & nc8 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'mais' & nc9 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'randensloot' & nc10 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'vanggewas' & nc11 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'eiwitgewas' & nc12 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    
  # set the score to zero when the measure is not applicable
    
    # add columns for the sector to which the farms belong
    fs0 <- c('fdairy','farable','ftree_nursery','fbulbs')
    fs1 <- paste0('f',sector)
    fs2 <- fs0[!fs0 %in% fs1]
    dt[,c(fs1) := 1]
    if(length(fs2) >= 1){ dt[,c(fs2) := 0] }
    
    if('sector' %in% colnames(dt)){
      
      dt[,c('fdairy','farable','ftree_nursery','fbulbs') := 1]
      dt[sector == 'dairy', c('ftree_nursery','farable','fbulbs') := 0]
      dt[sector == 'arable', c('ftree_nursery','fdairy','fbulbs') := 0]
      dt[sector == 'bulbs', c('ftree_nursery','fdairy','farable') := 0]
      dt[sector == 'tree_nursery', c('fbulbs','fdairy','farable') := 0]
    
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
    
    # add impact score for measure per opportunity index
    dt[, D_MEAS_NGW := D_OPI_NGW * effect_ngw]
    dt[, D_MEAS_NSW := D_OPI_NSW * effect_nsw]
    dt[, D_MEAS_PSW := D_OPI_PSW * effect_psw]
    dt[, D_MEAS_NUE := D_OPI_NUE * effect_nue]
    dt[, D_MEAS_WB := D_OPI_WB * effect_wb]
  
  
  # Calculate total measure score
  dt[, D_MEAS_TOT := (D_MEAS_NGW + D_MEAS_NSW + D_MEAS_PSW + D_MEAS_NUE + D_MEAS_WB ) /  5 - effect_costs * 0.01]
  
  # set impact of conflict measures to the highest score of those that are selected
  
  # add sort-id conflicting measures based on total integrative impact
  dt[, oid := frank(-D_MEAS_TOT, ties.method = 'first',na.last = 'keep'), by = c('id','bbwp_conflict')]
    
  
  # define an empty list
  list.meas <- list()
  
  # select for each field the top5 measures per objective
  for (i in 1:arg.length) {
    
    # Get the overall top measures
    top_bbwp_tot <- dt[id == i & D_MEAS_TOT > 0, ][order(oid,-D_MEAS_TOT)][1:5,bbwp_id]
    
    # Get the top measures for nitrate losses groundwater
    top_bbwp_ngw <- dt[id == i & D_MEAS_NGW > 0, ][order(oid,-D_MEAS_NGW)][1:5,bbwp_id]
    
    # Get the top measures for nitrogen loss surface water
    top_bbwp_nsw <- dt[id == i & D_MEAS_NSW > 0, ][order(oid,-D_MEAS_NSW)][1:5,bbwp_id]
    
    # Get the top measures for phosphorus loss surface water
    top_bbwp_psw <- dt[id == i & D_MEAS_PSW > 0, ][order(oid,-D_MEAS_PSW)][1:5,bbwp_id]
    
    # Get the top measures for water retention and availability
    top_bbwp_wb <- dt[id == i & D_MEAS_WB > 0, ][order(oid,-D_MEAS_WB)][1:5,bbwp_id]
    
    # Get the top measures for nutrient use efficiency
    top_bbwp_nue <- dt[id == i & D_MEAS_NUE > 0, ][order(oid,-D_MEAS_NUE)][1:5,bbwp_id]
    
    # add them to list
    list.meas[[i]] <- data.table(id = i,
                                 top_bbwp_tot = top_bbwp_tot,
                                 top_bbwp_ngw = top_bbwp_ngw,
                                 top_bbwp_nsw = top_bbwp_nsw,
                                 top_bbwp_psw = top_bbwp_psw,
                                 top_bbwp_wb = top_bbwp_wb,
                                 top_bbwp_nue = top_bbwp_nue)
  }
  
  # prepare output
  out <- data.table::rbindlist(list.meas)
  
  # remove NA for cases that no measures are needed at all
  out <- unique(out)
  
  # return output
  return(out)
}
