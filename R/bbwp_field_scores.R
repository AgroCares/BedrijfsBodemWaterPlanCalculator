#' Calculate the total score of five opportunity indicators for all fields in the Netherlands
#'
#' Estimate the potential to contribute to agronomic and environmental challenges in a region
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param M_DRAIN (boolean) is there tube drainage present in the field
#' @param A_P_CC (numeric) The plant available P content, measured via 0.01M CaCl2 extraction (mg / kg)
#' @param A_P_AL (numeric) The plant extractable P content, measured via ammonium lactate extraction (mg / kg)
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#' @param B_LU_BBWP (character) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param D_SA_W (numeric) The wet perimeter index of the field, fraction that field is surrounded by water
#' @param D_RISK_NGW (numeric) the risk for nitrate leaching to groundwater given field properties
#' @param D_RISK_NSW (numeric) the risk for nitrate leaching and runoff to surface water given field properties
#' @param D_RISK_PSW (numeric) the risk for phosphorus leaching and runoff to surface water given field properties
#' @param D_RISK_NUE (numeric) the efficiency of nitrogen and phosphorus fertilizer use given field properties
#' @param D_RISK_WB (numeric) the potential to buffer and store water and efficiently use water for plant growth given field properties
#' @param B_GWP (boolean) is the field located in a groundwater protected area (options: TRUE or FALSE)
#' @param B_AREA_DROUGHT (boolean) is the field located in an area with high risks for water deficiencies (options: TRUE or FALSE)
#' @param B_CT_PSW (numeric) the critical target for required reduction in P loss from agriculture (kg P / ha) to reach targets of KRW
#' @param B_CT_NSW (numeric) the critical target for required reduction in N loss from agriculture (kg N / ha) to reach targets of KRW
#' @param B_CT_PSW_MAX (numeric) the max critical target for P reduction loss (kg P / ha)
#' @param B_CT_NSW_MAX (numeric) the max critical target for N reduction loss (kg N / ha)
#' @param measures (data.table) the measures planned / done per fields
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'dairy', 'arable', 'tree_nursery', 'bulbs')
#' @param penalty (boolean) the option to apply a penalty for high risk BBWP field indicators 
#' @param B_LS_HYDROCAT (character) Landscape category for differentiating effect of measures on water buffering.
#' (options: "hoge_gronden", "flanken", "beekdalen", "lokale_laagtes", "polders")
#'   
#' @import data.table
#'
#' @export
# calculate the opportunities for a set of fields
bbwp_field_scores <- function(B_SOILTYPE_AGR, B_GWL_CLASS, A_P_CC,A_P_AL, B_SLOPE_DEGREE, B_LU_BBWP,B_AER_CBS,
                              M_DRAIN, D_SA_W, D_RISK_NGW, D_RISK_NSW, D_RISK_PSW, D_RISK_NUE, D_RISK_WB,
                              B_GWP, B_AREA_DROUGHT, B_CT_PSW, B_CT_NSW, 
                              B_CT_PSW_MAX = 0.5, B_CT_NSW_MAX = 5.0, measures, sector,penalty = TRUE, 
                              B_LS_HYDROCAT){
  
  # add visual bindings
  cfngw = cfwb = cfnsw = cfpsw = cfnue = NULL
  D_OPI_NGW = D_OPI_NSW = D_OPI_PSW = D_OPI_NUE = D_OPI_WB = NULL
  D_MEAS_NGW = D_MEAS_NSW = D_MEAS_PSW = D_MEAS_NUE = D_OPI_TOT = NULL 
  D_MEAS_WB = D_MES_PSW = D_MEAS_NGW = D_MEAS_PSW = effect_Wb = id = NULL
  S_BBWP_NGW = S_BBWP_NSW = S_BBWP_PSW = S_BBWP_NUE = S_BBWP_WB = S_BBWP_TOT = NULL
  code = value_min = value_max = choices = NULL
  
  # Load bbwp_parms
  bbwp_parms <- BBWPC::bbwp_parms
  
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(B_GWL_CLASS), length(A_P_CC),length(A_P_AL),length(B_AER_CBS),
                    length(B_SLOPE_DEGREE), length(B_LU_BBWP),length(M_DRAIN),length(D_SA_W),
                    length(D_RISK_NGW),length(D_RISK_NSW),length(D_RISK_PSW),length(D_RISK_NUE),
                    length(D_RISK_WB),length(B_GWP),length(B_AREA_DROUGHT),length(B_CT_PSW),
                    length(B_CT_NSW))
  
  # check inputs
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unlist(bbwp_parms[code == "B_SOILTYPE_AGR", choices]))
  checkmate::assert_subset(B_LU_BBWP, choices = unlist(bbwp_parms[code == "B_LU_BBWP", choices]))
  checkmate::assert_character(B_LU_BBWP, len = arg.length)
  checkmate::assert_numeric(A_P_CC, lower = bbwp_parms[code == "A_P_CC", value_min], upper = bbwp_parms[code == "A_P_CC", value_max],len = arg.length)
  checkmate::assert_numeric(A_P_AL, lower = bbwp_parms[code == "A_P_AL", value_min], upper = bbwp_parms[code == "A_P_AL", value_max],len = arg.length)
  checkmate::assert_numeric(B_SLOPE_DEGREE,lower = bbwp_parms[code == "B_SLOPE_DEGREE", value_min], upper = bbwp_parms[code == "B_SLOPE_DEGREE", value_max],len = arg.length)
  checkmate::assert_logical(M_DRAIN,len = arg.length)
  checkmate::assert_numeric(D_SA_W, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_RISK_NGW, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_RISK_NSW, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_RISK_PSW, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_RISK_NUE, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_RISK_WB, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_logical(B_GWP,len = arg.length)
  checkmate::assert_logical(B_AREA_DROUGHT,len = arg.length)
  checkmate::assert_numeric(B_CT_PSW, lower = 0, upper = 50, len = arg.length)
  checkmate::assert_numeric(B_CT_NSW, lower = 0, upper = 100, len = arg.length)

  # load, check and update the measures database
  dt.measures <- bbwp_check_meas(measures,eco = FALSE,score = TRUE)
  
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
                    D_RISK_NGW = D_RISK_NGW,
                    D_RISK_NSW = D_RISK_NSW,
                    D_RISK_PSW = D_RISK_PSW,
                    D_RISK_NUE = D_RISK_NUE,
                    D_RISK_WB = D_RISK_WB,
                    B_GWP = B_GWP,
                    B_AREA_DROUGHT = B_AREA_DROUGHT,
                    B_CT_PSW = B_CT_PSW,
                    B_CT_NSW = B_CT_NSW,
                    B_CT_PSW_MAX = B_CT_PSW_MAX,
                    B_CT_NSW_MAX = B_CT_NSW_MAX
                  )
  
  # do check op Gt
  dt[,B_GWL_CLASS := bbwp_check_gt(B_GWL_CLASS,B_AER_CBS = B_AER_CBS)]
  
  # calculate correction factors, depending on regional targets
  
    # correction when field is in a ground water protection zone
    dt[,cfngw := fifelse(B_GWP, 1, 0.5)]

    # lower the regional target for nitrate leaching (compared to the general target 1)
    dt[B_GWL_CLASS %in% c('GtI','GtII','GtIII'), cfngw := cfngw * 0.5]
    dt[B_SOILTYPE_AGR == 'veen', cfngw := cfngw * 0.1]
    
    # correction when field is in a region with high water deficiency risks
    dt[,cfwb := fifelse(B_AREA_DROUGHT, 1, 0.5)]
    
    # correction when field is in a region with high target for N load reduction surface water
    dt[,cfnsw := pmax(0,pmin(1,B_CT_NSW / B_CT_NSW_MAX))]
    
    # correction when field is in a region with high target for P load reduction surface water
    dt[,cfpsw := pmax(0,pmin(1,B_CT_PSW / B_CT_PSW_MAX))]
    
    # replace to max critical limit when no information is ready
    dt[is.na(cfpsw), cfpsw := 1]
    dt[is.na(cfnsw), cfnsw := 1]
    
    # correction for need for increased nutrient use efficiency
    dt[,cfnue := 0.5]
  
    # calculate the individual opportunity indexes
    dt[,D_OPI_NGW := (0.5 + cfngw/2) * OBIC::evaluate_logistic(D_RISK_NGW, b=6, x0=0.4, v=.7)]
    dt[,D_OPI_NSW := (0.5 + cfnsw/2) * OBIC::evaluate_logistic(D_RISK_NSW, b=6, x0=0.4, v=.7)]
    dt[,D_OPI_PSW := (0.5 + cfpsw/2) * OBIC::evaluate_logistic(D_RISK_PSW, b=6, x0=0.4, v=.7)]
    dt[,D_OPI_NUE := (0.5 + cfnue/2) * OBIC::evaluate_logistic(D_RISK_NUE, b=6, x0=0.4, v=.7)]
    dt[,D_OPI_WB := (0.5 + cfwb/2) * OBIC::evaluate_logistic(D_RISK_WB, b=6, x0=0.4, v=.7)]
    
    # column names for impact of measures on the five indexes (do not change order)
    mcols <- c('D_MEAS_NGW', 'D_MEAS_NSW', 'D_MEAS_PSW', 'D_MEAS_NUE', 'D_MEAS_WB', 'D_MEAS_TOT')
    
    # calculate the total score per indicator 
    if(nrow(dt.measures) > 0){
      
      
      # calculate (using the opportunity index as weighing, where 1 means high risk = high opportunity for measures to be taken)
      dt.meas.impact <- bbwp_meas_score(B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR, 
                                        B_LU_BBWP = dt$B_LU_BBWP,
                                        B_GWL_CLASS = dt$B_GWL_CLASS,
                                        B_AER_CBS = dt$B_AER_CBS,
                                        A_P_CC = dt$A_P_CC,
                                        A_P_AL = dt$A_P_AL,
                                        B_SLOPE_DEGREE = dt$B_SLOPE_DEGREE,
                                        M_DRAIN = dt$M_DRAIN,
                                        D_SA_W = dt$D_SA_W,
                                        D_OPI_NGW = dt$D_OPI_NGW,
                                        D_OPI_NSW = dt$D_OPI_NSW,
                                        D_OPI_PSW = dt$D_OPI_PSW,
                                        D_OPI_NUE = dt$D_OPI_NUE,
                                        D_OPI_WB = dt$D_OPI_WB,
                                        measures = measures, 
                                        sector = sector,
                                        B_LS_HYDROCAT = B_LS_HYDROCAT)
      
      # merge with dt
      dt <- merge(dt,dt.meas.impact,by='id')
      
      # set NA to zero
      dt[,c(mcols) := lapply(.SD,function(x) fifelse(is.na(x),0,x)),.SDcols = mcols]
      
    } else {
      
      # set impact of management to zero when no measures are applied
      dt[,c(mcols) := list(0,0,0,0,0,0)]
    }
    
  # update the field score with measures
  dt[,D_OPI_NGW := pmax(0,1 - pmax(0, D_OPI_NGW - D_MEAS_NGW))]
  dt[,D_OPI_NSW := pmax(0,1 - pmax(0, D_OPI_NSW - D_MEAS_NSW))]
  dt[,D_OPI_PSW := pmax(0,1 - pmax(0, D_OPI_PSW - D_MEAS_PSW))]
  dt[,D_OPI_NUE := pmax(0,1 - pmax(0, D_OPI_NUE - D_MEAS_NUE))]
  dt[,D_OPI_WB :=  pmax(0,1 - pmax(0, D_OPI_WB - D_MEAS_WB))]
  
  # Convert form 0-1 to 0-100 
  dt[,S_BBWP_NGW := 100 * D_OPI_NGW]
  dt[,S_BBWP_NSW := 100 * D_OPI_NSW]
  dt[,S_BBWP_PSW := 100 * D_OPI_PSW]
  dt[,S_BBWP_NUE := 100 * D_OPI_NUE]
  dt[,S_BBWP_WB := 100 * D_OPI_WB]
  
  dt[,S_BBWP_TOT := (S_BBWP_NGW * wf(S_BBWP_NGW, type="score",penalty = penalty) + 
                      S_BBWP_NSW * wf(S_BBWP_NSW, type="score",penalty = penalty) + 
                      S_BBWP_PSW * wf(S_BBWP_PSW, type="score",penalty = penalty) + 
                      S_BBWP_NUE * wf(S_BBWP_NUE, type="score",penalty = penalty) + 
                      S_BBWP_WB * wf(S_BBWP_WB, type="score",penalty)) /
       (wf(S_BBWP_NGW, type="score",penalty = penalty) + wf(S_BBWP_NSW, type="score",penalty = penalty) +  wf(S_BBWP_PSW, type="score",penalty = penalty) +  
          wf(S_BBWP_NUE, type="score",penalty = penalty) +  wf(S_BBWP_WB, type="score",penalty = penalty))]
  
  # order the fields
  setorder(dt, id)
  
  # extract value
  value <- dt[,mget(c('S_BBWP_NGW','S_BBWP_NSW','S_BBWP_PSW','S_BBWP_NUE','S_BBWP_WB','S_BBWP_TOT'))]
  
  # Round the values
  value <- value[, lapply(.SD, round, digits = 0)]
  
  # return value
  return(value)
}
