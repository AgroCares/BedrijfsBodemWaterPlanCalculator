#' Calculate the total score of five opportunity indicators for all fields in the Netherlands
#'
#' Estimate the potential to contribute to agronomic and environmental challenges in a region
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param M_DRAIN (boolean) is there tube drainage present in the field
#' @param A_P_SG (numeric) 
#' @param B_SLOPE (boolean)
#' @param B_LU_BRP (integer)
#' @param B_LU_BBWP (numeric) The BBWP category used for allocation of measures to BBWP crop categories
#' @param D_WP (numeric) The fraction of the parcel that is surrounded by surface water
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
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'melkveehouderij','akkerbouw','vollegrondsgroente','boomteelt','bollen','veehouderij','overig')
#' 
#'   
#' @import data.table
#'
#' @export
# calculate the opportunities for a set of fields
bbwp_field_scores <- function(B_SOILTYPE_AGR, B_GWL_CLASS, A_P_SG, B_SLOPE, B_LU_BRP, B_LU_BBWP,
                              M_DRAIN, D_WP, D_RISK_NGW, D_RISK_NSW, D_RISK_PSW, D_RISK_NUE, D_RISK_WB,
                              B_GWP, B_AREA_DROUGHT, B_CT_PSW, B_CT_NSW, 
                              B_CT_PSW_MAX = 0.5, B_CT_NSW_MAX = 5.0, measures, sector){
  
  cfngw = cfwb = cfnsw = cfpsw = cfnue = NULL
  D_OPI_NGW = D_OPI_NSW = D_OPI_PSW = D_OPI_NUE = D_OPI_WB = NULL
  D_MEAS_NGW = D_MEAS_NSW = D_MEAS_PSW = D_MEAS_NUE = D_OPI_TOT = NULL 
  D_MEAS_WB = D_MES_PSW = D_MEAS_NGW = D_MEAS_PSW = effect_Wb = id = NULL
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(B_GWL_CLASS), length(A_P_SG),
                    length(B_SLOPE), length(B_LU_BRP), length(B_LU_BBWP),length(M_DRAIN),length(D_WP),
                    length(D_RISK_NGW),length(D_RISK_NSW),length(D_RISK_PSW),length(D_RISK_NUE),
                    length(D_RISK_WB),length(B_GWP),length(B_AREA_DROUGHT),length(B_CT_PSW),
                    length(B_CT_NSW))
  
  # check inputs
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = c('duinzand','dekzand','zeeklei','rivierklei','maasklei',
                                                       'dalgrond','moerige_klei','veen','loess'))
  checkmate::assert_numeric(A_P_SG, lower = 0, upper = 120, len = arg.length)
  checkmate::assert_numeric(B_SLOPE, len = arg.length)
  checkmate::assert_integerish(B_LU_BRP, lower = 0, len = arg.length)
  checkmate::assert_integerish(B_LU_BBWP, lower = 0, upper = 9,len = arg.length)
  checkmate::assert_logical(M_DRAIN,len = arg.length)
  checkmate::assert_numeric(D_WP, lower = 0, upper = 1, len = arg.length)
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
                    A_P_SG = A_P_SG,
                    B_SLOPE = B_SLOPE,
                    B_LU_BRP = B_LU_BRP,
                    B_LU_BBWP = B_LU_BBWP,
                    M_DRAIN = M_DRAIN,
                    D_WP = D_WP,
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
  
  # calculate correction factors, depending on regional targets
  
    # correction when field is in a ground water protection zone
    dt[,cfngw := fifelse(B_GWP, 1, 0.5)]
    
    # correction when field is in a region with high water deficiency risks
    dt[,cfwb := fifelse(B_AREA_DROUGHT, 1, 0.5)]
    
    # correction when field is in a region with high target for N load reduction surface water
    dt[,cfnsw := B_CT_NSW / B_CT_NSW_MAX]
    
    # correction when field is in a region with high target for P load reduction surface water
    dt[,cfpsw := B_CT_PSW / B_CT_PSW_MAX]
    
    # replace to max critical limit when no information is ready
    dt[is.na(cfpsw), cfpsw := 1]
    dt[is.na(cfnsw), cfnsw := 1]
    
    # correction for need for increased nutrient use efficiency
    dt[, cfnue := 0.5]
  
  # calculate the individual opportunity indexes
  dt[,D_OPI_NGW := OBIC::evaluate_logistic(D_RISK_NGW, b=7, x0=0.5 - cfngw * .1, v=.7)]
  dt[,D_OPI_NSW := OBIC::evaluate_logistic(D_RISK_NSW, b=7, x0=0.5 - cfnsw * .1, v=.7)]
  dt[,D_OPI_PSW := OBIC::evaluate_logistic(D_RISK_PSW, b=7, x0=0.5 - cfpsw * .1, v=.7)]
  dt[,D_OPI_NUE := OBIC::evaluate_logistic(D_RISK_NUE, b=7, x0=0.5 - cfnue * .1, v=.7)]
  dt[,D_OPI_WB := OBIC::evaluate_logistic(D_RISK_WB, b=7, x0=0.5 - cfwb * .1, v=.7)]
  
  # calculate the change in opportunity indexes given the measures taken
  
    # column names for impact of measures on the five indexes (do not change order)
    mcols <- c('D_MEAS_NGW', 'D_MEAS_NSW', 'D_MEAS_PSW', 'D_MEAS_NUE', 'D_MEAS_WB', 'D_MEAS_TOT')
    
    # calculate the total score per indicator 
    if(nrow(dt.measures) > 0){
      
      # calculate
      dt.meas.impact <- bbwp_meas_score(B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR, 
                                        B_LU_BRP = dt$B_LU_BRP,
                                        B_LU_BBWP = dt$B_LU_BBWP,
                                        B_GWL_CLASS = dt$B_GWL_CLASS,
                                        A_P_SG = dt$A_P_SG,
                                        B_SLOPE = dt$B_SLOPE,
                                        M_DRAIN = dt$M_DRAIN,
                                        D_WP = dt$D_WP,
                                        D_OPI_NGW = dt$D_OPI_NGW,
                                        D_OPI_NSW = dt$D_OPI_NSW,
                                        D_OPI_PSW = dt$D_OPI_PSW,
                                        D_OPI_NUE = dt$D_OPI_NUE,
                                        D_OPI_WB = dt$D_OPI_WB,
                                        measures = measures, 
                                        sector = sector)
      
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
  dt[,D_OPI_NGW := 100 * D_OPI_NGW]
  dt[,D_OPI_NSW := 100 * D_OPI_NSW]
  dt[,D_OPI_PSW := 100 * D_OPI_PSW]
  dt[,D_OPI_NUE := 100 * D_OPI_NUE]
  dt[,D_OPI_WB :=  100 * D_OPI_WB]
  
  # calculate the integrative opportunity index (risk times impact)
  dt[,D_OPI_TOT := (D_OPI_NGW * wf(D_OPI_NGW, type="score") + 
                    D_OPI_NSW * wf(D_OPI_NSW, type="score") + 
                    D_OPI_PSW * wf(D_OPI_PSW, type="score") + 
                    D_OPI_NUE * wf(D_OPI_NUE, type="score") + 
                    D_OPI_WB * wf(D_OPI_WB, type="score")) /
       (wf(D_OPI_NGW, type="score") + wf(D_OPI_NSW, type="score") +  wf(D_OPI_PSW, type="score") +  wf(D_OPI_NUE, type="score") +  wf(D_OPI_WB, type="score"))]
  
  # order the fields
  setorder(dt, id)
  
  # rename the opportunity indexes to the final score
  setnames(dt,c('D_OPI_NGW','D_OPI_NSW','D_OPI_PSW','D_OPI_NUE','D_OPI_WB','D_OPI_TOT'),
           c('S_BBWP_NGW','S_BBWP_NSW','S_BBWP_PSW','S_BBWP_NUE','S_BBWP_WB','S_BBWP_TOT'))
  
  # extract value
  value <- dt[,mget(c('S_BBWP_NGW','S_BBWP_NSW','S_BBWP_PSW','S_BBWP_NUE','S_BBWP_WB','S_BBWP_TOT'))]
  
  # Round the values
  value <- value[, lapply(.SD, round, digits = 0)]
  
  # return value
  return(value)
}
