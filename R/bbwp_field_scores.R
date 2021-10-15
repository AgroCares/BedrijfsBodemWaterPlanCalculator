#' Calculate the total score of five opportunity indicators for all fields in the Netherlands
#'
#' Estimate the potential to contribute to agronomic and environmental challenges in a region
#'
#' @param B_BT_AK (character) The type of soil
#' @param B_GT (character) The groundwater table class
#' @param B_DRAIN (boolean) is there tube drainage present in the field
#' @param D_SA_W (numeric) The fraction of the parcel that is surrounded by surface water
#' @param D_RISK_NGW (numeric) the risk for nitrate leaching to groundwater given field properties
#' @param D_RISK_NSW (numeric) the risk for nitrate leaching and runoff to surface water given field properties
#' @param D_RISK_PSW (numeric) the risk for phosphorus leaching and runoff to surface water given field properties
#' @param D_RISK_NUE (numeric) the efficiency of nitrogen and phosphorus fertilizer use given field properties
#' @param D_RISK_WB (numeric) the potential to buffer and store water and efficiently use water for plant growth given field properties
#' @param B_AREA_GWP (boolean) is the field located in a groundwater protected area (options: TRUE or FALSE)
#' @param B_AREA_DROUGHT (boolean) is the field located in an area with high risks for water deficiencies (options: TRUE or FALSE)
#' @param B_CT_PSW (numeric) the critical target for required reduction in P loss from agriculture (kg P / ha) to reach targets of KRW
#' @param B_CT_NSW (numeric) the critical target for required reduction in N loss from agriculture (kg N / ha) to reach targets of KRW
#' @param B_CT_PSW_MAX (numeric) the max critical target for P reduction loss (kg P / ha)
#' @param B_CT_NSW_MAX (numeric) the max critical target for N reduction loss (kg N / ha)
#' @param B_MEAS (list) the measures planned / done per fields (measurement nr)
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'melkveehouderij','akkerbouw','vollegrondsgroente','boomteelt','bollen','veehouderij','overig')
#' 
#'   
#' @import data.table
#'
#' @export
# calculate the opportunities for a set of fields
bbwp_field_scores <- function(B_BT_AK,B_GT,B_DRAIN,D_SA_W,D_RISK_NGW,D_RISK_NSW,D_RISK_PSW,D_RISK_NUE,D_RISK_WB,
                          B_AREA_GWP,B_AREA_DROUGHT,B_CT_PSW,B_CT_NSW, 
                          B_CT_PSW_MAX = 0.5, B_CT_NSW_MAX = 5.0, B_MEAS,sector){
  
  cfngw = cfwb = cfnsw = cfpsw = cfnue = NULL
  D_OPI_NGW = D_OPI_NSW = D_OPI_PSW = D_OPI_NUE = D_OPI_WB = NULL
  
  # load table with measures 
  bbwp.meas <- getMeasures()
  
  # check length of the inputs
  arg.length <- max(length(B_BT_AK),length(B_GT),length(B_DRAIN),length(D_SA_W),
                    length(D_RISK_NGW),length(D_RISK_NSW),length(D_RISK_PSW),length(D_RISK_NUE),
                    length(D_RISK_WB),length(B_AREA_GWP),length(B_AREA_DROUGHT),length(B_CT_PSW),
                    length(B_CT_NSW),length(B_MEAS))
  
  # check inputs
  checkmate::assert_subset(B_BT_AK, choices = c('duinzand','dekzand','zeeklei','rivierklei','maasklei',
                                                'dalgrond','moerige_klei','veen','loess'))
  checkmate::assert_subset(B_GT, choices = c('-', 'GtI','GtII','GtIIb','GtIII','GtIIIb','GtIV','GtV','GtVb','GtVI','GtVII','GtVIII'))
  checkmate::assert_logical(B_DRAIN,len = arg.length)
  checkmate::assert_numeric(D_SA_W, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_RISK_NGW, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_RISK_NSW, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_RISK_PSW, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_RISK_NUE, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_RISK_WB, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_logical(B_AREA_GWP,len = arg.length)
  checkmate::assert_logical(B_AREA_DROUGHT,len = arg.length)
  checkmate::assert_numeric(B_CT_PSW, lower = 0, upper = 50, len = arg.length)
  checkmate::assert_numeric(B_CT_NSW, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_subset(unlist(B_MEAS), choices = bbwp.meas$bwp_id)
  
  # collect data in one data.table
  dt <- data.table(id = 1:arg.length,
                   B_BT_AK = B_BT_AK,
                   B_GT = B_GT,
                   B_DRAIN = B_DRAIN,
                   D_SA_W = D_SA_W,
                   D_RISK_NGW = D_RISK_NGW,
                   D_RISK_NSW = D_RISK_NSW,
                   D_RISK_PSW = D_RISK_PSW,
                   D_RISK_NUE = D_RISK_NUE,
                   D_RISK_WB = D_RISK_WB,
                   B_AREA_GWP = B_AREA_GWP,
                   B_AREA_DROUGHT = B_AREA_DROUGHT,
                   B_CT_PSW = B_CT_PSW,
                   B_CT_NSW = B_CT_NSW,
                   B_CT_PSW_MAX = B_CT_PSW_MAX,
                   B_CT_NSW_MAX = B_CT_NSW_MAX
                   )
  
  # add list of measures
  
  # calculate correction factors, depending on regional targets
  
    # correction when field is in a ground water protection zone
    dt[,cfngw := fifelse(B_AREA_GWP,1,0.5)]
    
    # correction when field is in a region with high water deficiency risks
    dt[,cfwb := fifelse(B_AREA_DROUGHT,1,0.5)]
    
    # correction when field is in a region with high target for N load reduction surface water
    dt[,cfnsw := B_CT_NSW / B_CT_NSW_MAX]
    
    # correction when field is in a region with high target for P load reduction surface water
    dt[,cfpsw := B_CT_PSW / B_CT_PSW_MAX]
    
    # replace to max critical limit when no information is ready
    dt[is.na(cfpsw),cfpsw := 1]
    dt[is.na(cfnsw),cfnsw := 1]
    
    # correction for need for increased nutrient use efficiency
    dt[,cfnue := 0.5]
   
  # calculate the individual opportunity indexes
  dt[,D_OPI_NGW := evaluate_logistic(D_RISK_NGW, b=7, x0=0.5 - cfngw * .1, v=.7)]
  dt[,D_OPI_NSW := evaluate_logistic(D_RISK_NSW, b=7, x0=0.5 - cfnsw * .1, v=.7)]
  dt[,D_OPI_PSW := evaluate_logistic(D_RISK_PSW, b=7, x0=0.5 - cfpsw * .1, v=.7)]
  dt[,D_OPI_NUE := evaluate_logistic(D_RISK_NUE, b=7, x0=0.5 - cfnue * .1, v=.7)]
  dt[,D_OPI_WB :=  evaluate_logistic(D_RISK_WB, b=7, x0=0.5 - cfwb * .1, v=.7)]
  
  # calculate the change in opportunity indexes given the measures taken
  
    # column names for impact of measures on the five indexes (do not change order)
    mcols <- c('D_MEAS_NGW','D_MEAS_NSW','D_MEAS_PSW','D_MEAS_NUE','D_MEAS_WB','D_MEAS_TOT')
    
    # estimate these indexes
    dt[,c(mcols) := bbwp_meas_score(B_BT_AK,B_GT,B_DRAIN,D_SA_W,D_OPI_NGW,D_OPI_NSW,D_OPI_PSW,D_OPI_NUE,D_OPI_WB,B_MEAS,sector)]
    
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
  
  # extract value
  value <- dt[,mget(c('D_OPI_NGW','D_OPI_NSW','D_OPI_PSW','D_OPI_NUE','D_OPI_WB','D_OPI_TOT'))]
  
  # Round the values
  value <- value[, lapply(.SD, round, digits = 0)]
  
  # return value
  return(value)
}
