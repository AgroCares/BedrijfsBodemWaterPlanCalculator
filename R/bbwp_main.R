#' Calculate the BBWP scores on field and farm level
#'
#' Estimate the potential to contribute to agronomic and environmental challenges in a region for a farm and assess the impact of farm measures taken.
#' A high BBWP score is indicative for the number of opportunities to improve soil quality and land use.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BRP (numeric) The crop type (conform BRP coding, preferable the most frequent crop on the field)
#' @param B_LU_BBWP (numeric) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SC_WENR (character) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006)
#' @param B_HELP_WENR (character) The soil type abbreviation, derived from 1:50.000 soil map
#' @param B_SLOPE (numeric) The slope of the field (degrees)
#' @param B_GWP (boolean) is the field located in a groundwater protected area (options: TRUE or FALSE)
#' @param B_AREA_DROUGHT (boolean) is the field located in an area with high risks for water deficiencies (options: TRUE or FALSE)
#' @param B_CT_PSW (numeric) the critical target for required reduction in P loss from agriculture (kg P / ha) to reach targets of KRW
#' @param B_CT_NSW (numeric) the critical target for required reduction in N loss from agriculture (kg N / ha) to reach targets of KRW
#' @param B_CT_PSW_MAX (numeric) the max critical target for P reduction loss (kg P / ha)
#' @param B_CT_NSW_MAX (numeric) the max critical target for N reduction loss (kg N / ha)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil (mg N / kg)
#' @param A_FE_OX (numeric) The aluminium content of soil (mmol+ / kg)
#' @param A_AL_OX (numeric) The iron content of soil (mmol+ / kg)
#' @param A_P_CC (numeric) The plant available P content, measured via 0.01M CaCl2 extraction (mg / kg)
#' @param A_P_AL (numeric) The plant extractable P content, measured via ammonium lactate extraction (mg / kg)
#' @param A_P_WA (numeric) The P-content of the soil extracted with water
#' @param A_P_SG (numeric) The P-saturation index (\%)
#' @param D_WP (numeric) The fraction of the parcel that is surrounded by surface water
#' @param D_RO_R (numeric) The risk that surface water runs off the parcel
#' @param D_AREA (numeric) the area of the field (\ m2 or \ ha) 
#' @param M_DRAIN (boolean) is there tube drainage present in the field
#' @param LSW (data.table) The surface water polygon for catchment or polder
#' @param measures (data.table) the measures planned / done per fields
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'melkveehouderij','akkerbouw','vollegrondsgroente','boomteelt','bollen','veehouderij','overig')
#' @param output (string) a vector specifying the output type of the function. Options: scores, measures 
#'  
#' @import data.table
#' @import OBIC
#'
#' @export
bbwp <- function(B_SOILTYPE_AGR, B_LU_BRP, B_LU_BBWP,B_GWL_CLASS, B_SC_WENR, B_HELP_WENR,B_SLOPE,
                 A_CLAY_MI, A_SAND_MI, A_SILT_MI, A_SOM_LOI, A_N_RT,A_FE_OX, A_AL_OX, A_P_CC, A_P_AL, A_P_WA, A_P_SG,
                 B_GWP, B_AREA_DROUGHT, B_CT_PSW, B_CT_NSW,B_CT_PSW_MAX = 0.5, B_CT_NSW_MAX = 5.0, 
                 D_WP, D_RO_R, D_AREA, 
                 M_DRAIN, LSW, 
                 measures, sector,output = 'scores'){
  
  # add visual binding
  field_id = NULL
  
  # check wrapper inputs that are not checked in the bbwp functions
  checkmate::assert_character(output)
  checkmate::assert_subset(output,choices = c('scores','measures'))
  
  # convert soil properties to a BBWP risk indicator
  dt <- bbwp_field_properties(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                              B_LU_BRP = B_LU_BRP, 
                              B_GWL_CLASS = B_GWL_CLASS, 
                              B_SC_WENR = B_SC_WENR, 
                              B_HELP_WENR = B_HELP_WENR,
                              B_SLOPE = B_SLOPE,
                              A_CLAY_MI = A_CLAY_MI, 
                              A_SAND_MI = A_SAND_MI, 
                              A_SILT_MI = A_SILT_MI, 
                              A_SOM_LOI = A_SOM_LOI, 
                              A_N_RT = A_N_RT,
                              A_FE_OX = A_FE_OX, 
                              A_AL_OX = A_AL_OX, 
                              A_P_CC = A_P_CC, 
                              A_P_AL = A_P_AL, 
                              A_P_WA = A_P_WA, 
                              A_P_SG = A_P_SG,
                              D_WP = D_WP, 
                              D_RO_R =  D_RO_R, 
                              LSW = LSW)
  
  # Aggregate BBWP risk indicators into five indicators
  dt.ind <- bbwp_field_indicators(D_NGW_SCR = dt$ngw_scr,
                                  D_NGW_LEA = dt$ngw_lea,
                                  D_NGW_NLV = dt$ngw_nlv,
                                  D_NSW_SCR = dt$nsw_scr,
                                  D_NSW_GWT = dt$nsw_gwt,
                                  D_NSW_RO = dt$nsw_ro,
                                  D_NSW_SLOPE = dt$nsw_slope,
                                  D_NSW_WS = dt$nsw_ws,
                                  D_NSW_NLV = dt$nsw_nlv,
                                  D_PSW_SCR = dt$psw_scr,
                                  D_PSW_GWT= dt$psw_gwt,
                                  D_PSW_RO = dt$psw_ro,
                                  D_PSW_SLOPE = dt$psw_slope,
                                  D_PSW_WS = dt$psw_ws,
                                  D_PSW_PCC = dt$psw_pcc,
                                  D_PSW_PSG = dt$psw_psg,
                                  D_PSW_PRET = dt$psw_pret,
                                  D_NUE_WRI = dt$npe_wri,
                                  D_NUE_PBI = dt$npe_pbi,
                                  D_NUE_WDRI = dt$npe_wdri,
                                  D_NUE_NLV = dt$npe_nlv,
                                  D_WUE_WWRI = dt$wue_wwri,
                                  D_WUE_WDRI = dt$wue_wdri,
                                  D_WUE_WHC = dt$wue_whc
                                )
  
  # Calculate BBWP field scores
  
    # when measures are requested as output, then BBWP field scores are derived for situation without measures
    if(output == 'measures'){measures <- NULL}
  
    # calculate BBWP field scores
    dt.fields  <- bbwp_field_scores(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                    B_GWL_CLASS = B_GWL_CLASS,
                                    A_P_SG = A_P_SG,
                                    B_SLOPE = B_SLOPE,
                                    B_LU_BRP = B_LU_BRP,
                                    B_LU_BBWP = B_LU_BBWP,
                                    M_DRAIN = M_DRAIN,
                                    D_WP = D_WP,
                                    D_RISK_NGW = dt.ind$D_RISK_NGW,
                                    D_RISK_NSW = dt.ind$D_RISK_NSW,
                                    D_RISK_PSW = dt.ind$D_RISK_PSW,
                                    D_RISK_NUE = dt.ind$D_RISK_NUE,
                                    D_RISK_WB = dt.ind$D_RISK_WB,
                                    B_GWP = B_GWP,
                                    B_AREA_DROUGHT = B_AREA_DROUGHT,
                                    B_CT_PSW = B_CT_PSW,
                                    B_CT_NSW = B_CT_NSW,
                                    B_CT_PSW_MAX = B_CT_PSW_MAX, 
                                    B_CT_NSW_MAX = B_CT_NSW_MAX,
                                    measures = measures,
                                    sector = sector
                                  )
  
  # Calculate the BBWP farm score
  dt.farm <- bbwp_farm_score(D_OPI_TOT = dt.fields$D_OPI_TOT,
                             D_OPI_NGW = dt.fields$D_OPI_NGW,
                             D_OPI_NSW = dt.fields$D_OPI_NSW,
                             D_OPI_PSW = dt.fields$D_OPI_PSW,
                             D_OPI_NUE = dt.fields$D_OPI_NUE,
                             D_OPI_WB = dt.fields$D_OPI_WB,
                             D_AREA = D_AREA)
                              
  
  # Retreive the best measures to improve
  dt.meas <- bbwp_meas_rank(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                            B_GWL_CLASS = B_GWL_CLASS,
                            A_P_SG = A_P_SG,
                            B_SLOPE = B_SLOPE,
                            B_LU_BRP = B_LU_BRP,
                            B_LU_BBWP = B_LU_BBWP,
                            M_DRAIN = M_DRAIN,
                            D_WP = D_WP,
                            D_OPI_NGW = dt.fields$D_OPI_NGW,
                            D_OPI_NSW = dt.fields$D_OPI_NSW,
                            D_OPI_PSW = dt.fields$D_OPI_PSW,
                            D_OPI_NUE = dt.fields$D_OPI_NUE,
                            D_OPI_WB = dt.fields$D_OPI_WB,
                            measures = NULL,
                            sector = sector
                          )
  
  # return output when preferred measures are requested
  if(output == 'measures'){
    
    # convert names of dt.meas
    setnames(dt.meas,gsub('\\.','_',colnames(dt.meas)))
    
    # convert dt.meas to a splitted list
    out <- split(dt.meas,by='id',keep.by = FALSE)
    
    # covnert each list again to a list
    out <- lapply(out,function(x) as.list(na.omit(x)))
    
    # set output object
    out <- data.table(field_id = sort(unique(dt.meas$id)),
                      measures = out)
    
  }
  
  # return output when BBWP field and farm scores are requested
  if(output == 'scores'){
    
    # Set the column names to lowercase
    setnames(dt.fields, colnames(dt.fields), tolower(colnames(dt.fields)))
    setnames(dt.farm, colnames(dt.farm), tolower(colnames(dt.farm)))
    
    # Add field id
    dt.fields[,field_id := .I]
    
    # set output object
    out <- list(farm = as.list(dt.farm),fields = dt.fields)
    
  }
  
  
  # return output
  return(out)
  
}
