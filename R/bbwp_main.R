#' Calculate the BBWP scores on field and farm level
#'
#' Estimate the potential to contribute to agronomic and environmental challenges in a region for a farm and assess the impact of farm measures taken.
#' A high BBWP score is indicative for the number of opportunities to improve soil quality and land use.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil, using agronomic classification
#' @param B_LU_BBWP (character) DEPRECATED The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SC_WENR (integer) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006). Options include: 1,2,3,4,5,10,11,401,901 and 902.
#' @param B_HELP_WENR (character) The soil type abbreviation, derived from 1:50.000 soil map
#' @param B_SLOPE (boolean) DEPRECATED, use B_SLOPE_DEGREE instead. Is the slope of the field, steeper than 3\%?
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
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
#' @param A_FE_OX (numeric) The aluminium content of soil (mmol / kg)
#' @param A_AL_OX (numeric) The iron content of soil (mmol / kg)
#' @param A_P_CC (numeric) The plant available P content, measured via 0.01M CaCl2 extraction (mg / kg)
#' @param A_P_AL (numeric) The plant extractable P content, measured via ammonium lactate extraction (mg / kg)
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P2O5 / L)
#' @param A_P_SG (numeric) The P-saturation index (\%)
#' @param D_SA_W (numeric) The wet perimeter index of the field, fraction that field is surrounded by water
#' @param D_RO_R (numeric) The risk that surface water runs off the parcel
#' @param B_AREA (numeric) the area of the field (m2) 
#' @param M_DRAIN (boolean) is there tube drainage present in the field
#' @param B_LSW_ID (character) An unique identifier for each Local Surface Water per field
#' @param LSW (data.table) The averaged soil properties (mean and sd) per Local Surface Water. Can be derived from bbwp_lsw_properties.
#' @param measures (data.table) the measures planned / done per fields
#' @param sector (string) a vector with the farm type given the agricultural sector (options: options: 'dairy', 'arable', 'tree_nursery', 'bulbs')
#' @param output (string) a vector specifying the output type of the function. Options: scores, measures 
#' @param penalty (boolean) the option to apply a penalty for high risk BBWP field indicators
#' @param B_LS_HYDROCAT (character) Landscape category for differentiating effect of measures on water buffering.
#' (options: "hoge_gronden", "flanken", "beekdalen", "lokale_laagtes", "polders")
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#'  
#' @details 
#' B_SLOPE_DEGREE should be used, for backwards compatibility B_SLOPE can still be used.
#'  At least one of the must be used, when both are supplied, B_SLOPE is ignored.
#' 
#' LSW is by default a data.table with LSW properties, being calculated from 
#' bbwp_lsw_properties. Note that all B_LSW_IDs should be pre-set in the LSW data.table.
#' 
#' \code{bbwp()} no longer supports '-' as valid input for B_GWL_CLASS. Users should use 
#' their best judgement to decide on the most suitable valid value for such fields.
#' Valid values can be found in \code{bbwp_parms[code == 'B_GWL_CLASS', choices]}
#' 
#' @import data.table
#' @import OBIC
#'  
#' @export
bbwp <- function(B_SOILTYPE_AGR, B_LU_BBWP = NA_character_, B_GWL_CLASS, B_SC_WENR, B_HELP_WENR,B_SLOPE = NULL,B_SLOPE_DEGREE = NULL,
                 B_AER_CBS,
                 A_CLAY_MI, A_SAND_MI, A_SILT_MI, A_SOM_LOI, A_N_RT,A_FE_OX, A_AL_OX, A_P_CC, A_P_AL, A_P_WA, A_P_SG,
                 B_GWP, B_AREA_DROUGHT, B_CT_PSW, B_CT_NSW,B_CT_PSW_MAX = 0.5, B_CT_NSW_MAX = 5.0, 
                 D_SA_W, D_RO_R, B_AREA, 
                 M_DRAIN, B_LSW_ID, LSW = NULL,
                 measures, sector,output = 'scores',penalty=TRUE, B_LS_HYDROCAT = NULL,
                 M_GREEN = M_GREEN, B_LU_BRP){
  
  # add visual binding
  field_id = code = value_min = value_max = choices = NULL
  bbwp_parms <- BBWPC::bbwp_parms
  er_crops <- BBWPC::er_crops
  
  # check wrapper inputs that are not checked in the bbwp functions
  checkmate::assert_character(output)
  checkmate::assert_subset(output,choices = c('scores','measures'))
  checkmate::assert_integerish(B_LU_BRP, any.missing = FALSE, min.len = 1)
  checkmate::assert_subset(B_LU_BRP, choices = unlist(bbwp_parms[code == 'B_LU_BRP', choices]))
  
  # check that either B_SLOPE_DEGREE or B_SLOPE is present
  checkmate::assert_false(all(
    is.null(B_SLOPE_DEGREE),
    is.null(B_SLOPE)))
  
  # use B_SLOPE only if B_SLOPE_DEGREE == NULL
  if(is.null(B_SLOPE_DEGREE)){
    
    # check is B_SLOPE is logical
    checkmate::assert_logical(B_SLOPE)
    
    # warn
    warning('B_SLOPE_DEGREE is missing, using B_SLOPE to set B_SLOPE_DEGREE to 3 when B_SLOPE == TRUE, else to 0.1. It is recommended to supply B_SLOPE_DEGREE, see function documentation.')
    
    # set B_SLOPE_DEGREE to default depending on B_SLOPE classification
    if(B_SLOPE){B_SLOPE_DEGREE = 3} else {B_SLOPE_DEGREE = 0.1}
    
  } else {
    checkmate::assert_numeric(B_SLOPE_DEGREE,
                              lower = bbwp_parms[code == "B_SLOPE_DEGREE", value_min],
                              upper = bbwp_parms[code == "B_SLOPE_DEGREE", value_max])
  }
    
  # estimate and check LSW properties
  if(is.null(LSW)){
    
    # check whether B_LSW_ID is present for each field
    if(length(B_LSW_ID) != length(A_SOM_LOI)){B_LSW_ID <- 1:length(A_SOM_LOI)}
    
    # set LSW to country mean properties
    LSW <- bbwp_lsw_properties(B_LSW_ID = B_LSW_ID)
  
    # print warning
    warning('There are no LSW properties supplied. The missing LSW properties have been replaced by mean soil properties from the Netherlands.')
    
    } else {
      
    # desired column names in LSW
    cols <- c("B_LSW_ID","B_SOM_LOI","B_CLAY_MI","B_SAND_MI","B_SILT_MI","B_N_RT","B_P_AL","B_P_CC","B_P_WA","B_P_SG",
              "B_FE_OX","B_AL_OX","B_SA_W","B_RO_R","B_SOM_LOI_SD", "B_CLAY_MI_SD", "B_SAND_MI_SD", "B_SILT_MI_SD", "B_N_RT_SD","B_P_AL_SD","B_P_CC_SD",
              "B_P_WA_SD","B_P_SG_SD","B_FE_OX_SD","B_AL_OX_SD","B_SA_W_SD","B_RO_R_SD")
    
    # check LSW format and column names
    checkmate::assert_data_table(LSW,nrow = length(unique(B_LSW_ID)))
    checkmate::assert_subset(colnames(LSW),choices = cols)
    checkmate::assert_subset(B_LSW_ID, choices = LSW$B_LSW_ID)
    
    # check if all B_LSW_ID are in the LSW data.table
    checkmate::assert_subset(LSW$B_LSW_ID,choices = B_LSW_ID)
    
    }
  
  # infer B_LU_BBWP from B_LU_BRP
  if(all(is.na(B_LU_BBWP))){
    temp <- data.table(B_LU_BRP = B_LU_BRP)
    temp <- merge(temp, er_crops[,c('B_LU_BRP', 'B_LU_BBWP')],
                  by = 'B_LU_BRP',
                  sort = FALSE,
                  all.x = TRUE)
    B_LU_BBWP <- temp$B_LU_BBWP
    rm(temp)
  } else{
    warning('B_LU_BBWP is deprecated as input argument. It can be infered from B_LU_BRP, please refrain from using B_LU_BBWP. Using your filled in B_LU_BBWP')
  }
  
  # convert soil properties to a BBWP risk indicator
  dt <- bbwp_field_properties(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                              B_LU_BBWP = B_LU_BBWP,
                              B_GWL_CLASS = B_GWL_CLASS, 
                              B_SC_WENR = B_SC_WENR, 
                              B_HELP_WENR = B_HELP_WENR,
                              B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                              B_AER_CBS = B_AER_CBS,
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
                              D_SA_W = D_SA_W, 
                              D_RO_R =  D_RO_R, 
                              B_LSW_ID = B_LSW_ID,
                              LSW = LSW,
                              B_LU_BRP = B_LU_BRP,
                              M_DRAIN = M_DRAIN,
                              M_GREEN = M_GREEN)
  
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
                                  D_WUE_WHC = dt$wue_whc,
                                  D_WUE_GWR = dt$wue_gwr,
                                  penalty = penalty
                                )
  
  # Calculate BBWP field scores
  
    # when measures are requested as output, then BBWP field scores are derived for situation without measures
    if(output == 'measures'){measures <- NULL}
  
    # calculate BBWP field scores
    dt.fields  <- bbwp_field_scores(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                    B_GWL_CLASS = B_GWL_CLASS,
                                    A_P_CC = A_P_CC,
                                    A_P_AL = A_P_AL,
                                    B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                                    B_LU_BBWP = B_LU_BBWP,
                                    B_AER_CBS = B_AER_CBS,
                                    M_DRAIN = M_DRAIN,
                                    D_SA_W = D_SA_W,
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
                                    sector = sector,
                                    penalty = penalty,
                                    B_LS_HYDROCAT = B_LS_HYDROCAT
                                  )
  
  # Calculate the BBWP farm score
  dt.farm <- bbwp_farm_score(S_BBWP_TOT = dt.fields$S_BBWP_TOT,
                             S_BBWP_NGW = dt.fields$S_BBWP_NGW,
                             S_BBWP_NSW = dt.fields$S_BBWP_NSW,
                             S_BBWP_PSW = dt.fields$S_BBWP_PSW,
                             S_BBWP_NUE = dt.fields$S_BBWP_NUE,
                             S_BBWP_WB = dt.fields$S_BBWP_WB,
                             B_AREA = B_AREA)
                              
  # return output when preferred measures are requested
  if(output == 'measures'){
    
    # Retreive the best measures to improve. note that here the BBWP field score is input (varying 0 and 100) where 100 equals "targets met"
    dt.meas <- bbwp_meas_rank(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                              B_GWL_CLASS = B_GWL_CLASS,
                              A_P_CC = A_P_CC,
                              A_P_AL = A_P_AL,
                              B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                              B_LU_BBWP = B_LU_BBWP,
                              B_AER_CBS = B_AER_CBS,
                              M_DRAIN = M_DRAIN,
                              D_SA_W = D_SA_W,
                              S_BBWP_NGW = dt.fields$S_BBWP_NGW,
                              S_BBWP_NSW = dt.fields$S_BBWP_NSW,
                              S_BBWP_PSW = dt.fields$S_BBWP_PSW,
                              S_BBWP_NUE = dt.fields$S_BBWP_NUE,
                              S_BBWP_WB = dt.fields$S_BBWP_WB,
                              measures = NULL,
                              sector = sector,
                              B_LS_HYDROCAT = B_LS_HYDROCAT
                              )
    
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
