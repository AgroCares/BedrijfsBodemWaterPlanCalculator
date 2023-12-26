#' Create the LSW data.table from regional soil properties per Local Surface Water or any Regional Geometry
#' 
#' For each of the fields (or spatial geometries) for which BBWP field properties are estimated, the function requires regional soil properties (mean and standard deviation).
#' These properties can be derived from any spatial dataset. This helper function creates a data.table allowing unit checks on input variables.
#' 
#' @param B_LSW_ID (integer) An unique identifier for each Local Surface Water
#' @param B_SOM_LOI (numeric) The mean percentage organic matter in the soil per LSW (\%)
#' @param B_CLAY_MI (numeric) The mean clay content of the soil per LSW (\%)
#' @param B_SAND_MI (numeric) The mean sand content of the soil per LSW (\%)
#' @param B_SILT_MI (numeric) The mean silt content of the soil per LSW (\%)
#' @param B_N_RT (numeric) The mean organic nitrogen content of the soil per LSW in mg N / kg
#' @param B_P_AL (numeric) The mean P-AL content of the soil  per LSW
#' @param B_P_CC (numeric) The mean plant available P content, extracted with 0.01M CaCl2 per LSW (mg / kg)
#' @param B_P_WA (numeric) The mean P-content of the soil extracted with water  per LSW (mg P2O5 / 100 ml soil)
#' @param B_P_SG (numeric) The mean P saturation degree per LSW (\%)
#' @param B_FE_OX (numeric) The mean aluminium content of soil  per LSW (mmol / kg)
#' @param B_AL_OX (numeric) The mean iron content of soil per LSW (mmol / kg)
#' @param B_RO_R (numeric) The mean risk that surface water runs off the parcel per LSW
#' @param B_SA_W (numeric) The mean wet perimeter index of all fields in a LSW, being the mean fraction of a field is surrounded by water
#' @param B_SOM_LOI_SD (numeric) The standard deviation of percentage organic matter in the soil per LSW (\%)
#' @param B_CLAY_MI_SD (numeric) The standard deviation of clay content of the soil per LSW (\%)
#' @param B_SAND_MI_SD (numeric) The standard deviation of sand content of the soil per LSW (\%)
#' @param B_SILT_MI_SD (numeric) The standard deviation of silt content of the soil per LSW (\%)
#' @param B_N_RT_SD (numeric) The standard deviation of organic nitrogen content of the soil per LSW in mg N / kg
#' @param B_P_AL_SD (numeric) The standard deviation of P-AL content of the soil  per LSW
#' @param B_P_CC_SD (numeric) The standard deviation of plant available P content, extracted with 0.01M CaCl2 per LSW (mg / kg)
#' @param B_P_WA_SD (numeric) The standard deviation of P-content of the soil extracted with water  per LSW (mg P2O5 / 100 ml soil)
#' @param B_P_SG_SD (numeric) The standard deviation of P saturation degree per LSW (\%)
#' @param B_FE_OX_SD (numeric) The standard deviation of aluminium content of soil  per LSW (mmol / kg)
#' @param B_AL_OX_SD (numeric) The standard deviation of iron content of soil per LSW (mmol / kg)
#' @param B_RO_R_SD (numeric) The standard deviation of the risk that surface water runs off the parcel per LSW
#' @param B_SA_W_SD (numeric) The standard deviation of the wet perimeter index of all fields in a LSW
#' 
#' @import data.table
#' 
#' @details 
#' Each user can apply its own estimates of regional soil properties (mean and sd) controlling fate of N and P in soils. 
#' If no information is given, the mean properties for all Dutch agricultural soils are used. 
#' The default spatial geometry is a Local surface Water, but any region can be used as long as the mean and sd of the relevant soil properties are known.
#' 
#' @export
bbwp_lsw_properties  <- function(B_LSW_ID, 
                                 B_SOM_LOI = 8.65,
                                 B_CLAY_MI = 15.8,
                                 B_SAND_MI = 60.5,
                                 B_SILT_MI = 23.71,
                                 B_N_RT = 3834,
                                 B_P_AL = 49,
                                 B_P_CC = 2.71,
                                 B_P_WA = 40,
                                 B_P_SG = 22, 
                                 B_FE_OX = 83,
                                 B_AL_OX = 40,
                                 B_RO_R = 0.5,
                                 B_SA_W = 0.47,
                                 B_SOM_LOI_SD = 6.67,
                                 B_CLAY_MI_SD = 13.45,
                                 B_SAND_MI_SD = 23.5,
                                 B_SILT_MI_SD = 11.7,
                                 B_N_RT_SD = 2928,
                                 B_P_AL_SD = 13.5,
                                 B_P_CC_SD = 1.51,
                                 B_P_WA_SD = 15.6,
                                 B_P_SG_SD = 14, 
                                 B_FE_OX_SD = 59,
                                 B_AL_OX_SD = 19,
                                 B_RO_R_SD = 0.3,
                                 B_SA_W_SD = 0.33){
  
  # add visual bindings
  value_min = value_max = code = NULL
  
  # load internal table with parameters
  bbwp_parms <- BBWPC::bbwp_parms
  
  # check length inputs
  arg.length <- max(length(B_CLAY_MI), length(B_SAND_MI), length(B_SILT_MI), length(B_SOM_LOI), length(B_N_RT), 
                    length(B_FE_OX), length(B_AL_OX), length(B_P_CC), length(B_P_AL),length(B_P_WA), length(B_P_SG),
                    length(B_SA_W)
                    )
  
  # check input values of the mean parameters
  checkmate::assert_numeric(B_CLAY_MI, lower = bbwp_parms[code == "A_CLAY_MI", value_min], upper = bbwp_parms[code == "A_CLAY_MI", value_max],len = arg.length)
  checkmate::assert_numeric(B_SAND_MI, lower = bbwp_parms[code == "A_SAND_MI", value_min], upper = bbwp_parms[code == "A_SAND_MI", value_max],len = arg.length)
  checkmate::assert_numeric(B_SILT_MI, lower = bbwp_parms[code == "A_SILT_MI", value_min], upper = bbwp_parms[code == "A_SILT_MI", value_max],len = arg.length)
  checkmate::assert_numeric(B_SOM_LOI, lower = bbwp_parms[code == "A_SOM_LOI", value_min], upper = bbwp_parms[code == "A_SOM_LOI", value_max],len = arg.length)
  checkmate::assert_numeric(B_N_RT, lower = bbwp_parms[code == "A_N_RT", value_min], upper = bbwp_parms[code == "A_N_RT", value_max],len = arg.length)
  checkmate::assert_numeric(B_AL_OX, lower = bbwp_parms[code == "A_AL_OX", value_min], upper = bbwp_parms[code == "A_AL_OX", value_max],len = arg.length)
  checkmate::assert_numeric(B_FE_OX, lower = bbwp_parms[code == "A_FE_OX", value_min], upper = bbwp_parms[code == "A_FE_OX", value_max],len = arg.length)
  checkmate::assert_numeric(B_P_SG, lower = bbwp_parms[code == "A_P_SG", value_min], upper = bbwp_parms[code == "A_P_SG", value_max],len = arg.length)
  checkmate::assert_numeric(B_P_CC, lower = bbwp_parms[code == "A_P_CC", value_min], upper = bbwp_parms[code == "A_P_CC", value_max],len = arg.length)
  checkmate::assert_numeric(B_P_AL, lower = bbwp_parms[code == "A_P_AL", value_min], upper = bbwp_parms[code == "A_P_AL", value_max],len = arg.length)
  checkmate::assert_numeric(B_P_WA, lower = bbwp_parms[code == "A_P_WA", value_min], upper = bbwp_parms[code == "A_P_WA", value_max],len = arg.length)
  checkmate::assert_numeric(B_SA_W, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(B_RO_R, lower = bbwp_parms[code == "D_RO_R", value_min], upper = bbwp_parms[code == "D_RO_R", value_max],len = arg.length)
  
  # check variation in SD values, requires values bigger than zero
  checkmate::assert_numeric(B_CLAY_MI_SD, lower = 0,len = arg.length)
  checkmate::assert_numeric(B_SAND_MI_SD, lower = 0,len = arg.length)
  checkmate::assert_numeric(B_SILT_MI_SD, lower = 0,len = arg.length)
  checkmate::assert_numeric(B_SOM_LOI_SD, lower = 0,len = arg.length)
  checkmate::assert_numeric(B_N_RT_SD, lower = 0,len = arg.length)
  checkmate::assert_numeric(B_AL_OX_SD, lower = 0,len = arg.length)
  checkmate::assert_numeric(B_FE_OX_SD, lower = 0,len = arg.length)
  checkmate::assert_numeric(B_P_SG_SD, lower = 0,len = arg.length)
  checkmate::assert_numeric(B_P_CC_SD, lower = 0,len = arg.length)
  checkmate::assert_numeric(B_P_AL_SD, lower = 0,len = arg.length)
  checkmate::assert_numeric(B_P_WA_SD, lower = 0,len = arg.length)
  checkmate::assert_numeric(B_SA_W_SD, lower = 0,len = arg.length)
  checkmate::assert_numeric(B_RO_R_SD, lower = 0,len = arg.length)
  
  # make a data.table with the input data
  dt <- data.table(B_LSW_ID = B_LSW_ID, 
                   B_SOM_LOI = B_SOM_LOI,
                   B_CLAY_MI = B_CLAY_MI,
                   B_SAND_MI = B_SAND_MI,
                   B_SILT_MI = B_SILT_MI,
                   B_N_RT = B_N_RT,
                   B_P_AL = B_P_AL,
                   B_P_CC = B_P_CC,
                   B_P_WA = B_P_WA,
                   B_P_SG = B_P_SG, 
                   B_FE_OX = B_FE_OX,
                   B_AL_OX = B_AL_OX,
                   B_SA_W = B_SA_W,
                   B_RO_R = B_RO_R,
                   B_SOM_LOI_SD = B_SOM_LOI_SD,
                   B_CLAY_MI_SD = B_CLAY_MI_SD,
                   B_SAND_MI_SD = B_SAND_MI_SD,
                   B_SILT_MI_SD = B_SILT_MI_SD,
                   B_N_RT_SD = B_N_RT_SD,
                   B_P_AL_SD = B_P_AL_SD,
                   B_P_CC_SD = B_P_CC_SD,
                   B_P_WA_SD = B_P_WA_SD,
                   B_P_SG_SD = B_P_SG_SD, 
                   B_FE_OX_SD = B_FE_OX_SD,
                   B_AL_OX_SD = B_AL_OX_SD,
                   B_SA_W_SD = B_SA_W_SD,
                   B_RO_R_SD = B_RO_R_SD)
    
  
  # return data.table
  return(dt)
}
