#' Calculate the relative impact of field properties given its position in the landscape
#'
#' Estimate the relative ranking of field properties given their contribution to nutrient losses to aquatic ecosystems as well as nutrient and water efficiency.
#' A high rank is indicative for the number of opportunities to improve soil quality and land use.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil, using agronomic classification
#' @param B_LU_BBWP (character) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SC_WENR (character) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006)
#' @param B_HELP_WENR (character) The soil type abbreviation, derived from 1:50.000 soil map
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil (mg N / kg)
#' @param A_FE_OX (numeric) The aluminium content of soil (mmol+ / kg)
#' @param A_AL_OX (numeric) The iron content of soil (mmol+ / kg)
#' @param A_P_CC (numeric) The plant available P content, measured via 0.01M CaCl2 extraction (mg / kg)
#' @param A_P_AL (numeric) The plant extractable P content, measured via ammonium lactate extraction (mg / kg)
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P2O5 / L)
#' @param A_P_SG (numeric) The P-saturation index (\%)
#' @param D_SA_W (numeric) The wet perimeter index of the field, fraction that field is surrounded by water
#' @param D_RO_R (numeric) The risk that surface water runs off the parcel
#' @param B_LSW_ID (integer) An unique identifier for each Local Surface Water per field
#' @param LSW (data.table) The averaged soil properties (mean and sd) per Local Surface Water
#'  
#' @import data.table
#' @import OBIC
#'
#' @export
bbwp_field_properties <- function(B_SOILTYPE_AGR, B_LU_BBWP, B_GWL_CLASS, B_SC_WENR, B_HELP_WENR,B_SLOPE_DEGREE,B_AER_CBS,
                                  A_CLAY_MI, A_SAND_MI, A_SILT_MI, A_SOM_LOI, A_N_RT,
                                  A_FE_OX, A_AL_OX, A_P_CC, A_P_AL, A_P_WA, A_P_SG,
                                  D_SA_W, D_RO_R, B_LSW_ID,LSW) {
  
  ngw_scr = croptype.nleach = nf = ngw_lea = ngw_nlv = B_LU_BRP = NULL
  nsw_scr = nsw_gwt = nsw_ro = nsw_ws = nsw_nlv = nsw_slope = NULL 
  psw_scr = psw_gwt = psw_ro = psw_ws = psw_pcc = psw_pvg = psw_pret = psw_slope = NULL 
  npe_wri = npe_pbi = npe_wdri = npe_nlv = wue_wwri = wue_wdri = wue_whc = NULL
  crop_code = soiltype = leaching_to_set = soiltype.n = bodem = gewas = pnorm = NULL
  
  B_SOM_LOI = B_CLAY_MI = B_SAND_MI = B_SILT_MI = B_N_RT = B_P_AL = B_P_CC = B_P_WA = B_P_SG = NULL
  B_FE_OX = B_AL_OX = B_SA_W = B_RO_R = B_SOM_LOI_SD = B_CLAY_MI_SD = B_SAND_MI_SD = B_SILT_MI_SD = B_N_RT_SD = B_P_AL_SD = B_P_CC_SD = NULL
  B_P_WA_SD = B_P_SG_SD = B_FE_OX_SD = B_AL_OX_SD = B_SA_W_SD = B_RO_R_SD = NULL
  id = code = value_min = value_max = choices = NULL
  psw_psg = B_GT = crop_category = NULL
  
  # Load bbwp_parms
  bbwp_parms <- BBWPC::bbwp_parms
  
  # check length inputs
  arg.length <- max(length(B_SOILTYPE_AGR), length(B_LU_BBWP), length(B_GWL_CLASS), length(B_SC_WENR), length(B_HELP_WENR),length(B_AER_CBS),
                    length(A_CLAY_MI), length(A_SAND_MI), length(A_SILT_MI), length(A_SOM_LOI), length(A_N_RT), length(B_SLOPE_DEGREE),
                    length(A_FE_OX), length(A_AL_OX), length(A_P_CC), length(A_P_AL),length(A_P_WA), length(A_P_SG),
                    length(D_SA_W), length(D_RO_R)
                  )
  
  # reformat B_AER_CBS
  B_AER_CBS <- bbwp_format_aer(B_AER_CBS)
  
  # check inputs B parameters
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unlist(bbwp_parms[code == "B_SOILTYPE_AGR", choices]))
  checkmate::assert_subset(B_SC_WENR, choices = unlist(bbwp_parms[code == "B_SC_WENR", choices]))
  checkmate::assert_subset(B_HELP_WENR, choices = c(unique(OBIC::waterstress.obic$soilunit), "unknown"), empty.ok = FALSE)
  checkmate::assert_numeric(B_SLOPE_DEGREE, lower = bbwp_parms[code == "B_SLOPE_DEGREE", value_min], upper = bbwp_parms[code == "B_SLOPE_DEGREE", value_max],len = arg.length)
  checkmate::assert_subset(B_LU_BBWP, choices = unlist(bbwp_parms[code == "B_LU_BBWP", choices]))
  checkmate::assert_character(B_LU_BBWP, len = arg.length)
  # check inputs A parameters
  checkmate::assert_numeric(A_CLAY_MI, lower = bbwp_parms[code == "A_CLAY_MI", value_min], upper = bbwp_parms[code == "A_CLAY_MI", value_max],len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = bbwp_parms[code == "A_SAND_MI", value_min], upper = bbwp_parms[code == "A_SAND_MI", value_max],len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = bbwp_parms[code == "A_SILT_MI", value_min], upper = bbwp_parms[code == "A_SILT_MI", value_max],len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = bbwp_parms[code == "A_SOM_LOI", value_min], upper = bbwp_parms[code == "A_SOM_LOI", value_max],len = arg.length)
  checkmate::assert_numeric(A_N_RT, lower = bbwp_parms[code == "A_N_RT", value_min], upper = bbwp_parms[code == "A_N_RT", value_max],len = arg.length)
  checkmate::assert_numeric(A_AL_OX, lower = bbwp_parms[code == "A_AL_OX", value_min], upper = bbwp_parms[code == "A_AL_OX", value_max],len = arg.length)
  checkmate::assert_numeric(A_FE_OX, lower = bbwp_parms[code == "A_FE_OX", value_min], upper = bbwp_parms[code == "A_FE_OX", value_max],len = arg.length)
  checkmate::assert_numeric(A_P_SG, lower = bbwp_parms[code == "A_P_SG", value_min], upper = bbwp_parms[code == "A_P_SG", value_max],len = arg.length)
  checkmate::assert_numeric(A_P_CC, lower = bbwp_parms[code == "A_P_CC", value_min], upper = bbwp_parms[code == "A_P_CC", value_max],len = arg.length)
  checkmate::assert_numeric(A_P_AL, lower = bbwp_parms[code == "A_P_AL", value_min], upper = bbwp_parms[code == "A_P_AL", value_max],len = arg.length)
  checkmate::assert_numeric(A_P_WA, lower = bbwp_parms[code == "A_P_WA", value_min], upper = bbwp_parms[code == "A_P_WA", value_max],len = arg.length)
  
  # check inputs D parameters
  checkmate::assert_numeric(D_SA_W, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_RO_R, lower = bbwp_parms[code == "D_RO_R", value_min], upper = bbwp_parms[code == "D_RO_R", value_max],len = arg.length)

  # load in the datasets for soil and crop types and N leaching fractions
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  nleach_table <- as.data.table(OBIC::nleach_table)
  nleach_table <- nleach_table[leaching_to_set == 'gw']
  
  # copy input in one data.table
  dt <- data.table(id = 1:arg.length,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
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
                   D_RO_R = D_RO_R,
                   B_LSW_ID = B_LSW_ID)
         
  # do check op Gt
  dt[,B_GWL_CLASS := bbwp_check_gt(B_GWL_CLASS,B_AER_CBS = B_AER_CBS)]
  
  # add crop names and categories
  dt <- merge(dt, LSW, by = 'B_LSW_ID')
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  
  # add crop categories
  dt[B_LU_BBWP %in% c('gras_permanent','gras_tijdelijk'), crop_category := 'grasland']
  dt[B_LU_BBWP %in% c('mais'), crop_category := 'mais']
  dt[B_LU_BBWP %in% c('rustgewas','rooivrucht','groenten','bollensierteelt','boomfruitteelt','vanggewas','eiwitgewas'), crop_category := 'akkerbouw']
  dt[B_LU_BBWP %in% c('natuur','randensloot'), crop_category := 'natuur']
  
  # set a  default crop for estimating water stress per B_LU_BBWP category
  dt[B_LU_BBWP == 'gras_permanent', B_LU_BRP := 265] # permanent gras
  dt[B_LU_BBWP == 'gras_tijdelijk', B_LU_BRP := 266] # tijdelijk grasland
  dt[B_LU_BBWP == 'rustgewas', B_LU_BRP := 233] # wintertarwe
  dt[B_LU_BBWP == 'rooivrucht', B_LU_BRP := 2014] # aardappel
  dt[B_LU_BBWP == 'groenten', B_LU_BRP := 2759] # rode kool
  dt[B_LU_BBWP == 'bollensierteelt', B_LU_BRP := 176] # bloembol 
  dt[B_LU_BBWP == 'boomfruitteelt', B_LU_BRP := 1096] # appelboom 
  dt[B_LU_BBWP == 'natuur', B_LU_BRP := 335] # natuur
  dt[B_LU_BBWP == 'mais', B_LU_BRP := 259] # mais
  dt[B_LU_BBWP == 'randensloot', B_LU_BRP := 372] # rand langs bouwland
  dt[B_LU_BBWP == 'vanggewas', B_LU_BRP := 3504] # bladrammenas
  dt[B_LU_BBWP == 'eiwitgewas', B_LU_BRP := 258] # luzerne
  
  # estimate field properties that contribute to the risk to N losses to groundwater -------
  
  # reclassify soil compaction risk (scr) into a numeric value
  # a high value is indicative of high risk of leaching of nitrogen to groundwater
  dt[B_SC_WENR %in% c(902, 901, 401),ngw_scr := 1]
  dt[B_SC_WENR == 1, ngw_scr := 1]
  dt[B_SC_WENR %in% c(2, 10), ngw_scr := 0.8]
  dt[B_SC_WENR == 3, ngw_scr := 0.6]
  dt[B_SC_WENR == 4, ngw_scr := 0.4]
  dt[B_SC_WENR %in% c(5, 11), ngw_scr := 0.2]
  
  # Re-categorize crop types
  dt[, croptype.nleach := crop_category]
  dt[crop_category == "natuur" | crop_category == "akkerbouw" , croptype.nleach := "akkerbouw"]
  dt[crop_category == "grasland" , croptype.nleach := "gras"]
  
  # merge fraction of N leaching into 'dt', based on soil type x crop type x grondwatertrap
  dt <- merge(dt, nleach_table[, list(bodem, gewas, B_GT, nf)], 
              by.x = c("soiltype.n", "croptype.nleach", "B_GWL_CLASS"), 
              by.y = c("bodem", "gewas", "B_GT"), sort = FALSE, all.x = TRUE)
  
  # for situations that nf is unknown
  dt[is.na(nf), nf := 0.5]
  
  # rank the risk on soil N leaching to groundwater given crop type, soil type and gt
  # a high value means high risks for N leaching
  dt[,ngw_lea := nf / max(nleach_table$nf)]
  
  # rank the nitrogen content as an estimate of total N content: a high value means high risk for N leaching
  dt[,ngw_nlv := pnorm(q = A_N_RT, mean = B_N_RT, sd = B_N_RT_SD)]
  
  # estimate field properties that contribute to the risk to N losses to surface water -------
  
  # reclassify soil compaction risk (scr) into a numeric value
  dt[,nsw_scr := 1 - ngw_scr]
  
  # reclassify the groundwater table (gwt) into a numeric value
  dt[B_GWL_CLASS %in% c('GtI', '-'), nsw_gwt := 1]
  dt[B_GWL_CLASS %in% c('GtIIb','GtIIIb','GtVb'), nsw_gwt := 0.9]
  dt[B_GWL_CLASS %in% c('GtII','GtIII','GtV'), nsw_gwt := 0.8]
  dt[B_GWL_CLASS %in% c('GtIV'), nsw_gwt := 0.7]
  dt[B_GWL_CLASS %in% c('GtVI'), nsw_gwt := 0.6]
  dt[B_GWL_CLASS %in% c('GtVII'), nsw_gwt := 0.5]
  dt[B_GWL_CLASS %in% c('GtVIII'), nsw_gwt := 0.4]
  
  # rank the risk for surface runoff (van Hattum, 2011)
  # higher risk is associated to increased risks for N runoff
  dt[,nsw_ro := pnorm(q = D_RO_R, mean = B_RO_R, sd = B_RO_R_SD)]
  
  # classify fields with a high slope as extra vulnerable for surface runoff
  # with fields with slope > 2% being vulnerabile (Groenendijk, 2020)
  dt[,nsw_slope := pmax(0.2,pmin(1,B_SLOPE_DEGREE/2))]
  
  # assess the risk for wet surroundings (Van Gerven, 2018): a high fraction equals a high risk
  # higher risk is associated to increased risks for N runoff
  dt[,nsw_ws := pmin(1,pmax(0,D_SA_W))]
  
  # rank the risk for N pool in soil: higher NLV is associated to increased risks for N runoff
  dt[,nsw_nlv := ngw_nlv]
  
  # do nlv correction for grassland
  dt[crop_category == "grasland", nsw_nlv := pmax(0, nsw_nlv - 0.5)]
  
  # estimate field properties that contribute to the risk to P losses to surface water -------
  
  # reclassify soil compaction risk (scr) into a numeric value
  dt[,psw_scr := nsw_scr]
  
  # reclassify the groundwater table (gwt) into a numeric value
  dt[,psw_gwt := nsw_gwt]
  
  # rank the risk for surface runoff (van Hattum, 2011)
  dt[,psw_ro := nsw_ro]
  
  # classify fields with a high slope as extra vulnerable for surface runoff
  # with fields with slope > 2% being vulnerabile (Groenendijk, 2020)
  dt[,psw_slope := nsw_slope]
  
  # rank the risk for wet surroundings (Van Gerven, 2018)
  dt[,psw_ws := nsw_ws]
  
  # rank the risk for P pools in soil
  dt[,psw_pcc := pnorm(q = A_P_CC, mean = B_P_CC, sd = B_P_CC_SD)]
  dt[,psw_psg := pnorm(q = A_P_SG, mean = B_P_SG, sd = B_P_SG_SD)]
  dt[,psw_pret := 1- pnorm(q =  A_AL_OX + A_FE_OX, 
                           mean = B_AL_OX + B_FE_OX, 
                           sd =  sqrt(B_AL_OX_SD^2 + B_FE_OX_SD^2))]
  
  # estimate field properties that contribute to the N and P efficiency of P inputs -------
  
  # Replace '-' with 'unknown'
  dt[! B_GWL_CLASS %in% c('GtI','GtII','GtIII','GtIV','GtV', 'GtVI','GtVII','GtVIII'), B_GWL_CLASS := '-']
  
  # calculate the OBIC water risk index for combined drought and wetstress (% yield reduction)
  dt[, npe_wri := 1] # When B_HELP_WENR is `unknown`
  if (nrow(dt[B_HELP_WENR != 'unknown',]) > 0) {
    dt[B_HELP_WENR != 'unknown', npe_wri := calc_waterstressindex(
      B_HELP_WENR = B_HELP_WENR, 
      B_LU_BRP = B_LU_BRP, 
      B_GWL_CLASS = B_GWL_CLASS, 
      WSI = 'waterstress'
    ) * 0.01]
  }
  
  # calculate the P-availability-index (P fertilizer is more efficient on low PBI)
  dt[,npe_pbi := calc_phosphate_availability(
    B_LU_BRP = B_LU_BRP,
    A_P_AL = A_P_AL,
    A_P_CC = A_P_CC,
    A_P_WA = A_P_WA
  )]
  
  # transform npe_pbi to an index between 0 and 1
  dt[,npe_pbi := ind_phosphate_availability(npe_pbi)]
  
  # calculate the drought stress, as factor controlling N-efficiency on grassland
  dt[, npe_wdri := 1] # When B_HELP_WENR is `unknown`
  if (nrow(dt[B_HELP_WENR != 'unknown',]) > 0) {
    dt[B_HELP_WENR != 'unknown', npe_wdri := calc_waterstressindex(
      B_HELP_WENR = B_HELP_WENR,
      B_LU_BRP = B_LU_BRP,
      B_GWL_CLASS = B_GWL_CLASS,
      WSI = 'droughtstress'
    ) * 0.01]
  }
  
  # rank the risk for N efficiency : low A_N_RT means high potential for improvement NUE
  dt[,npe_nlv := 1 - nsw_nlv]
  
  # estimate field properties that contribute to the water retention and water efficiency -------
  
  # calculate the OBIC water risk index for wetstress (% yield reduction)
  dt[, wue_wwri := 1] # When B_HELP_WENR is `unknown`
  if (nrow(dt[B_HELP_WENR != 'unknown',]) > 0) {
    dt[B_HELP_WENR != 'unknown', wue_wwri := calc_waterstressindex(
      B_HELP_WENR = B_HELP_WENR,
      B_LU_BRP = B_LU_BRP,
      B_GWL_CLASS = B_GWL_CLASS,
      WSI = 'wetnessstress'
    ) * 0.01]
  }
  
  # calculate the OBIC water risk index for droughtstress (% yield reduction)
  dt[,wue_wdri := npe_wdri]
  
  # calculate the possibility to store water (water holding capacity)
  dt[,wue_whc := calc_waterretention(
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_SILT_MI = A_SILT_MI,
    A_SOM_LOI = A_SOM_LOI,
    type = 'water holding capacity'
  )]
  
  # transform wue_whc to an index between 0 and 1
  dt[,wue_whc := 1 - evaluate_logistic(wue_whc, b = 25, x0 = 0.4,v = 0.35)]
  
  # order the fields
  setorder(dt, id)
  
  # what are the calculated relative impact of field properties 
  scol <- colnames(dt)[grepl('^wue|^npe|^psw|^nsw|^ngw',colnames(dt))]
  
  # select relevant output
  out <- dt[, mget(scol)]
  
  # return output
  return(out)
  
}
