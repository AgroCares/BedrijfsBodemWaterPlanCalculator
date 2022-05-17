#' Calculate the relative impact of field properties given its position in the landscape
#'
#' Estimate the relative ranking of field properties given their contribution to nutrient losses to aquatic ecosystems as well as nutrient and water efficiency.
#' A high rank is indicative for the number of opportunities to improve soil quality and land use.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BRP (numeric) The crop type (conform BRP coding, preferable the most frequent crop on the field)
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SC_WENR (character) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006)
#' @param B_HELP_WENR (character) The soil type abbreviation, derived from 1:50.000 soil map
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
#' @param LSW (data.table) The surface water polygon for catchment or polder
#'  
#' @import data.table
#' @import OBIC
#'
#' @export
# calculate the relative impact of field properties given its position in the landscape
bbwp_field_properties <- function(B_SOILTYPE_AGR, B_LU_BRP, B_GWL_CLASS, B_SC_WENR, B_HELP_WENR,
                                  A_CLAY_MI, A_SAND_MI, A_SILT_MI, A_SOM_LOI, A_N_RT,
                                  A_FE_OX, A_AL_OX, A_P_CC, A_P_AL ,A_P_WA, A_P_SG,
                                  D_WP, D_RO_R, LSW){
  
  ngw_scr = croptype.nleach = nf = ngw_lea = ngw_nlv = NULL
  nsw_scr = nsw_gwt = nsw_ro = nsw_ws = nsw_nlv = NULL 
  psw_scr = psw_gwt = psw_ro = psw_ws = psw_pcc = psw_pvg = psw_pret = NULL 
  npe_wri = npe_pbi = npe_wdri = npe_nlv = wue_wwri = wue_wdri = wue_whc = NULL
  
  # check length inputs
  arg.length <- max(
    length(B_SOILTYPE_AGR), length(B_LU_BRP), length(B_GWL_CLASS), length(B_SC_WENR), length(B_HELP_WENR),
    length(A_CLAY_MI), length(A_SAND_MI), length(A_SILT_MI), length(A_SOM_LOI), length(A_N_RT),
    length(A_FE_OX), length(A_AL_OX), length(A_P_CC), length(A_P_AL),length(A_P_WA), length(A_P_SG),
    length(D_WP), length(D_RO_R)
  )
  
  # check inputs B parameters
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype))
  checkmate::assert_subset(B_GWL_CLASS, choices = c('-', 'GtI','GtII','GtIIb','GtIII','GtIIIb','GtIV','GtV','GtVb','GtVI','GtVII','GtVIII'))
  checkmate::assert_subset(B_SC_WENR, choices = c(3, 4, 1, 401, 902, 2, 901, 5, 11, 10))
  checkmate::assert_subset(B_HELP_WENR, choices = c(unique(waterstress.obic$soilunit), "unknown"), empty.ok = FALSE)
  
  # check inputs A parameters
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_N_RT, lower = 0, upper = 100000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_AL_OX, lower = 0, upper = 1000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_FE_OX, lower = 0, upper = 1000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_P_SG, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_P_CC, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_P_AL, lower = 0, upper = 250, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_P_WA, lower = 0, upper = 250, any.missing = FALSE, len = arg.length)
  
  # check inputs D parameters
  checkmate::assert_numeric(D_WP, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_RO_R, lower = 0, upper = 1, len = arg.length)
  
  checkmate::assert_data_table(LSW, nrows = arg.length)
  
  # load in the datasets for soil and crop types and N leaching fractions
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  nleach_table <- as.data.table(OBIC::nleach_table)
  nleach_table <- nleach_table[leaching_to_set == 'gw']
  
  # copy input in one data.table
  dt <- data.table(
    field_id = 1:arg.length,
    B_SOILTYPE_AAGR = B_SOILTYPE_AGR,
    B_LU_BRP = B_LU_BRP,
    B_GWL_CLASS = B_GWL_CLASS,
    B_SC_WENR = B_SC_WENR, 
    B_HELP_WENR = B_HELP_WENR,
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
    D_RO_R = D_RO_R,
    LSW = LSW
  )
  
  # add crop names and categories
  dt <- merge(dt, crops.obic[, list(crop_code, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AAGR", by.y = "soiltype")
  
  
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
  dt[,ngw_nlv := pnorm(q = A_N_RT, mean = LSW.mean_n_rt, sd = LSW.sd_n_rt)]
  
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
  dt[,nsw_ro := pnorm(q = D_RO_R, mean = LSW.mean_ro_r, sd = LSW.sd_ro_r)]
  
  # rank the risk for wet surroundings (Van Gerven, 2018)
  # higher risk is associated to increased risks for N runoff
  dt[,nsw_ws := pnorm(q = D_WP, mean = LSW.mean_wp, sd = LSW.sd_wp)]
  
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
  
  # rank the risk for wet surroundings (Van Gerven, 2018)
  dt[,psw_ws := nsw_ws]
  
  # rank the risk for P pools in soil
  dt[,psw_pcc := pnorm(q = A_P_CC, mean = LSW.mean_p_cc, sd = LSW.sd_p_cc)]
  dt[,psw_psg := pnorm(q = A_P_SG, mean = LSW.mean_p_sg, sd = LSW.sd_p_sg)]
  dt[,psw_pret := 1- pnorm(q =  A_AL_OX + A_FE_OX, 
                           mean = LSW.mean_al_ox + LSW.mean_fe_ox, 
                           sd =  sqrt(LSW.sd_al_ox^2 + LSW.sd_fe_ox^2))]
  
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
  
  # return outputs
  setorder(dt, field_id)
  
  # what are the calculated relative impact of field properties 
  scol <- colnames(dt)[grepl('^wue|^npe|^psw|^nsw|^ngw',colnames(dt))]
  
  # select relevant output
  out <- dt[, mget(scol)]
  
  # return output
  return(out)
  
}
