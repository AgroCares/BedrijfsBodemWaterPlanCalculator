# script to check whether the addition of  `BLN::bln_wat_groundwater_recharge()`
# to bbwp_field_properties and the inclusion of `D_WUE_GWR` in bbwp_field_indicators
# affects the S_BBWP_WB score calculated by bbwp_field_scores

## Original functions ======
org_field_properties <- function(B_SOILTYPE_AGR, B_LU_BBWP, B_GWL_CLASS, B_SC_WENR, B_HELP_WENR,B_SLOPE_DEGREE,B_AER_CBS,
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
  
  # reformat B_AER_CBS and B_CS_WENR
  B_AER_CBS <- bbwp_format_aer(B_AER_CBS)
  B_SC_WENR <- bbwp_format_sc_wenr(B_SC_WENR)
  
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
  dt <- merge(dt, LSW, by = 'B_LSW_ID',all.x = TRUE)
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype",all.x=TRUE)
  
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

org_field_indicators<- function(D_NGW_SCR,D_NGW_LEA,D_NGW_NLV,
                                D_NSW_SCR,D_NSW_GWT,D_NSW_RO,D_NSW_SLOPE, D_NSW_WS,D_NSW_NLV,
                                D_PSW_SCR,D_PSW_GWT,D_PSW_RO,D_PSW_SLOPE,D_PSW_WS,D_PSW_PCC,D_PSW_PSG,D_PSW_PRET,
                                D_NUE_WRI,D_NUE_PBI,D_NUE_WDRI,D_NUE_NLV,
                                D_WUE_WWRI,D_WUE_WDRI,D_WUE_WHC, penalty = TRUE){
  
  # add visual bindings
  D_RISK_NGW = D_RISK_NSW = D_RISK_PSW = D_RISK_NUE = D_RISK_WB = id = NULL
  risk_cor = value = group = risk = mcf = WS = SLOPE = NULL
  
  # check length inputs
  arg.length <- max(
    length(D_NGW_SCR),length(D_NGW_LEA),length(D_NGW_NLV),
    length(D_NSW_SCR),length(D_NSW_GWT),length(D_NSW_RO),length(D_NSW_WS),length(D_NSW_NLV),length(D_NSW_SLOPE),
    length(D_PSW_SCR),length(D_PSW_GWT),length(D_PSW_RO),length(D_PSW_SLOPE),length(D_PSW_WS),length(D_PSW_PCC),length(D_PSW_PSG),length(D_PSW_PRET),
    length(D_NUE_WRI),length(D_NUE_PBI),length(D_NUE_WDRI),length(D_NUE_NLV),
    length(D_WUE_WWRI),length(D_WUE_WDRI),length(D_WUE_WHC)
  )
  
  # copy input in one data.table
  dt <- data.table(id = 1:arg.length,
                   D_NGW_SCR = D_NGW_SCR,
                   D_NGW_LEA = D_NGW_LEA,
                   D_NGW_NLV = D_NGW_NLV,
                   D_NSW_SCR = D_NSW_SCR,
                   D_NSW_GWT = D_NSW_GWT,
                   D_NSW_RO = D_NSW_RO,
                   D_NSW_SLOPE = D_NSW_SLOPE,
                   D_NSW_WS = D_NSW_WS,
                   D_NSW_NLV = D_NSW_NLV,
                   D_PSW_SCR = D_PSW_SCR,
                   D_PSW_GWT = D_PSW_GWT,
                   D_PSW_RO = D_PSW_RO,
                   D_PSW_SLOPE = D_PSW_SLOPE,
                   D_PSW_WS = D_PSW_WS,
                   D_PSW_PCC = D_PSW_PCC,
                   D_PSW_PSG = D_PSW_PSG,
                   D_PSW_PRET = D_PSW_PRET,
                   D_NUE_WRI = D_NUE_WRI,
                   D_NUE_PBI = D_NUE_PBI,
                   D_NUE_WDRI = D_NUE_WDRI,
                   D_NUE_NLV = D_NUE_NLV,
                   D_WUE_WWRI = D_WUE_WWRI,
                   D_WUE_WDRI = D_WUE_WDRI,
                   D_WUE_WHC = D_WUE_WHC
  )
  
  # melt the data.table to simplify corrections
  dt.melt <- data.table::melt(dt, id.vars = 'id',variable.name = 'risk')
  
  # add correction factor based on risk itself
  dt.melt[,risk_cor := wf(value,type = "indicators",penalty = penalty)]
  
  # add groups of risk indicators
  dt.melt[,group := gsub('_[A-Z]+$','',gsub('D_','',risk))]
  
  # add manual weighing factor for risks
  dt.melt[,mcf := 1]
  dt.melt[group=='NGW' & grepl('_LEA$',risk), mcf := 3]
  dt.melt[group=='NGW' & grepl('_NLV$',risk), mcf := 2]
  dt.melt[group=='NSW' & grepl('_NLV$',risk), mcf := 3]
  dt.melt[group=='PSW' & grepl('_SCR$|_RO$|_WS$',risk), mcf := 2]
  dt.melt[group=='NUE' & grepl('_PBI$',risk), mcf := 2]
  dt.melt[group=='WUE' & grepl('_WHC$',risk), mcf := 2]
  
  
  # minimize risks when there are no ditches around the field (wet surrounding fraction < 0.2)
  
  # add criteria properties as column (to use as filter)
  dt.melt[,WS := value[risk=='D_NSW_WS'],by='id']
  dt.melt[,SLOPE := value[risk=='D_NSW_SLOPE'],by='id']
  
  # ensure that the final risk after aggregation gets the value 0.1 or 0.01
  dt.melt[WS <= 0.2 & SLOPE < 1 & group %in% c('NSW','PSW'), c('mcf','risk_cor','value') :=  list(1,1000,0.1)]
  dt.melt[WS <= 0.1 & SLOPE < 1 & group %in% c('NSW','PSW'), c('mcf','risk_cor','value') :=  list(1,1000,0.01)]
  dt.melt[,c('WS','SLOPE') := NULL]
  
  # calculate the mean aggregated risk indicators
  dt <- dt.melt[,list(risk = sum(risk_cor * value * mcf)/sum(risk_cor * mcf)),by=c('id','group')]
  dt <- dcast(dt,id~group,value.var='risk')
  
  # replace output names
  setnames(dt,old=c('NGW','NSW','NUE','PSW','WUE'),new = c('D_RISK_NGW','D_RISK_NSW','D_RISK_NUE','D_RISK_PSW','D_RISK_WB'))
  
  # sort output based on id
  setorder(dt,id)
  
  # extract output
  out <- dt[,mget(c('D_RISK_NGW','D_RISK_NSW','D_RISK_PSW','D_RISK_NUE','D_RISK_WB'))]
  
  # return output
  return(out)
  
  
}

org_field_scores <-  function(B_SOILTYPE_AGR, B_GWL_CLASS, A_P_CC,A_P_AL, B_SLOPE_DEGREE, B_LU_BBWP,B_AER_CBS,
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

# input data =============
LSW = data.table(B_LSW_ID = 5, 
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
                 B_SA_W_SD = 0.33)

B_SOILTYPE_AGR  <-  c('dekzand', 'loess')
B_LU_BBWP  <-  c('gras_permanent','rooivrucht')
B_GWL_CLASS  <-  c('GtIII', '-')
B_SC_WENR  <-  c(4, 2) 
B_HELP_WENR  <-  c('AZW1AwF', 'AZW1AwF')
B_AER_CBS  <-  c('LG05','LG14')
B_SLOPE_DEGREE  <-  c(1.5,0.8)
A_CLAY_MI  <-  c(15, 5)
A_SAND_MI  <-  c(45, 65)
A_SILT_MI  <-  c(40, 30)
A_SOM_LOI  <-  c(5, 15) 
A_N_RT  <-  c(4200, 1000)
A_FE_OX  <-  c(500, 500) 
A_AL_OX  <-  c(150, 150)
A_P_CC  <-  c(5, 1) 
A_P_AL  <-  c(65, 5)
A_P_WA  <-  c(52, 5)
A_P_SG  <-  c(38, 78)
D_SA_W  <-  c(0.5, 1)
D_RO_R  <-  c(0.5, 0)
B_LSW_ID  <-  5
M_DRAIN  <-  c(FALSE, FALSE)
M_GREEN  <-  c(FALSE, TRUE)
penalty = TRUE

# comparison =====
### field properties ====
org.prop <- org_field_properties(
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
  D_RO_R =  D_RO_R, 
  B_LSW_ID = B_LSW_ID,
  LSW = LSW
)

new.prop <- bbwp_field_properties(
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
  D_RO_R =  D_RO_R, 
  B_LSW_ID = B_LSW_ID,
  LSW = LSW,
  M_DRAIN = M_DRAIN,
  M_GREEN = M_GREEN
)

testthat::test_that('Test that the output is the same except for the new column',{
  expect_true(ncol(org.prop) == ncol(new.prop)-1)
  
  org.cols <- names(org.prop)
  expect_identical(new.prop[,..org.cols],
                   org.prop[,..org.cols])
  
  expect_false(identical(
     new.prop,
     org.prop
  ))
})

### field_indicators ======
org.ind <- org_field_indicators(D_NGW_SCR = org.prop$ngw_scr,
                                D_NGW_LEA = org.prop$ngw_lea,
                                D_NGW_NLV = org.prop$ngw_nlv,
                                D_NSW_SCR = org.prop$nsw_scr,
                                D_NSW_GWT = org.prop$nsw_gwt,
                                D_NSW_RO = org.prop$nsw_ro,
                                D_NSW_SLOPE = org.prop$nsw_slope,
                                D_NSW_WS = org.prop$nsw_ws,
                                D_NSW_NLV = org.prop$nsw_nlv,
                                D_PSW_SCR = org.prop$psw_scr,
                                D_PSW_GWT= org.prop$psw_gwt,
                                D_PSW_RO = org.prop$psw_ro,
                                D_PSW_SLOPE = org.prop$psw_slope,
                                D_PSW_WS = org.prop$psw_ws,
                                D_PSW_PCC = org.prop$psw_pcc,
                                D_PSW_PSG = org.prop$psw_psg,
                                D_PSW_PRET = org.prop$psw_pret,
                                D_NUE_WRI = org.prop$npe_wri,
                                D_NUE_PBI = org.prop$npe_pbi,
                                D_NUE_WDRI = org.prop$npe_wdri,
                                D_NUE_NLV = org.prop$npe_nlv,
                                D_WUE_WWRI = org.prop$wue_wwri,
                                D_WUE_WDRI = org.prop$wue_wdri,
                                D_WUE_WHC = org.prop$wue_whc,
                                penalty = penalty
)

new.ind <- bbwp_field_indicators(D_NGW_SCR = new.prop$ngw_scr,
                                 D_NGW_LEA = new.prop$ngw_lea,
                                 D_NGW_NLV = new.prop$ngw_nlv,
                                 D_NSW_SCR = new.prop$nsw_scr,
                                 D_NSW_GWT = new.prop$nsw_gwt,
                                 D_NSW_RO = new.prop$nsw_ro,
                                 D_NSW_SLOPE = new.prop$nsw_slope,
                                 D_NSW_WS = new.prop$nsw_ws,
                                 D_NSW_NLV = new.prop$nsw_nlv,
                                 D_PSW_SCR = new.prop$psw_scr,
                                 D_PSW_GWT= new.prop$psw_gwt,
                                 D_PSW_RO = new.prop$psw_ro,
                                 D_PSW_SLOPE = new.prop$psw_slope,
                                 D_PSW_WS = new.prop$psw_ws,
                                 D_PSW_PCC = new.prop$psw_pcc,
                                 D_PSW_PSG = new.prop$psw_psg,
                                 D_PSW_PRET = new.prop$psw_pret,
                                 D_NUE_WRI = new.prop$npe_wri,
                                 D_NUE_PBI = new.prop$npe_pbi,
                                 D_NUE_WDRI = new.prop$npe_wdri,
                                 D_NUE_NLV = new.prop$npe_nlv,
                                 D_WUE_WWRI = new.prop$wue_wwri,
                                 D_WUE_WDRI = new.prop$wue_wdri,
                                 D_WUE_WHC = new.prop$wue_whc,
                                 D_WUE_GWR = new.prop$wue_gwr,
                                 penalty = penalty
)

testthat::test_that("Test that the output is the same except for D_RISK_WB which should differ",{
  expect_identical(org.ind[,.(D_RISK_NGW, D_RISK_NSW, D_RISK_PSW, D_RISK_NUE)],
                   new.ind[,.(D_RISK_NGW, D_RISK_NSW, D_RISK_PSW, D_RISK_NUE)])
  expect_false(identical(
    org.ind$D_RISK_WB,
    new.ind$D_RISK_WB
  ))
})

print("Note, new D_RISK_WB values are higher")

### field_scores ======
# properties for field scores
B_GWP <- c(FALSE, FALSE)
B_AREA_DROUGHT <- c(TRUE, TRUE)
B_CT_PSW <- c(25, 25)
B_CT_NSW = c(50, 50) 
B_CT_PSW_MAX = 0.5 
B_CT_NSW_MAX = 5.0

org.score <- bbwp_field_scores(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                               B_GWL_CLASS = B_GWL_CLASS,
                               A_P_CC = A_P_CC,
                               A_P_AL = A_P_AL,
                               B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                               B_LU_BBWP = B_LU_BBWP,
                               B_AER_CBS = B_AER_CBS,
                               M_DRAIN = M_DRAIN,
                               D_SA_W = D_SA_W,
                               D_RISK_NGW = org.ind$D_RISK_NGW,
                               D_RISK_NSW = org.ind$D_RISK_NSW,
                               D_RISK_PSW = org.ind$D_RISK_PSW,
                               D_RISK_NUE = org.ind$D_RISK_NUE,
                               D_RISK_WB = org.ind$D_RISK_WB,
                               B_GWP = B_GWP,
                               B_AREA_DROUGHT = B_AREA_DROUGHT,
                               B_CT_PSW = B_CT_PSW,
                               B_CT_NSW = B_CT_NSW,
                               B_CT_PSW_MAX = B_CT_PSW_MAX, 
                               B_CT_NSW_MAX = B_CT_NSW_MAX,
                               measures = NULL,
                               sector = sector,
                               penalty = penalty,
                               B_LS_HYDROCAT = B_LS_HYDROCAT
)

new.score <- bbwp_field_scores(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                               B_GWL_CLASS = B_GWL_CLASS,
                               A_P_CC = A_P_CC,
                               A_P_AL = A_P_AL,
                               B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                               B_LU_BBWP = B_LU_BBWP,
                               B_AER_CBS = B_AER_CBS,
                               M_DRAIN = M_DRAIN,
                               D_SA_W = D_SA_W,
                               D_RISK_NGW = new.ind$D_RISK_NGW,
                               D_RISK_NSW = new.ind$D_RISK_NSW,
                               D_RISK_PSW = new.ind$D_RISK_PSW,
                               D_RISK_NUE = new.ind$D_RISK_NUE,
                               D_RISK_WB = new.ind$D_RISK_WB,
                               B_GWP = B_GWP,
                               B_AREA_DROUGHT = B_AREA_DROUGHT,
                               B_CT_PSW = B_CT_PSW,
                               B_CT_NSW = B_CT_NSW,
                               B_CT_PSW_MAX = B_CT_PSW_MAX, 
                               B_CT_NSW_MAX = B_CT_NSW_MAX,
                               measures = NULL,
                               sector = sector,
                               penalty = penalty,
                               B_LS_HYDROCAT = B_LS_HYDROCAT
)


testthat::test_that("Test that S_BBWP_WB is different but the other scores remain unchanged", {
  expect_identical(org.score[,.(S_BBWP_NGW, S_BBWP_NSW, S_BBWP_PSW, S_BBWP_NUE)],
                   new.score[,.(S_BBWP_NGW, S_BBWP_NSW, S_BBWP_PSW, S_BBWP_NUE)])
  
  expect_false(
    identical(org.score$S_BBWP_TOT, new.score$S_BBWP_TOT)
  )
  
  expect_false(
    identical(org.score$S_BBWP_WB, new.score$S_BBWP_WB)
  )
})

### farm_score =====
# farm_score parameters
B_AREA = c(10, 10)


org.farm <- bbwp_farm_score(S_BBWP_TOT = org.score$S_BBWP_TOT,
                           S_BBWP_NGW = org.score$S_BBWP_NGW,
                           S_BBWP_NSW = org.score$S_BBWP_NSW,
                           S_BBWP_PSW = org.score$S_BBWP_PSW,
                           S_BBWP_NUE = org.score$S_BBWP_NUE,
                           S_BBWP_WB = org.score$S_BBWP_WB,
                           B_AREA = B_AREA)

new.farm <- bbwp_farm_score(S_BBWP_TOT = new.score$S_BBWP_TOT,
                           S_BBWP_NGW = new.score$S_BBWP_NGW,
                           S_BBWP_NSW = new.score$S_BBWP_NSW,
                           S_BBWP_PSW = new.score$S_BBWP_PSW,
                           S_BBWP_NUE = new.score$S_BBWP_NUE,
                           S_BBWP_WB = new.score$S_BBWP_WB,
                           B_AREA = B_AREA)

testthat::test_that("Test that S_BBWP_WB is different but the other scores remain unchanged", {
  expect_identical(org.farm[,.(S_BBWP_NGW, S_BBWP_NSW, S_BBWP_PSW, S_BBWP_NUE)],
                   new.farm[,.(S_BBWP_NGW, S_BBWP_NSW, S_BBWP_PSW, S_BBWP_NUE)])
  
  expect_false(
    identical(org.farm$S_BBWP_TOT, new.farm$S_BBWP_TOT)
  )
  
  expect_false(
    identical(org.farm$S_BBWP_WB, new.farm$S_BBWP_WB)
  )
})

# Plot differences ======
pdt <- copy(org.score)
pdt[,bbwpVersion := "original"]
pdt <- rbindlist(list(pdt, new.score), fill = TRUE)
pdt[is.na(bbwpVersion), bbwpVersion := "new"]
pdt[,bbwpVersion := as.factor(bbwpVersion)]
pdt <- melt(pdt, id.vars = "bbwpVersion")

ggplot2::ggplot(data = pdt, 
                ggplot2::aes(x = bbwpVersion,
                             y = value,
                             col = bbwpVersion,
                             fill = bbwpVersion)) +
  ggplot2::geom_violin() +
  NMI.Themes::theme_NMI() +
  NMI.Themes::scale_fill_nmi() +
  NMI.Themes::scale_col_nmi() +
  ggplot2::ylim(0, 100) +
  ggplot2::facet_wrap(facets = "variable")
