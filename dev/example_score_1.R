library(jsonlite)

# Examples overgenomen van BBWP Service

# Example 1: calculate the BBWP Farm score


# Get required inputs -----------------------------------------------------

input <- fromJSON("dev/examples/bbwp_score_1.json")

properties <- as.data.table(input$fields$properties)
lsw <- as.data.table(input$fields$lsw)
measures <- input$fields$measures
sector <- input$farm$properties$sector

# Format B_GWL_CLASS to OBIC
properties[, b_gwl_class := gsub('a|b|s', '', b_gwl_class)]
properties[b_gwl_class == 'unknown', b_gwl_class := '-']
properties[b_gwl_class != '-', b_gwl_class := paste0('Gt', b_gwl_class)]

for (i in 1:length(measures)) {
  if (length(measures[[i]]) > 0) {
    measures[[i]]$id <- i
  } else {
  }
}



# Calculate the relative ranking of the properties ------------------------

dt.ranking <- bbwp_field_properties(
  B_SOILTYPE_AGR = properties$b_soiltype_agr,
  B_LU_BRP = properties$b_lu_brp,
  B_GWL_CLASS = properties$b_gwl_class,
  B_SC_WENR = properties$b_sc_wenr, 
  B_HELP_WENR = properties$b_help_wenr,
  A_CLAY_MI = as.numeric(properties$a_clay_mi),
  A_SAND_MI = as.numeric(properties$a_sand_mi),
  A_SILT_MI = as.numeric(properties$a_silt_mi),
  A_SOM_LOI = as.numeric(properties$a_som_loi), 
  A_N_RT = as.numeric(properties$a_n_rt),
  A_FE_OX = as.numeric(properties$a_fe_ox), 
  A_AL_OX = as.numeric(properties$a_al_ox), 
  A_P_CC = as.numeric(properties$a_p_cc), 
  A_P_AL = as.numeric(properties$a_p_al),
  A_P_WA = as.numeric(properties$a_p_wa), 
  A_P_SG = as.numeric(properties$a_p_sg),
  D_WP = as.numeric(properties$d_wp), 
  D_RO_R = as.numeric(properties$d_ro_r),
  LSW = lsw
)


# Calculate the risk indicators ------------------------------------------------------

dt.ind <- bbwp_field_indicators(
  D_NGW_SCR = dt.ranking$ngw_scr,
  D_NGW_LEA = dt.ranking$ngw_lea,
  D_NGW_NLV = dt.ranking$ngw_nlv,
  D_NSW_SCR = dt.ranking$nsw_scr,
  D_NSW_GWT = dt.ranking$nsw_gwt,
  D_NSW_RO = dt.ranking$nsw_ro,
  D_NSW_WS = dt.ranking$nsw_ws,
  D_NSW_NLV = dt.ranking$nsw_nlv,
  D_PSW_SCR = dt.ranking$psw_scr,
  D_PSW_GWT= dt.ranking$psw_gwt,
  D_PSW_RO = dt.ranking$psw_ro,
  D_PSW_WS = dt.ranking$psw_ws,
  D_PSW_PCC = dt.ranking$psw_pcc,
  D_PSW_PSG = dt.ranking$psw_psg,
  D_PSW_PRET = dt.ranking$psw_pret,
  D_NUE_WRI = dt.ranking$npe_wri,
  D_NUE_PBI = dt.ranking$npe_pbi,
  D_NUE_WDRI = dt.ranking$npe_wdri,
  D_NUE_NLV = dt.ranking$npe_nlv,
  D_WUE_WWRI = dt.ranking$wue_wwri,
  D_WUE_WDRI = dt.ranking$wue_wdri,
  D_WUE_WHC = dt.ranking$wue_whc
)


# Calculate the scores ----------------------------------------------------

dt.fields <- bbwp_field_scores(
  B_SOILTYPE_AGR = properties$b_soiltype_agr,
  B_GWL_CLASS = properties$b_gwl_class,
  A_P_SG = as.numeric(properties$a_p_sg),
  B_SLOPE = rep(FALSE, nrow(dt.ind)),
  B_LU_BRP = properties$b_lu_brp,
  M_DRAIN = properties$m_drain,
  D_WP = as.numeric(properties$d_wp),
  D_RISK_NGW = dt.ind$D_RISK_NGW,
  D_RISK_NSW = dt.ind$D_RISK_NSW,
  D_RISK_PSW = dt.ind$D_RISK_PSW,
  D_RISK_NUE = dt.ind$D_RISK_NUE,
  D_RISK_WB = dt.ind$D_RISK_WB,
  B_GWP = properties$b_gwp,
  B_AREA_DROUGHT = rep(TRUE, nrow(dt.ind)),
  B_CT_PSW = as.numeric(properties$b_ct_psw),
  B_CT_NSW = as.numeric(properties$b_ct_nsw),
  B_CT_PSW_MAX = 0.5, 
  B_CT_NSW_MAX = 5.0,
  measures = measures,
  sector = sector
)

# Calculate the score for the farm ----------------------------------------

dt.farm <- bbwp_farm_score(
  D_OPI_TOT = dt.fields$D_OPI_TOT,
  D_OPI_NGW = dt.fields$D_OPI_NGW,
  D_OPI_NSW = dt.fields$D_OPI_NSW,
  D_OPI_PSW = dt.fields$D_OPI_PSW,
  D_OPI_NUE = dt.fields$D_OPI_NUE,
  D_OPI_WB = dt.fields$D_OPI_WB,
  D_AREA = rep(1, nrow(dt.fields))
)    




# extra (not needed direct in workflow): meas_score -----------------------

# calculate the individual opportunity indexes
dt <- copy(properties)
dt[,D_OPI_NGW := OBIC::evaluate_logistic(dt.ind$D_RISK_NGW, b=7, x0=0.5 - 1 * .1, v=.7)]
dt[,D_OPI_NSW := OBIC::evaluate_logistic(dt.ind$D_RISK_NSW, b=7, x0=0.5 - 1 * .1, v=.7)]
dt[,D_OPI_PSW := OBIC::evaluate_logistic(dt.ind$D_RISK_PSW, b=7, x0=0.5 - 1 * .1, v=.7)]
dt[,D_OPI_NUE := OBIC::evaluate_logistic(dt.ind$D_RISK_NUE, b=7, x0=0.5 - 1 * .1, v=.7)]
dt[,D_OPI_WB :=  OBIC::evaluate_logistic(dt.ind$D_RISK_WB, b=7, x0=0.5 - 1 * .1, v=.7)]

bbwp_meas_score(
  B_SOILTYPE_AGR = properties$b_soiltype_agr, 
  B_GWL_CLASS = properties$b_gwl_class,  
  A_P_SG, B_SLOPE = properties$a_p_sg, 
  B_LU_BRP = properties$b_lu_brp, 
  M_DRAIN = properties$m_drain, 
  D_WP = properties$d_wp,
  D_OPI_NGW = dt$D_OPI_NGW, 
  D_OPI_NSW = dt$D_OPI_NSW, 
  D_OPI_PSW = dt$D_OPI_PSW, 
  D_OPI_NUE = dt$D_OPI_NUE, 
  D_OPI_WB = dt$D_OPI_WB,
  measures, 
  sector)
