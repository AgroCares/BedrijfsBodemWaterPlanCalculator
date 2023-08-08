# Make a list of variables for checkmates
library(pandex);library(data.table);library(stringr)

  
  # Load table and select parameters
  bbwp_parms <- pandex::nmi_parameters
  bbwp_parms <- bbwp_parms[code %in% c("B_SOILTYPE_AGR","B_LU_BBWP","B_LU_BRP","B_GWL_CLASS","B_SC_WENR","B_HELP_WENR","B_SLOPE_DEGREE","B_AER_CBS",
                                       "A_CLAY_MI","A_SAND_MI","A_SILT_MI","A_SOM_LOI","A_N_RT","A_FE_OX","A_AL_OX","A_P_CC","A_P_AL",
                                       "A_P_WA", "A_P_SG","B_CT_PSW","B_CT_NSW","B_CT_PSW_MAX","B_CT_NSW_MAX","D_SA_W","D_RO_R", "B_AREA")]
  
  # Unpack options
  for(this.code in bbwp_parms[enum == TRUE, code]){
    bbwp_parms[code == this.code, choices := list(pandex::enum_opts(this.code))]
  }
  bbwp_parms[code == 'B_GWL_CLASS', choices := list(paste0('Gt',pandex::enum_opts("B_GWL_CLASS")))]

  # Set upper for B_AREA
  bbwp_parms[code == 'B_AREA', value_max := 500000000]
  
  # Save data
  usethis::use_data(bbwp_parms,overwrite = TRUE)
