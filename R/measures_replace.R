require("data.table")
setwd('C:/R_packages_Tessa/BedrijfsBodemWaterPlanCalculator/dev')
measures_from_main <- fread("measures_from_main.csv")
measures <- fread("measures.csv")

colnames(measures)

measures$psw_psg_low <- measures_from_main$psw_psg_low
measures$psw_psg_medium <- measures_from_main$psw_psg_medium
measures$psw_psg_high <- measures_from_main$psw_psg_high
measures$nsw_gwl_lowh <- measures_from_main$psw_psg_high