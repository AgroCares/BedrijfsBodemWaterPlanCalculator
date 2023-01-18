# tests B&N 25 November 2022

# LOCATION TO STORE CSV TESTS
loc <- 'C:/Astrid/B&N_tests_25nov22/'

### test scenarios on request for BenN ###
# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# measures 
measures <- NULL
measures$bbwp_status <- NULL


## bedrijf 1
#measures taken 
measures <- rbind(data.table(id = 2, dt.measures[grepl('EB4B',eco_id)]),
                  data.table(id = 3, dt.measures[grepl('EB13A',eco_id)]),
                  data.table(id = 4, dt.measures[grepl('EB13B',eco_id)]))

measures$bbwp_status <- 'none'



# uitgangspunten bedrijf 1
B_SOILTYPE_AGR = c('zeeklei','zeeklei','dekzand','dekzand','zeeklei','zeeklei','zeeklei')
B_GWL_CLASS = rep('GtIII',7)
A_P_SG = rep(12,7)
B_SLOPE_DEGREE = rep(1.5,7)
B_AER_CBS = rep('LG06',7)
B_LU_BBWP = c('gras_tijdelijk','rooivrucht','rooivrucht','rooivrucht','groenten','groenten','randensloot')
B_LU_BRP = c(266,1909,256,262,2789,2799,334)
B_LU_ARABLE_ER = c(F,T,T,T,T,T,F)
B_LU_PRODUCTIVE_ER = c(T,T,T,T,T,T,F)
B_LU_CULTIVATED_ER = c(T,T,T,T,T,T,T)
M_DRAIN = rep(T,7)
D_SA_W = rep(0.5,7)
B_AREA = c(10000,20000,30000,40000,50000,60000,1000)
farmscore = 100
medalscore = "gold"
measures = measures
sector = c('arable','dairy')
output = 'scores'

bedrijfsscore1 <- ecoregeling(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                              B_GWL_CLASS = B_GWL_CLASS,
                              B_AER_CBS = B_AER_CBS,
                              A_P_SG = A_P_SG,
                              B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                              B_AREA = B_AREA,
                              B_LU_BBWP = B_LU_BBWP,
                              B_LU_BRP = B_LU_BRP,
                              B_LU_ARABLE_ER = B_LU_ARABLE_ER,
                              B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                              B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                              M_DRAIN = M_DRAIN, 
                              D_SA_W = D_SA_W,
                              measures = measures, 
                              sector = sector,
                              farmscore = farmscore,
                              medalscore = medalscore,
                              output = 'scores',
                              pdf = F
)

# get farmscores
farmscores <- as.vector(unlist(bedrijfsscore1$farm))[1:10]
scorenames <- as.vector(unlist(names(bedrijfsscore1$farm)))[1:10]
out1 <- data.table(thema = scorenames,score = farmscores)
out1a <- as.data.table(bedrijfsscore1$fields)
out1a <- out1a[, !c("s_er_tot")]

# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
write.csv2(aim,paste0(loc, "aimbedrijf1.csv"))

# write scores
write.csv2(out1,paste0(loc, "scoresbedrijf1.csv"))
write.csv2(out1a,paste0(loc, "scoresveldenbedrijf1.csv"))







## uitgangspunten bedrijf 2A
measures <- rbind(data.table(id = 1, dt.measures[grepl('EB20',eco_id)]),
                  data.table(id = 4, dt.measures[grepl('EB7A|EB11A',eco_id)]))

measures$bbwp_status <- 'none'

B_SOILTYPE_AGR = c('veen','zeeklei','zeeklei','dekzand','dekzand')
B_GWL_CLASS = rep('GtIII',5)
A_P_SG = rep(12,5)
B_SLOPE_DEGREE = rep(1.5,5)
B_AER_CBS = rep('LG02',5)
B_LU_BBWP = c('rustgewas','rustgewas','rooivrucht','rooivrucht','rooivrucht')
B_LU_BRP = c(233,233,256,2017,2785)
B_LU_ARABLE_ER = c(T,T,T,T,T)
B_LU_PRODUCTIVE_ER = c(T,T,T,T,T)
B_LU_CULTIVATED_ER = c(T,T,T,T,T)
M_DRAIN = rep(T,5)
D_SA_W = rep(0.5,5)
B_AREA = c(10000,20000,20000,30000,40000)
farmscore = 100
medalscore = "gold"
measures = measures
sector = c('arable','dairy')
output = 'scores'


bedrijfsscore2 <- ecoregeling(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                              B_GWL_CLASS = B_GWL_CLASS,
                              B_AER_CBS = B_AER_CBS,
                              A_P_SG = A_P_SG,
                              B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                              B_AREA = B_AREA,
                              B_LU_BBWP = B_LU_BBWP,
                              B_LU_BRP = B_LU_BRP,
                              B_LU_ARABLE_ER = B_LU_ARABLE_ER,
                              B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                              B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                              M_DRAIN = M_DRAIN, 
                              D_SA_W = D_SA_W,
                              measures = measures, 
                              sector = sector,
                              farmscore = farmscore,
                              medalscore = medalscore,
                              output = 'scores',
                              pdf = F
)

# get farmscores
farmscores <- as.vector(unlist(bedrijfsscore2$farm))[1:10]
scorenames <- as.vector(unlist(names(bedrijfsscore2$farm)))[1:10]
out2 <- data.table(thema = scorenames,score = farmscores)
out2a <- as.data.table(bedrijfsscore2$fields)
out2a <- out2a[, !c("s_er_tot")]

# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
write.csv2(aim,paste0(loc, "aimbedrijf2A.csv"))

# write scores
write.csv2(out2,paste0(loc, "scoresbedrijf2A.csv"))
write.csv2(out2a,paste0(loc, "scoresveldenbedrijf2A.csv"))







## uitgangspunten bedrijf 2B

measures <- rbind(data.table(id = 1, dt.measures[grepl('EB13A',eco_id)]),
                  data.table(id = 3, dt.measures[grepl('EB13B',eco_id)]),
                  data.table(id = 5, dt.measures[grepl('EB13A',eco_id)]))

measures$bbwp_status <- 'hello check'

B_SOILTYPE_AGR = c('veen','zeeklei','zeeklei','dekzand','dekzand','dekzand')
B_GWL_CLASS = rep('GtIII',6)
A_P_SG = rep(12,6)
B_SLOPE_DEGREE = rep(1.5,6)
B_AER_CBS = rep('LG02',6)
B_LU_BBWP = c('rustgewas','rustgewas','rooivrucht','rooivrucht','rooivrucht','randensloot')
B_LU_BRP = c(233,233,256,2017,2785,337)
B_LU_ARABLE_ER = c(T,T,T,T,T,T)
B_LU_PRODUCTIVE_ER = c(T,T,T,T,T,F)
B_LU_CULTIVATED_ER = c(T,T,T,T,T,T)
M_DRAIN = rep(T,6)
D_SA_W = rep(0.5,6)
B_AREA = c(10000,20000,20000,30000,40000,1000)
farmscore = 100
medalscore = "gold"
measures = measures
sector = c('arable','dairy')
output = 'scores'


bedrijfsscore2 <- ecoregeling(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                              B_GWL_CLASS = B_GWL_CLASS,
                              B_AER_CBS = B_AER_CBS,
                              A_P_SG = A_P_SG,
                              B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                              B_AREA = B_AREA,
                              B_LU_BBWP = B_LU_BBWP,
                              B_LU_BRP = B_LU_BRP,
                              B_LU_ARABLE_ER = B_LU_ARABLE_ER,
                              B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                              B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                              M_DRAIN = M_DRAIN, 
                              D_SA_W = D_SA_W,
                              measures = measures, 
                              sector = sector,
                              farmscore = farmscore,
                              medalscore = medalscore,
                              output = 'scores'
)

# get farmscores
farmscores <- as.vector(unlist(bedrijfsscore2$farm))[1:10]
scorenames <- as.vector(unlist(names(bedrijfsscore2$farm)))[1:10]
out2 <- data.table(thema = scorenames,score = farmscores)
out2a <- as.data.table(bedrijfsscore2$fields)
out2a <- out2a[, !c("s_er_tot")]

# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
write.csv2(aim,paste0(loc, "aimbedrijf2B.csv"))

# write scores
write.csv2(out2,paste0(loc, "scoresbedrijf2B.csv"))
write.csv2(out2a,paste0(loc, "scoresveldenbedrijf2B.csv"))







## uitgangspunten bedrijf 3A
measures <- rbind(data.table(id = 2, dt.measures[grepl('EG10A|EG1C|EG5B',eco_id)]),
                  data.table(id = 3, dt.measures[grepl('EB12|EB15',eco_id)]))

measures$bbwp_status <- 'none'

B_SOILTYPE_AGR = c('dekzand','dekzand','zeeklei','loess','dekzand')
B_GWL_CLASS = rep('GtIII',5)
A_P_SG = rep(12,5)
B_SLOPE_DEGREE = rep(1.5,5)
B_AER_CBS = rep('LG04',5)
B_LU_BBWP = c('natuur','gras_tijdelijk','rooivrucht','gras_permanent','gras_tijdelijk')
B_LU_BRP = c(2621,266,1909,265,266)
B_LU_ARABLE_ER = c(T,F,T,F,F)
B_LU_PRODUCTIVE_ER = c(F,T,T,T,T)
B_LU_CULTIVATED_ER = c(F,T,T,T,T)
M_DRAIN = rep(T,5)
D_SA_W = rep(0.5,5)
B_AREA = c(1000,20000,30000,40000,20000)
farmscore = 100
medalscore = "gold"
measures = measures
sector = c('arable','dairy')
output = 'scores'



bedrijfsscore3 <- ecoregeling(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                              B_GWL_CLASS = B_GWL_CLASS,
                              B_AER_CBS = B_AER_CBS,
                              A_P_SG = A_P_SG,
                              B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                              B_AREA = B_AREA,
                              B_LU_BBWP = B_LU_BBWP,
                              B_LU_BRP = B_LU_BRP,
                              B_LU_ARABLE_ER = B_LU_ARABLE_ER,
                              B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                              B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                              M_DRAIN = M_DRAIN, 
                              D_SA_W = D_SA_W,
                              measures = measures, 
                              sector = sector,
                              farmscore = farmscore,
                              medalscore = medalscore,
                              output = 'scores',
                              pdf=T
)

# get farmscores
farmscores <- as.vector(unlist(bedrijfsscore3$farm))[1:10]
scorenames <- as.vector(unlist(names(bedrijfsscore3$farm)))[1:10]
out3 <- data.table(thema = scorenames,score = farmscores)
out3a <- as.data.table(bedrijfsscore3$fields)
out3a <- out3a[, !c("s_er_tot")]

# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
write.csv2(aim,paste0(loc, "aimbedrijf3A.csv"))

# write scores
write.csv2(out3,paste0(loc, "scoresbedrijf3A.csv"))
write.csv2(out3a,paste0(loc, "scoresveldenbedrijf3A.csv"))







## uitgangspunten bedrijf 3B
measures <- rbind(data.table(id = 1, dt.measures[grepl('EG5B',eco_id)]))

measures$bbwp_status <- 'hello check'

B_SOILTYPE_AGR = c('dekzand','dekzand','zeeklei','loess','dekzand')
B_GWL_CLASS = rep('GtIII',5)
A_P_SG = rep(12,5)
B_SLOPE_DEGREE = rep(1.5,5)
B_AER_CBS = rep('LG04',5)
B_LU_BBWP = c('natuur','gras_tijdelijk','rooivrucht','gras_permanent','randensloot')
B_LU_BRP = c(2621,266,1909,265,3721)
B_LU_ARABLE_ER = c(T,F,T,F,T)
B_LU_PRODUCTIVE_ER = c(F,T,T,T,F)
B_LU_CULTIVATED_ER = c(F,T,T,T,T)
M_DRAIN = rep(T,5)
D_SA_W = rep(0.5,5)
B_AREA = c(1000,20000,30000,40000,1000)
farmscore = 100
medalscore = "gold"
measures = measures
sector = c('arable','dairy')
output = 'scores'



bedrijfsscore3 <- ecoregeling(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                              B_GWL_CLASS = B_GWL_CLASS,
                              B_AER_CBS = B_AER_CBS,
                              A_P_SG = A_P_SG,
                              B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                              B_AREA = B_AREA,
                              B_LU_BBWP = B_LU_BBWP,
                              B_LU_BRP = B_LU_BRP,
                              B_LU_ARABLE_ER = B_LU_ARABLE_ER,
                              B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                              B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                              M_DRAIN = M_DRAIN, 
                              D_SA_W = D_SA_W,
                              measures = measures, 
                              sector = sector,
                              farmscore = farmscore,
                              medalscore = medalscore,
                              output = 'scores'
)

# get farmscores
farmscores <- as.vector(unlist(bedrijfsscore3$farm))[1:10]
scorenames <- as.vector(unlist(names(bedrijfsscore3$farm)))[1:10]
out3 <- data.table(thema = scorenames,score = farmscores)
out3a <- as.data.table(bedrijfsscore3$fields)
out3a <- out3a[, !c("s_er_tot")]

# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
write.csv2(aim,paste0(loc, "aimbedrijf3B.csv"))

# write scores
write.csv2(out3,paste0(loc, "scoresbedrijf3B.csv"))
write.csv2(out3a,paste0(loc, "scoresveldenbedrijf3B.csv"))













## uitgangspunten bedrijf 4A
measures <- rbind(data.table(id = 1, dt.measures[grepl('EG8A',eco_id)]),
                  data.table(id = 2, dt.measures[grepl('EG7B',eco_id)]),
                  data.table(id = 4, dt.measures[grepl('EG16C',eco_id)]),
                  data.table(id = 5, dt.measures[grepl('EG14',eco_id)]),
                  data.table(id = 6, dt.measures[grepl('EG13',eco_id)]),
                  data.table(id = 7, dt.measures[grepl('EG14',eco_id)])
                  )


measures$bbwp_status <- 'none'


B_SOILTYPE_AGR = c('veen','veen','zeeklei','zeeklei','dekzand','dekzand','dekzand')
B_GWL_CLASS = rep('GtIII',7)
A_P_SG = rep(12,7)
B_SLOPE_DEGREE = rep(1.7,7)
B_AER_CBS = rep('LG09',7)
B_LU_BBWP = c('randensloot','gras_permanent','gras_tijdelijk','gras_permanent','randensloot','randensloot','randensloot')
B_LU_BRP = c(343,265,266,265,343,343,343)
B_LU_ARABLE_ER = c(F,F,F,F,F,F,F)
B_LU_PRODUCTIVE_ER = c(F,T,T,T,F,F,F)
B_LU_CULTIVATED_ER = c(F,T,T,T,F,F,F)
M_DRAIN = rep(T,7)
D_SA_W = rep(0.7,7)
B_AREA = c(10000,20000,30000,40000,1000,2000,3000)
farmscore = 100
medalscore = "gold"
measures = measures
sector = c('arable','dairy')
output = 'scores'



bedrijfsscore4 <- ecoregeling(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                              B_GWL_CLASS = B_GWL_CLASS,
                              B_AER_CBS = B_AER_CBS,
                              A_P_SG = A_P_SG,
                              B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                              B_AREA = B_AREA,
                              B_LU_BBWP = B_LU_BBWP,
                              B_LU_BRP = B_LU_BRP,
                              B_LU_ARABLE_ER = B_LU_ARABLE_ER,
                              B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                              B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                              M_DRAIN = M_DRAIN, 
                              D_SA_W = D_SA_W,
                              measures = measures, 
                              sector = sector,
                              farmscore = farmscore,
                              medalscore = medalscore,
                              output = 'scores',
                              pdf = F
)

# get farmscores
farmscores <- as.vector(unlist(bedrijfsscore4$farm))[1:10]
scorenames <- as.vector(unlist(names(bedrijfsscore4$farm)))[1:10]
out4 <- data.table(thema = scorenames,score = farmscores)
out4a <- as.data.table(bedrijfsscore4$fields)
out4a <- out4a[, !c("s_er_tot")]


# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
write.csv2(aim,paste0(loc, "aimbedrijf4A.csv"))

# write scores
write.csv2(out4,paste0(loc, "scoresbedrijf4A.csv"))
write.csv2(out4a,paste0(loc, "scoresveldenbedrijf4A.csv"))





## uitgangspunten bedrijf 4B
measures <- rbind(data.table(id = 5, dt.measures[grepl('EG14',eco_id)]))

measures$bbwp_status <- 'hello check'

B_SOILTYPE_AGR = c('veen','veen','zeeklei','zeeklei','dekzand','dekzand','dekzand')
B_GWL_CLASS = rep('GtIII',7)
A_P_SG = rep(12,7)
B_SLOPE_DEGREE = rep(1.7,7)
B_AER_CBS = rep('LG09',7)
B_LU_BBWP = c('randensloot','gras_permanent','gras_tijdelijk','gras_permanent','randensloot','randensloot','randensloot')
B_LU_BRP = c(343,265,266,265,343,343,343)
B_LU_ARABLE_ER = c(F,F,F,F,F,F,F)
B_LU_PRODUCTIVE_ER = c(F,T,T,T,F,F,F)
B_LU_CULTIVATED_ER = c(F,T,T,T,F,F,F)
M_DRAIN = rep(T,7)
D_SA_W = rep(0.7,7)
B_AREA = c(10000,20000,30000,40000,2000,1000,2000)
farmscore = 100
medalscore = "gold"
measures = measures
sector = c('arable','dairy')
output = 'scores'



bedrijfsscore4 <- ecoregeling(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                              B_GWL_CLASS = B_GWL_CLASS,
                              B_AER_CBS = B_AER_CBS,
                              A_P_SG = A_P_SG,
                              B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                              B_AREA = B_AREA,
                              B_LU_BBWP = B_LU_BBWP,
                              B_LU_BRP = B_LU_BRP,
                              B_LU_ARABLE_ER = B_LU_ARABLE_ER,
                              B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                              B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                              M_DRAIN = M_DRAIN, 
                              D_SA_W = D_SA_W,
                              measures = measures, 
                              sector = sector,
                              farmscore = farmscore,
                              medalscore = medalscore,
                              output = 'scores'
)

# get farmscores
farmscores <- as.vector(unlist(bedrijfsscore4$farm))[1:10]
scorenames <- as.vector(unlist(names(bedrijfsscore4$farm)))[1:10]
out4 <- data.table(thema = scorenames,score = farmscores)
out4a <- as.data.table(bedrijfsscore4$fields)
out4a <- out4a[, !c("s_er_tot")]


# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
write.csv2(aim,paste0(loc, "aimbedrijf4B.csv"))

# write scores
write.csv2(out4,paste0(loc, "scoresbedrijf4B.csv"))
write.csv2(out4a,paste0(loc, "scoresveldenbedrijf4B.csv"))









## uitgangspunten bedrijf 4C
measures <- rbind(data.table(id = 5, dt.measures[grepl('EG13',eco_id)]))

measures$bbwp_status <- 'hello check'

B_SOILTYPE_AGR = c('veen','veen','zeeklei','zeeklei','dekzand','dekzand','dekzand')
B_GWL_CLASS = rep('GtIII',7)
A_P_SG = rep(12,7)
B_SLOPE_DEGREE = rep(1.7,7)
B_AER_CBS = rep('LG09',7)
B_LU_BBWP = c('randensloot','gras_permanent','gras_tijdelijk','gras_permanent','randensloot','randensloot','randensloot')
B_LU_BRP = c(343,265,266,265,343,343,343)
B_LU_ARABLE_ER = c(F,F,F,F,F,F,F)
B_LU_PRODUCTIVE_ER = c(F,T,T,T,F,F,F)
B_LU_CULTIVATED_ER = c(F,T,T,T,F,F,F)
M_DRAIN = rep(T,7)
D_SA_W = rep(0.7,7)
B_AREA = c(10000,20000,30000,40000,2000,1000,2000)
farmscore = 100
medalscore = "gold"
measures = measures
sector = c('arable','dairy')
output = 'scores'



bedrijfsscore4 <- ecoregeling(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                              B_GWL_CLASS = B_GWL_CLASS,
                              B_AER_CBS = B_AER_CBS,
                              A_P_SG = A_P_SG,
                              B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                              B_AREA = B_AREA,
                              B_LU_BBWP = B_LU_BBWP,
                              B_LU_BRP = B_LU_BRP,
                              B_LU_ARABLE_ER = B_LU_ARABLE_ER,
                              B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                              B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                              M_DRAIN = M_DRAIN, 
                              D_SA_W = D_SA_W,
                              measures = measures, 
                              sector = sector,
                              farmscore = farmscore,
                              medalscore = medalscore,
                              output = 'scores'
)

# get farmscores
farmscores <- as.vector(unlist(bedrijfsscore4$farm))[1:10]
scorenames <- as.vector(unlist(names(bedrijfsscore4$farm)))[1:10]
out4 <- data.table(thema = scorenames,score = farmscores)
out4a <- as.data.table(bedrijfsscore4$fields)
out4a <- out4a[, !c("s_er_tot")]

# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
write.csv2(aim,paste0(loc, "aimbedrijf4C.csv"))

# write scores
write.csv2(out4,paste0(loc, "scoresbedrijf4C.csv"))
write.csv2(out4a,paste0(loc, "scoresveldenbedrijf4C.csv"))





## uitgangspunten bedrijf 5
measures <- rbind(data.table(id = 2, dt.measures[grepl('EG8A',eco_id)]))

measures$bbwp_status <- 'hello check'

B_SOILTYPE_AGR = c('veen','veen','zeeklei','zeeklei','dekzand','dekzand','dekzand')
B_GWL_CLASS = rep('GtIII',7)
A_P_SG = rep(12,7)
B_SLOPE_DEGREE = rep(1.7,7)
B_AER_CBS = rep('LG09',7)
B_LU_BBWP = c('randensloot','gras_permanent','gras_tijdelijk','gras_permanent','randensloot','randensloot','randensloot')
B_LU_BRP = c(343,265,266,265,343,343,343)
B_LU_ARABLE_ER = c(F,F,F,F,F,F,F)
B_LU_PRODUCTIVE_ER = c(F,T,T,T,F,F,F)
B_LU_CULTIVATED_ER = c(F,T,T,T,F,F,F)
M_DRAIN = rep(T,7)
D_SA_W = rep(0.7,7)
B_AREA = c(10000,20000,30000,40000,1000,2000,3000)
farmscore = 100
medalscore = "gold"
measures = measures
sector = c('arable','dairy')
output = 'scores'


bedrijfsscore5 <- ecoregeling(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                              B_GWL_CLASS = B_GWL_CLASS,
                              B_AER_CBS = B_AER_CBS,
                              A_P_SG = A_P_SG,
                              B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                              B_AREA = B_AREA,
                              B_LU_BBWP = B_LU_BBWP,
                              B_LU_BRP = B_LU_BRP,
                              B_LU_ARABLE_ER = B_LU_ARABLE_ER,
                              B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                              B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                              M_DRAIN = M_DRAIN, 
                              D_SA_W = D_SA_W,
                              measures = measures, 
                              sector = sector,
                              farmscore = farmscore,
                              medalscore = medalscore,
                              output = 'scores'
)

# get farmscores
farmscores <- as.vector(unlist(bedrijfsscore5$farm))[1:10]
scorenames <- as.vector(unlist(names(bedrijfsscore5$farm)))[1:10]
out5 <- data.table(thema = scorenames,score = farmscores)
out5a <- as.data.table(bedrijfsscore5$fields)
out5a <- out5a[, !c("s_er_tot")]

# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
write.csv2(aim,paste0(loc, "aimbedrijf5.csv"))

# write scores
write.csv2(out5,paste0(loc, "scoresbedrijf5.csv"))
write.csv2(out5a,paste0(loc, "scoresveldenbedrijf5.csv"))



#### test 6 ####
measures <- rbind(data.table(id = 1, dt.measures[grepl('EB10A',eco_id)]),
                  data.table(id = 2, dt.measures[grepl('EG10B|EB12',eco_id)]))


measures$bbwp_status <- 'hello check'

B_SOILTYPE_AGR = c('dekzand','rivierklei')
B_GWL_CLASS = c('GtVI','GtIIIb')
A_P_SG = c(52.63,23.04)
B_SLOPE_DEGREE = c(0.69,1.7)
B_AER_CBS = c('LG04','LG10')
B_LU_BBWP = c('gras_permanent','bollensierteelt')
B_LU_BRP = c(265,1000)
B_LU_ARABLE_ER = c(F,T)
B_LU_PRODUCTIVE_ER = c(T,T)
B_LU_CULTIVATED_ER = c(T,T)
M_DRAIN = rep(T,2)
D_SA_W = c(0.78,0.87)
B_AREA = c(58200,13100)
farmscore = 100
medalscore = "gold"
measures = measures
sector = c('arable','dairy','tree_nursery','bulbs')
output = 'scores'


bedrijfsscore_6 <- ecoregeling(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                               B_GWL_CLASS = B_GWL_CLASS,
                               B_AER_CBS = B_AER_CBS,
                               A_P_SG = A_P_SG,
                               B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                               B_AREA = B_AREA,
                               B_LU_BBWP = B_LU_BBWP,
                               B_LU_BRP = B_LU_BRP,
                               B_LU_ARABLE_ER = B_LU_ARABLE_ER,
                               B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                               B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                               M_DRAIN = M_DRAIN, 
                               D_SA_W = D_SA_W,
                               measures = measures, 
                               sector = sector,
                               farmscore = farmscore,
                               medalscore = medalscore,
                               output = 'scores'
)

# get farmscores
farmscores <- as.vector(unlist(bedrijfsscore_6$farm))[1:10]
scorenames <- as.vector(unlist(names(bedrijfsscore_6$farm)))[1:10]
out6 <- data.table(thema = scorenames,score = farmscores)
out6a <- as.data.table(bedrijfsscore_6$fields)
out6a <- out6a[, !c("s_er_tot")]

# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
write.csv2(aim,paste0(loc, "aimbedrijf6.csv"))

# write scores
write.csv2(out6,paste0(loc, "scoresbedrijf6.csv"))
write.csv2(out6a,paste0(loc, "scoresveldenbedrijf6.csv"))



#### test 7 ####
measures <- rbind(data.table(id = 1, dt.measures[grepl('EB189',eco_id)]),
                  data.table(id = 2, dt.measures[grepl('EG1C|EB12',eco_id)]),
                  data.table(id = 3, dt.measures[grepl('EB18|EB7A',eco_id)]))

measures$bbwp_status <- 'hello check'

B_SOILTYPE_AGR = c('dekzand','dekzand','dekzand','dekzand')
B_GWL_CLASS = c('Vb','III','Vb','Vb')
A_P_SG = c(56.11,56.28,52.87,60.43)
B_SLOPE_DEGREE = c(0.48,0.52,0.45,0.45)
B_AER_CBS = c('LG04','LG04','LG04','LG04')
B_LU_BBWP = c('mais','rooivrucht','rooivrucht','eiwitgewas')
B_LU_BRP = c(259,262,2785,239)
B_LU_ARABLE_ER = c(T,T,T,T)
B_LU_PRODUCTIVE_ER = c(T,T,T,T)
B_LU_CULTIVATED_ER = c(T,T,T,T)
M_DRAIN = c(T,T,F,F)
D_SA_W = c(0.04,0.39,0.55,0.24)
B_AREA = c(1000,30000,20000,10000)
farmscore = 100
medalscore = "gold"
measures = measures
sector = c('arable','dairy')
output = 'scores'
pdf = TRUE

bedrijfsscore <- ecoregeling(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                             B_GWL_CLASS = B_GWL_CLASS,
                             B_AER_CBS = B_AER_CBS,
                             A_P_SG = A_P_SG,
                             B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                             B_AREA = B_AREA,
                             B_LU_BBWP = B_LU_BBWP,
                             B_LU_BRP = B_LU_BRP,
                             B_LU_ARABLE_ER = B_LU_ARABLE_ER,
                             B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                             B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                             M_DRAIN = M_DRAIN, 
                             D_SA_W = D_SA_W,
                             measures = measures, 
                             sector = sector,
                             farmscore = farmscore,
                             medalscore = medalscore,
                             output = 'scores',
)


# get farm scores
farmscores <- as.vector(unlist(bedrijfsscore$farm))[1:10]
scorenames <- as.vector(unlist(names(bedrijfsscore$farm)))[1:10]
out7 <- data.table(thema = scorenames,score = farmscores)
out7a <- as.data.table(bedrijfsscore$fields)
out7a <- out7a[, !c("s_er_tot")]

# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
write.csv2(aim,paste0(loc, "aimbedrijf6.csv"))

# write scores
write.csv2(out7,paste0(loc, "scoresbedrijf6.csv"))
write.csv2(out7a,paste0(loc, "scoresveldenbedrijf6.csv"))







