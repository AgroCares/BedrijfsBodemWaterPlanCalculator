# test voor B en N - 2 november

# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

#### test voor B en N ####
measures <- rbind(data.table(id = 1, dt.measures[grepl('EG1C|EB8A|EB7',eco_id)]),
                  data.table(id = 3, dt.measures[grepl('EG5A|EB10B|EB12|EB11A',eco_id)]))
                  
measures$bbwp_status <- 'hello check'

B_SOILTYPE_AGR = c('dekzand','dekzand','dekzand','dekzand')
B_GWL_CLASS = c('Vb','III','Vb','Vb')
A_P_SG = c(56.11,56.28,52.87,60.43)
B_SLOPE_DEGREE = c(0.48,0.52,0.45,0.45)
B_AER_CBS = c('LG04','LG04','LG04','LG04')
B_LU_BBWP = c('mais','rustgewas','rustgewas','randensloot')
B_LU_BRP = c(259,235,235,343)
B_LU_ARABLE_ER = c(T,T,T,T)
B_LU_PRODUCTIVE_ER = c(T,T,T,F)
B_LU_CULTIVATED_ER = c(T,T,T,F)
M_DRAIN = c(T,T,F,F)
D_SA_W = c(0.04,0.39,0.55,0.24)
B_AREA = c(1000,30000,20000,10000)
farmscore = 100
medalscore = "gold"
measures = measures
sector = c('arable','dairy')
output = 'scores'


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
                                  output = 'scores'
)

# get farm scores
farmscores <- as.vector(unlist(bedrijfsscore$farm))[1:10]
scorenames <- as.vector(unlist(names(bedrijfsscore$farm)))[1:10]
out5 <- data.table(thema = scorenames,score = farmscores)
out5a <- as.data.table(bedrijfsscore$fields)

# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)


#### test voor B en N ####
measures <- rbind(data.table(id = 1, dt.measures[grepl('EB10A',eco_id)]))
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
                             output = 'scores'
)

# get farmscores
farmscores <- as.vector(unlist(bedrijfsscore$farm))[1:10]
scorenames <- as.vector(unlist(names(bedrijfsscore$farm)))[1:10]
out5 <- data.table(thema = scorenames,score = farmscores)
out5a <- as.data.table(bedrijfsscore$fields)
