library(data.table)
library(BBWPC)

# LOCATION TO STORE CSV TESTS
loc <- 'C:/Astrid/BBWP_ecoregeling bestanden/'

# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# de bron data
B_SOILTYPE_AGR = c('dekzand')
B_GWL_CLASS = c('GtIII')
A_P_SG = c(12)
B_SLOPE_DEGREE = c(1.5)
B_AER_CBS = c('LG03')
B_LU_BBWP = c('eiwitgewas')
B_LU_BRP = c(244)
B_LU_ARABLE_ER = c(T)
B_LU_PRODUCTIVE_ER = c(T)
B_LU_CULTIVATED_ER = c(T)
M_DRAIN = c(TRUE)
D_SA_W = c(0.5)
B_AREA = c(10)
farmscore = 100
medalscore = "gold"
measures = dt.measures
sector = c('arable')
output = 'scores'

# list 1 bevat field and farm score, averaged on farm level
# list 2 bevat only farm measures (all measures for crop rotation plus level==farm)
list1 = list2 = list()

for(i in 1:nrow(dt.measures)){
  
  # assume the measures are not for GLB / ANLB
  measures <- rbind(data.table(id = 1, dt.measures[i]))
  measures$bbwp_status <- 'nothing'
  
  aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR,B_AREA = B_AREA, medalscore = medalscore)
  
  field <- er_field_scores(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                           B_AER_CBS = B_AER_CBS,
                           B_AREA = B_AREA,
                           B_LU_BBWP = B_LU_BBWP,
                           B_LU_BRP = B_LU_BRP,
                           B_LU_ARABLE_ER = B_LU_ARABLE_ER,
                           B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                           B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                           measures = measures, sector)
  list1[[i]] <- copy(cbind(field,aim))
  
  # add the generic farm score as baseline
  # this gives the averaged ER score based on the crops in crop rotation plan
  dt.farm <- er_croprotation(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                             B_LU_BBWP = B_LU_BBWP,
                             B_LU_BRP = B_LU_BRP,
                             B_LU_ARABLE_ER = B_LU_ARABLE_ER,
                             B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                             B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                             B_AER_CBS = B_AER_CBS,
                             B_AREA = B_AREA,
                             measures = measures,
                             sector = sector)
  
  list2[[i]] <- copy(dt.farm)
  
  # print
  print(i)
}

# field scores and aim
dt1 <- rbindlist(list1)
# dt.farm score for all crop rotation measures and measures where level == farm
dt2 <- rbindlist(list2)

# write as csv
#fwrite(dt2,paste0(loc, "farm_244_zand_LG03_akkerbouw.csv"))
#fwrite(dt1,paste0(loc, "field_244_zand_LG03_akkerbouw.csv"))

### total farm with three fields ###

# test case: 3x 10 ha, tijdelijk grasland, sand, LG05, dairy: with multiple measures
aantal = 3

# input data
B_SOILTYPE_AGR = rep('dekzand',aantal)
B_GWL_CLASS = rep('GtIII',aantal)
A_P_SG = rep(12,aantal)
B_SLOPE_DEGREE = rep(1.5,aantal)
B_AER_CBS = rep('LG05',aantal)
B_LU_BBWP = rep('gras_tijdelijk',aantal)
B_LU_BRP = rep(266,aantal)
B_LU_ARABLE_ER = rep(F,aantal)
B_LU_PRODUCTIVE_ER = rep(T,aantal)
B_LU_CULTIVATED_ER = rep(T,aantal)
M_DRAIN = rep(T,aantal)
D_SA_W = rep(0.5,aantal)
B_AREA = c(10,10,10)
farmscore = 100
medalscore = "gold"
measures = measures
sector = c('dairy')
output = 'scores'

measures <- rbind(data.table(id = 1, dt.measures[grepl('B138',bbwp_id)]),#EG16B
                  data.table(id = 2, dt.measures[grepl('EG2B$|EG1A',eco_id)]),
                  data.table(id = 3, dt.measures[grepl('EG14|EG18',eco_id)]))

measures$bbwp_status <- 'hello check'

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
                              output = 'scores'
)
#quick fix under bedrijfsscore1 farm must also get variable field_id, otherwise dimensions don't match
bedrijfsscore1$farm$field_id <- bedrijfsscore1$fields$field_id

#  fwrite(bedrijfsscore1,paste0(loc, "bedrijf1.csv"))

#  fwrite(bedrijfsscore1$fields,paste0(loc, "bedrijf1_fields.csv"))

### total farm with four fields ###


# test case: 4x 10 ha, luzerne, sand, arable, LG01: with multiple measures
aantal = 4

# input data
B_SOILTYPE_AGR = rep('dekzand',aantal)
B_GWL_CLASS = rep('GtIII',aantal)
A_P_SG = rep(12,aantal)
B_SLOPE_DEGREE = rep(1.5,aantal)
B_AER_CBS = rep('LG01',aantal)
B_LU_BBWP = rep('eiwitgewas',aantal)
B_LU_BRP = rep(258,aantal)
B_LU_ARABLE_ER = rep(T,aantal)
B_LU_PRODUCTIVE_ER = rep(T,aantal)
B_LU_CULTIVATED_ER = rep(T,aantal)
M_DRAIN = rep(T,aantal)
D_SA_W = rep(0.5,aantal)
B_AREA = c(10,10,10,20)
farmscore = 100
medalscore = "gold"
measures = measures
sector = c('arable')
output = 'scores'

measures <- rbind(data.table(id = 1, dt.measures[grepl('EG16B|EB13A',eco_id)]),
                  data.table(id = 2, dt.measures[grepl('EG2B$|EB8',eco_id)]),
                  data.table(id = 3, dt.measures[grepl('EG14|EB18',eco_id)]),
                  data.table(id = 4, dt.measures[grepl('EB15|EB3|EB23A|EB11A',eco_id)]))

measures$bbwp_status <- 'hello check'

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

#quick fix under bedrijfsscore1 farm must also get variable field_id, otherwise dimensions don't match
bedrijfsscore2$farm$field_id <- bedrijfsscore2$fields$field_id

#fwrite(bedrijfsscore2,paste0(loc, "bedrijf2.csv"))  
#fwrite(bedrijfsscore2$fields,paste0(loc, "bedrijf2_field.csv"))


## Bedrijf 3aanpassingen: Boerennatuur willen combinatie van binnen een zone 
#2x dekzand, maar andere maatregelen per perceel
#met niet=prodcutief land

aantal = 2
# input data
B_SOILTYPE_AGR = c('dekzand','dekzand')
B_GWL_CLASS = c('GtIII','GtIII')
A_P_SG = c(12,12)
B_SLOPE_DEGREE = c(1.5,1.5)
B_AER_CBS = c('LG01','LG01')
B_LU_BBWP = c('eiwitgewas','eiwitgewas')
B_LU_BRP = c(258,258)
B_LU_ARABLE_ER = c(T,T)
B_LU_PRODUCTIVE_ER = c(T,T)
B_LU_CULTIVATED_ER = c(T,T)
M_DRAIN = c(T,T)
D_SA_W = c(0.5,0.5)
B_AREA = c(10,10)
farmscore = 100
medalscore = "gold"
measures = measures
sector = c('arable')
output = 'scores'

measures <- rbind(data.table(id = 1, dt.measures[grepl('EG16B|EB13A',bbwp_id)]),
                  data.table(id = 2, dt.measures[grepl('EG20C5',eco_id)])
)

measures$bbwp_status <- 'hello check'

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
#quick fix under bedrijfsscore1 farm must also get variable field_id, otherwise dimensions don't match
bedrijfsscore3$farm$field_id <- bedrijfsscore3$fields$field_id

# fwrite(bedrijfsscore3,paste0(loc, "bedrijf3.csv"))  
# fwrite(bedrijfsscore3$fields,paste0(loc, "bedrijf3_field.csv"))


######################################################################################################################  
# LOCATION TO STORE CSV TESTS
loc <- 'C:/Astrid/BBWP_ecoregeling bestanden/'

### test scenarios on request for BenN ###
# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# measures 
measures <- NULL
measures$bbwp_status <- NULL


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
                              output = 'scores'
)

# get farmscores
farmscores <- as.vector(unlist(bedrijfsscore1$farm))[1:10]
scorenames <- as.vector(unlist(names(bedrijfsscore1$farm)))[1:10]
out1 <- data.table(thema = scorenames,score = farmscores)
out1a <- as.data.table(bedrijfsscore1$fields)

# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
fwrite(aim,paste0(loc, "aimbedrijf1.csv"))

# write scores
fwrite(out1,paste0(loc, "scoresbedrijf1.csv"))
fwrite(out1a,paste0(loc, "scoresveldenbedrijf1.csv"))







## uitgangspunten bedrijf 2A
measures <- rbind(data.table(id = 1, dt.measures[grepl('EB13A',eco_id)]),
                  data.table(id = 3, dt.measures[grepl('EB13B',eco_id)]),
                  data.table(id = 5, dt.measures[grepl('EB13A',eco_id)]))

measures$bbwp_status <- 'hello check'

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
                              output = 'scores'
)

# get farmscores
farmscores <- as.vector(unlist(bedrijfsscore2$farm))[1:10]
scorenames <- as.vector(unlist(names(bedrijfsscore2$farm)))[1:10]
out2 <- data.table(thema = scorenames,score = farmscores)
out2a <- as.data.table(bedrijfsscore2$fields)

# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
fwrite(aim,paste0(loc, "aimbedrijf2A.csv"))

# write scores
fwrite(out2,paste0(loc, "scoresbedrijf2A.csv"))
fwrite(out2a,paste0(loc, "scoresveldenbedrijf2A.csv"))







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
# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
fwrite(aim,paste0(loc, "aimbedrijf2B.csv"))

# write scores
fwrite(out2,paste0(loc, "scoresbedrijf2B.csv"))
fwrite(out2a,paste0(loc, "scoresveldenbedrijf2B.csv"))







## uitgangspunten bedrijf 3A
measures <- rbind(data.table(id = 1, dt.measures[grepl('EG1C',eco_id)]))

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

# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
fwrite(aim,paste0(loc, "aimbedrijf3A.csv"))

# write scores
fwrite(out3,paste0(loc, "scoresbedrijf3A.csv"))
fwrite(out3a,paste0(loc, "scoresveldenbedrijf3A.csv"))







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

# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
fwrite(aim,paste0(loc, "aimbedrijf3B.csv"))

# write scores
fwrite(out3,paste0(loc, "scoresbedrijf3B.csv"))
fwrite(out3a,paste0(loc, "scoresveldenbedrijf3B.csv"))













## uitgangspunten bedrijf 4A
measures <- rbind(data.table(id = 5, dt.measures[grepl('EG14',eco_id)]),
                  data.table(id = 6, dt.measures[grepl('EG13',eco_id)]))


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
B_AREA = c(10000,20000,30000,40000,1000,2500,3000)
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
# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
fwrite(aim,paste0(loc, "aimbedrijf4A.csv"))

# write scores
fwrite(out4,paste0(loc, "scoresbedrijf4A.csv"))
fwrite(out4a,paste0(loc, "scoresveldenbedrijf4A.csv"))





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

# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
fwrite(aim,paste0(loc, "aimbedrijf4B.csv"))

# write scores
fwrite(out4,paste0(loc, "scoresbedrijf4B.csv"))
fwrite(out4a,paste0(loc, "scoresveldenbedrijf4B.csv"))









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
# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
fwrite(aim,paste0(loc, "aimbedrijf4C.csv"))

# write scores
fwrite(out4,paste0(loc, "scoresbedrijf4C.csv"))
fwrite(out4a,paste0(loc, "scoresveldenbedrijf4C.csv"))





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
# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
fwrite(aim,paste0(loc, "aimbedrijf5.csv"))

# write scores
fwrite(out5,paste0(loc, "scoresbedrijf5.csv"))
fwrite(out5a,paste0(loc, "scoresveldenbedrijf5.csv"))



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

# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
fwrite(aim,paste0(loc, "aimbedrijf6.csv"))

# write scores
fwrite(out6,paste0(loc, "scoresbedrijf6.csv"))
fwrite(out6a,paste0(loc, "scoresveldenbedrijf6.csv"))



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

# get aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AREA = B_AREA, 
                   medalscore = medalscore,thresholds = TRUE)
aimscore <- as.vector(unlist(aim))
aimname <- as.vector(unlist(names(aim)))[!grepl("landscape_br|landscape_si",names(aim))]
aim <- data.table(threshold = aimname,score = aimscore)
fwrite(aim,paste0(loc, "aimbedrijf6.csv"))

# write scores
fwrite(out7,paste0(loc, "scoresbedrijf6.csv"))
fwrite(out7a,paste0(loc, "scoresveldenbedrijf6.csv"))



