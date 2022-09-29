library(data.table)
library(BBWPC)

# LOCATION TO STORE CSV TESTS
loc <- 'C:/BBWP-ecoregeling/tests_BenN/'

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
  
  
  
  
  
### test scenarios on request for BenN ###
  
  measures <- rbind(data.table(id = 2, dt.measures[grepl('EB4B',eco_id)]),
                    data.table(id = 3, dt.measures[grepl('EB13A',eco_id)]),
                    data.table(id = 4, dt.measures[grepl('EB13B',eco_id)]))
  
  measures$bbwp_status <- 'hello check'
  
  
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
  B_AREA = c(1,2,3,4,5,6,0.1)
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
  
  fwrite(out1,paste0(loc, "scoresbedrijf1.csv"))
  fwrite(out1a,paste0(loc, "scoresveldenbedrijf1.csv"))
  
  
  ## uitgangspunten bedrijf 2
  
  measures <- rbind(data.table(id = 1, dt.measures[grepl('EB20|EB13A',eco_id)]),
                    data.table(id = 3, dt.measures[grepl('EB13B',eco_id)]))
  
  
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
  B_AREA = c(1,2,2,3,4,0.1)
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
  
  fwrite(out2,paste0(loc, "scoresbedrijf2.csv"))
  fwrite(out2a,paste0(loc, "scoresveldenbedrijf2.csv"))
  
  ## uitgangspunten bedrijf 3
  measures <- rbind(data.table(id = 2, dt.measures[grepl('EG10A|EG1C',eco_id)]),
                    data.table(id = 3, dt.measures[grepl('EB12|EG5B',eco_id)]),
                    data.table(id = 5, dt.measures[grepl('EB16B',eco_id)]))
  
  
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
  B_AREA = c(0.1,2,3,4,0.1)
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
  
  fwrite(out3,paste0(loc, "scoresbedrijf3.csv"))
  fwrite(out3a,paste0(loc, "scoresveldenbedrijf3.csv"))
  
  ## uitgangspunten bedrijf 4
  measures <- rbind(data.table(id = 2, dt.measures[grepl('EG7B|EG8A',eco_id)]),
                    data.table(id = 4, dt.measures[grepl('EG16C',eco_id)]),
                    data.table(id = 5, dt.measures[grepl('EG14',eco_id)]),
                    data.table(id = 6, dt.measures[grepl('EG13',eco_id)]),
                    data.table(id = 7, dt.measures[grepl('EG14',eco_id)]))
  
  
  measures$bbwp_status <- 'hello check'
  
  
  B_SOILTYPE_AGR = c('veen','veen','zeeklei','zeeklei','dekzand','dekzand','dekzand')
  B_GWL_CLASS = rep('GtIII',7)
  A_P_SG = rep(12,7)
  B_SLOPE_DEGREE = rep(1.7,7)
  B_AER_CBS = rep('LG04',7)
  B_LU_BBWP = c('randensloot','gras_permanent','gras_tijdelijk','gras_permanent','randensloot','randensloot','randensloot')
  B_LU_BRP = c(343,265,266,265,343,343,343)
  B_LU_ARABLE_ER = c(F,F,F,F,F,F,F)
  B_LU_PRODUCTIVE_ER = c(F,T,T,T,F,F,F)
  B_LU_CULTIVATED_ER = c(F,T,T,T,F,F,F)
  M_DRAIN = rep(T,7)
  D_SA_W = rep(0.7,7)
  B_AREA = c(1,2,3,4,0.1,0.2,0.3)
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
  
  fwrite(out4,paste0(loc, "scoresbedrijf4.csv"))
  fwrite(out4a,paste0(loc, "scoresveldenbedrijf4.csv"))