library(data.table)
library(BBWPC)

# LOCATION TO STORE CSV TESTS
loc <- 'C:/R_packages_Tessa'

# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# de bron data
B_SOILTYPE_AGR = c('rivierklei')
B_GWL_CLASS = c('GtIII')
A_P_SG = c(12)
B_SLOPE_DEGREE = c(1.5)
B_AER_CBS = c('LG01')
B_LU_BBWP = c('gras_tijdelijk') #aangepast, was "eiwitgewas". Eiwitgewas voegde overal de punten van EB2 toe.
B_LU_BRP = c(800)
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
#(dt2,paste0(loc, "farm_800_klei_LG01_akkerbouw.csv"))
#fwrite(dt1,paste0(loc, "field_800_klei_LG01_akkerbouw.csv"))

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
                    data.table(id = 2, dt.measures[grepl('EG2B$',eco_id)]),
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
  B_LU_ARABLE_ER = rep(F,aantal)
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
  
  measures <- rbind(data.table(id = 1, dt.measures[grepl('EG16B|EB13A',bbwp_id)]),
                    data.table(id = 2, dt.measures[grepl('EG2B$|EB8',eco_id)]),
                    data.table(id = 3, dt.measures[grepl('EG14|EB18',eco_id)]),
                    data.table(id = 4, dt.measures[grepl('EB15|EB3|EB23A|EB11',eco_id)]))
  
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
  
#  fwrite(bedrijfsscore2,paste0(loc, "bedrijf2.csv"))  
#  fwrite(bedrijfsscore2$fields,paste0(loc, "bedrijf2_field.csv"))
  
  
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
  B_LU_ARABLE_ER = c(T,F)
  B_LU_PRODUCTIVE_ER = c(F,T)
  B_LU_CULTIVATED_ER = c(F,T)
  M_DRAIN = c(T,T)
  D_SA_W = c(0.5,0.5)
  B_AREA = c(10,10)
  farmscore = 100
  medalscore = "gold"
  measures = measures
  sector = c('arable')
  output = 'scores'
  
  measures <- rbind(data.table(id = 1, dt.measures[grepl('EG16B|EB13A',bbwp_id)]),
                    data.table(id = 2, dt.measures[grepl('EG20',eco_id)])
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
  
#  fwrite(bedrijfsscore3,paste0(loc, "bedrijf3.csv"))