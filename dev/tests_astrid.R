
# --- test 1 ----------

# this is a test for a single soil
# test case: 10 ha, tijdelijk grasland, measure B138, "Zet grasland om naar productiegericht soortenrijk grasland"

  # get internal table with measures
  dt.measures <- as.data.table(BBWPC::bbwp_measures)
  dt.measures <- dt.measures[!is.na(eco_id)]
  
  # make measurement list for 2 of the 4 fields
  measures <- rbind(data.table(id = 1, dt.measures[grepl('B138',bbwp_id)]))
  measures$bbwp_status <- 'given for ANLB'

  # de brondata
  B_SOILTYPE_AGR = c('dekzand')
  B_GWL_CLASS = c('GtIII')
  A_P_SG = c(12)
  B_SLOPE_DEGREE = c(1.5)
  B_AER_CBS = c('LG05')
  B_LU_BBWP = c(2)
  B_LU_ECO1 = c(F)
  B_LU_ECO2 = c(F)
  B_LU_ECO3 = c(F)
  B_LU_ECO4 = c(F)
  B_LU_ECO5 = c(T)
  B_LU_ECO6 = c(F) 
  B_LU_ECO7 = c(F)
  B_LU_ECO8 = c(F)
  B_LU_ECO9 = c(T)
  B_LU_ECO10 = c(T)
  M_DRAIN = c(TRUE)
  D_SA_W = c(0.5)
  B_AREA = c(10)
  farmscore = 100
  medalscore = "gold"
  measures = measures
  sector = c('dairy')
  output = 'scores'

# bereken de doelscores om goud te halen
  # om goud te halen is nodig: 7 punten voor bodem, 10.5 voor water, 3.5 voor klimaat, 7 voor biodiversiteit en 7 voor landschap
  # totaalscore is dan 35, en het voldoet aan de minimale eisen gesteld voor zand
  aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR,B_AREA = B_AREA, medalscore = medalscore)
  
  # bereken de score op het perceel
  # afgerond betekent dit dat voor climate and water het doel wordt gehaald (waarde 100)
  # voor soil slechts voor 29%, voor biodiversity 54% en voor landschap 43%
  field <- er_field_scores(B_SOILTYPE_AGR, B_AER_CBS,B_AREA,
                           B_LU_BBWP,B_LU_ECO1,B_LU_ECO2, B_LU_ECO3, B_LU_ECO4, B_LU_ECO5, 
                           B_LU_ECO6, B_LU_ECO7,B_LU_ECO8, B_LU_ECO9,B_LU_ECO10,
                           B_CT_SOIL =aim$B_CT_SOIL, 
                           B_CT_WATER = aim$B_CT_WATER,
                           B_CT_CLIMATE = aim$B_CT_CLIMATE,
                           B_CT_BIO = aim$B_CT_BIO,
                           B_CT_LANDSCAPE = aim$B_CT_LANDSCAPE, 
                           measures = measures, sector)
  
  # als ik verder inzoom 1
  
  # de maatregel B138 levert de volgende punten op:
  # climate:8, soil: 2, water:8, landscape:3, biodviersity:3, 300 euro per ha
  
  # er is sprake van bodemtype gebonden urgency
  # urgency op zand: punten soil 1, water 1.5, climate 1, bio 1.25 en landscape 1
  # dat levert dus op:
  # climate:8, soil: 2, water:12, landscape:3, biodviersity:3.75, 300 euro per ha
  
  # in de testset was aan de maatregel toegevoegd: "applied as ANLB".
  # the measure has the accumulation rule "only score", so reward is set to 0
  
  # compared to the aim:
  # climate = 8 / 7 = 1.14 = 114 % = 100%

  
# ---- example 2 -------
  
  # this is an example for the same soil for 3 fields, but then with multiple measures
  aantal = 3
  
  # de brondata
  B_SOILTYPE_AGR = rep('dekzand',aantal)
  B_GWL_CLASS = rep('GtIII',aantal)
  A_P_SG = rep(12,aantal)
  B_SLOPE_DEGREE = rep(1.5,aantal)
  B_AER_CBS = rep('LG05',aantal)
  B_LU_BBWP = rep(2,aantal)
  B_LU_ECO1 = rep(F,aantal)
  B_LU_ECO2 =  rep(F,aantal)
  B_LU_ECO3 =  rep(F,aantal)
  B_LU_ECO4 =  rep(F,aantal)
  B_LU_ECO5 = rep(T,aantal)
  B_LU_ECO6 =  rep(F,aantal) 
  B_LU_ECO7 = rep(F,aantal)
  B_LU_ECO8 = rep(F,aantal)
  B_LU_ECO9 = rep(T,aantal)
  B_LU_ECO10 = rep(T,aantal)
  M_DRAIN = rep(T,aantal)
  D_SA_W = rep(0.5,aantal)
  B_AREA = rep(10,aantal)
  farmscore = 100
  medalscore = "gold"
  measures = measures
  sector = c('dairy')
  output = 'scores'
  
  measures <- rbind(data.table(id = 1, dt.measures[grepl('B138',bbwp_id)]),
                    data.table(id = 2, dt.measures[grepl('EG2B$',eco_id)]),
                    data.table(id = 3, dt.measures[grepl('EG14|EG18',eco_id)])
                    )
  measures$bbwp_status <- 'hello check'
  
  aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR,B_AREA = B_AREA, medalscore = medalscore)
  
  field <- er_field_scores(B_SOILTYPE_AGR, B_AER_CBS,B_AREA,
                           B_LU_BBWP,B_LU_ECO1,B_LU_ECO2, B_LU_ECO3, B_LU_ECO4, B_LU_ECO5, 
                           B_LU_ECO6, B_LU_ECO7,B_LU_ECO8, B_LU_ECO9,B_LU_ECO10,
                           B_CT_SOIL =aim$B_CT_SOIL, 
                           B_CT_WATER = aim$B_CT_WATER,
                           B_CT_CLIMATE = aim$B_CT_CLIMATE,
                           B_CT_BIO = aim$B_CT_BIO,
                           B_CT_LANDSCAPE = aim$B_CT_LANDSCAPE, 
                           measures = measures, sector)
  
  test <- ecoregeling(B_SOILTYPE_AGR, B_GWL_CLASS, B_SLOPE_DEGREE,B_AER_CBS,
                      B_LU_BBWP,B_LU_ECO1,B_LU_ECO2, B_LU_ECO3, B_LU_ECO4, B_LU_ECO5, 
                      B_LU_ECO6, B_LU_ECO7,B_LU_ECO8, B_LU_ECO9,B_LU_ECO10,
                      A_P_SG,D_SA_W, B_AREA,M_DRAIN, farmscore, 
                      measures, sector, output = 'scores', medalscore = 'gold')
  
  
  
  
  
  
er_aim.gold <- er_farm_aim(B_SOILTYPE_AGR = 'dekzand', B_AREA = 10, medalscore = "gold") 
er_aim.gold[,B_CT_TOTAL := B_CT_SOIL + B_CT_WATER + B_CT_CLIMATE + B_CT_BIO + B_CT_LANDSCAPE]
er_aim.gold[,REWARD := 175]
er_aim.gold <- melt(er_aim.gold,id.vars = 'farmid',variable.name = 'indicator',value.name = 'er_gold')
er_aim.gold[,indicator := gsub('B_CT_','',indicator)]

dt.field <- test$fields
setnames(dt.field,toupper(colnames(dt.field)))

dt.field <- data.table(id = 1,
                 SOIL = dt.field$S_ER_SOIL,
                 WATER = dt.field$S_ER_WATER,
                 CLIMATE = dt.field$S_ER_CLIMATE,
                 BIO = dt.field$S_ER_BIODIVERSITY,
                 LANDSCAPE = dt.field$S_ER_LANDSCAPE,
                 TOTAL = dt.field$S_ER_TOT,
                 REWARD = dt.field$S_ER_REWARD,
                 B_AREA = 10)
dt.field <- melt(dt.field,id.vars = c('id','B_AREA'),variable.name = 'indicator')

dt.field <- merge(dt.field,er_aim.gold[,.(indicator,er_gold)],by='indicator',all.x = TRUE)
# dit was de originele score (let op: groter dan 100 is hier gelijk aan 100)
dt.field[,score := value * er_gold * 0.01]


