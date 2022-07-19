

##  Test alle maatregelen voor een veld van 1 ha met bodemtype zand en sector "arable"
#
# 
# **Specificaties veld**
#   
#   
# Area: 1 ha
# 
# Bodemtype: zand
# 
# CBS-landbouwgebied: LG04 (Oostelijk veehouderijgebied)
# 
# Hoofdgewas op perceel: gerst,zomer- (BRP-code: 236)
# 
# Sector: akkerbouw
# 
# Bbwp_status = 'given for ANLB'
# 
# 
# **Uitleg bij de puntenscores**
#   
#   
# De maatregel EB12 levert de volgende punten op:
#   
#   climate: 4, soil: 6, water: 6, landscape: 2, biodiversity: 2, 250 euro per ha
# 
# er is sprake van bodemtype gebonden urgency
# 
# urgency op zand: punten soil 1, water 1.5, climate 1, biodiversity: 1.25 en landscape 1
# dat levert dus op:
#   
#   climate: 4, soil: 6, water: 9, landscape: 2, biodiversity: 2.5, 250 euro per ha
# 
# in de testset was aan de maatregel toegevoegd: "applied as ANLB".
# 
# the measure has the accumulation rule "both score and reward"
# 
# 
# 
# De doelscores (aim) om goud te halen wordt bepaald op basis van de Wegingsfactoren voor beleidsmatige urgentie en beoogde
# 
# verdeling van de punten over de vergroeningsthema’s. Om goud te halen is op zand gronden nodig:
#   
#   7 punten voor bodem, 10.5 voor water, 3.5 voor klimaat, 7 voor biodiversiteit en 7 voor landschap
# 
# de totaalscore is dan 35, en het voldoet aan de minimale eisen gesteld voor zand
# 
# in verhouding tot de aim betekent dit een score van (in de aim is de drempelwaarde verwerkt):
#   
#   climate = 4/3.5 = 1.14 = 114% = 100%
#   
#   soil = 3 / 7 = 0.43 = 43 % = 43%
#   
#   Afgerond betekent dit dat voor climate het doel wordt gehaald (waarde 100)
# 
# en voor soil slechts voor 43%.


# load package
require(BBWPC)
require(data.table)

# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)

out <- list()

for(i in 1:length(dt.measures)){
  
  # input:
  B_SOILTYPE_AGR = 'dekzand'
  B_GWL_CLASS = 'GtIII'
  A_P_SG = 0.4
  B_SLOPE_DEGREE = 1.5
  B_AER_CBS = 'LG04'
  B_LU_BBWP = 3
  B_LU_ECO1 = F
  B_LU_ECO2 = F
  B_LU_ECO3 = F
  B_LU_ECO4 = F
  B_LU_ECO5 = F
  B_LU_ECO6 = F 
  B_LU_ECO7 = F
  B_LU_ECO8 = F
  B_LU_ECO9 = F
  B_LU_ECO10 = F
  M_DRAIN = TRUE
  D_SA_W = 0
  B_AREA = 1
  farmscore = 100
  measures = NULL
  sector = 'arable'
  output = 'scores'
  medalscore = 'gold'
  
  # get internal table with measures
  dt.measures <- as.data.table(BBWPC::bbwp_measures)
  dt.measures <- dt.measures[!is.na(eco_id)]
  
  x <- dt.measures$eco_id[i]
  
  
  # make measurement list for 3  fields
  measures <- rbind(data.table(id = 1, dt.measures[eco_id == x]))
  
  measures$bbwp_status <- 'given for ANLB'
  
  # calculate aim
  aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR,B_AREA = B_AREA, medalscore = medalscore)
  
  # calulate field scores
  field <- er_field_scores(B_SOILTYPE_AGR, B_AER_CBS,B_AREA,
                           B_LU_BBWP,B_LU_ECO1,B_LU_ECO2, B_LU_ECO3, B_LU_ECO4, B_LU_ECO5, 
                           B_LU_ECO6, B_LU_ECO7,B_LU_ECO8, B_LU_ECO9,B_LU_ECO10,
                           B_CT_SOIL =aim$B_CT_SOIL, 
                           B_CT_WATER = aim$B_CT_WATER,
                           B_CT_CLIMATE = aim$B_CT_CLIMATE,
                           B_CT_BIO = aim$B_CT_BIO,
                           B_CT_LANDSCAPE = aim$B_CT_LANDSCAPE, 
                           measures = measures, sector)
  
  out[[i]] <- data.table(maatregel= x,field) 
  
}


fieldscores <- do.call(rbind, out)

write.csv2(fieldscores,"dev/fieldscores_zand_LG04_zomergerst_akkerbouw.csv")





## Test dezelfde maatregelen voor een veld van 1 ha met bodem type klei en sector "dairy"

# **Specificaties veld**
#   
#   
#   Area: 1 ha
# 
# Bodemtype: klei
# 
# CBS-landbouwgebied: LG01 (Bouwhoek en Hogeland)
# 
# Hoofdgewas op perceel: grasland, tijdelijk (BRP-code: 266)
# 
# Sector: melkveehouderij
# 
# Bbwp_status = 'given for ANLB'
# 


# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)

out <- list()

for(i in 1:length(dt.measures)){
  
  # input:
  B_SOILTYPE_AGR = 'dekzand'
  B_GWL_CLASS = 'GtIII'
  A_P_SG = 0.4
  B_SLOPE_DEGREE = 1.5
  B_AER_CBS = 'LG01'
  B_LU_BBWP = 2
  B_LU_ECO1 = F
  B_LU_ECO2 = F
  B_LU_ECO3 = F
  B_LU_ECO4 = F
  B_LU_ECO5 = F
  B_LU_ECO6 = F 
  B_LU_ECO7 = F
  B_LU_ECO8 = F
  B_LU_ECO9 = T
  B_LU_ECO10 = T
  M_DRAIN = TRUE
  D_SA_W = 0
  B_AREA = 1
  farmscore = 100
  measures = NULL
  sector = 'dairy'
  output = 'scores'
  medalscore = 'gold'
  
  # get internal table with measures
  dt.measures <- as.data.table(BBWPC::bbwp_measures)
  dt.measures <- dt.measures[!is.na(eco_id)]
  
  x <- dt.measures$eco_id[i]
  
  
  # make measurement list for 3  fields
  measures <- rbind(data.table(id = 1, dt.measures[eco_id == x]))
  
  measures$bbwp_status <- 'given for ANLB'
  
  # calculate aim
  aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR,B_AREA = B_AREA, medalscore = medalscore)
  
  # calulate field scores
  field <- er_field_scores(B_SOILTYPE_AGR, B_AER_CBS,B_AREA,
                           B_LU_BBWP,B_LU_ECO1,B_LU_ECO2, B_LU_ECO3, B_LU_ECO4, B_LU_ECO5, 
                           B_LU_ECO6, B_LU_ECO7,B_LU_ECO8, B_LU_ECO9,B_LU_ECO10,
                           B_CT_SOIL =aim$B_CT_SOIL, 
                           B_CT_WATER = aim$B_CT_WATER,
                           B_CT_CLIMATE = aim$B_CT_CLIMATE,
                           B_CT_BIO = aim$B_CT_BIO,
                           B_CT_LANDSCAPE = aim$B_CT_LANDSCAPE, 
                           measures = measures, sector)
  
  out[[i]] <- data.table(maatregel= x,field) 
  
}


fieldscores <- do.call(rbind, out)

write.csv2(fieldscores,"dev/fieldscores_klei_LG01_tijdelijkgrasland_melkveehouderij.csv")



## Test voor een bedrijf met 3 velden en meerdere maatregelen per veld

# 
# **Specificaties veld 1**
#   
#   Area: 100 ha
# 
# Bodemtype: zand
# 
# CBS-landbouwgebied: LG05 (Centraal Veehouderijgebied)
# 
# Hoofdgewas op perceel: gerst,zomer- (BRP-code: 236)
# 
# Sector: akkerbouw
# 
# Bbwp_status = 'given for ANLB'
# 
# Maatregelen op veld 1: EB11A en EG16
# 
# **Specificaties veld 2**
#   
#   Area: 80 ha
# 
# Bodemtype: loess
# 
# CBS-landbouwgebied: LG14 (Zuid-Limburg)
# 
# Hoofdgewas op perceel: aardappelen, zetmeel (BRP-code: 2017)
# 
# Sector: akkerbouw
# 
# Bbwp_status = 'given for ANLB'
# 
# Maatregelen op veld 2: EB4A, EB8
# 
# **Specificaties veld 3**
#   
#   Area: 0.5 ha
# 
# Bodemtype: klei
# 
# CBS-landbouwgebied: LG02 (Veenkoloniën en Oldambt)
# 
# Hoofdgewas op perceel: rand, grenzend aan bouwland, hoofdzakelijk bestaand uit een ander gewas dan gras (BRP-code: 345)
# 
# Sector: akkerbouw
# 
# Bbwp_status = 'given for ANLB'
# 
# Maatregelen op veld 3: EG20A4,EG6A

# input:
B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei')
B_GWL_CLASS = c('GtIII', 'GtI', 'GtV')
A_P_SG = c(0.4, 0.8, 1)
B_SLOPE_DEGREE = c(1.5,4,1.5)
B_AER_CBS = c('LG05','LG14','LG02')
B_LU_BBWP = c(3,4,10)
B_LU_ECO1 = c(F,T,F)
B_LU_ECO2 = c(F,F,F)
B_LU_ECO3 = c(F,F,F)
B_LU_ECO4 = c(F,F,F)
B_LU_ECO5 = c(F,F,F)
B_LU_ECO6 = c(F,F,F) 
B_LU_ECO7 = c(T,F,F)
B_LU_ECO8 = c(T,T,T)
B_LU_ECO9 = c(T,T,F)
B_LU_ECO10 = c(T,T,T)
M_DRAIN = c(TRUE, FALSE, TRUE)
D_SA_W = c(0, 0.5, 1)
B_AREA = c(100,80,0.5)
farmscore = 100
measures = NULL
sector = 'arable'
output = 'scores'
medalscore = 'gold' 

# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# make measurement list for 3  fields
measures <- rbind(data.table(id = 1, dt.measures[grepl('EB11A|EG16B',eco_id)]),
                  data.table(id = 2, dt.measures[grepl('EB4A|EB8',eco_id)]),
                  data.table(id = 3, dt.measures[grepl('EG20A4|EG6A',eco_id)]))
measures$bbwp_status <- 'given for ANLB'

# calculate aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR,B_AREA = B_AREA, medalscore = medalscore)

# calculate field scores
field <- er_field_scores(B_SOILTYPE_AGR, B_AER_CBS,B_AREA,
                         B_LU_BBWP,B_LU_ECO1,B_LU_ECO2, B_LU_ECO3, B_LU_ECO4, B_LU_ECO5, 
                         B_LU_ECO6, B_LU_ECO7,B_LU_ECO8, B_LU_ECO9,B_LU_ECO10,
                         B_CT_SOIL =aim$B_CT_SOIL, 
                         B_CT_WATER = aim$B_CT_WATER,
                         B_CT_CLIMATE = aim$B_CT_CLIMATE,
                         B_CT_BIO = aim$B_CT_BIO,
                         B_CT_LANDSCAPE = aim$B_CT_LANDSCAPE, 
                         measures = measures, sector)

# calculate field scores and farm score
test <- ecoregeling(B_SOILTYPE_AGR, B_GWL_CLASS, B_SLOPE_DEGREE,B_AER_CBS,
                    B_LU_BBWP,B_LU_ECO1,B_LU_ECO2, B_LU_ECO3, B_LU_ECO4, B_LU_ECO5, 
                    B_LU_ECO6, B_LU_ECO7,B_LU_ECO8, B_LU_ECO9,B_LU_ECO10,
                    A_P_SG,D_SA_W, B_AREA,M_DRAIN, farmscore, 
                    measures, sector, output = 'scores', medalscore = 'gold')

write.csv2(test,"dev/fieldscores_farm_3fields.csv")




## Test voor een bedrijf met 2 velden en meerdere maatregelen per veld

# 
# **Specificaties veld 1**
#   
#   Area: 10 ha
# 
# Bodemtype: dekzand
# 
# CBS-landbouwgebied: LG05 (Centraal Veehouderijgebied)
# 
# Hoofdgewas op perceel: winterpeen, productie (BRP-code: 2785)
# 
# Sector: akkerbouw, veehouderij
# 
# Bbwp_status = 'given for ANLB'
# 
# Maatregelen op veld 1: EB12, EB19
# 
# **Specificaties veld 2**
#   
#   Area: 15 ha
# 
# Bodemtype: zeeklei
# 
# CBS-landbouwgebied: LG05 (Centraal Veehouderijgebied)
# 
# Hoofdgewas op perceel: luzerne (BRP-code: 258)
# 
# Sector: akkerbouw, veehouderij
# 
# Bbwp_status = 'given for ANLB'
# 
# Maatregelen op veld 2: EG1C,EB2


# input:
B_SOILTYPE_AGR = c('dekzand','rivierklei')
B_GWL_CLASS = c('GtIII','GtV')
A_P_SG = c(0.4, 1)
B_SLOPE_DEGREE = c(1.5,1.5)
B_AER_CBS = c('LG05','LG05')
B_LU_BBWP = c(4,12)
B_LU_ECO1 = c(T,T)
B_LU_ECO2 = c(F,F)
B_LU_ECO3 = c(F,F)
B_LU_ECO4 = c(F,T)
B_LU_ECO5 = c(F,T)
B_LU_ECO6 = c(F,T) 
B_LU_ECO7 = c(F,T)
B_LU_ECO8 = c(T,T)
B_LU_ECO9 = c(T,T)
B_LU_ECO10 = c(T,T)
M_DRAIN = c(FALSE , TRUE)
D_SA_W = c(0.5, 1)
B_AREA = c(10,15)
farmscore = 100
measures = NULL
sector = c('arable','dairy')
output = 'scores'
medalscore = 'gold'

# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# make measurement list for 3  fields
measures <- rbind(data.table(id = 1, dt.measures[grepl('EB12|EG19',eco_id)]),
                  data.table(id = 2, dt.measures[grepl('EG1C|EB2',eco_id)]))
measures$bbwp_status <- 'given for ANLB'

# calculate aim
aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR,B_AREA = B_AREA, medalscore = medalscore)

# calculate field scores
field <- er_field_scores(B_SOILTYPE_AGR, B_AER_CBS,B_AREA,
                         B_LU_BBWP,B_LU_ECO1,B_LU_ECO2, B_LU_ECO3, B_LU_ECO4, B_LU_ECO5, 
                         B_LU_ECO6, B_LU_ECO7,B_LU_ECO8, B_LU_ECO9,B_LU_ECO10,
                         B_CT_SOIL =aim$B_CT_SOIL, 
                         B_CT_WATER = aim$B_CT_WATER,
                         B_CT_CLIMATE = aim$B_CT_CLIMATE,
                         B_CT_BIO = aim$B_CT_BIO,
                         B_CT_LANDSCAPE = aim$B_CT_LANDSCAPE, 
                         measures = measures, sector)

# calculate field scores and farm score
test <- ecoregeling(B_SOILTYPE_AGR, B_GWL_CLASS, B_SLOPE_DEGREE,B_AER_CBS,
                    B_LU_BBWP,B_LU_ECO1,B_LU_ECO2, B_LU_ECO3, B_LU_ECO4, B_LU_ECO5, 
                    B_LU_ECO6, B_LU_ECO7,B_LU_ECO8, B_LU_ECO9,B_LU_ECO10,
                    A_P_SG,D_SA_W, B_AREA,M_DRAIN, farmscore, 
                    measures, sector, output = 'scores', medalscore = 'gold')

write.csv2(test,"dev/fieldscores_farm_2fields.csv")