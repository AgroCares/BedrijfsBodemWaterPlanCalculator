
# uitgewerkte tests for ecoregelingen 

# --- test 1 verder uitgewerkt----------

# this is a test for a single soil
# test case: 10 ha, tijdelijk grasland, measure B138, "Zet grasland om naar productiegericht soortenrijk grasland"

# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# make measurement list for 2 of the 4 fields
measures <- rbind(data.table(id = 1, dt.measures[grepl('B138',bbwp_id)]))
measures$bbwp_status <- 'given for ANLB'

# de bron data
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


# ---- example 2 (verder gecheckt) -------
require(data.table);require(BBWPC)
# this is an example for the same soil and crop type (tijdelijk grasland) for 3 fields, but then with multiple measures
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

# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]
measures <- rbind(data.table(id = 1, dt.measures[grepl('B138',bbwp_id)]),
                  data.table(id = 2, dt.measures[grepl('EG2B$',eco_id)]),
                  data.table(id = 3, dt.measures[grepl('EG14|EG18',eco_id)])
)
measures$bbwp_status <- 'applied as ANLB'

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

field2 <- er_field_scores(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                          B_AER_CBS = B_AER_CBS,
                          B_AREA = B_AREA,
                          B_LU_BBWP = B_LU_BBWP,
                          B_LU_ECO1 = B_LU_ECO1,
                          B_LU_ECO2 = B_LU_ECO2, 
                          B_LU_ECO3 = B_LU_ECO3, 
                          B_LU_ECO4 = B_LU_ECO4, 
                          B_LU_ECO5 = B_LU_ECO5, 
                          B_LU_ECO6 = B_LU_ECO6, 
                          B_LU_ECO7 = B_LU_ECO7,
                          B_LU_ECO8 = B_LU_ECO8, 
                          B_LU_ECO9 = B_LU_ECO9,
                          B_LU_ECO10 = B_LU_ECO10,
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

B_CT_SOIL =aim$B_CT_SOIL
B_CT_WATER = aim$B_CT_WATER
B_CT_CLIMATE = aim$B_CT_CLIMATE
B_CT_BIO = aim$B_CT_BIO
B_CT_LANDSCAPE = aim$B_CT_LANDSCAPE 

#VELD1
# de maatregel B138 levert de volgende punten op:
# climate:8, soil: 2, water:8, landscape:3, biodviersity:3, 300 euro per ha

# er is sprake van bodemtype gebonden urgency
# urgency op zand: punten soil 1, water 1.5, climate 1, bio 1.25 en landscape 1
# dat levert dus op:
# climate:8, soil: 2, water:12, landscape:3, biodviersity:3.75, 300 euro per ha

# in de testset was aan de maatregel toegevoegd: "applied as ANLB".
# the measure has the accumulation rule "only score", so reward is set to 0

# compared to the aim:
# climate = 8 / 3.5 = 2.28 = 228 % = 100%
# soil = 2 / 7 = 0.29 = 29 % = 29%
# water = 12 / 10.5 = 1.14 = 114 % = 100%
# landscape = 3 / 7 = 0.42 = 42 % = 42%
# bio = 3.75 / 7 = 0.54 = 54%

#VELD2
# de maatregel EG2B levert de volgende punten op:
# climate:0, soil: 0, water:0, landscape:4, bio:8, 75 euro per ha

# er is sprake van bodemtype gebonden urgency
# urgency op zand: punten soil 1, water 1.5, climate 1, bio 1.25 en landscape 1
# dat levert dus op:
# climate:0, soil: 0, water:0, landscape:4, bio:10, 75 euro per ha

# compared to the aim:
# climate = 0 / 7 = 0 = 0 % = 0%
# soil = 0 / 7 = 0 = 0 % = 0%
# water = 0 / 7 = 0 = 0 % = 0%
# landscape = 6 / 7 = 0.57 = 57 % = 57%
# bio = 10 / 7 = 1.43 = 143 % = 100%

#VELD3
# de maatregel EG14 levert de volgende punten op:
# climate:0, soil: 8, water:8, landscape:4, bio:2, 1167 euro per ha

# er is sprake van bodemtype gebonden urgency
# urgency op zand: punten soil 1, water 1.5, climate 1, bio 1.25 en landscape 1
# dat levert dus op:
# climate:0, soil: 8, water:12, landscape:4, bio:2.5, 1167 euro per ha

# in de testset was aan de maatregel toegevoegd: "applied as ANLB".
# the measure has the accumulation rule "only score", so reward is set to 0

# EG14 is not applicable regarding arable, productive and cultivated (eco8,9,10)


# de maatregel EG18 levert de volgende punten op:
# climate:2, soil: 4, water:7, landscape:3, bio:4, 100 euro per ha

# er is sprake van bodemtype gebonden urgency
# urgency op zand: punten soil 1, water 1.5, climate 1, bio 1.25 en landscape 1
# dat levert dus op:
# climate:2, soil: 4, water:10.5, landscape:3, bio:5, 100 euro per ha

# in de testset was aan de maatregel toegevoegd: "applied as ANLB".
# the measure has the accumulation rule "both", so reward is set to 100


# compared to the aim:
# climate = 2 / 3.5 = 0.57 = 29 % = 29%
# soil = 4 / 7 = 0.57 = 57 % = 57%
# water = 10.5 / 10.5 = 1 = 100 % = 100%
# landscape = 3 / 7 = 0.43 = 43 % = 43%
# bio = 5 / 7 = 0.71 = 71 % = 71%


### BIJ VELD 3 WORDT ER_CLIMATE NIET GOED BEREKEND ###
### BIJ VELD 1 KRIJGT HIJ TOCH WEL REWARD TERWIJL ANLB IS "SCORE ONLY"


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


# ---- test 3: een bedrijf met drie percelen. Sector "arable"-------

# Voor een bedrijf met drie percelen. Sector "arable"

# Gewas veld 1 (100 ha): gerst,zomer- (BRP-code: 236)
# Bodem type veld 1: zand
# maatregelen op veld 1: EB11A, EG16B
# CBS regio: LG05
# 
# Gewas veld 2 (80 ha): aardappelen, zetmeel (BRP-code: 2017)
# Bodem type veld 2: loess
# maatregelen op veld 2: EB4A,EB8
# CBS regio: LG14
# 
# Gewas veld 3 (0.5 ha): rand, grenzend aan bouwland, hoofdzakelijk bestaand uit een ander gewas dan gras (BRP-code: 345)
# Bodem type veld 3: klei
# maatregelen op veld 3: EG20A4,EG6A
# CBS regio: LG02

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

# calculate scores and reward
test <- ecoregeling(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
                    B_GWL_CLASS = c('GtIII', 'GtI', 'GtV'),
                    A_P_SG = c(0.4, 0.8, 1),
                    B_SLOPE_DEGREE = c(1.5,4,1.5),
                    B_AER_CBS = c('LG05','LG14','LG02'),
                    B_LU_BBWP = c(3,4,10),
                    B_LU_ECO1 = c(F,T,F),
                    B_LU_ECO2 = c(F,F,F),
                    B_LU_ECO3 = c(F,F,F),
                    B_LU_ECO4 = c(F,F,F),
                    B_LU_ECO5 = c(F,F,F),
                    B_LU_ECO6 = c(F,F,F), 
                    B_LU_ECO7 = c(T,F,F),
                    B_LU_ECO8 = c(T,T,T),
                    B_LU_ECO9 = c(T,T,F),
                    B_LU_ECO10 = c(T,T,T),
                    M_DRAIN = c(TRUE, FALSE, TRUE),
                    D_SA_W = c(0, 0.5, 1),
                    B_AREA = c(100,80,0.5),
                    farmscore = 100,
                    measures = NULL,
                    sector = 'arable',
                    output = 'scores',
                    medalscore = 'gold'
)



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


#VELD1
# de maatregel EB11A levert de volgende punten op:
# climate:5, soil: 8, water:8, landscape:2, biodiversity:2, 275 euro per ha

# er is sprake van bodemtype gebonden urgency
# urgency op zand: punten soil 1, water 1.5, climate 1, bio 1.25 en landscape 1
# dat levert dus op:
# climate:5, soil: 8, water: 12, landscape: 2, biodiversity: 2.5, 275 euro per ha (LG05 =1)

# in de testset was aan de maatregel toegevoegd: "applied as ANLB".
# the measure has the accumulation rule "both", so reward is set to 275


# de maatregel EG16B levert de volgende punten op:
# climate:8, soil: 2, water:8, landscape:3, biodversity:3, 300 euro per ha 

# not applicable on bouwland (deze maatregel mag niet worden genomen op een een gewas dat bouwland is), so score and reward is zero

# compared to the aim:
# climate = 5 / 3.5 = 100%
# soil = 8 / 8.6 = 93%
# water = 12 / 9.7 =100%
# landscape = 2 / 5.4 = 37%
# bio = 2.5 / 7.8 = 32%



#VELD2
# de maatregel EB4A levert de volgende punten op:
# climate:0, soil: 5, water:5, landscape:10, bio:10, 216 euro per ha

# er is sprake van bodemtype gebonden urgency
# urgency op loess: punten soil 1.5, water 1.25, climate 1, bio 1.25 en landscape 1.25
# dat levert dus op:
# climate:0, soil: 7.5, water:6.25, landscape:12.5, bio:12.5, 216 euro per ha (LG14 =1)

# in de testset was aan de maatregel toegevoegd: "applied as ANLB".
# the measure has the accumulation rule "score only", so reward is set to 0


# de maatregel EB8 levert de volgende punten op:
# climate:5, soil: 5, water: 5, landscape: 0, bio: 0, 155 euro per ha

# er is sprake van bodemtype gebonden urgency
# urgency op loess: punten soil 1.5, water 1.25, climate 1, bio 1.25 en landscape 1.25
# dat levert dus op:
# climate:5, soil: 7.5, water: 6.25, landscape: 0, bio: 0, 155 euro per ha

# in de testset was aan de maatregel toegevoegd: "applied as ANLB".
# the measure has the accumulation rule "score only", so reward is set to 0

# compared to the aim: 
# climate = 5 / 3.5 = 100%
# soil = 15 / 8.6 = 100%
# water = 13 / 9.7 = 100%
# landscape = 12.5 / 5.4 = 100%
# bio = 12.5 / 7.8 = 100%



#VELD3
# de maatregel EG20A4 levert de volgende punten op:
# climate:5, soil: 0, water:5, landscape: 10, bio: 10, 979 euro per ha

# er is sprake van bodemtype gebonden urgency
# urgency op klei: punten soil 1.5, water 1.25, climate 1, bio 1 en landscape 1
# dat levert dus op:
# climate:5, soil: 0, water:6.25, landscape:10, bio:10, 979 euro per ha

# in de testset was aan de maatregel toegevoegd: "applied as ANLB".
# the measure has the accumulation rule "only score", so reward is set to 0

# de maatregel EG6A levert de volgende punten op:
# not applicable on arable and furthermore a farm measure

# both measures compared to the aim: 
# climate = 5 / 3.5 = 100%
# soil = 0 / 8.6 = 0%
# water = 5 / 9.7 = 52%
# landscape = 10 / 5.4 = 100%
# bio = 10 / 7.8 = 100%


# ---- test 4: alle maatregelen voor een veld van 1 ha (zand, akkerbouw) #####
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

# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)

# create empty list to store output generated by the for loop
out <- list()

# for loop that loops through each measure
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
  
  # selection of the measure eco_id
  x <- dt.measures$eco_id[i]
  
  # make measurement list for 1  field
  measures <- rbind(data.table(id = 1, dt.measures[eco_id == x]))
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
  
  out[[i]] <- data.table(maatregel= x,field) 
  
}


fieldscores <- do.call(rbind, out)

print(fieldscores)
print(aim)


# ---- test 5: alle maatregelen voor een veld van 1 ha (klei, melkveehouderij) #####
# Area: 1 ha
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


# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)

# create empty list to store output generated by the for loop
out <- list()

# for loop that loops through each measure
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
  
  # selection of the measure eco_id
  x <- dt.measures$eco_id[i]
  
  
  # make measurement list for 1  field
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

print(fieldscores)


# ---- test 6: voor een bedrijf met twee percelen. Sector c("arable","dairy") -------

# Voor een bedrijf met twee percelen, sector c("arable","dairy")

# Gewas veld 1 (10 ha): winterpeen, productie (BRP-code: 2785)
# Bodem type veld 1: zand
# maatregelen op veld 1: EB12, EB19
# CBS regio: LG05 Centraal veehouderijgebied

# Gewas veld 2 (15 ha): luzerne (BRP-code: 258)
# Bodem type veld 2: klei
# maatregelen op veld 2: EG1C,EB2
# CBS regio: LG05 Centraal veehouderijgebied


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


er_aim.gold <- er_farm_aim(B_SOILTYPE_AGR = c('dekzand','rivierklei'), B_AREA = c(10,15), medalscore = "gold") 
er_aim.gold[,B_CT_TOTAL := B_CT_SOIL + B_CT_WATER + B_CT_CLIMATE + B_CT_BIO + B_CT_LANDSCAPE]
er_aim.gold[,REWARD := 175]
er_aim.gold <- melt(er_aim.gold,id.vars = 'farmid',variable.name = 'indicator',value.name = 'er_gold')
er_aim.gold[,indicator := gsub('B_CT_','',indicator)]

dt.field <- test$fields
setnames(dt.field,toupper(colnames(dt.field)))

dt.field <- data.table(id = c(1,2),
                       SOIL = dt.field$S_ER_SOIL,
                       WATER = dt.field$S_ER_WATER,
                       CLIMATE = dt.field$S_ER_CLIMATE,
                       BIO = dt.field$S_ER_BIODIVERSITY,
                       LANDSCAPE = dt.field$S_ER_LANDSCAPE,
                       TOTAL = dt.field$S_ER_TOT,
                       REWARD = dt.field$S_ER_REWARD,
                       B_AREA = c(10,15))
dt.field <- melt(dt.field,id.vars = c('id','B_AREA'),variable.name = 'indicator')

dt.field <- merge(dt.field,er_aim.gold[,.(indicator,er_gold)],by='indicator',all.x = TRUE)
# dit was de originele score (let op: groter dan 100 is hier gelijk aan 100)
dt.field[,score := value * er_gold * 0.01]





#VELD1
# de maatregel EB12 levert de volgende punten op:
# climate:4, soil: 6, water:6, landscape:2, biodiversity:2, 250 euro per ha

# er is sprake van bodemtype gebonden urgency
# urgency op zand: punten soil 1, water 1.5, climate 1, bio 1.25 en landscape 1
# dat levert dus op:
# climate:4, soil: 6, water: 9, landscape: 2, biodiversity: 2.5, 275 euro per ha (LG05 =1)

# in de testset was aan de maatregel toegevoegd: "applied as ANLB".
# the measure has the accumulation rule "both", so reward is set to 275


# de maatregel EB9 levert de volgende punten op:
# climate:5, soil: 4, water:1, landscape:0, biodversity:0, 200 euro per ha 

# er is sprake van bodemtype gebonden urgency
# urgency op zand: punten soil 1, water 1.5, climate 1, bio 1.25 en landscape 1
# dat levert dus op:
# climate:1, soil: 4, water: 1.5, landscape: 0, biodiversity: 0, 200 euro per ha (LG05 =1)

# in de testset was aan de maatregel toegevoegd: "applied as ANLB".
# the measure has the accumulation rule "score only", so reward is set to 0

# compared to the aim:
# climate = 4+1 / 3.5 = 100%
# soil = 6+4 / 9.1 = 100%
# water = 10.5 / 9.45 = 100%
# landscape = 1 / 7 = 14%
# bio = 2.5 / 5.95 = 42%



#VELD2
# de maatregel EG1C levert de volgende punten op:
# climate:0, soil: 5, water:5, landscape:10, bio:10, 216 euro per ha

# measure not applicable regarding bouwland (eco7)


# de maatregel EB2 levert de volgende punten op:
# climate:8, soil: 5, water: 0, landscape: 2, bio: 5, 2150 euro per ha

# er is sprake van bodemtype gebonden urgency
# urgency op klei: punten soil 1.5, water 1.25, climate 1, bio 1 en landscape 1
# dat levert dus op:
# climate:8, soil: 7.5, water: 0, landscape: 2, bio: 5, 2150 euro per ha

# in de testset was aan de maatregel toegevoegd: "applied as ANLB".
# the measure has the accumulation rule "score only", so reward is set to 0

# compared to the aim: 
# climate = 8 / 3.5 = 100%
# soil = 7.5 / 9.1 = 82%
# water = 0 / 9.45 = 0%
# landscape = 2 / 7 = 29%
# bio = 12.5 / 5.95 = 100%



# ---- test 7: voor een bedrijf met twee percelen. Sector c("dairy") -------

# Voor een bedrijf met twee percelen, sector c("dairy")

# Gewas veld 1 (10 ha): winterpeen, productie (BRP-code: 2785)
# Bodem type veld 1: zand
# maatregelen op veld 1: EB12, EB19
# CBS regio: LG05 Centraal veehouderijgebied

# Gewas veld 2 (15 ha): luzerne (BRP-code: 258)
# Bodem type veld 2: klei
# maatregelen op veld 2: EG1C,EB2
# CBS regio: LG05 Centraal veehouderijgebied


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
sector = c('dairy')
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


er_aim.gold <- er_farm_aim(B_SOILTYPE_AGR = c('dekzand','rivierklei'), B_AREA = c(10,15), medalscore = "gold") 
er_aim.gold[,B_CT_TOTAL := B_CT_SOIL + B_CT_WATER + B_CT_CLIMATE + B_CT_BIO + B_CT_LANDSCAPE]
er_aim.gold[,REWARD := 175]
er_aim.gold <- melt(er_aim.gold,id.vars = 'farmid',variable.name = 'indicator',value.name = 'er_gold')
er_aim.gold[,indicator := gsub('B_CT_','',indicator)]

dt.field <- test$fields
setnames(dt.field,toupper(colnames(dt.field)))

dt.field <- data.table(id = c(1,2),
                       SOIL = dt.field$S_ER_SOIL,
                       WATER = dt.field$S_ER_WATER,
                       CLIMATE = dt.field$S_ER_CLIMATE,
                       BIO = dt.field$S_ER_BIODIVERSITY,
                       LANDSCAPE = dt.field$S_ER_LANDSCAPE,
                       TOTAL = dt.field$S_ER_TOT,
                       REWARD = dt.field$S_ER_REWARD,
                       B_AREA = c(10,15))
dt.field <- melt(dt.field,id.vars = c('id','B_AREA'),variable.name = 'indicator')

dt.field <- merge(dt.field,er_aim.gold[,.(indicator,er_gold)],by='indicator',all.x = TRUE)
# dit was de originele score (let op: groter dan 100 is hier gelijk aan 100)
dt.field[,score := value * er_gold * 0.01]





#VELD1
# de maatregel EB12 levert de volgende punten op:
# climate:4, soil: 6, water:6, landscape:2, biodiversity:2, 250 euro per ha

# er is sprake van bodemtype gebonden urgency
# urgency op zand: punten soil 1, water 1.5, climate 1, bio 1.25 en landscape 1
# dat levert dus op:
# climate:4, soil: 6, water: 9, landscape: 2, biodiversity: 2.5, 275 euro per ha (LG05 =1)

# in de testset was aan de maatregel toegevoegd: "applied as ANLB".
# the measure has the accumulation rule "both", so reward is set to 275


# de maatregel EB9 levert de volgende punten op:
# climate:5, soil: 4, water:1, landscape:0, biodversity:0, 200 euro per ha 

# er is sprake van bodemtype gebonden urgency
# urgency op zand: punten soil 1, water 1.5, climate 1, bio 1.25 en landscape 1
# dat levert dus op:
# climate:1, soil: 4, water: 1.5, landscape: 0, biodiversity: 0, 200 euro per ha (LG05 =1)

# in de testset was aan de maatregel toegevoegd: "applied as ANLB".
# the measure has the accumulation rule "score only", so reward is set to 0

# compared to the aim:
# climate = 4+1 / 3.5 = 100%
# soil = 6+4 / 9.1 = 100%
# water = 10.5 / 9.45 = 100%
# landscape = 1 / 7 = 14%
# bio = 2.5 / 5.95 = 42%



#VELD2
# de maatregel EG1C levert de volgende punten op:
# climate:0, soil: 5, water:5, landscape:10, bio:10, 216 euro per ha

# measure not applicable regarding bouwland (eco7)


# de maatregel EB2 levert de volgende punten op:
# climate:8, soil: 5, water: 0, landscape: 2, bio: 5, 2150 euro per ha

# er is sprake van bodemtype gebonden urgency
# urgency op klei: punten soil 1.5, water 1.25, climate 1, bio 1 en landscape 1
# dat levert dus op:
# climate:8, soil: 7.5, water: 0, landscape: 2, bio: 5, 2150 euro per ha

# in de testset was aan de maatregel toegevoegd: "applied as ANLB".
# the measure has the accumulation rule "score only", so reward is set to 0

# compared to the aim: 
# climate = 8 / 3.5 = 100%
# soil = 7.5 / 9.1 = 82%
# water = 0 / 9.45 = 0%
# landscape = 2 / 7 = 29%
# bio = 12.5 / 5.95 = 100%


