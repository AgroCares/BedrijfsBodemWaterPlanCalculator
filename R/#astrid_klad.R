
# set the score to zero for the measures taken on farm level when not applicable
dt1[grepl("^EG4",eco_id) & BBWP_CAT != "c9" & B_LU_ECO1 != "c9" &  B_LU_ECO2 != "c9" & B_LU_ECO3 != "c9", c(cols) := 0] 
dt1[grepl("^EG7",eco_id) & BBWP_CAT != "c1|c2" & B_LU_ECO1 != "c1|c2" &  B_LU_ECO2 != "c1|c2" & B_LU_ECO3 != "c1|c2", c(cols) := 0]
dt1[grepl("^EG15",eco_id) & BBWP_CAT != "c10" & B_LU_ECO1 != "c10" &  B_LU_ECO2 != "c10" & B_LU_ECO3 != "c10", c(cols) := 0]
dt1[grepl("^EG16",eco_id) & BBWP_CAT != "c1|c3" & B_LU_ECO1 != "c1|c3" &  B_LU_ECO2 != "c1|c3" & B_LU_ECO3 != "c1|c3", c(cols) := 0]
dt1[grepl("^EG17",eco_id) & BBWP_CAT != "c1|c3" & B_LU_ECO1 != "c1|c3" &  B_LU_ECO2 != "c1|c3" & B_LU_ECO3 != "c1|c3", c(cols) := 0]
dt1[grepl("^EG19",eco_id) & BBWP_CAT != "c9" & B_LU_ECO1 != "c9" &  B_LU_ECO2 != "c9" & B_LU_ECO3 != "c9", c(cols) := 0]
dt1[grepl("^EG20",eco_id) & BBWP_CAT != "c8", c(cols) := 0]
dt1[grepl("^EG21",eco_id) & BBWP_CAT != "c6|c8" & B_LU_ECO1 != "c6|c8" &  B_LU_ECO2 != "c6|c8" & B_LU_ECO3 != "c6|c8", c(cols) := 0]
dt1[eco_id == "EB1A|EB1B|EB1C" & BBWP_CAT != "c2|c3|c5|c6|c12|c15|c16" & B_LU_ECO1 != "c2|c3|c5|c6|c12|c15|c16" &  B_LU_ECO2 != "c2|c3|c5|c6|c12|c15|c16" & B_LU_ECO3 != "c2|c3|c5|c6|c12|c15|c16", c(cols) := 0]
dt1[eco_id == "EB2" & BBWP_CAT != "c5|c12|c16" & B_LU_ECO1 != "c5|c12|c16" &  B_LU_ECO2 != "c5|c12|c16" & B_LU_ECO3 != "c5|c12|c16", c(cols) := 0]
dt1[grepl("^EB3",eco_id) & BBWP_CAT != "c1|c3|c12|c15|c16" & B_LU_ECO1 != "c1|c3|c12|c15|c16" &  B_LU_ECO2 != "c1|c3|c12|c15|c16" & B_LU_ECO3 != "c1|c3|c12|c15|c16", c(cols) := 0]
dt1[grepl("^EB6",eco_id) & BBWP_CAT != "c2|c3|c5|c6|c11|c12|c16" & B_LU_ECO1 != "c2|c3|c5|c6|c11|c12|c16" &  B_LU_ECO2 != "c2|c3|c5|c6|c11|c12|c16" & B_LU_ECO3 != "c2|c3|c5|c6|c11|c12|c16", c(cols) := 0]
dt1[grepl("^EB7",eco_id) & BBWP_CAT != "c4" & B_LU_ECO1 != "c4" &  B_LU_ECO2 != "c4" & B_LU_ECO3 != "c4", c(cols) := 0]
dt1[grepl("^EB8",eco_id) & BBWP_CAT != "c2|c3|c4|c5|c6|c12|c15|c16" & B_LU_ECO1 != "c2|c3|c4|c5|c6|c12|c15|c16" &  B_LU_ECO2 != "c2|c3|c4|c5|c6|c12|c15|c16" & B_LU_ECO3 != "c2|c3|c4|c5|c6|c12|c15|c16", c(cols) := 0]
dt1[grepl("^EB9",eco_id)& BBWP_CAT != "c3|c15|c16" & B_LU_ECO1 != "c3|c15|c16" &  B_LU_ECO2 != "c3|c15|c16" & B_LU_ECO3 != "c3|c15|c16", c(cols) := 0]
dt1[grepl("^EB11",eco_id) & BBWP_CAT != "c2|c3|c5|c6|c12|c16" & B_LU_ECO1 != "c2|c3|c5|c6|c12|c16" &  B_LU_ECO2 != "c2|c3|c5|c6|c12|c16" & B_LU_ECO3 != "c2|c3|c5|c6|c12|c16", c(cols) := 0]
dt1[grepl("^EB16",eco_id) & BBWP_CAT != "c10" & B_LU_ECO1 != "c10" &  B_LU_ECO2 != "c10" & B_LU_ECO3 != "c10", c(cols) := 0]
dt1[grepl("^EB17",eco_id) & BBWP_CAT != "c2|c16" & B_LU_ECO1 != "c2|c16" &  B_LU_ECO2 != "c2|c16" & B_LU_ECO3 != "c2|c16", c(cols) := 0]
dt1[grepl("^EB23",eco_id) & BBWP_CAT != "c10" & B_LU_ECO1 != "c10" &  B_LU_ECO2 != "c10" & B_LU_ECO3 != "c10", c(cols) := 0]

dt[grepl("^EG4",eco_id) & BBWP_CAT != "c9", c(cols) := 0] 
dt[grepl("^EG7",eco_id) & BBWP_CAT != "c1|c2", c(cols) := 0]
dt[grepl("^EG15",eco_id) & BBWP_CAT != "c10", c(cols) := 0]
dt[grepl("^EG16",eco_id) & BBWP_CAT != "c1|c3", c(cols) := 0]
dt[grepl("^EG17",eco_id) & BBWP_CAT != "c1|c3", c(cols) := 0]
dt[grepl("^EG19",eco_id) & BBWP_CAT != "c9", c(cols) := 0]
dt[grepl("^EG20",eco_id) & BBWP_CAT != "c8", c(cols) := 0]
dt[grepl("^EG21",eco_id) & BBWP_CAT != "c6|c8", c(cols) := 0]
dt[eco_id == "EB1A|EB1B|EB1C" & BBWP_CAT != "c2|c3|c5|c6|c12|c15|c16", c(cols) := 0]
dt[eco_id == "EB2" & BBWP_CAT != "c5|c12|c16", c(cols) := 0]
dt[grepl("^EB3",eco_id) & BBWP_CAT != "c1|c3|c12|c15|c16", c(cols) := 0]
dt[grepl("^EB6",eco_id) & BBWP_CAT != "c2|c3|c5|c6|c11|c12|c16", c(cols) := 0]
dt[grepl("^EB7",eco_id) & BBWP_CAT != "c4" & B_LU_ECO1 != "c4", c(cols) := 0]
dt[grepl("^EB8",eco_id) & BBWP_CAT != "c2|c3|c4|c5|c6|c12|c15|c16", c(cols) := 0]
dt[grepl("^EB9",eco_id) & BBWP_CAT != "c3|c15|c16", c(cols) := 0]
dt[grepl("^EB11",eco_id) & BBWP_CAT != "c2|c3|c5|c6|c12|c16", c(cols) := 0]
dt[grepl("^EB16",eco_id) & BBWP_CAT != "c10", c(cols) := 0]
dt[grepl("^EB17",eco_id) & BBWP_CAT != "c2|c16", c(cols) := 0]
dt[grepl("^EB23",eco_id) & BBWP_CAT != "c10", c(cols) := 0]




# wat hier staat is: het veld heeft de volgende crop categorie B_LU_BBWP, en de maatregel is geldig is voor de cropcategorie BBWP_CAT ..., B_LU_ECO1 ... etc, 
# dus als het veld crop categorie 1 heeft, maar de maatregel niet geldt voor categorie 1 dan mag er geen score worden gegeven. 
dt[B_LU_BBWP == 1 & BBWP_CAT != 1 & B_LU_ECO1 != 1 &  B_LU_ECO2 != 1 & B_LU_ECO3 != 1, c(cols) := 0]
dt[B_LU_BBWP == 2 & BBWP_CAT != 2 & B_LU_ECO1 != 2 &  B_LU_ECO2 != 2 & B_LU_ECO3 != 2, c(cols) := 0]
dt[B_LU_BBWP == 3 & BBWP_CAT != 3 & B_LU_ECO1 != 3 &  B_LU_ECO2 != 3 & B_LU_ECO3 != 3, c(cols) := 0]
dt[B_LU_BBWP == 4 & BBWP_CAT != 4 & B_LU_ECO1 != 4 &  B_LU_ECO2 != 4 & B_LU_ECO3 != 4, c(cols) := 0]
dt[B_LU_BBWP == 5 & BBWP_CAT != 5 & B_LU_ECO1 != 5 &  B_LU_ECO2 != 5 & B_LU_ECO3 != 5, c(cols) := 0]
dt[B_LU_BBWP == 6 & BBWP_CAT != 6 & B_LU_ECO1 != 6 &  B_LU_ECO2 != 6 & B_LU_ECO3 != 6, c(cols) := 0]
dt[B_LU_BBWP == 7 & BBWP_CAT != 7 & B_LU_ECO1 != 7 &  B_LU_ECO2 != 7 & B_LU_ECO3 != 7, c(cols) := 0]
dt[B_LU_BBWP == 8 & BBWP_CAT != 8 & B_LU_ECO1 != 8 &  B_LU_ECO2 != 8 & B_LU_ECO3 != 8, c(cols) := 0]
dt[B_LU_BBWP == 9 & BBWP_CAT != 9 & B_LU_ECO1 != 9 &  B_LU_ECO2 != 9 & B_LU_ECO3 != 9, c(cols) := 0]
dt[B_LU_BBWP == 10 & BBWP_CAT != 10 & B_LU_ECO1 != 10 &  B_LU_ECO2 != 10 & B_LU_ECO3 != 10, c(cols) := 0]
dt[B_LU_BBWP == 11 & BBWP_CAT != 11 & B_LU_ECO1 != 11 &  B_LU_ECO2 != 11 & B_LU_ECO3 != 11, c(cols) := 0]
dt[B_LU_BBWP == 12 & BBWP_CAT != 12 & B_LU_ECO1 != 12 &  B_LU_ECO2 != 12 & B_LU_ECO3 != 12, c(cols) := 0]
dt[B_LU_BBWP == 13 & BBWP_CAT != 13 & B_LU_ECO1 != 13 &  B_LU_ECO2 != 13 & B_LU_ECO3 != 13, c(cols) := 0]
dt[B_LU_BBWP == 14 & BBWP_CAT != 14 & B_LU_ECO1 != 14 &  B_LU_ECO2 != 14 & B_LU_ECO3 != 14, c(cols) := 0]
dt[B_LU_BBWP == 15 & BBWP_CAT != 15 & B_LU_ECO1 != 15 &  B_LU_ECO2 != 15 & B_LU_ECO3 != 15, c(cols) := 0]
dt[B_LU_BBWP == 16 & BBWP_CAT != 16 & B_LU_ECO1 != 16 &  B_LU_ECO2 != 16 & B_LU_ECO3 != 16, c(cols) := 0]
dt[B_LU_BBWP == 17 & BBWP_CAT != 17 & B_LU_ECO1 != 17 &  B_LU_ECO2 != 17 & B_LU_ECO3 != 17, c(cols) := 0]


# dit komt later of in een ander script, bijv in farm_score!
#dt.meas.taken <- dt.meas.taken[, .(id,eco_id,level,er_soil,er_water,er_climate,er_biodiversity,er_landscape,er_profit)] # hier moet dan een 
#datatable komen met de volgende kolommen erin: de id, eco_id, level, alle eco scores en reward/profit en CORRECTION FACTOR for regional 
# reward (komt later in het script erbij), en de CROP CATEGORY COLUMN die je hebt ge-melt.


# test for astrid file
# er_croprotation 

require(data.table);require(readxl);

#----input test data-------#
B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen')
B_LU_BRP = c(265,265,265,265)
B_LU_BBWP = c(1,1,1,1)
B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland')
B_AREA = c(45,18,0.8,6)
B_CT_SOIL = 20
B_CT_WATER = 15
B_CT_CLIMATE = 8
B_CT_BIO = 24
B_CT_LANDSCAPE = 20
measures = NULL 
sector = 'dairy'

# IF MEASURES ARE TAKEN
# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]
# make measurement list for 2 of the 4 fields
measures <- rbind(data.table(id = 1, dt.measures[c(2,5,18,28,32,3,38,43,62)]),
                  data.table(id = 3, dt.measures[c(7,21,30,46,5)]))

#----------------------------#


# reformat B_AER_CBS
B_AER_CBS <- bbwp_format_aer(B_AER_CBS)

# check length of the inputs
arg.length <- max(length(B_SOILTYPE_AGR),length(B_LU_BRP),length(B_LU_BBWP),length(B_AER_CBS))

# get internal table with importance of environmental challenges
dt.er.scoring <- as.data.table(BBWPC::er_scoring)
setnames(dt.er.scoring,gsub('cf_','',colnames(dt.er.scoring)))
dt.er.urgency <- melt(dt.er.scoring[type=='urgency'],
                      id.vars='soiltype',
                      measure.vars = c('soil', 'water', 'climate',  'biodiversity', 'landscape'),
                      variable.name = 'indicator',
                      value.name = 'urgency')

# check and update the measure table
dt.meas.taken <- bbwp_check_meas(dt = measures, eco = TRUE, score = TRUE)


# filter for measures on farm level 
#dt.meas.taken <- dt.meas.taken[level == "farm",]  # WAAROM DOE JE DAT HIER?? Is in principe niet nodig als je hiervoor de crop merge doet?

# collect data in one data.table
dt <- data.table(id = 1:arg.length,
                 B_AER_CBS = B_AER_CBS,
                 B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                 B_LU_BRP = B_LU_BRP,
                 B_LU_BBWP = B_LU_BBWP,
                 B_AREA = B_AREA,
                 B_CT_SOIL = B_CT_SOIL, 
                 B_CT_WATER = B_CT_WATER,
                 B_CT_CLIMATE = B_CT_CLIMATE,
                 B_CT_BIO = B_CT_BIO,
                 B_CT_LANDSCAPE = B_CT_LANDSCAPE,
                 # measures = measures #, # komt dit gedeelte met measures pas bij farmscore erbij???
                 sector = sector
)

# merge all measures to the given fields
dt <- merge(dt,dt.meas.taken, by='id', all=TRUE )

# add bbwp table for financial reward correction factor per AER
dt.er.reward <- as.data.table(BBWPC::er_aer_reward)

# merge with regional correction factor for the financial reward
dt <- merge(dt,dt.er.reward[,.(statcode,er_cf)], by.x = 'B_AER_CBS',by.y = 'statcode')


# set scores to zero when measures are not applicable given the crop type

# columns with the Ecoregelingen ranks and reward
cols <- c('er_soil','er_water','er_biodiversity','er_climate','er_landscape','er_profit')

# set first all missing data impacts to 0
dt[,c(cols) := lapply(.SD, function(x) fifelse(is.na(x),0,x)), .SDcols = cols]

# set the score to zero for the measures taken on farm level when not applicable for given crop category
dt[grepl("^EG4",eco_id) & BBWP_CAT != "c9", c(cols) := 0] 
dt[grepl("^EG7",eco_id) & BBWP_CAT != "c1|c2", c(cols) := 0]
dt[grepl("^EG15",eco_id) & BBWP_CAT != "c10", c(cols) := 0]
dt[grepl("^EG16",eco_id) & BBWP_CAT != "c1|c3", c(cols) := 0]
dt[grepl("^EG17",eco_id) & BBWP_CAT != "c1|c3", c(cols) := 0]
dt[grepl("^EG19",eco_id) & BBWP_CAT != "c9", c(cols) := 0]
dt[grepl("^EG20",eco_id) & BBWP_CAT != "c8", c(cols) := 0]
dt[grepl("^EG21",eco_id) & BBWP_CAT != "c6|c8", c(cols) := 0]
dt[eco_id == "EB1A|EB1B|EB1C" & BBWP_CAT != "c2|c3|c5|c6|c12|c15|c16", c(cols) := 0]
dt[eco_id == "EB2" & BBWP_CAT != "c5|c12|c16", c(cols) := 0]
dt[grepl("^EB3",eco_id) & BBWP_CAT != "c1|c3|c12|c15|c16", c(cols) := 0]
dt[grepl("^EB6",eco_id) & BBWP_CAT != "c2|c3|c5|c6|c11|c12|c16", c(cols) := 0]
dt[grepl("^EB7",eco_id) & BBWP_CAT != "c4" & B_LU_ECO1 != "c4", c(cols) := 0]
dt[grepl("^EB8",eco_id) & BBWP_CAT != "c2|c3|c4|c5|c6|c12|c15|c16", c(cols) := 0]
dt[grepl("^EB9",eco_id) & BBWP_CAT != "c3|c15|c16", c(cols) := 0]
dt[grepl("^EB11",eco_id) & BBWP_CAT != "c2|c3|c5|c6|c12|c16", c(cols) := 0]
dt[grepl("^EB16",eco_id) & BBWP_CAT != "c10", c(cols) := 0]
dt[grepl("^EB17",eco_id) & BBWP_CAT != "c2|c16", c(cols) := 0]
dt[grepl("^EB23",eco_id) & BBWP_CAT != "c10", c(cols) := 0]
#dt[grepl("^EG22",eco_id) & BBWP_CAT != "c17" & B_LU_ECO1 != "c17" &  B_LU_ECO2 != "c17" & B_LU_ECO3 != "c17", c(cols) := 0]
# bbwp =1 , crop1 =0 --> 0  

# set the score and profit to zero when the measure is not applicable given sector

# add columns for the sector to which the farms belong
fs0 <- c('fdairy','farable','ftree_nursery','fbulbs')
fs1 <- paste0('f',sector)
fs2 <- fs0[!fs0 %in% fs1]
dt[,c(fs1) := 1]
dt[,c(fs2) := 0]

# estimate whether sector allows applicability
dt[, fsector := fdairy * dairy + farable * arable + ftree_nursery * tree_nursery + fbulbs * bulbs]

# adapt the score and profit to zero when measure is not applicable
dt[fsector == 0, c(cols) := 0][,value := value + 0]

# set the score and profit to zero when the measure is not applicable given soil type

# adapt the score and profit when the soil type limits the applicability of measures
dt[grepl('klei', B_SOILTYPE_AGR) & clay == 0 , c(cols) := 0][,value := value + 0]
dt[grepl('zand|dal', B_SOILTYPE_AGR) & sand == 0 , c(cols) := 0][,value := value + 0]
dt[grepl('veen', B_SOILTYPE_AGR) & peat == 0 , c(cols) := 0][,value := value + 0]
dt[grepl('loess', B_SOILTYPE_AGR) & loess == 0 , c(cols) := 0][,value := value + 0]


# set the score and profit to zero when the measure is not applicable given the percentage area of the total farm to which the measure applies 

# add filter for rustgewas (EB1) and estimate percentage rustgewassen
dt[,cf := fifelse(eco_id == "EB1" & BBWP_CAT == "c3",1,0)]
dt[,B_AREA_RR := sum(B_AREA * cf) / sum(B_AREA)] 

# adapt the score and profit for EB1 based on percentage area rustgewassen
dt[B_AREA_RR < 21 & eco_id == 'EB1A', c(cols) := 0]
dt[B_AREA_RR >= 21 & B_AREA_RR <= 35 & eco_id == 'EB1A', value := value + er_profit]
dt[B_AREA_RR < 36 & eco_id == 'EB1B', c(cols) := 0]
dt[B_AREA_RR >= 36 & B_AREA_RR <= 50 & eco_id == 'EB1B', value := value + er_profit]
dt[B_AREA_RR < 50 & eco_id == 'EB1C', c(cols) := 0]
dt[B_AREA_RR > 50 & eco_id == 'EB1C', value := value + er_profit]

# adapt the score and profit for kleinschalig landschap (EG22) en (EB25)
dt[B_AREA < 2 & eco_id == 'EG22|EB25', value := value + er_profit]
dt[B_AREA > 2 & eco_id == 'EG22|EB25',  c(cols) := 0]

# add filter for 'houtopstanden en water- en moeraselementen' (EG20) and estimate percentage houtopstanden en water- en moeraselementen
dt[,cf := fifelse(eco_id == "EG20" & BBWP_CAT == "c8",1,0)] 
dt[,B_AREA_N := sum(B_AREA * cf) / sum(B_AREA)] 

# adapt the score and profit for EB20 based on percentage area nature (N) (houtwallen en water/moeraselementen)
dt[B_AREA_N < 1 & eco_id == 'EB20A', c(cols) := 0]
dt[B_AREA_N >= 1 & B_AREA_N <= 3.5 & eco_id == 'EB20A', value := value + er_profit]
dt[B_AREA_N < 3.5 & eco_id == 'EB20B', c(cols) := 0]
dt[B_AREA_N >= 3.5 & B_AREA_N <= 5 & eco_id == 'EB20B', value := value + er_profit]
dt[B_AREA_N < 5 & eco_id == 'EB20C', c(cols) := 0]
dt[B_AREA_N > 5 & eco_id == 'EB20C', value := value + er_profit]

#EB22 # HIER NOG GOED NAAR KIJKEN 
# add filter for niet productieve oppervlakte (NP) 'houtopstanden, water- en moeraselementen, akkerranden' (EB22) and estimate percentage NP
dt[,cf := fifelse(grepl('^EB22',eco_id) & Productief == 0 & BBWP_CAT == "c8|c10",1,0)] 
dt[,B_AREA_NP := sum(B_AREA * cf) / sum(B_AREA)] 

# adapt the score and profit for EB22 based on percentage area niet productieve oppervlakte (NP) 
dt[B_AREA_NP < 1 & eco_id == 'EB22A', c(cols) := 0]
dt[B_AREA_NP >= 1 & B_AREA_NP <= 3.5 & eco_id == 'EB22A', value := value + er_profit]
dt[B_AREA_NP < 3.5 & eco_id == 'EB22B', c(cols) := 0]
dt[B_AREA_NP >= 3.5 & B_AREA_NP <= 5 & eco_id == 'EB22B', value := value + er_profit]
dt[B_AREA_NP < 5 & eco_id == 'EB22C', c(cols) := 0]
dt[B_AREA_NP > 5 & eco_id == 'EB22C', value := value + er_profit]

# set score and profit to 0 when measure EB23 is not taken in Bouwland
dt[Bouwland == 0 & grepl('^EB23',eco_id), c(cols) := 0][,value := value + 0]

# set score and profit to 0 when a 'vanggewas' is cultivated for measure toepassen mengteelt (EB5)
dt[grepl('^EB5',eco_id) & BBWP_CAT == "c11", c(cols) := 0][,value := value + 0]

#measures on farm scale --> look whether action is needed (no farm/field dependency but still a -1 in the ER_puntenregeling table) 
#EG6 --> dit gaat over melk  
#EG11 --> dit gaat over type mest
#EB14 --> dit gaat over de OS balans
#EB18 --> dit gaat over gewasbeschermingsmiddelen 
#EB24 --> hiervoor moet percentage sloot bekend zijn

# multiply by (political) urgency

# first add soil type for political and environmental urgency
dt[grepl('klei', B_SOILTYPE_AGR) , soiltype := 'klei']
dt[grepl('zand|dal', B_SOILTYPE_AGR), soiltype := 'zand']
dt[grepl('veen', B_SOILTYPE_AGR), soiltype := 'veen']
dt[grepl('loess', B_SOILTYPE_AGR), soiltype := 'loess']

# melt dt
dt <- melt(dt,
           id.vars = c('id','bbwp_id','soiltype','bbwp_conflict'),
           measure = patterns(erscore = "^er_"),
           variable.name = 'indicator',
           value.name = 'value')
dt[,indicator := gsub('er_', '',cols[indicator])]

# merge with urgency table
dt <- merge(dt,dt.er.urgency, by= c('soiltype','indicator'),all.x = TRUE)

# adapt the score based on urgency
dt[indicator != 'profit', value := value * urgency]


# dcast to add totals, to be used to update scores when measures are conflicting

cols <- c('biodiversity', 'climate', 'landscape', 'soil','water','total')
dt2 <- dcast(dt, id + soiltype + bbwp_id + bbwp_conflict ~ indicator, value.var = 'value')
dt2[, total := biodiversity + climate + landscape + soil + water]
dt2[, oid := frank(-total, ties.method = 'first',na.last = 'keep'), by = c('id','bbwp_conflict')]
dt2[oid > 1, c(cols) := 0]

# calculate the weighed average ER score (points/ ha) for the whole farm due to measures taken
dt.field <- dt2[,lapply(.SD,sum), .SDcols = cols, by = 'id']

# calculate total reward per field (euro / ha)
dt.reward <- dt[indicator == 'profit',list(S_ER_REWARD = sum(value)),by = 'id']

# add reward to the field
dt.field <- merge(dt.field,dt.reward,by='id')

# setnames
setnames(dt.field,
         c('biodiversity', 'climate', 'landscape', 'soil','water','total'),
         c('D_MEAS_BIO', 'D_MEAS_CLIM', 'D_MEAS_LAND', 'D_MEAS_SOIL', 'D_MEAS_WAT','D_MEAS_TOT'))

# order to ensure field order
setorder(dt.field, id)










# reformat crop table (include for each measure the crop categories to which the measure applies)

#load crop table (under construction)
dt1<-  as.data.table(read_excel("C:/Astrid/BBWP-ecoregeling bestanden/b_lu_brp_GR220623_decast_for_client.xlsx"))

# change format from wide to long
dt2 <- melt(dt1, 
            id.vars = c("B_LU_BRP","B_LU_NAME"),
            measure.vars = patterns("^c"),
            variable.name = "BBWP_CAT",
            value.name = "boolean")

# keep only rows that indicate to which category the crop type belongs 
dt2 <- dt2[boolean == 1,]
dt2 <- dt2[,boolean := NULL]

# change format back from long to wide 
dt2 <- dcast(dt2,
             B_LU_BRP ~ rowid(B_LU_BRP),
             value.var = "BBWP_CAT", sep = ".")

# create extra columns when crop belongs in multiple categories in order that each row has unique crop type
dt1 <- setDT(dt2, key = "B_LU_BRP")[dt1, B_LU_NAME := i.B_LU_NAME]
setnames(dt1,c("1","2","3"),c("BBWP_CAT","B_LU_ECO1","B_LU_ECO2"))
setcolorder(dt1,c("B_LU_BRP","B_LU_NAME","BBWP_CAT"))

# load crop list and select relevant columns 
dt3 <- er_crops[,c("er_description","b_lu_name") := NULL]

# merge measures table with crop categories 
dt3 <- merge(dt3,dt1, by.x= "b_lu_brp", by.y = "B_LU_BRP")

# select relevant columns and keep only unique combinations
dt3 <- dt3[, c("eco_id","BBWP_CAT","B_LU_ECO1","B_LU_ECO2")]
dt3 <- (unique(dt3))

#rearrange order of eco_id in a way that the same eco_id's are among each other
dt1 <- dt3[order(eco_id)]






# Dummy dataset 
dt1 <- data.table(B_LU_BBWP = c(1,4,8),
                  eco_id = c("EG7","EG15","EG18"),
                  BBWP_CAT = c("c3","c1","c2"), 
                  B_LU_ECO1 = c("c10","c12","c14"), 
                  B_LU_ECO2 = c("c11",NA,NA), 
                  B_LU_ECO3 = c("c4",NA,NA), 
                  er_soil= c(1,1,1),
                  er_water= c(1,1,1),
                  er_biodiversity= c(1,1,1),
                  er_climate= c(1,1,1),
                  er_landscape= c(1,1,1),
                  er_profit = c(1,1,1),
                  sector= "dairy",
                  B_SOILTYPE_AGR= "dekzand")


dt[grepl("^EG2",eco_id) & BBWP_CAT != "c1|c2|c9|c10|c11|c12|c13|c14|c15|c16" & B_LU_ECO1 != "c1|c2|c9|c10|c11|c12|c13|c14|c15|c16" &  B_LU_ECO2 != "c1|c2|c9|c10|c11|c12|c13|c14|c15|c16" & B_LU_ECO3 != "c1|c2|c9|c10|c11|c12|c13|c14|c15|c16", c(cols) := 0][,value := value + 0] 
dt[grepl("^EG4",eco_id) & BBWP_CAT != "c9" & B_LU_ECO1 != "c9" &  B_LU_ECO2 != "c9" & B_LU_ECO3 != "c9", c(cols) := 0][,value := value + 0]
dt[grepl("^EG7",eco_id) & BBWP_CAT != "c1|c2" & B_LU_ECO1 != "c1|c2" &  B_LU_ECO2 != "c1|c2" & B_LU_ECO3 != "c1|c2", c(cols) := 0][,value := value + 0]
dt[grepl("^EG15",eco_id) & BBWP_CAT != "c10" & B_LU_ECO1 != "c10" &  B_LU_ECO2 != "c10" & B_LU_ECO3 != "c10", c(cols) := 0][,value := value + 0]
dt[grepl("^EG16",eco_id) & BBWP_CAT != "c1|c3" & B_LU_ECO1 != "c1|c3" &  B_LU_ECO2 != "c1|c3" & B_LU_ECO3 != "c1|c3", c(cols) := 0][,value := value + 0]
dt[grepl("^EG17",eco_id) & BBWP_CAT != "c1|c3" & B_LU_ECO1 != "c1|c3" &  B_LU_ECO2 != "c1|c3" & B_LU_ECO3 != "c1|c3", c(cols) := 0][,value := value + 0]
dt[grepl("^EG19",eco_id) & BBWP_CAT != "c9" & B_LU_ECO1 != "c9" &  B_LU_ECO2 != "c9" & B_LU_ECO3 != "c9", c(cols) := 0][,value := value + 0]
dt[grepl("^EG20",eco_id) & BBWP_CAT != "c8", c(cols) := 0][,value := value + 0]
dt[grepl("^EG21",eco_id) & BBWP_CAT != "c6|c8" & B_LU_ECO1 != "c6|c8" &  B_LU_ECO2 != "c6|c8" & B_LU_ECO3 != "c6|c8", c(cols) := 0][,value := value + 0]
dt[eco_id == "EB1A|EB1B|EB1C" & BBWP_CAT != "c2|c3|c5|c6|c12|c15|c16" & B_LU_ECO1 != "c2|c3|c5|c6|c12|c15|c16" &  B_LU_ECO2 != "c2|c3|c5|c6|c12|c15|c16" & B_LU_ECO3 != "c2|c3|c5|c6|c12|c15|c16", c(cols) := 0][,value := value + 0]
dt[eco_id == "EB2" & BBWP_CAT != "c5|c12|c16" & B_LU_ECO1 != "c5|c12|c16" &  B_LU_ECO2 != "c5|c12|c16" & B_LU_ECO3 != "c5|c12|c16", c(cols) := 0][,value := value + 0]
dt[grepl("^EB3",eco_id) & BBWP_CAT != "c1|c3|c12|c15|c16" & B_LU_ECO1 != "c1|c3|c12|c15|c16" &  B_LU_ECO2 != "c1|c3|c12|c15|c16" & B_LU_ECO3 != "c1|c3|c12|c15|c16", c(cols) := 0][,value := value + 0]
dt[grepl("^EB6",eco_id) & BBWP_CAT != "c2|c3|c5|c6|c11|c12|c16" & B_LU_ECO1 != "c2|c3|c5|c6|c11|c12|c16" &  B_LU_ECO2 != "c2|c3|c5|c6|c11|c12|c16" & B_LU_ECO3 != "c2|c3|c5|c6|c11|c12|c16", c(cols) := 0][,value := value + 0]
dt[grepl("^EB7",eco_id) & BBWP_CAT != "c4" & B_LU_ECO1 != "c4" &  B_LU_ECO2 != "c4" & B_LU_ECO3 != "c4", c(cols) := 0][,value := value + 0]
dt[grepl("^EB8",eco_id) & BBWP_CAT != "c2|c3|c4|c5|c6|c12|c15|c16" & B_LU_ECO1 != "c2|c3|c4|c5|c6|c12|c15|c16" &  B_LU_ECO2 != "c2|c3|c4|c5|c6|c12|c15|c16" & B_LU_ECO3 != "c2|c3|c4|c5|c6|c12|c15|c16", c(cols) := 0][,value := value + 0]
dt[grepl("^EB9",eco_id)& BBWP_CAT != "c3|c15|c16" & B_LU_ECO1 != "c3|c15|c16" &  B_LU_ECO2 != "c3|c15|c16" & B_LU_ECO3 != "c3|c15|c16", c(cols) := 0][,value := value + 0]
dt[grepl("^EB11",eco_id) & BBWP_CAT != "c2|c3|c5|c6|c12|c16" & B_LU_ECO1 != "c2|c3|c5|c6|c12|c16" &  B_LU_ECO2 != "c2|c3|c5|c6|c12|c16" & B_LU_ECO3 != "c2|c3|c5|c6|c12|c16", c(cols) := 0][,value := value + 0]
dt[grepl("^EB16",eco_id) & BBWP_CAT != "c10" & B_LU_ECO1 != "c10" &  B_LU_ECO2 != "c10" & B_LU_ECO3 != "c10", c(cols) := 0][,value := value + 0]
dt[grepl("^EB17",eco_id) & BBWP_CAT != "c2|c16" & B_LU_ECO1 != "c2|c16" &  B_LU_ECO2 != "c2|c16" & B_LU_ECO3 != "c2|c16", c(cols) := 0][,value := value + 0]
dt[grepl("^EB23",eco_id) & BBWP_CAT != "c10" & B_LU_ECO1 != "c10" &  B_LU_ECO2 != "c10" & B_LU_ECO3 != "c10", c(cols) := 0][,value := value + 0]
#dt[grepl("^EG22",eco_id) & BBWP_CAT != "c8|c10" & B_LU_ECO1 != "c8|c10" &  B_LU_ECO2 != "c8|c10" & B_LU_ECO3 != "c8|c10", c(cols) := 0][,value := value + 0]
# bbwp =1 , crop1 =0 --> 0  

levels(dt$BBWP_CAT) = c("c0","c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12","c13","c14","c15","c16")
levels(dt$B_LU_ECO1) = c("c0","c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12","c13","c14","c15","c16")
levels(dt$B_LU_ECO2) = c("c0","c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12","c13","c14","c15","c16")



# add scores and profit based on number of hectares for measures on farm level
# EG2
dt[,value := fifelse(grepl("EG2A",eco_id), value + (F_AREA * 50),value)] 
dt[,value := fifelse(grepl("EG2B",eco_id), value + (F_AREA * 75),value)]

# EG11
dt[,value := fifelse(grepl("^EG11",eco_id), value + (F_AREA * 115),value)]

# EG13
dt[,value := fifelse(grepl("EG13",eco_id),value + (F_AREA * 120), value)]

# EG14
dt[,value := fifelse(grepl("EG14",eco_id),value + (F_AREA * 200), value)]

# EG20 
dt[,value := fifelse(grepl("EG20A",eco_id),value + (F_AREA * 0), value)] #nog aan te passen door B&N
dt[,value := fifelse(grepl("EG20B",eco_id),value + (F_AREA * 0), value)] #nog aan te passen door B&N
dt[,value := fifelse(grepl("EG20C",eco_id),value + (F_AREA * 0), value)] #nog aan te passen door B&N

# EB18
dt[,value := fifelse(grepl("EB18",eco_id),value + (F_AREA * 300), value)]

# EB19
dt[,value := fifelse(grepl("EB19",eco_id),value + (F_AREA * 150), value)]



# calculate total farm area
dt1 <- dt[, F_AREA := sum(B_AREA)][id == "1",]

# add total farm area to dt
dt <- dt[, F_AREA := dt1$F_AREA]


# add scores from farm measures taken???

###-----------------------------------------------------------------###

# load, check and update the measures database
dt.measures <- bbwp_check_meas(measures,eco = FALSE,score = TRUE)

# select measures that apply on farm scale
dt.measures <- dt.measures[grepl("farm",level),]

# calculate the total score per indicator
if(nrow(dt.measures) > 0){
  
}


# calculate the change in opportunity indexes given the measures taken

# column names for impact of measures on the five indexes (do not change order)
mcols <- c('D_MEAS_NGW', 'D_MEAS_NSW', 'D_MEAS_PSW', 'D_MEAS_NUE', 'D_MEAS_WB', 'D_MEAS_TOT')

# calculate the score when no farm level measures are taken (no changes on farm score, so total field score is same as farm score)

# calculate the score when farm level measures are taken 

# filter/select measures that apply on farm scale (filter on farm level --> dt.measurefarm )

# check whether measures are taken on farm level (bijv nrow dt.measurefarm > 0)

# calculate the impact of farm level measures based on the valuation of measures in excel

# update the farm scores with farm measures

# Dummy dataset 
# dt1 <- data.table(B_LU_BBWP = c(1,4,8),
#                   eco_id = c("EG7","EG15","EG18"),
#                   BBWP_CAT = c("c3","c1","c2"), 
#                   B_LU_ECO1 = c("c10","c12","c14"), 
#                   B_LU_ECO2 = c("c11",NA,NA), 
#                   B_LU_ECO3 = c("c4",NA,NA), 
#                   er_soil= c(1,1,1),
#                   er_water= c(1,1,1),
#                   er_biodiversity= c(1,1,1),
#                   er_climate= c(1,1,1),
#                   er_landscape= c(1,1,1),
#                   er_euro_ha = c(1,1,1),
#                   sector= "dairy",
#                   B_SOILTYPE_AGR= "dekzand")
#----------------------------#

#measures on farm scale --> look whether action is needed (no farm/field dependency but still a -1 in the ER_puntenregeling table) 
# is needed, hier moet je per hectare uitrekenen hoeveel geld je krijgt --> dat is makkelijk als euro's zijn uitgedrukt in per hectare want dan doe je total_AREA maal bedrag dat erbij hoort. Maar wat doe je als euro/farm?  
#EG2 --> gaat over andere dingen 
#EG6 --> dit gaat over melk  
#EG11 --> dit gaat over type mest
#EG12 --> gaat over uitstoot op farm niveau 
#EG14 
#EB14 --> dit gaat over de OS balans
#EB18 --> dit gaat over gewasbeschermingsmiddelen 
#EB24 --> hiervoor moet percentage sloot bekend zijn

test <- dt[,c(1,12,13,14,54:75)]




# create empty list  

# create column with the 
for (i in 1:nrow(dt1) {
  
  # select measure 
  m1 <- dt1[i,c("eco_id","cumulatie_list_ER","acc.id")]
  
  # create column with all measures (that clash)
  acc <- unlist(m1$cumulatie_list_ER)
  acc <- c("EG10A","EG10B")
  d2 <- data.table(eco_id = c(m1$eco_id,acc),
                   acc.id = m1$acc.id)
  
  # merge with measures taken
  d3 <- merge(d2,dt.meas.taken)
  
  d3dummie <- data.table(eco_id= c("EG11B","EG10A","EG10B"),
                         er_soil= c(10,8,5),
                         er_water= c(2,6,9),
                         er_climate= c(9,10,2),
                         er_landscape= c(0,1,0),
                         er_biodiversity= c(2,0,10),
                         er_euro_ha = c(300,20,550),
                         er_euro_farm = c(0,0,0),
                         acc.id = 1)
  
  
  if(nrow(d3) >1){ 
    
    # select cols for score
    cols <- c('er_soil','er_water','er_biodiversity','er_climate','er_landscape','er_euro_ha', 'er_euro_farm')
    
    # get for each column the max score
    d4 <- d3dummie[, c(cols) := lapply(.SD,max), .SDcols = c(cols), by = acc.id]
    d5 <- d3dummie
    
  }else{
    
  }
  
}



# HIER HAD IK ER NOG MAAR 13 over:

#load crop table (under construction)
dt1<-  as.data.table(read_excel("dev/b_lu_brp_GR220707_decast_for_client.xlsx"))

# create new crop categories in new columns
dt1[,nc1:= fifelse(crop_cat1==1,1,0)]
dt1[,nc2:= fifelse(crop_cat2==1,1,0)]
dt1[,nc3:= fifelse(crop_cat3==1|c15==1|c27==1|c19==1,1,0)] #c19
dt1[,nc4:= fifelse(crop_cat4==1,1,0)]
dt1[,nc5:= fifelse(crop_cat5==1,1,0)]
dt1[,nc6:= fifelse(crop_cat6==1,1,0)]
dt1[,nc7:= fifelse(crop_cat7==1,1,0)]
dt1[,nc8 := fifelse(crop_cat8 == 1|c22==1|c25==1,1,0)]
dt1[,nc9:= fifelse(crop_cat9==1|c21==1|c20==1,1,0)] #c20
dt1[,tmp1:= nc1+nc2+nc3+nc4+nc5+nc6+nc7+nc8+nc9]
dt1[,nc10:= fifelse(tmp1== 0 & (c10==1|c28==1|c29==1|c26==1|c23==1),1,0)]
dt1[,nc11:= fifelse(tmp1== 0 & (c11==1|c16==1|c24==1),1,0)]
dt1[,nc12:= fifelse(tmp1== 0 & (c12==1|c18==1|c17==1),1,0)]
dt1[,nc13:= fifelse(crop_cat8==1|c10==1|c11==1|c15==1|c21==1|c17==1,1,0)]
dt1[,nc14:= fifelse(crop_cat4==1|crop_cat9==1|c16==1|c19==1,1,0)] #c19
dt1[,nc15:= fifelse(c20==1,1,0)] #c20
dt1[,nc16:= fifelse(c10==1|c12==1|c22==11|c24==1,1,0)]
dt1[,nc17:= fifelse(c18==1|c23==1,1,0)]
dt1[,nc18:= fifelse(c14==1|c25==1|c28==1,1,0)]
dt1[,nc19:= fifelse(c26==1|c27==1|c29==1|c13==1,1,0)]
dt1[,nc20:= fifelse(c17==1,1,0)]


# change format from wide to long
dt2 <- melt(dt1, 
            id.vars = c("B_LU_BRP","B_LU_NAME"),
            measure.vars = patterns("^nc"),
            variable.name = "BBWP_CAT",
            value.name = "boolean")

# keep only rows that indicate to which category the crop type belongs 
dt2 <- dt2[boolean == 1,]
dt2 <- dt2[,boolean := NULL]

# create data tables to separate BBWP_CAT into two columns of which the first (bcat) includes nc1-nc12 and the second (ecat) nc13-nc19
dt2[grepl("nc1$|nc2$|nc3|nc4|nc5|nc6|nc7|nc8|nc9|nc10|nc11|nc12",BBWP_CAT), type := "bbwp"]
dt2[grepl("nc13|nc14|nc15|nc16|nc17|nc18|nc19|nc20",BBWP_CAT), type := "eco"]

dt2[, test:= duplicated(B_LU_BRP)]
dt2[ test == T,][type=="bbwp",]



#--------------#

#load crop table (under construction)
dt1<-  as.data.table(read_excel("dev/b_lu_brp_GR220707_decast_for_client.xlsx"))

# create new crop categories in new columns
dt1[,nc1:= fifelse(crop_cat1==1,1,0)]
dt1[,nc2:= fifelse(crop_cat2==1,1,0)]
dt1[,nc3:= fifelse(crop_cat3==1|c15==1|c27==1|c19==1,1,0)] #c19
dt1[,nc4:= fifelse(crop_cat4==1,1,0)]
dt1[,nc5:= fifelse(crop_cat5==1,1,0)]
dt1[,nc6:= fifelse(crop_cat6==1,1,0)]
dt1[,nc7:= fifelse(crop_cat7==1,1,0)]
dt1[,nc8:= fifelse(crop_cat8 == 1|c22==1|c25==1,1,0)]
dt1[,nc9:= fifelse(crop_cat9==1|c21==1,1,0)] #c20
dt1[,tmp1:= nc1+nc2+nc3+nc4+nc5+nc6+nc7+nc8+nc9]
dt1[,nc10:= fifelse(tmp1== 0 & (c10==1|c28==1|c29==1|c26==1|c23==1|c20==1),1,0)]
dt1[,nc11:= fifelse(tmp1== 0 & (c11==1|c16==1|c13==1),1,0)]
dt1[,nc12:= fifelse(tmp1== 0 & c11==0 & (c12==1|c17==1),1,0)] #c11==0 toegevoegd
dt1[,nc13:= fifelse(crop_cat8==1|c10==1|c11==1|c15==1|c21==1|c17==1,1,0)]
dt1[,nc14:= fifelse(crop_cat4==1|crop_cat9==1|c16==1|c19==1,1,0)]
dt1[,nc15:= fifelse(c20==1|c24==1,1,0)] 
dt1[,nc16:= fifelse(c12==1|c22==11|c29==1,1,0)]
dt1[,nc17:= fifelse(c18==1|c23==1,1,0)]
dt1[,nc18:= fifelse(c14==1|c25==1|c28==1,1,0)]
dt1[,nc19:= fifelse(c26==1|c27==1|c29==1|c13==1,1,0)]
dt1[,nc20:= fifelse(c18==1,1,0)]
dt1[,nc21:= fifelse(c24==1,1,0)]

# change format from wide to long
dt2 <- melt(dt1, 
            id.vars = c("B_LU_BRP","B_LU_NAME"),
            measure.vars = patterns("^nc"),
            variable.name = "BBWP_CAT",
            value.name = "boolean")

# keep only rows that indicate to which category the crop type belongs 
dt2 <- dt2[boolean == 1,]
dt2 <- dt2[,boolean := NULL]

# create data tables to separate BBWP_CAT into two columns of which the first (bcat) includes nc1-nc12 and the second (ecat) nc13-nc19
dt2[grepl("nc1$|nc2$|nc3|nc4|nc5|nc6|nc7|nc8|nc9|nc10|nc11|nc12",BBWP_CAT), type := "bbwp"]
dt2[grepl("nc13|nc14|nc15|nc16|nc17|nc18|nc19",BBWP_CAT), type := "eco1"]
dt2[grepl("nc20|nc21",BBWP_CAT), type := "eco2"]


dt2[, test:= duplicated(B_LU_BRP)]
dt2[ test == T,]#[type=="bbwp",]

dt3 <- dcast(dt2, B_LU_BRP + B_LU_NAME ~ type, value.var = "BBWP_CAT", fun.aggregate = toString)  










# change format from wide to long
dt2 <- melt(dt1, 
            id.vars = c("B_LU_BRP","B_LU_NAME"),
            measure.vars = patterns("^nc"),
            variable.name = "BBWP_CAT",
            value.name = "boolean")

# keep only rows that indicate to which category the crop type belongs 
#dt2 <- dt2[boolean == 1,]
#dt2 <- dt2[,c("boolean","B_LU_NAME"):= NULL]
dt2 <- dt2[,"B_LU_NAME" := NULL]

# set type for bbwp categories and eco categories
#dt2[grepl("nc1$|nc2$|nc3|nc4|nc5|nc6|nc7|nc8|nc9|nc10|nc11|nc12",BBWP_CAT), type := "bbwp"]
#dt2[grepl("nc13|nc14|nc15|nc16|nc17|nc18|nc19|nc20",BBWP_CAT), type := "eco"]

# reshape to wide format to do check (may be removed later)
dt3 <- dcast(dt2, B_LU_BRP ~ BBWP_CAT, value.var = "boolean") 

# rename variables
setnames(dt2,'nc13','eco1')


dt2[grepl("nc13",BBWP_CAT), type := "eco1"]
dt2[grepl("nc14",BBWP_CAT), type := "eco2"]
dt2[grepl("nc15",BBWP_CAT), type := "eco3"]
dt2[grepl("nc16",BBWP_CAT), type := "eco4"]
dt2[grepl("nc17",BBWP_CAT), type := "eco5"]
dt2[grepl("nc18",BBWP_CAT), type := "eco6"]
dt2[grepl("nc19",BBWP_CAT), type := "eco7"]
dt2[grepl("nc20",BBWP_CAT), type := "eco8"]




# reformat crop table in order to use these categories to calculate scores based on crop rotation (and bypass the -1 in crop categories for farm measures) ##HIER MAAKT TESSA OOK EEN SCRIPT VOOR??

#load crop table (under construction)
dt1<-  as.data.table(read_excel("C:/Astrid/BBWP-ecoregeling bestanden/b_lu_brp_GR220623_decast_for_client.xlsx"))

# change format from wide to long
dt2 <- melt(dt1, 
            id.vars = c("B_LU_BRP","B_LU_NAME"),
            measure.vars = patterns("^c"),
            variable.name = "BBWP_CAT",
            value.name = "boolean")

# keep only rows that indicate to which category the crop type belongs 
dt2 <- dt2[boolean == 1,]
dt2 <- dt2[,boolean := NULL]

# change format back from long to wide 
dt2 <- dcast(dt2,
             B_LU_BRP ~ rowid(B_LU_BRP),
             value.var = "BBWP_CAT", sep = ".")

# create extra columns when crop belongs in multiple categories in order that each row has unique crop type
dt1 <- setDT(dt2, key = "B_LU_BRP")[dt1, B_LU_NAME := i.B_LU_NAME]
setnames(dt1,c("1","2","3"),c("BBWP_CAT","B_LU_ECO1","B_LU_ECO2"))
setcolorder(dt1,c("B_LU_BRP","B_LU_NAME","BBWP_CAT"))

# load crop list and select relevant columns 
dt3 <- er_crops[,c("er_description","b_lu_name") := NULL]

# merge measures table with crop categories 
dt3 <- merge(dt3,dt1, by.x= "b_lu_brp", by.y = "B_LU_BRP")

# select relevant columns and keep only unique combinations
dt3 <- dt3[, c("eco_id","BBWP_CAT","B_LU_ECO1","B_LU_ECO2")]
dt3 <- (unique(dt3))

# rearrange order of eco_id in a way that the same eco_id's are among each other
dt1 <- dt3[order(eco_id)]

# remove dt2 and dt3
rm(dt2,dt3)

# collect data in one data.table
dt <- data.table(id = 1:arg.length,
                 B_AER_CBS = B_AER_CBS,
                 B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                 B_LU_BRP = B_LU_BRP,
                 B_LU_BBWP = B_LU_BBWP,
                 B_AREA = B_AREA,
                 B_CT_SOIL = B_CT_SOIL, 
                 B_CT_WATER = B_CT_WATER,
                 B_CT_CLIMATE = B_CT_CLIMATE,
                 B_CT_BIO = B_CT_BIO,
                 B_CT_LANDSCAPE = B_CT_LANDSCAPE,
                 sector = sector
)

# merge the measures table with the table that indicates to which categories the measures apply
# prepare merge: add to each measure in dt1 A,B,C # DIT WERKT NOG NIET ZOALS HET ZOU MOETEN!!!!
dta <- copy(dt1)
dtb <- copy(dt1)
dtc <- copy(dt1)
dta <- dta[, eco_id := paste0(eco_id, "A")]
dtb <- dtb[, eco_id := paste0(eco_id, "B")]
dtc <- dtc[, eco_id := paste0(eco_id, "C")]
dt1 <- rbind(dta,dtb,dtc)
dt1[, BBWP_CAT := as.character(BBWP_CAT)]
dt1[, B_LU_ECO1 := as.character(B_LU_ECO1)]
dt1[, B_LU_ECO2 := as.character(B_LU_ECO2)]
dt1[, BBWP_CAT := fifelse(is.na(BBWP_CAT), "c0",BBWP_CAT)]
dt1[, B_LU_ECO1 := fifelse(is.na(B_LU_ECO1), "c0",B_LU_ECO1)]
dt1[, B_LU_ECO2 := fifelse(is.na(B_LU_ECO2), "c0",B_LU_ECO2)]
dt.meas.taken <- merge(dt.meas.taken, dt1, by= "eco_id", all.x=TRUE)
dt.meas.taken <- unique(dt.meas.taken, by = c("eco_id","id"))

# merge all measures to the given fields
dt <- merge(dt,dt.meas.taken, by='id', all=TRUE )

#dt[grepl("^EG2",eco_id) & BBWP_CAT != "c1|c2|c9|c10|c11|c12|c13|c14|c15|c16" & B_LU_ECO1 != "c1|c2|c9|c10|c11|c12|c13|c14|c15|c16" &  B_LU_ECO2 != "c1|c2|c9|c10|c11|c12|c13|c14|c15|c16", c(cols) := 0][,value := value + 0] 
dt[grepl("^EG2",eco_id) & BBWP_CAT == FALSE & B_LU_ECO1 == FALSE &  B_LU_ECO2 != "c1|c2|c9|c10|c11|c12|c13|c14|c15|c16", c(cols) := 0][,value := value + 0] 
dt[grepl("^EG4",eco_id) & BBWP_CAT != "c9" & B_LU_ECO1 != "c9" &  B_LU_ECO2 != "c9", c(cols) := 0][,value := value + 0]
dt[grepl("^EG7",eco_id) & BBWP_CAT != "c1|c2" & B_LU_ECO1 != "c1|c2" &  B_LU_ECO2 != "c1|c2", c(cols) := 0][,value := value + 0]
dt[grepl("^EG15",eco_id) & BBWP_CAT != "c10" & B_LU_ECO1 != "c10" &  B_LU_ECO2 != "c10", c(cols) := 0][,value := value + 0]
dt[grepl("^EG16",eco_id) & BBWP_CAT != "c1|c3" & B_LU_ECO1 != "c1|c3" &  B_LU_ECO2 != "c1|c3", c(cols) := 0][,value := value + 0]
dt[grepl("^EG17",eco_id) & BBWP_CAT != "c1|c3" & B_LU_ECO1 != "c1|c3" &  B_LU_ECO2 != "c1|c3", c(cols) := 0][,value := value + 0]
dt[grepl("^EG19",eco_id) & BBWP_CAT != "c9" & B_LU_ECO1 != "c9" &  B_LU_ECO2 != "c9", c(cols) := 0][,value := value + 0]
dt[grepl("^EG20",eco_id) & BBWP_CAT != "c8" & B_LU_ECO1 != "c8" &  B_LU_ECO2 != "c8", c(cols) := 0][,value := value + 0]
dt[grepl("^EG21",eco_id) & BBWP_CAT != "c6|c8" & B_LU_ECO1 != "c6|c8" &  B_LU_ECO2 != "c6|c8", c(cols) := 0][,value := value + 0]
dt[eco_id == "EB1A|EB1B|EB1C" & BBWP_CAT != "c2|c3|c5|c6|c12|c15|c16" & B_LU_ECO1 != "c2|c3|c5|c6|c12|c15|c16" &  B_LU_ECO2 != "c2|c3|c5|c6|c12|c15|c16", c(cols) := 0][,value := value + 0]
dt[eco_id == "EB2" & BBWP_CAT != "c5|c12|c16" & B_LU_ECO1 != "c5|c12|c16" &  B_LU_ECO2 != "c5|c12|c16", c(cols) := 0][,value := value + 0]
dt[grepl("^EB3",eco_id) & BBWP_CAT != "c1|c3|c12|c15|c16" & B_LU_ECO1 != "c1|c3|c12|c15|c16" &  B_LU_ECO2 != "c1|c3|c12|c15|c16", c(cols) := 0][,value := value + 0]
dt[grepl("^EB6",eco_id) & BBWP_CAT != "c2|c3|c5|c6|c11|c12|c16" & B_LU_ECO1 != "c2|c3|c5|c6|c11|c12|c16" &  B_LU_ECO2 != "c2|c3|c5|c6|c11|c12|c16", c(cols) := 0][,value := value + 0]
dt[grepl("^EB7",eco_id) & BBWP_CAT != "c4" & B_LU_ECO1 != "c4" &  B_LU_ECO2 != "c4", c(cols) := 0][,value := value + 0]
dt[grepl("^EB8",eco_id) & BBWP_CAT != "c2|c3|c4|c5|c6|c12|c15|c16" & B_LU_ECO1 != "c2|c3|c4|c5|c6|c12|c15|c16" &  B_LU_ECO2 != "c2|c3|c4|c5|c6|c12|c15|c16", c(cols) := 0][,value := value + 0]
dt[grepl("^EB9",eco_id) & BBWP_CAT != "c3|c15|c16" & B_LU_ECO1 != "c3|c15|c16" &  B_LU_ECO2 != "c3|c15|c16", c(cols) := 0][,value := value + 0]
dt[grepl("^EB11",eco_id) & BBWP_CAT != "c2|c3|c5|c6|c12|c16" & B_LU_ECO1 != "c2|c3|c5|c6|c12|c16" &  B_LU_ECO2 != "c2|c3|c5|c6|c12|c16", c(cols) := 0][,value := value + 0]
dt[grepl("^EB16",eco_id) & BBWP_CAT != "c10" & B_LU_ECO1 != "c10" &  B_LU_ECO2 != "c10", c(cols) := 0][,value := value + 0]
dt[grepl("^EB17",eco_id) & BBWP_CAT != "c2|c16" & B_LU_ECO1 != "c2|c16" &  B_LU_ECO2 != "c2|c16", c(cols) := 0][,value := value + 0]
dt[grepl("^EB23",eco_id) & BBWP_CAT != "c10" & B_LU_ECO1 != "c10" &  B_LU_ECO2 != "c10", c(cols) := 0][,value := value + 0]
#dt[grepl("^EG22",eco_id) & BBWP_CAT != "c8|c10" & B_LU_ECO1 != "c8|c10" &  B_LU_ECO2 != "c8|c10", c(cols) := 0][,value := value + 0]
# bbwp =1 , crop1 =0 --> 0  
#dt[grepl("^EG2",eco_id) & BBWP_CAT == "c1|c2|c9|c10|c11|c12|c13|c14|c15|c16" | B_LU_ECO1 != "c1|c2|c9|c10|c11|c12|c13|c14|c15|c16" |  B_LU_ECO2 != "c1|c2|c9|c10|c11|c12|c13|c14|c15|c16", c(cols) := lapply(.SD,function(x) x), .SDcols = cols][,value := value + 0] 

#dt[grepl("^EB17",eco_id) & BBWP_CAT == "c2|c16" | B_LU_ECO1 == "c2|c16" | B_LU_ECO2 == "c2|c16" & Bouwland == 1, c(cols) := c(cols)][,value := value + er_euro_ha]

#--------------#

# Update BBWP & ECO crop categories by Astrid Berndsen on 220712

# load in csv with crop list
er_crops <- as.data.table(read_excel("dev/b_lu_brp_GR220707_decast_for_client.xlsx"))

# transform old crop categories (c1-c29) and add ecoregelingen, results in the following 20 categories:
# cat1 = permanent grasland
# cat2 = tijdelijk grasland, zoden graszaad etc
# cat3 = rustgewassen (niet grassen)
# cat4 = rooivruchten
# cat5 = groenten
# cat6 = bloembollen en sierteelt
# cat7 = boomteelt, fruit etc
# cat8 = natuur
# cat9 = mais
# cat10 = kruidenrijke rand
# cat11 = vanggewas
# cat12 = eiwitgewas
# eco1 includes: natuur; (kruidenrijke) rand; vanggewas; wortelspruit gewas; rooivruchten (voorjaar); maiskolvenschroot;
# eco2 includes: rooivruchten (najaar); mais; groenbemesters; sloten langs grasland; bufferstrook langs bouwland;
# eco3 includes: sloten langs grasland of bouwland; groenebraak;
# eco4 includes: eiwitgewas; heg,haag,struweel; akkerranden,keverbanken;
# eco5 includes: rustgewassen (niet grassen); voedergewas; overig hout;
# eco6 includes: meerjarig gewas; riet,poelen;
# eco7 includes: diepwortelend; natte teelten; granen;
# eco8 includes: voedergewas; groene braak;
er_crops[,nc1:= fifelse(crop_cat1==1,1,0)]
er_crops[,nc2:= fifelse(crop_cat2==1,1,0)]
er_crops[,nc3:= fifelse(crop_cat3==1|c15==1|c27==1|c19==1,1,0)]
er_crops[,nc4:= fifelse(crop_cat4==1,1,0)]
er_crops[,nc5:= fifelse(crop_cat5==1,1,0)]
er_crops[,nc6:= fifelse(crop_cat6==1,1,0)]
er_crops[,nc7:= fifelse(crop_cat7==1,1,0)]
er_crops[,nc8:= fifelse(crop_cat8 == 1|c22==1|c25==1,1,0)]
er_crops[,nc9:= fifelse(crop_cat9==1|c21==1,1,0)] #c20
er_crops[,tmp1:= nc1+nc2+nc3+nc4+nc5+nc6+nc7+nc8+nc9]
er_crops[,nc10:= fifelse(tmp1== 0 & (c10==1|c28==1|c29==1|c26==1|c23==1|c20==1),1,0)]
er_crops[,nc11:= fifelse(tmp1== 0 & (c11==1|c16==1|c13==1),1,0)]
er_crops[,nc12:= fifelse(tmp1== 0 & c11==0 & (c12==1|c17==1),1,0)]
er_crops[,B_LU_ECO1:= fifelse(crop_cat8==1|c10==1|c11==1|c15==1|c21==1|c17==1,1,0)]
er_crops[,B_LU_ECO2:= fifelse(crop_cat4==1|crop_cat9==1|c16==1|c19==1,1,0)]
er_crops[,B_LU_ECO3:= fifelse(c20==1|c24==1,1,0)]
er_crops[,B_LU_ECO4:= fifelse(c12==1|c22==11|c29==1,1,0)]
er_crops[,B_LU_ECO5:= fifelse(c18==1|c23==1,1,0)]
er_crops[,B_LU_ECO6:= fifelse(c14==1|c25==1|c28==1,1,0)]
er_crops[,B_LU_ECO7:= fifelse(c26==1|c27==1|c29==1|c13==1,1,0)]
er_crops[,B_LU_ECO8:= fifelse(c18==1|c24==1,1,0)]

#keep relevant columns and remove rows without B_LU_BRP code
er_crops[,c(3:31,35,45):= NULL]
er_crops <- er_crops[complete.cases(B_LU_BRP),]

# rename cols
setnames(er_crops,c("nc1","nc2","nc3","nc4","nc5","nc6","nc7","nc8","nc9","nc10","nc11","nc12"),c("cat1","cat2","cat3","cat4","cat5","cat6","cat7","cat8","cat9","cat10","cat11","cat12"))

# each BBWP category in one column
er_crops[cat1==1, B_LU_BBWP := "cat_1"]
er_crops[cat2==1, B_LU_BBWP := "cat_2"]
er_crops[cat3==1, B_LU_BBWP := "cat_3"]
er_crops[cat4==1, B_LU_BBWP := "cat_4"]
er_crops[cat5==1, B_LU_BBWP := "cat_5"]
er_crops[cat6==1, B_LU_BBWP := "cat_6"]
er_crops[cat7==1, B_LU_BBWP := "cat_7"]
er_crops[cat8==1, B_LU_BBWP := "cat_8"]
er_crops[cat9==1, B_LU_BBWP := "cat_9"]
er_crops[cat10==1, B_LU_BBWP := "cat_10"]
er_crops[cat11==1, B_LU_BBWP := "cat_11"]
er_crops[cat12==1, B_LU_BBWP := "cat_12"]

# keep column with data on crop categories
er_crops <- er_crops[, c(1,18:26)]

# load b_lu_brp pandex table and remove old B_LU_BBWP column
load("data/b_lu_brp.rda")
b_lu_brp[,B_LU_BBWP := NULL]

# merge bbwp category column and eco category columns into the b_lu_brp table
merge(b_lu_brp,er_crops,by = "B_LU_BRP")

#overwrite old table
# save measures as bbwp table
use_data(b_lu_brp, overwrite = TRUE)

#---------------#


# load in csv with crop list
er_crops <- as.data.table(read_excel("dev/b_lu_brp_GR220707_decast_for_client.xlsx"))

# transform old cropcategories in new categories
# eco1 includes: natuur; (kruidenrijke) rand; vanggewas; wortelspruit gewas; rooivruchten (voorjaar); maiskolvenschroot;
# eco2 includes: rooivruchten (najaar); mais; groenbemesters; sloten langs grasland; bufferstrook langs bouwland;
# eco3 includes: sloten langs grasland of bouwland; groenebraak;
# eco4 includes: eiwitgewas; heg,haag,struweel; akkerranden,keverbanken;
# eco5 includes: rustgewassen (niet grassen); voedergewas; overig hout;
# eco6 includes: meerjarig gewas; riet,poelen;
# eco7 includes: diepwortelend; natte teelten; granen;
# eco8 includes: voedergewas; groene braak;
  er_crops[,nc1:= fifelse(crop_cat1==1,1,0)]
  er_crops[,nc2:= fifelse(crop_cat2==1,1,0)]
  er_crops[,nc3:= fifelse(crop_cat3==1,1,0)]
  er_crops[,nc4:= fifelse(crop_cat4==1,1,0)]
  er_crops[,nc5:= fifelse(crop_cat5==1,1,0)]
  er_crops[,nc6:= fifelse(crop_cat6==1,1,0)]
  er_crops[,nc7:= fifelse(crop_cat7==1,1,0)]
  er_crops[,nc8:= fifelse(crop_cat8==1,1,0)] 
  er_crops[,nc9:= fifelse(crop_cat9==1,1,0)] 
  er_crops[,tmp1:= nc1+nc2+nc3+nc4+nc5+nc6+nc7+nc8+nc9]
  er_crops[,nc3:= fifelse(tmp1==0 & (c15==1|c27==1|c14==1),1,0)]
  er_crops[,nc4:= fifelse(tmp1==0 & (c17==1),1,0)]
  er_crops[,nc8:= fifelse(tmp1==0 & (c22==1|c25==1|c24==1|c23==1|c26==1),1,0)]
  er_crops[,nc9:= fifelse(tmp1==0 & (c21==1),1,0)]
  er_crops[,tmp2:= nc1+nc2+nc3+nc4+nc5+nc6+nc7+nc8+nc9]
  er_crops[,nc10:= fifelse(tmp1==0 & tmp2==0 & (c10==1|c28==1|c29==1|c20==1|c19==1),1,0)]
  er_crops[,nc11:= fifelse(tmp1==0 & tmp2==0 & (c11==1|c16==1|c13==1),1,0)]
  er_crops[,nc12:= fifelse(tmp1==0 & tmp2==0 & nc11==0 & (c12==1|c18==1),1,0)]
  er_crops[,nc13:= fifelse(crop_cat8==1|c10==1|c11==1|c15==1|c21==1|c17==1,1,0)]
  er_crops[,nc14:= fifelse(crop_cat4==1|crop_cat9==1|c16==1|c19==1,1,0)]
  er_crops[,nc15:= fifelse(c20==1|c24==1,1,0)] 
  er_crops[,nc16:= fifelse(c12==1|c22==11|c29==1,1,0)]
  er_crops[,nc17:= fifelse(c23==1|c18==1,1,0)] 
  er_crops[,nc18:= fifelse(c14==1|c25==1|c28==1,1,0)]
  er_crops[,nc19:= fifelse(c26==1|c27==1|c29==1|c13==1,1,0)]
  #er_crops[,nc20:= fifelse(c18==1|c24==1,1,0)]

  #c18, c24, c14 voedergewas wordt eiwitgewas
  
  # als crop_cat3 ongelijk is aan 
  

# change format from wide to long
dt2 <- melt(er_crops, 
            id.vars = c("B_LU_BRP","B_LU_NAME"),
            measure.vars = patterns("^nc"),
            variable.name = "BBWP_CAT",
            value.name = "boolean")

# keep only rows that indicate to which category the crop type belongs 
dt2 <- dt2[boolean == 1,]
dt2 <- dt2[,boolean := NULL]

# create data tables to separate BBWP_CAT into two columns of which the first (bcat) includes nc1-nc12 and the second (ecat) nc13-nc19
dt2[grepl("nc1$|nc2$|nc3|nc4|nc5|nc6|nc7|nc8|nc9|nc10|nc11|nc12",BBWP_CAT), type := "bbwp"]
dt2[grepl("nc13|nc14|nc15|nc16|nc17|nc18|nc19",BBWP_CAT), type := "eco1"]

dt2[, test:= duplicated(B_LU_BRP)]
dt2[ test == T,][type=="bbwp",]



# als van de maatregelen die worden ingestuurd de ACC_ER op 1 staat, maak dan een tabel met de maatregel in de rij en de maatregelen die in de list ACC_ER_LIST staan met bijbehorende scores
# selecteer dan uit die tabel de hoogste score en de hoogste reward en voeg die toe bij de betreffende maatregel
# zorg ervoor dat als een van deze maatregelen nog een keer komt dat die dan niet nogmaals wordt gerekend

# filter the measures that accumulate and assign to each accumulation a unique ID number in order to do calculations per group
# 
#     dt1 <- dt[Accumulatie_ER_met_ER == 1,]
#     dt1 <- dt1[, acc.id := .I]
#   
#     # if there is an accumulation then do the following calculations
#     if(nrow(dt1)>0){
#    
#     #unlist cumulatie_list_ER, keep acc.id column
#     dt2 = dt1[ , .(acc.id = rep(acc.id, lengths(cumulatie_list_ER)), cumulatie_list_ER = unlist(cumulatie_list_ER))]
#     # eigenlijk krijg je dan:
#     dt2 = data.table(acc.id = c(1,2,2),
#                      cumulatie_list_ER = c("EG1D","EG3A","EG10B"))
#     
#     #merge 
#     dt2 = dt2[dt1[ , !'cumulatie_list_ER'], on = 'acc.id']  
#       
#     # als cumulatie_list_E voorkomt in eco_id kolom: dan maximum nemen van beide rijen
#     cols <- c('er_soil','er_water','er_biodiversity','er_climate','er_landscape','er_euro_ha', 'er_euro_farm')
#     test <- dt2[eco_id %in% cumulatie_list_ER & cumulatie_list_ER %in% eco_id, c(cols) := lapply(.SD,max), .SDcols = c(cols)]
#     
#     }
# 
#   # reset score when ECO measure clashes with ANLb   
#     
#     


