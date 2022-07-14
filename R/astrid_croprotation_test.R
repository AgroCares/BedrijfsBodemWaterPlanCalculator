#' Calculate the crop rotation based total score for five opportunity indicators
#'
#' Estimate the actual contribution of crop rotation given aims for soil quality, water quality, climate, biodiversity and landscape
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BRP (integer)
#' @param B_LU_BBWP (numeric) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param B_CT_SOIL (numeric) the target value for soil quality conform Ecoregeling scoring
#' @param B_CT_WATER (numeric) the target value for water quality conform Ecoregeling scoring
#' @param B_CT_CLIMATE (numeric) the target value for climate conform Ecoregeling scoring
#' @param B_CT_BIO (numeric) the target value for biodiversity conform Ecoregeling scoring
#' @param B_CT_LANDSCAPE (numeric) the target value for landscape quality conform Ecoregeling scoring
#' @param B_AREA (numeric) the area of the field (m2) 
#' @param measures (list) the measures planned /done (measure nr)
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'melkveehouderij','akkerbouw','vollegrondsgroente','boomteelt','bollen','veehouderij','overig')
#'    
#' @import data.table
#' @import stats
#' @import readxl
#'
#' @export
# calculate the opportunities for a set of fields
er_croprotation <- function(B_SOILTYPE_AGR, B_LU_BRP, B_LU_BBWP,B_AER_CBS,B_AREA,
                            B_CT_SOIL, B_CT_WATER,B_CT_CLIMATE,B_CT_BIO,B_CT_LANDSCAPE,
                            measures, sector){
  
  # add visual bindings
  . = eco_id = farmid = b_lu_brp = type = erscore = B_AREA_RR = indicator = NULL
  EG15 = EG22 = cf = EB1A = EB1B = EB1C = EB2 = EB3 = EB8 = EB9 = NULL
  urgency = soiltype = NULL
  er_profit = statcode = er_cf = id = value = reward = NULL
  
  #----input test data-------#
  
  # B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen')
  # B_LU_BRP = c(265,265,265,265)
  # B_LU_BBWP = c(1,1,1,1)
  # B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland')
  # B_AREA = c(45,18,0.8,6)
  # B_CT_SOIL = 20
  # B_CT_WATER = 15
  # B_CT_CLIMATE = 8
  # B_CT_BIO = 24
  # B_CT_LANDSCAPE = 20
  # measures = NULL
  # sector = 'dairy'
  # 
  # # IF MEASURES ARE TAKEN
  # # get internal table with measures
  # dt.measures <- as.data.table(BBWPC::bbwp_measures)
  # dt.measures <- dt.measures[!is.na(eco_id)]
  # # make measurement list for 2 of the 4 fields
  # measures <- rbind(data.table(id = 1, dt.measures[c(2,5,18,28,32,3,38,43,62)]),
  #                  data.table(id = 3, dt.measures[c(7,21,30,46,5)]))
  # setnames(measures,"er_profit","er_euro_ha", skip_absent = TRUE)

  #------------------#
  
  # check inputs
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = c('duinzand','dekzand','zeeklei','rivierklei','maasklei',
                                                       'dalgrond','moerige_klei','veen','loess'))
  checkmate::assert_integerish(B_LU_BRP, lower = 0, len = arg.length)
  checkmate::assert_integerish(B_LU_BBWP, lower = 0, len = arg.length)
  checkmate::assert_numeric(B_CT_SOIL, lower = 0, upper = 5000, min.len = 1)
  checkmate::assert_numeric(B_CT_WATER, lower = 0, upper = 5000, min.len = 1)
  checkmate::assert_numeric(B_CT_CLIMATE, lower = 0, upper = 5000,min.len = 1)
  checkmate::assert_numeric(B_CT_BIO, lower = 0, upper = 5000, min.len = 1)
  checkmate::assert_numeric(B_CT_LANDSCAPE, lower = 0, upper = 5000,min.len = 1)

  
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
  
  # add bbwp table for financial reward correction factor per AER
  dt.er.reward <- as.data.table(BBWPC::er_aer_reward)
  
  # merge with regional correction factor for the financial reward
  dt <- merge(dt,dt.er.reward[,.(statcode,er_cf)], by.x = 'B_AER_CBS',by.y = 'statcode')

  # set scores to zero when measures are not applicable given the crop type
  
    # add baseline profit
    dt[is.na(dt)] <- NA_real_
    dt[, value := 0]
  
    # columns with the Ecoregelingen ranks and reward
    cols <- c('er_soil','er_water','er_biodiversity','er_climate','er_landscape','er_euro_ha')
    
    # set first all missing data impacts to 0
    dt[,c(cols) := lapply(.SD, function(x) fifelse(is.na(x),0,x)), .SDcols = cols]
  
    # set the score and profit to zero for the measures when not applicable for given crop category
    dt[B_LU_BBWP == 1 & crop_cat1 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 2 & crop_cat2 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 3 & crop_cat3 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 4 & crop_cat4 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 5 & crop_cat5 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 6 & crop_cat6 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 7 & crop_cat7 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 8 & crop_cat8 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 9 & crop_cat9 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 10 & crop_cat10 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 11 & crop_cat11 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 12 & crop_cat12 <= 0, c(cols) := 0]
    dt[B_LU_ECO1 == 1 & eco1 <= 0, c(cols) := 0]
    dt[B_LU_ECO2 == 1 & eco2 <= 0, c(cols) := 0]
    dt[B_LU_ECO3 == 1 & eco3 <= 0, c(cols) := 0]
    dt[B_LU_ECO4 == 1 & eco4 <= 0, c(cols) := 0]
    dt[B_LU_ECO5 == 1 & eco5 <= 0, c(cols) := 0]
    dt[B_LU_ECO6 == 1 & eco6 <= 0, c(cols) := 0]
    dt[B_LU_ECO7 == 1 & eco7 <= 0, c(cols) := 0]

  # set score to zero when measures is not applicable given Bouwland, Productief or Beteelbaar 
    
    # get internal table of BRP codes with data on Bouwland, Productief or Beteelbaar and rename these cols as preparation on merge
    dt.er.crops <- as.data.table(BBWPC::er_crops)
    setnames(dt.er.crops,c("bouwland","productive","Beteelbaar"),c("is_bouwland","is_productief","is_beteelbaar"))
    
    # merge dt with selected columns from dt.er.crops
    dt <- merge(dt,dt.er.crops[,c("b_lu_brp","is_bouwland","is_productief","is_beteelbaar")], by.x = "B_LU_BRP", by.y = "b_lu_brp", all.x = TRUE)
    dt[is_bouwland == 1 & Bouwland <= 0, c(cols):= 0]
    dt[is_productief == 1 & Productief <= 0, c(cols):= 0]
    dt[is_beteelbaar == 1 & Beteelbaar <= 0, c(cols):= 0]

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
    
  # multiply by (political) urgency
    
    # first add soil type for political and environmental urgency
    dt[grepl('klei', B_SOILTYPE_AGR) , soiltype := 'klei']
    dt[grepl('zand|dal', B_SOILTYPE_AGR), soiltype := 'zand']
    dt[grepl('veen', B_SOILTYPE_AGR), soiltype := 'veen']
    dt[grepl('loess', B_SOILTYPE_AGR), soiltype := 'loess']
    
    # melt dt
    dt <- melt(dt,
               id.vars = c('id','bbwp_id','eco_id','soiltype'),  #'bbwp_conflict'),
               measure = patterns(erscore = "^er_"),
               variable.name = 'indicator',
               value.name = 'value')
    dt[,indicator := gsub('er_', '',cols[indicator])]
    
    # merge with urgency table
    dt <- merge(dt,dt.er.urgency, by= c('soiltype','indicator'),all.x = TRUE)
    
    # adapt the score based on urgency
    dt[indicator != 'euro_ha', value := value * urgency]

    # dcast
    dt <- dcast(dt, id + soiltype + bbwp_id + eco_id  ~ indicator, value.var = 'value') #+ bbwp_conflict
    cols <- c('biodiversity', 'climate', 'landscape', 'soil','water','euro_ha')
    dt <- dt[,mget(c('id',cols))]
    
    # sum measures score per field
    dt <- dt[, lapply(.SD, sum), by = "id"]

    # calculate the weighed average ER score (points/ ha) for the whole farm due to measures taken
    dt <- dt[,lapply(.SD,sum), .SDcols = cols, by = 'id']

    # order to ensure field order
    setorder(dt, id) 

    # return the Ecoregelingen Score based on Crop Rotation Only
    return(dt)
}
  
  