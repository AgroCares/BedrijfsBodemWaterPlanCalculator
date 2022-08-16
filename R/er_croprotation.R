#' Calculate the crop rotation based total score for five opportunity indicators
#'
#' Estimate the actual contribution of crop rotation given aims for soil quality, water quality, climate, biodiversity and landscape
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BBWP (character) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_LU_ARABLE_ER (boolean) does the crop fall within the ER category "arable"
#' @param B_LU_PRODUCTIVE_ER (boolean) does the crop fall within the ER category "productive"
#' @param B_LU_CULTIVATED_ER (boolean) does the crop fall within the ER category "cultivated"
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param B_CT_SOIL (numeric) the target value for soil quality conform Ecoregeling scoring  (score / ha)
#' @param B_CT_WATER (numeric) the target value for water quality conform Ecoregeling scoring (score / ha)
#' @param B_CT_CLIMATE (numeric) the target value for climate conform Ecoregeling scoring (score / ha)
#' @param B_CT_BIO (numeric) the target value for biodiversity conform Ecoregeling scoring (score / ha)
#' @param B_CT_LANDSCAPE (numeric) the target value for landscape quality conform Ecoregeling scoring (score / ha)
#' @param B_AREA (numeric) the area of the field (m2) 
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'dairy', 'arable', 'tree_nursery', 'bulbs')
#' @param measures (list) The measures planned / done per fields
#'    
#' @import data.table
#' @import stats
#'
#' @export
# calculate the opportunities for a set of fields
er_croprotation <- function(B_SOILTYPE_AGR, B_AER_CBS,B_AREA,
                            B_LU_BBWP,B_LU_BRP, 
                            B_LU_ARABLE_ER, B_LU_PRODUCTIVE_ER,B_LU_CULTIVATED_ER,
                            B_CT_SOIL, B_CT_WATER,B_CT_CLIMATE,B_CT_BIO,B_CT_LANDSCAPE,
                            measures, sector){
  
  # add visual bindings
  . = eco_id = farmid = b_lu_brp = type = erscore = B_AREA_RR = indicator = NULL
  urgency = soiltype = er_profit = statcode = er_cf = id = value = reward = ec1 = ec2 = NULL
  level = nc1 = nc2 = nc3 = nc4 = nc5 = nc6 = nc7 = nc8 = nc9 = nc10 = nc11 = nc12 = NULL
  fsector = fdairy = dairy = farable = arable = ftree_nursery = tree_nursery = patterns = fbulbs = bulbs = NULL
  clay = sand = silt = peat = bbwp_id = B_AREA_REL = S_ER_REWARD = euro_farm = NULL
  fr_soil = er_reward = fr_soil = reward_cf = regio_factor = euro_ha = oid = water = soil = climate = biodiversity = landscape = climate = total = NULL
  er_total = er_climate = er_soil = er_water = er_landscape = er_biodiversity = NULL
  eco_app = b_lu_arable_er = b_lu_productive_er = b_lu_cultivated_er = NULL
  
  # reformat B_AER_CBS
  B_AER_CBS <- bbwp_format_aer(B_AER_CBS)
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(B_LU_BBWP),length(B_LU_BRP),length(B_AER_CBS),
                    length(B_LU_ARABLE_ER),length(B_LU_PRODUCTIVE_ER),length(B_LU_CULTIVATED_ER))
  
  # check inputs
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = c('duinzand','dekzand','zeeklei','rivierklei','maasklei',
                                                       'dalgrond','moerige_klei','veen','loess'))
  checkmate::assert_subset(B_LU_BBWP,
                           choices = c('groenten','bollensierteelt','boomfruitteelt','rustgewas','eiwitgewas',
                                       'rooivrucht','mais','gras_permanent','gras_tijdelijk','natuur',
                                       'randensloot','vanggewas'))
  checkmate::assert_character(B_LU_BBWP, len = arg.length)
  checkmate::assert_integerish(B_LU_BRP, len = arg.length)
  checkmate::assert_numeric(B_CT_SOIL, lower = 0, upper = 1000, min.len = 1)
  checkmate::assert_numeric(B_CT_WATER, lower = 0, upper = 1000, min.len = 1)
  checkmate::assert_numeric(B_CT_CLIMATE, lower = 0, upper = 1000,min.len = 1)
  checkmate::assert_numeric(B_CT_BIO, lower = 0, upper = 1000, min.len = 1)
  checkmate::assert_numeric(B_CT_LANDSCAPE, lower = 0, upper = 1000,min.len = 1)
  checkmate::assert_logical(B_LU_ARABLE_ER,len = arg.length)
  checkmate::assert_logical(B_LU_PRODUCTIVE_ER,len = arg.length)
  checkmate::assert_logical(B_LU_CULTIVATED_ER,len = arg.length)
  
  # check and update the measure table
  dt.meas.farm <- bbwp_check_meas(dt = measures, eco = TRUE, score = TRUE)
  dt.meas.field <- bbwp_check_meas(dt = NULL, eco = TRUE, score = FALSE)
  dt.meas.eco <- as.data.table(BBWPC::er_measures)
    
  # subset both measurement tables
  dt.meas.field <- dt.meas.field[grepl('EB1$|EB2$|EB3$|EB8|EB9',eco_id) & level == 'field',]
  dt.meas.farm <- dt.meas.farm[level == 'farm']
  
  # add bbwp table for financial reward correction factor per AER
  dt.er.reward <- as.data.table(BBWPC::er_aer_reward)
  
  # get internal table with importance of environmental challenges
  dt.er.scoring <- as.data.table(BBWPC::er_scoring)
  setnames(dt.er.scoring,gsub('cf_','',colnames(dt.er.scoring)))
  dt.er.urgency <- melt(dt.er.scoring[type=='urgency'],
                        id.vars='soiltype',
                        measure.vars = c('soil', 'water', 'climate',  'biodiversity', 'landscape'),
                        variable.name = 'indicator',
                        value.name = 'urgency')
  
  # collect total areas on farm level
  dt.farm <- data.table(area_farm = sum(B_AREA),
                        area_arable = sum(B_AREA * B_LU_ARABLE_ER),
                        area_productive = sum(B_AREA * B_LU_PRODUCTIVE_ER),
                        area_cultivated = sum(B_AREA * B_LU_CULTIVATED_ER))
  
  # collect data in one data.table
  dt <- data.table(id = 1:arg.length,
                   B_AER_CBS = B_AER_CBS,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_LU_BBWP = B_LU_BBWP,
                   B_LU_BRP = B_LU_BRP,
                   B_LU_ARABLE_ER = B_LU_ARABLE_ER, 
                   B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                   B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                   B_AREA = B_AREA,
                   B_CT_SOIL = B_CT_SOIL, 
                   B_CT_WATER = B_CT_WATER,
                   B_CT_CLIMATE = B_CT_CLIMATE,
                   B_CT_BIO = B_CT_BIO,
                   B_CT_LANDSCAPE = B_CT_LANDSCAPE)
  
  # add regional correction value for price
  dt <- merge(dt,dt.er.reward[,.(statcode,reward_cf = er_cf)], 
              by.x = 'B_AER_CBS',by.y = 'statcode',all.x = TRUE)
  
  # add soil type for political and environmental urgency
  dt[grepl('klei', B_SOILTYPE_AGR) , soiltype := 'klei']
  dt[grepl('zand|dal', B_SOILTYPE_AGR), soiltype := 'zand']
  dt[grepl('veen', B_SOILTYPE_AGR), soiltype := 'veen']
  dt[grepl('loess', B_SOILTYPE_AGR), soiltype := 'loess']
  
  # merge with the measures taken
  dt.field <- dt[,as.list(dt.meas.field),by=names(dt)]
  
  # merge with the Ecoregeling ~ Measure list to evaluate applicability
  dt.field <- merge(dt.field,
                    dt.meas.eco, 
                    by = c('B_LU_BRP','eco_id'),
                    all.x = TRUE)
  dt.field[is.na(eco_app),eco_app := 0]
  
    # columns with the Ecoregelingen ranks and reward
    cols <- c('er_soil','er_water','er_biodiversity','er_climate','er_landscape','er_euro_ha', 'er_euro_farm')
     
    # set first all missing data impacts to 0
    dt.field[,c(cols) := lapply(.SD, function(x) fifelse(is.na(x),0,x)), .SDcols = cols]
  
    # set the score to zero when not applicable for given BBWP category
    dt.field[B_LU_BBWP == 'gras_permanent' & nc1 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'gras_tijdelijk' & nc2 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'rustgewas' & nc3 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'rooivrucht' & nc4 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'groenten' & nc5 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'bollensierteelt' & nc6 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'boomfruitteelt' & nc7 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'natuur' & nc8 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'mais' & nc9 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'randensloot' & nc10 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'vanggewas' & nc11 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'eiwitgewas' & nc12 == 0, c(cols) := 0]
    
    # set the score to zero when not applicable for a given ER combined category
    dt.field[eco_app == 0, c(cols) := 0]
   
    # set measures not applicable on arable, cultivated or productive land
    dt.field[B_LU_ARABLE_ER  == TRUE & b_lu_arable_er  == 0, c(cols) := 0]
    dt.field[B_LU_PRODUCTIVE_ER == TRUE & b_lu_productive_er == 0, c(cols) := 0]
    dt.field[B_LU_CULTIVATED_ER  == TRUE & b_lu_cultivated_er == 0, c(cols) := 0]
    dt[B_LU_ARABLE_ER  == FALSE & b_lu_arable_er  == 1, c(cols) := 0]
    dt[B_LU_PRODUCTIVE_ER == FALSE & b_lu_productive_er == 1, c(cols) := 0]
    dt[B_LU_CULTIVATED_ER  == FALSE & b_lu_cultivated_er == 1, c(cols) := 0]
    
    # add columns for the sector to which the farms belong
    fs0 <- c('fdairy','farable','ftree_nursery','fbulbs')
    fs1 <- paste0('f',sector)
    fs2 <- fs0[!fs0 %in% fs1]
    dt.field[,c(fs1) := 1]
    dt.field[,c(fs2) := 0]
    
    # estimate whether sector allows applicability
    dt.field[, fsector := fdairy * dairy + farable * arable + ftree_nursery * tree_nursery + fbulbs * bulbs] 
    
    # adapt the score when measure is not applicable
    dt.field[fsector == 0, c(cols) := 0]
    
    # adapt the score when the soil type limits the applicability of measures
    dt.field[grepl('klei', B_SOILTYPE_AGR) & clay == FALSE , c(cols) := 0]
    dt.field[grepl('zand|dal', B_SOILTYPE_AGR) & sand == FALSE , c(cols) := 0]
    dt.field[grepl('veen', B_SOILTYPE_AGR) & peat == FALSE , c(cols) := 0]
    dt.field[grepl('loess', B_SOILTYPE_AGR) & loess == FALSE , c(cols) := 0]
    
    # columns to be updated
    cols.sel <- c('er_climate','er_soil','er_water','er_landscape','er_biodiversity')
    
    # measure EB1. Cultivate rustgewas on a field
    cols.ad1 <- c(3,3,3,0,1)
    cols.ad2 <- c(4,4,4,1,1)
    dt.field[, er_total := er_climate + er_soil + er_water + er_landscape + er_biodiversity]
    dt.field[bbwp_id == 'G54' & er_total > 0, B_AREA_REL := sum(B_AREA) * 100 / dt.farm$area_arable]
    dt.field[bbwp_id == 'G54' & er_total > 0 & B_AREA_REL <= 20, c(cols.sel) := 0]
    dt.field[bbwp_id == 'G54' & er_total > 0 & B_AREA_REL > 35 & B_AREA_REL <= 50, c(cols.sel) := Map('+',mget(cols.sel),cols.ad1)]
    dt.field[bbwp_id == 'G54' & er_total > 0 & B_AREA_REL > 50, c(cols.sel) := Map('+',mget(cols.sel),cols.ad2)]
    
    # multiply by (political) urgency
    
      # melt dt
      dt.field <- melt(dt.field,
               id.vars = c('id','bbwp_id','soiltype','bbwp_conflict','reward_cf','regio_factor'),
               measure = patterns(erscore = "^er_"),
               variable.name = 'indicator',
               value.name = 'value')
      dt.field[,indicator := gsub('er_', '',indicator)]
    
    # merge with urgency table
    dt.field <- merge(dt.field,dt.er.urgency, by= c('soiltype','indicator'),all.x = TRUE)
    
    # adapt the score based on urgency
    dt.field[!grepl('euro',indicator), value := value * urgency]
    
    # add area
    dt.field <- merge(dt.field,dt[,.(id,B_AREA)],by='id')
    
    # dcast to add totals, to be used to update scores when measures are conflicting
    cols <- c('biodiversity', 'climate', 'landscape', 'soil','water','total')
    dt2 <- dcast(dt.field, id + soiltype + bbwp_id + bbwp_conflict + reward_cf + B_AREA + regio_factor ~ indicator, value.var = 'value')
    dt2[, total := biodiversity + climate + landscape + soil + water]
    dt2[, oid := frank(-total, ties.method = 'first',na.last = 'keep'),by = c('id','bbwp_conflict')]
    dt2[oid > 1, c(cols) := 0]
    
    # calculate the weighed average ER score (points/ ha) for the whole farm due to measures taken
    dt.field.score <- dt2[,lapply(.SD,function(x) sum(x*B_AREA)/dt.farm$area_farm), .SDcols = cols]
    dt.field.reward <- dt2[,list(er_reward = max(euro_ha[total>0],0),
                                 B_AREA = B_AREA[1],
                                 reward_cf = reward_cf[1],
                                 regio_factor = regio_factor[1]),by=id]
    dt.field.reward[regio_factor== 1, er_reward := er_reward * reward_cf]
    dt.field.reward <- dt.field.reward[,list(er_reward = weighted.mean(x = er_reward,w=B_AREA))]
    
    # calculate the total score for farm measures
    dt.farm.soiltype <- dt[,list(fr_soil = sum(B_AREA) / sum(dt$B_AREA)),by = soiltype]
    dt.farm.urgency <- merge(dt.er.urgency[soiltype %in% dt$soiltype], dt.farm.soiltype,by= 'soiltype')
    dt.farm.urgency <- dt.farm.urgency[,list(urgency = weighted.mean(x = urgency,w = fr_soil)),by = indicator]
    
    # add farm measures when present
    if(nrow(dt.meas.farm) > 0){
      
      # columns with the Ecoregelingen ranks and reward
      cols <- c('er_soil','er_water','er_biodiversity','er_climate','er_landscape','er_euro_ha', 'er_euro_farm')
      
      # set first all missing data impacts to 0
      dt.meas.farm[,c(cols) := lapply(.SD, function(x) fifelse(is.na(x),0,x)), .SDcols = cols]
      
      # add columns for the sector to which the farms belong
      fs0 <- c('fdairy','farable','ftree_nursery','fbulbs')
      fs1 <- paste0('f',sector)
      fs2 <- fs0[!fs0 %in% fs1]
      dt.meas.farm[,c(fs1) := 1]
      dt.meas.farm[,c(fs2) := 0]
      
      # estimate whether sector allows applicability
      dt.meas.farm[, fsector := fdairy * dairy + farable * arable + ftree_nursery * tree_nursery + fbulbs * bulbs] 
      
      # adapt the score when measure is not applicable
      dt.meas.farm[fsector == 0, c(cols) := 0]
      
      # multiply by (political) urgency
      dt3 <- melt(dt.meas.farm, 
                  id.vars = c('bbwp_id','bbwp_conflict'),
                  measure = patterns(erscore = "^er_"),
                  variable.name = 'indicator',
                  value.name = 'value')
      dt3[,indicator := gsub('er_', '',indicator)]
      dt3 <- merge(dt3,dt.farm.urgency[,.(indicator,urgency)],by= 'indicator',all.x = T)
      
      # adapt the score based on urgency
      dt3[!grepl('euro',indicator), value := value * urgency]
      
      # dcast to add totals, to be used to update scores when measures are conflicting
      cols <- c('biodiversity', 'climate', 'landscape', 'soil','water','total')
      dt4 <- dcast(dt3, bbwp_id + bbwp_conflict  ~ indicator, value.var = 'value')
      dt4[, total := biodiversity + climate + landscape + soil + water]
      dt4[, oid := frank(-total, ties.method = 'first',na.last = 'keep'),by = c('bbwp_conflict')]
      dt4[oid > 1, c(cols) := 0]
      
      # add correction reward
      cfr <- weighted.mean(x = dt$reward_cf, w = dt$B_AREA)
      
      # sum total score (score per hectare)
      dt.farm.score <- dt4[,lapply(.SD,sum), .SDcols = cols]
      dt.farm.reward <- dt4[,list(er_reward = fifelse(total>0, cfr * (max(euro_ha[total>0]) + max(euro_farm[total>0]) / dt.farm$area_farm), 0))]
      
    } else {
      
      dt.farm.score <- data.table(biodiversity = 0,
                                  climate = 0,
                                  landscape = 0,
                                  soil = 0,
                                  water = 0,
                                  total = 0)
      dt.farm.reward <- data.table(er_reward = 0)
    }
    
    
  # total score for farm measures and crop rotation
  out <- dt.farm.score + dt.field.score
  out[,farmid := 1]
  out[, total := biodiversity + climate + landscape + soil + water]
  out[,S_ER_REWARD := dt.farm.reward + dt.field.reward]
  setcolorder(out,'farmid')
  
  # return the Ecoregelingen Score based on Crop Rotation and Farm Measures
  # output has units score / ha and euro/ha
  return(out)
}
