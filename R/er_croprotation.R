#' Calculate the crop rotation based total score for five opportunity indicators
#'
#' Estimate the actual contribution of crop rotation given aims for soil quality, water quality, climate, biodiversity and landscape
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BBWP (numeric) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_LU_ECO1 (boolean) does the crop belong in Ecoregeling category 1
#' @param B_LU_ECO2 (boolean) does the crop belong in Ecoregeling category 2
#' @param B_LU_ECO3 (boolean) does the crop belong in Ecoregeling category 3
#' @param B_LU_ECO4 (boolean) does the crop belong in Ecoregeling category 4
#' @param B_LU_ECO5 (boolean) does the crop belong in Ecoregeling category 5
#' @param B_LU_ECO6 (boolean) does the crop belong in Ecoregeling category 6
#' @param B_LU_ECO7 (boolean) does the crop belong in Ecoregeling category 7
#' @param B_LU_ECO8 (boolean) does the crop fall within the category "arable"
#' @param B_LU_ECO9 (boolean) does the crop fall within the category "productive"
#' @param B_LU_ECO10 (boolean) does the crop fall within the category "cultivated"
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param B_CT_SOIL (numeric) the target value for soil quality conform Ecoregeling scoring
#' @param B_CT_WATER (numeric) the target value for water quality conform Ecoregeling scoring
#' @param B_CT_CLIMATE (numeric) the target value for climate conform Ecoregeling scoring
#' @param B_CT_BIO (numeric) the target value for biodiversity conform Ecoregeling scoring
#' @param B_CT_LANDSCAPE (numeric) the target value for landscape quality conform Ecoregeling scoring
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
                            B_LU_BBWP,B_LU_ECO1,B_LU_ECO2, B_LU_ECO3, B_LU_ECO4, B_LU_ECO5, 
                            B_LU_ECO6, B_LU_ECO7,B_LU_ECO8, B_LU_ECO9,B_LU_ECO10,
                            B_CT_SOIL, B_CT_WATER,B_CT_CLIMATE,B_CT_BIO,B_CT_LANDSCAPE,
                            measures, sector){
  
  # add visual bindings
  . = eco_id = farmid = b_lu_brp = type = erscore = B_AREA_RR = indicator = NULL
  EG15 = EG22 = cf = EB1A = EB1B = EB1C = EB2 = EB3 = EB8 = EB9 = NULL
  urgency = soiltype = NULL
  er_profit = statcode = er_cf = id = value = reward = NULL
  
  # reformat B_AER_CBS
  B_AER_CBS <- bbwp_format_aer(B_AER_CBS)
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(B_LU_BBWP),length(B_AER_CBS),
                    length(B_LU_ECO1),length(B_LU_ECO2),length(B_LU_ECO3),length(B_LU_ECO4),
                    length(B_LU_ECO5),length(B_LU_ECO6),length(B_LU_ECO7),length(B_LU_ECO8),
                    length(B_LU_ECO9),length(B_LU_ECO10))
  
  # check inputs
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = c('duinzand','dekzand','zeeklei','rivierklei','maasklei',
                                                       'dalgrond','moerige_klei','veen','loess'))
  checkmate::assert_integerish(B_LU_BBWP, lower = 0, len = arg.length)
  checkmate::assert_numeric(B_CT_SOIL, lower = 0, upper = 1000, min.len = 1)
  checkmate::assert_numeric(B_CT_WATER, lower = 0, upper = 1000, min.len = 1)
  checkmate::assert_numeric(B_CT_CLIMATE, lower = 0, upper = 1000,min.len = 1)
  checkmate::assert_numeric(B_CT_BIO, lower = 0, upper = 1000, min.len = 1)
  checkmate::assert_numeric(B_CT_LANDSCAPE, lower = 0, upper = 1000,min.len = 1)
  checkmate::assert_logical(B_LU_ECO1,len = arg.length)
  checkmate::assert_logical(B_LU_ECO2,len = arg.length)
  checkmate::assert_logical(B_LU_ECO3,len = arg.length)
  checkmate::assert_logical(B_LU_ECO4,len = arg.length)
  checkmate::assert_logical(B_LU_ECO5,len = arg.length)
  checkmate::assert_logical(B_LU_ECO6,len = arg.length)
  checkmate::assert_logical(B_LU_ECO7,len = arg.length)
  checkmate::assert_logical(B_LU_ECO8,len = arg.length)
  checkmate::assert_logical(B_LU_ECO9,len = arg.length)
  checkmate::assert_logical(B_LU_ECO10,len = arg.length)
  
  # check and update the measure table
  dt.meas.farm <- bbwp_check_meas(dt = measures, eco = TRUE, score = TRUE)
  dt.meas.field <- bbwp_check_meas(dt = NULL, eco = TRUE, score = FALSE)

  # subset both measurement tables
  dt.meas.field <- dt.meas.field[grepl('EB1$|EB2$|EB3$|EB8|EB9',eco_id) & level == 'field',]
  dt.meas.farm <- dt.meas.farm[level == 'farm']
  
  # collect total areas on farm level
  dt.farm <- data.table(area_farm = sum(B_AREA),
                        area_arable = sum(B_AREA * B_LU_ECO8),
                        area_productive = sum(B_AREA * B_LU_ECO9),
                        area_cultivated = sum(B_AREA * B_LU_ECO10))
  
  # collect data in one data.table
  dt <- data.table(id = 1:arg.length,
                   B_AER_CBS = B_AER_CBS,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
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
                   B_AREA = B_AREA,
                   B_CT_SOIL = B_CT_SOIL, 
                   B_CT_WATER = B_CT_WATER,
                   B_CT_CLIMATE = B_CT_CLIMATE,
                   B_CT_BIO = B_CT_BIO,
                   B_CT_LANDSCAPE = B_CT_LANDSCAPE,
                   sector = sector)
  
  # add soil type for political and environmental urgency
  dt[grepl('klei', B_SOILTYPE_AGR) , soiltype := 'klei']
  dt[grepl('zand|dal', B_SOILTYPE_AGR), soiltype := 'zand']
  dt[grepl('veen', B_SOILTYPE_AGR), soiltype := 'veen']
  dt[grepl('loess', B_SOILTYPE_AGR), soiltype := 'loess']
  
  # add relative 
  
  # merge with the measures
  dt.field <- dt[,as.list(dt.meas.field),by=names(dt)]
  
    # columns with the Ecoregelingen ranks and reward
    cols <- c('er_soil','er_water','er_biodiversity','er_climate','er_landscape','er_euro_ha', 'er_euro_farm')
     
    # set first all missing data impacts to 0
    dt.field[,c(cols) := lapply(.SD, function(x) fifelse(is.na(x),0,x)), .SDcols = cols]
  
    # set the score to zero when not applicable for given BBWP or ECO crop category
    dt.field[(B_LU_BBWP == 1 & nc1 == 0) | (B_LU_BBWP == 2 & nc2 == 0), c(cols) := 0]
    dt.field[(B_LU_BBWP == 3 & nc3 == 0) | (B_LU_BBWP == 4 & nc4 == 0), c(cols) := 0]
    dt.field[(B_LU_BBWP == 5 & nc5 == 0) | (B_LU_BBWP == 6 & nc6 == 0), c(cols) := 0]
    dt.field[(B_LU_BBWP == 7 & nc7 == 0) | (B_LU_BBWP == 8 & nc8 == 0), c(cols) := 0]
    dt.field[(B_LU_BBWP == 9 & nc9 == 0) | (B_LU_BBWP == 10 & nc10 == 0), c(cols) := 0]
    dt.field[(B_LU_BBWP == 11 & nc11 == 0) | (B_LU_BBWP == 12 & nc12 == 0), c(cols) := 0]
    dt.field[(B_LU_ECO1 == 1 & eco1 == 0) | (B_LU_ECO2 == 1 & eco2 == 0), c(cols) := 0]
    dt.field[(B_LU_ECO3 == 1 & eco3 == 0) | (B_LU_ECO4 == 1 & eco4 == 0), c(cols) := 0]
    dt.field[(B_LU_ECO5 == 1 & eco5 == 0) | (B_LU_ECO6 == 1 & eco6 == 0), c(cols) := 0]
    dt.field[B_LU_ECO7 == 1 & eco7 == 0, c(cols) := 0]
  
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
    dt.field[bbwp_id == 'G54', B_AREA_REL := sum(B_AREA) * 100 / dt.farm$area_arable]
    dt.field[bbwp_id == 'G54' & B_AREA_REL <= 20, c(cols.sel) := 0]
    dt.field[bbwp_id == 'G54' & B_AREA_REL > 35 & B_AREA_REL <= 50, c(cols.sel) := Map('+',mget(cols.sel),cols.ad1)]
    dt.field[bbwp_id == 'G54' & B_AREA_REL > 50, c(cols.sel) := Map('+',mget(cols.sel),cols.ad2)]
    
    # multiply by (political) urgency
    
      # melt dt
      dt.field <- melt(dt.field,
               id.vars = c('id','bbwp_id','soiltype','bbwp_conflict'),
               measure = patterns(erscore = "^er_"),
               variable.name = 'indicator',
               value.name = 'value')
      dt.field[,indicator := gsub('er_', '',cols[indicator])]
    
    # merge with urgency table
    dt.field <- merge(dt.field,dt.er.urgency, by= c('soiltype','indicator'),all.x = TRUE)
    
    # adapt the score based on urgency
    dt.field[!grepl('euro',indicator), value := value * urgency]
    
    # dcast to add totals, to be used to update scores when measures are conflicting
    cols <- c('biodiversity', 'climate', 'landscape', 'soil','water','total')
    dt2 <- dcast(dt.field, id + soiltype + bbwp_id + bbwp_conflict ~ indicator, value.var = 'value')
    dt2[, total := biodiversity + climate + landscape + soil + water]
    dt2[, oid := frank(-total, ties.method = 'first',na.last = 'keep'),by = c('id','bbwp_conflict')]
    dt2[oid > 1, c(cols) := 0]
    
    # calculate the weighed average ER score (points/ ha) for the whole farm due to measures taken
    dt.field <- dt2[,lapply(.SD,sum), .SDcols = cols, by = 'id']
     
    # calculate the total score for farm measures
    dt.farm.soiltype <- dt[,list(fr_soil = B_AREA / sum(dt$B_AREA)),by = soiltype]
    dt.farm.urgency <- merge(dt.er.urgency[soiltype %in% dt$soiltype], dt.farm.soiltype,by= 'soiltype')
    dt.farm.urgency <- dt.farm.urgency[,list(urgency = weighted.mean(x = urgency,w = fr_soil)),by = indicator]
    
    dt3 <- melt(dt.meas.farm, 
                id.vars = c('bbwp_id','bbwp_conflict'),
                measure = patterns(erscore = "^er_"),
                variable.name = 'indicator',
                value.name = 'value')
    dt3[,indicator := gsub('er_', '',indicator)]
    dt3 <- merge(dt3,dt.farm.urgency,by= 'indicator',all.x = T)
    
    # adapt the score based on urgency
    dt3[!grepl('euro',indicator), value := value * urgency]
    
    # dcast to add totals, to be used to update scores when measures are conflicting
    cols <- c('biodiversity', 'climate', 'landscape', 'soil','water','total')
    dt4 <- dcast(dt3, bbwp_id + bbwp_conflict ~ indicator, value.var = 'value')
    dt4[, total := biodiversity + climate + landscape + soil + water]
    dt4[, oid := frank(-total, ties.method = 'first',na.last = 'keep'),by = c('bbwp_conflict')]
    dt4[oid > 1, c(cols) := 0]
    
    # sum total score
    dt.farm <- dt4[,lapply(.SD,sum), .SDcols = cols]
    
    
  # add bbwp table for financial reward correction factor per AER
  dt.er.reward <- as.data.table(BBWPC::er_aer_reward)
  
  
  ## CHECK: express as weighted mean score over all farm area???
  
  
  
  ----- # financien -----
  # merge with regional correction factor for the financial reward
  dt.fin <- merge(dt,dt.er.reward[,.(statcode,er_cf)], by.x = 'B_AER_CBS',by.y = 'statcode',allow.cartesian = TRUE)
  
  # 
  
    # add filter for rustgewas (EB1) and estimate percentage rustgewassen
    dt.fin[,cf := fifelse(B_LU_BRP %in% dt.er.crops[eco_id=='EB1',b_lu_brp],1,0)]
    dt.fin[,B_AREA_RR := sum(B_AREA * cf) / sum(B_AREA)]
  
    # combine with all measures
    dt.fin <- cbind(dt.fin[rep(id,each = nrow(dt.er.meas)),.(id,er_cf,B_LU_BRP,B_AREA,B_AREA_RR)],
                    dt.er.meas[rep(1:.N,nrow(dt.fin)),])
  
    # add baseline profit
    dt.fin[, value := 0]
    
    # add kruidenrijke randen (EG15)
    dt.fin[B_LU_BRP %in% dt.er.crops[eco_id == 'EG15',b_lu_brp] & eco_id == 'EG15', value := value + er_profit]
    
    # add kleinschalig landschap (EG22)
    dt.fin[B_AREA < 2 & eco_id == 'EG22', value := value + er_profit]
    
    # add percentage rustgewassen (EB1)
    dt.fin[B_AREA_RR > 20 & B_AREA_RR <= 30 & eco_id == 'EB1' , value := value + er_profit]
    dt.fin[B_AREA_RR > 30 & B_AREA_RR <= 40 & eco_id == 'EB1' , value := value + er_profit]
    dt.fin[B_AREA_RR > 40 & eco_id == 'EB1' , value := value + er_profit]
    
    # add eiwitgewassen (EB2)
    dt.fin[B_LU_BRP %in% dt.er.crops[eco_id=='EB2',b_lu_brp] & eco_id == 'EB2' , value := value + er_profit]
    
    # add meerjarige gewassen (EB3)
    dt.fin[B_LU_BRP %in% dt.er.crops[eco_id=='EB3',b_lu_brp] & eco_id == 'EB3' , value := value + er_profit]
    
    # add diepwortelende gewassen (EB8)
    dt.fin[B_LU_BRP %in% dt.er.crops[eco_id=='EB8',b_lu_brp] & eco_id == 'EB8' , value := value + er_profit]
    
    # teelt van gewassen met een gunstige wortel-spruit (EB9)
    dt.fin[B_LU_BRP %in% dt.er.crops[eco_id=='EB9',b_lu_brp] & eco_id == 'EB9' , value := value + er_profit]
    
    # add all financial rewards per field
    dt.fin <- dt.fin[,list(reward = sum(value * er_cf * B_AREA))]
  
   
    # calculate the weighed average ER score (points/ ha) for the whole farm due to crop rotation 
    dt.score <- dt.score[,list(erscore = weighted.mean(erscore * urgency, B_AREA)),by = indicator]
    
    # add a farm id
    dt.score[,farmid := 1]
  
  # dcast the table to make selection easier
  out <- dcast(dt.score,farmid~indicator,value.var ='erscore')
  
  # add financial reward for crop rotation
  out$S_ER_REWARD <- dt.fin
  
  # return the Ecoregelingen Score based on Crop Rotation Only
  return(out)
}
