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
#'    
#' @import data.table
#' @import stats
#'
#' @export
# calculate the opportunities for a set of fields
er_croprotation <- function(B_SOILTYPE_AGR, B_LU_BRP, B_LU_BBWP,B_AER_CBS,B_AREA,
                            B_CT_SOIL, B_CT_WATER,B_CT_CLIMATE,B_CT_BIO,B_CT_LANDSCAPE){
  
  # add visual bindings
  . = eco_id = farmid = b_lu_brp = type = erscore = B_AREA_RR = indicator = NULL
  EG15 = EG22 = cf = EB1A = EB1B = EB1C = EB2 = EB3 = EB8 = EB9 = NULL
  urgency = soiltype = NULL
  er_profit = statcode = er_cf = id = value = reward = NULL
  
  # reformat B_AER_CBS
  B_AER_CBS <- bbwp_format_aer(B_AER_CBS)
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(B_LU_BRP),length(B_LU_BBWP),length(B_AER_CBS))
  
  # check inputs
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = c('duinzand','dekzand','zeeklei','rivierklei','maasklei',
                                                       'dalgrond','moerige_klei','veen','loess'))
  checkmate::assert_integerish(B_LU_BRP, lower = 0, len = arg.length)
  checkmate::assert_integerish(B_LU_BBWP, lower = 0, len = arg.length)
  checkmate::assert_numeric(B_CT_SOIL, lower = 0, upper = 1000, min.len = 1)
  checkmate::assert_numeric(B_CT_WATER, lower = 0, upper = 1000, min.len = 1)
  checkmate::assert_numeric(B_CT_CLIMATE, lower = 0, upper = 1000,min.len = 1)
  checkmate::assert_numeric(B_CT_BIO, lower = 0, upper = 1000, min.len = 1)
  checkmate::assert_numeric(B_CT_LANDSCAPE, lower = 0, upper = 1000,min.len = 1)
  
  # check and update the measure table
  dt.er.meas <- bbwp_check_meas(dt = NULL, eco = TRUE, score = FALSE)
  dt.er.meas <- dt.er.meas[,.(eco_id,er_profit)]
    
  # add bbwp table for crop rotation related measures
  dt.er.farm <- as.data.table(BBWPC::er_farm_measure)
  dt.er.farm <- dcast(dt.er.farm,indicator~eco_id,value.var = 'er_score')
  
  # add bwwp table for crop lists relevant for ecoregeling
  dt.er.crops <- as.data.table(BBWPC::er_crops)
  dt.er.crops <- dt.er.crops[,.(eco_id,b_lu_brp)]
  
  # get internal table with importance of environmental challenges
  dt.er.scoring <- as.data.table(BBWPC::er_scoring)
  setnames(dt.er.scoring,gsub('cf_','',colnames(dt.er.scoring)))
  dt.er.urgency <- melt(dt.er.scoring[type=='urgency'],
                        id.vars='soiltype',
                        measure.vars = c('soil', 'water', 'climate',  'biodiversity', 'landscape'),
                        variable.name = 'indicator',
                        value.name = 'urgency')
  
  # add bbwp table for financial reward correction factor per AER
  dt.er.reward <- as.data.table(BBWPC::er_aer_reward)
  
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
                   B_CT_LANDSCAPE = B_CT_LANDSCAPE
  )
  
  
  # merge with regional correction factor for the financial reward
  dt.fin <- merge(dt,dt.er.reward[,.(statcode,er_cf)], by.x = 'B_AER_CBS',by.y = 'statcode')
  
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
  
  # adapt dt to get the Ecoregeling score
    
    # columns with the Ecoregelingen ranks
    cols <- c('soil','water','biodiversity','climate','landscape')
  
    # add the generic farm score as baseline
  
    # start with zero points
    dt[, c(cols) := list(0,0,0,0,0)]
  
    # melt the data.table to simplify addition of basic ER points
    dt.score <-  melt(dt, 
                      id.vars = c('id','B_SOILTYPE_AGR','B_LU_BRP','B_AREA'),
                      measure.vars = cols,
                      variable.name = 'indicator',
                      value.name = 'm0')
  
    # merge dt.farm with the farm crop rotation based measures
    dt.score <- merge(dt.score,dt.er.farm,by='indicator')
  
    # apply filters and selections
  
      # start value for ER score and ER reward
      dt.score[,erscore := 0]
     
      # add kruidenrijke randen (EG15)
      dt.score[B_LU_BRP %in% dt.er.crops[eco_id=='EG15',b_lu_brp], erscore := erscore + EG15]
          
      # add kleinschalig landschap (EG22)
      dt.score[B_AREA < 2, erscore := erscore + EG22]
         
      # add filter for rustgewas (EB1)
      dt.score[,cf := fifelse(B_LU_BRP %in% dt.er.crops[eco_id=='EB1',b_lu_brp],1,0)]
      
      # add percentage rustgewassen (EB1)
      dt.score[,B_AREA_RR := sum(B_AREA * cf) / sum(B_AREA)]
      dt.score[B_AREA_RR > 20 & B_AREA_RR <= 30, erscore := erscore + EB1A]
      dt.score[B_AREA_RR > 30 & B_AREA_RR <= 40, erscore := erscore + EB1B]
      dt.score[B_AREA_RR > 40, erscore := erscore + EB1C]
  
      # add eiwitgewassen (EB2)
      dt.score[B_LU_BRP %in% dt.er.crops[eco_id=='EB2',b_lu_brp], erscore := erscore + EB2]
      
      # add meerjarige gewassen (EB3)
      dt.score[B_LU_BRP %in% dt.er.crops[eco_id=='EB3',b_lu_brp], erscore := erscore + EB3]
      
      # add diepwortelende gewassen (EB8)
      dt.score[B_LU_BRP %in% dt.er.crops[eco_id=='EB8',b_lu_brp], erscore := erscore + EB8]
      
      # teelt van gewassen met een gunstige wortel-spruit (EB9)
      dt.score[B_LU_BRP %in% dt.er.crops[eco_id=='EB9',b_lu_brp], erscore := erscore + EB9]
      
  # merge with soil specific urgency table
  
    # add soil type for political and environmental urgency
    dt.score[grepl('klei', B_SOILTYPE_AGR) , soiltype := 'klei']
    dt.score[grepl('zand|dal', B_SOILTYPE_AGR), soiltype := 'zand']
    dt.score[grepl('veen', B_SOILTYPE_AGR), soiltype := 'veen']
    dt.score[grepl('loess', B_SOILTYPE_AGR), soiltype := 'loess']
    
    # merge with soil specific urgency table
    dt.score <- merge(dt.score,dt.er.urgency, by= c('indicator','soiltype'))
    
    # calculate the weighed average ER score (points/ ha) for the whole farm due to crop rotation 
    dt.score <- dt.score[,list(erscore = weighted.mean(erscore * urgency, B_AREA)),by = indicator]
    
    # add a farm id
    dt.score[,farmid := 1]
  
  # dcast the table to make selection easier
  out <- dcast(dt.score,farmid~indicator,value.var ='erscore')
  
  # add financial reward for crop rotation
  out$reward <- dt.fin
  
  # return the Ecoregelingen Score based on Crop Rotation Only
  return(out)
}
