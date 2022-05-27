#' Calculate the crop rotation based total score for five opportunity indicators
#'
#' Estimate the actual contribution of crop rotation given aims for soil quality, water quality, climate, biodiversity and landscape
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BRP (integer)
#' @param B_LU_BBWP (numeric) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_CT_SOIL (numeric) the target value for soil quality conform Ecoregeling scoring
#' @param B_CT_WATER (numeric) the target value for water quality conform Ecoregeling scoring
#' @param B_CT_CLIMATE (numeric) the target value for climate conform Ecoregeling scoring
#' @param B_CT_BIO (numeric) the target value for biodiversity conform Ecoregeling scoring
#' @param B_CT_LANDSCAPE (numeric) the target value for landscape quality conform Ecoregeling scoring
#' @param D_AREA (numeric) the area of the field (\ m2 or \ ha) 
#'    
#' @import data.table
#' @import stats
#'
#' @export
# calculate the opportunities for a set of fields
er_croprotation <- function(B_SOILTYPE_AGR, B_LU_BRP, B_LU_BBWP,D_AREA,
                            B_CT_SOIL, B_CT_WATER,B_CT_CLIMATE,B_CT_BIO,B_CT_LANDSCAPE){
  
  # add visual bindings
  . = eco_id = farmid = b_lu_brp = type = erscore = D_AREA_RR = indicator = NULL
  EG15 = EG22 = cf = EB1A = EB1B = EB1C = EB2 = EB3 = EB8 = EB9 = NULL
  urgency = soiltype = NULL
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(B_LU_BRP),length(B_LU_BBWP))
  
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
  
  
  # collect data in one data.table
  dt <- data.table(id = 1:arg.length,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_LU_BRP = B_LU_BRP,
                   B_LU_BBWP = B_LU_BBWP,
                   D_AREA = D_AREA,
                   B_CT_SOIL = B_CT_SOIL, 
                   B_CT_WATER = B_CT_WATER,
                   B_CT_CLIMATE = B_CT_CLIMATE,
                   B_CT_BIO = B_CT_BIO,
                   B_CT_LANDSCAPE = B_CT_LANDSCAPE
  )
  
  # columns with the Ecoregelingen ranks
  cols <- c('soil','water','biodiversity','climate','landscape')
  
  # add the generic farm score as baseline
  
  # start with zero points
  dt[, c(cols) := list(0,0,0,0,0)]
  
  # melt the data.table to simplify addition of basic ER points
  dt <-  melt(dt, 
              id.vars = c('id','B_SOILTYPE_AGR','B_LU_BRP','D_AREA'),
              measure.vars = cols,
              variable.name = 'indicator',
              value.name = 'm0')
  
  # merge dt.farm with the farm crop rotation based measures
  dt <- merge(dt,dt.er.farm,by='indicator')
  
  # apply filters and selections
  
  # start value
  dt[,erscore:=0]
  
  # add kruidenrijke randen (EG15)
  dt[B_LU_BRP %in% dt.er.crops[eco_id=='EG15',b_lu_brp], erscore := erscore + EG15]
  
  # add kleinschalig landschap (EG22)
  dt[D_AREA < 2, erscore := erscore + EG22]
  
  # add filter for rustgewas (EB1)
  dt[,cf := fifelse(B_LU_BRP %in% dt.er.crops[eco_id=='EB1',b_lu_brp],1,0)]
  
  # add percentage rustgewassen (EB1)
  dt[,D_AREA_RR := sum(D_AREA * cf) / sum(D_AREA)]
  dt[D_AREA_RR > 20 & D_AREA_RR <= 30, erscore := erscore + EB1A]
  dt[D_AREA_RR > 30 & D_AREA_RR <= 40, erscore := erscore + EB1B]
  dt[D_AREA_RR > 40, erscore := erscore + EB1C]
  
  # add eiwitgewassen (EB2)
  dt[B_LU_BRP %in% dt.er.crops[eco_id=='EB2',b_lu_brp], erscore := erscore + EB2]
  
  # add meerjarige gewassen (EB3)
  dt[B_LU_BRP %in% dt.er.crops[eco_id=='EB3',b_lu_brp], erscore := erscore + EB3]
  
  # add diepwortelende gewassen (EB8)
  dt[B_LU_BRP %in% dt.er.crops[eco_id=='EB8',b_lu_brp], erscore := erscore + EB8]
  
  # teelt van gewassen met een gunstige wortel-spruit (EB9)
  dt[B_LU_BRP %in% dt.er.crops[eco_id=='EB9',b_lu_brp], erscore := erscore + EB9]
  
  # merge with soil specific urgency table
  
  # add soil type for political and environmental urgency
  dt[grepl('klei', B_SOILTYPE_AGR) , soiltype := 'klei']
  dt[grepl('zand|dal', B_SOILTYPE_AGR), soiltype := 'zand']
  dt[grepl('veen', B_SOILTYPE_AGR), soiltype := 'veen']
  dt[grepl('loess', B_SOILTYPE_AGR), soiltype := 'loess']
  
  # merge with soil specific urgency table
  dt <- merge(dt,dt.er.urgency, by= c('indicator','soiltype'))
  
  # calculate the weighed average ER score (points/ ha) for the whole farm due to crop rotation 
  dt <- dt[,list(erscore = weighted.mean(erscore * urgency, D_AREA)),by = indicator]
  
  # add a farm id
  dt[,farmid := 1]
  
  # dcast the table to make selection easier
  dt <- dcast(dt,farmid~indicator,value.var ='erscore')
  
  # return the Ecoregelingen Score based on Crop Rotation Only
  return(dt)
}