#' Calculate the chance that soil property is lower than the measured value given the variation in a catchment
#'
#' assuming a normal distributed density function. The chance can be used as a relative ranking position of the field given the variation inside the catchment.
#'
#' @param smean (numeric) The mean value for a numeric soil property in a given catchment
#' @param ssd (numeric) The variation (standard deviation) among a numeric soil property in a given catchment
#' @param svalue (numeric) The measured value of a soil property of a field inside a catchment
#' 
#'
#' @export
# calculate the ranking position of one or multiple field given a normal distributed soil property in a single catchment
cdf_rank <- function(smean,ssd,svalue){
    
    # set lower and maximum limit for range
    range.lower <- min(svalue,smean - 3 * ssd)
    range.upper <- max(svalue,smean + 3 * ssd)
    
    # set range
    range <- sort(c(seq(range.lower,range.upper,length.out = 1000),svalue))
    
    # make cumulative density function with pnorm
    cdf <- stats::pnorm(range, smean, ssd)
    
    # exctract the probability that a value is below given svalue
    out <- cdf[range %in% svalue]
    
    # return likelyhood
    return(out)
  }
  

#' Helper function to weight and correct the risk and scores
#' 
#' @param x The risk or score value to be weighted
#' @param type Use the weighing function for indicators or score
#' 
#' @export
wf <- function(x, type = "indicators") {
  
  if (type == "indicators") {
    
    y <- 1 / (1 - x + 0.2)
    
  } else if (type == "score") {
    
    y <- 1 / (x * 0.01 + 0.2)
    
  }
  
  return(y)
}

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

#' Helper function to check, update and extend the table with measurements
#' 
#' @param dt (data.table) a data.table containing the bbwp measures with or without properties
#' @param eco (boolean) get only the measures for Ecoregeling (with a given eco_id)
#' @param score (boolean) is check done for scoring of subsets of full list of measures
#' 
#' @export
bbwp_check_meas <- function(dt,eco = FALSE, score = TRUE){
  
  # add visual bindings
  eco_id = bbwp_id = NULL
  
  # get internal table with measures
  dt.measures <- as.data.table(BBWPC::bbwp_measures)
  
  # perform general checks
  
    # check format of the measures given
    checkmate::assert_data_table(dt,null.ok = TRUE)
  
    # check if function argument eco is boolean
    checkmate::assert_logical(eco,len = 1)
  
    # check argument score is boolean
    checkmate::assert_logical(score,len = 1)
    
  # select from internal table only the Ecoregeling measures
  if(eco == TRUE){dt.measures <- dt.measures[!is.na(eco_id)]}
  
  # select the relevant columns as output
  cols.use <- colnames(dt.measures)[!grepl('summary|descr|url|mok|boot',colnames(dt.measures))]
    
  # adapt measurements table when input is given
    
    # select the internal BBWP table when dt is missing and required output is list of measures
    if(is.null(dt) & score == FALSE){dt <- dt.measures}
    
    # set score to zero when no measures are given as input
    if(is.null(dt) & score == TRUE){dt <- data.table(id = 1, bbwp_id = 'NONE')}
    
    # do checks on table with measures
    if(nrow(dt) > 0){
      
      # check if bbwp-id is present (unique per measure)
      checkmate::assert_true('bbwp_id' %in% colnames(dt))
      
      # which columns are missing in dt
      cols.miss <- unique(c('bbwp_id',colnames(dt.measures)[!colnames(dt.measures) %in% colnames(dt)]))
      
      # merge measurement properties with the input list of available measures
      dt <- merge(dt, dt.measures[,mget(cols.miss)],by='bbwp_id')
      
    }
    
  # add also missing measures when function requires full list
  if(score == FALSE){
    
      # check if farm-id is present
      checkmate::assert_true('id' %in% colnames(dt))
      
      # perform check
      dt.miss <- dt.measures[!bbwp_id %in% dt$bbwp_id]
      
      # add missing ones to dt
      dt <- rbind(dt,dt.miss,fill = TRUE)
      
    }
  
  # set all scoring, applicabilility and effects to zero when data is missing
    
    # get relevant colums to be converted
    scols <- colnames(dt)[grepl('^nsw|^ngw|^psw|^p_|^n_|^effect|^er',colnames(dt))]
    
    # update the columns
    dt[,c(scols) := lapply(.SD, function(x) fifelse(is.na(x),0,x)), .SDcols = scols]
    
    # select these columns
    dt <- dt[,mget(cols.use)]
    
  # return output
  return(dt)
    
  
}
