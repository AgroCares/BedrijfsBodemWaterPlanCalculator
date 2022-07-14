#' Calculate the total score of five opportunity indicators conform Ecoregelingen Scoring
#'
#' Estimate the potential to contribute to agronomic and environmental challenges in a region given aims for soil quality, water quality, climate, biodiversity and landscape
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
#' @param measures (list) the measures planned / done per fields (measurement nr)
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'melkveehouderij','akkerbouw','vollegrondsgroente','boomteelt','bollen','veehouderij','overig')
#'    
#' @import data.table
#' @import stats
#'
#' @export
# calculate the opportunity indices for a set of fields
er_field_scores <- function(B_SOILTYPE_AGR, B_LU_BRP, B_LU_BBWP,B_AER_CBS,
                            B_AREA,
                            B_CT_SOIL, B_CT_WATER,B_CT_CLIMATE,B_CT_BIO,B_CT_LANDSCAPE, 
                            measures = NULL, sector){
  
  # add visual bindings
  value = . = eco_id = b_lu_brp = type = erscore = EG15 = EG22 = cf = EB1A = EB1B = EB1C = NULL
  B_AREA_RR = EB2 = EB3 = EB8 = EB9 = soiltype = urgency = indicator = farmid = NULL
  D_OPI_SOIL = D_OPI_WATER = D_OPI_CLIMATE = D_OPI_BIO = D_OPI_LANDSCAPE = NULL
  D_MEAS_BIO = D_MEAS_CLIM = D_MEAS_LAND = D_MEAS_SOIL = D_MEAS_WAT = NULL
  cfSOIL = cfWAT = cfCLIM = cfBIO = cfLAND = D_OPI_TOT = id = S_ER_REWARD = NULL
 
  # reformat B_AER_CBS
  B_AER_CBS <- bbwp_format_aer(B_AER_CBS)
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(B_LU_BRP),length(B_LU_BBWP),
                    length(B_AREA),length(B_AER_CBS))
  
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
  
  # check and update the measure table
  dt.er.meas <- bbwp_check_meas(measures, eco = TRUE, score = TRUE)
  
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
                   B_AER_CBS = B_AER_CBS,
                   B_AREA = B_AREA,
                   B_CT_SOIL = B_CT_SOIL, 
                   B_CT_WATER = B_CT_WATER,
                   B_CT_CLIMATE = B_CT_CLIMATE,
                   B_CT_BIO = B_CT_BIO,
                   B_CT_LANDSCAPE = B_CT_LANDSCAPE
                  )
  
  # columns with the Ecoregelingen ranks
  cols <- c('soil','water','biodiversity','climate','landscape')
  
  # add the generic farm score as baseline
  # this gives the averaged ER score based on the crops in crop rotation plan
  dt.farm <- er_croprotation(B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR,
                             B_LU_BRP = dt$B_LU_BRP,
                             B_LU_BBWP = dt$B_LU_BBWP,
                             B_AER_CBS = dt$B_AER_CBS,
                             B_AREA = dt$B_AREA,
                             B_CT_SOIL = dt$B_CT_SOIL,
                             B_CT_WATER = dt$B_CT_WATER,
                             B_CT_CLIMATE = dt$B_CT_CLIMATE,
                             B_CT_BIO = dt$B_CT_BIO,
                             B_CT_LANDSCAPE = dt$B_CT_LANDSCAPE) #,
                             #measures = measures)
  
  # calculate the change in opportunity indexes given the measures taken
  
  # column names for impact of measures on the five indexes (do not change order)
  # these are not yet converted to a 0-1 scale
    
    # set colnames for the impact of measures
    mcols <- c('D_MEAS_BIO', 'D_MEAS_CLIM', 'D_MEAS_LAND', 'D_MEAS_SOIL', 'D_MEAS_WAT','S_ER_REWARD')
  
    # calculate the total score per indicator 
    if(nrow(dt.er.meas) > 0){
      
      # calculate
      dt.meas.impact <- er_meas_score(B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR, 
                                      B_AER_CBS = dt$B_AER_CBS,
                                      B_LU_BRP = dt$B_LU_BRP,
                                      B_LU_BBWP = B_LU_BBWP,
                                      B_LU_ECO1 = B_LU_ECO1,
                                      B_LU_ECO2 = B_LU_ECO2,
                                      B_LU_ECO3 = B_LU_ECO3,
                                      B_LU_ECO4 = B_LU_ECO4,
                                      B_LU_ECO5 = B_LU_ECO5,
                                      B_LU_ECO6 = B_LU_ECO6,
                                      B_LU_ECO7 = B_LU_ECO7,
                                      B_AER_CBS = B_AER_CBS,
                                      B_AREA = B_AREA,
                                      measures = measures, 
                                      sector = sector)

      # merge with dt
      dt <- merge(dt,dt.meas.impact,by='id')
      
      # set NA to zero
      dt[,c(mcols) := lapply(.SD,function(x) fifelse(is.na(x),0,x)),.SDcols = mcols]
      
    } else {
      
      # set impact of management to zero when no measures are applied
      dt[,c(mcols) := list(0,0,0,0,0,0)]
    }
    
    
    # what is the opportunity to contribute to environmental challenges
    
    # in theory is that maximum, since there are not yet measures applied
    # so the OPI is difference between 1 and the current farm score derived from crop rotation
    # in addition, when measures are taken this also reduces the distance to target
    dt[, D_OPI_SOIL := (dt.farm$soil + D_MEAS_SOIL)/ B_CT_SOIL]
    dt[, D_OPI_WATER :=  (dt.farm$water + D_MEAS_WAT) / B_CT_WATER]
    dt[, D_OPI_CLIMATE :=  (dt.farm$climate + D_MEAS_CLIM)/ B_CT_CLIMATE]
    dt[, D_OPI_BIO :=  (dt.farm$biodiversity + D_MEAS_BIO) / B_CT_BIO]
    dt[, D_OPI_LANDSCAPE :=  (dt.farm$landscape + D_MEAS_LAND) / B_CT_LANDSCAPE]
    
    # ensure score is between 0 and 1
    dt[, D_OPI_SOIL := 100 * pmax(0,pmin(1,D_OPI_SOIL))]
    dt[, D_OPI_WATER := 100 * pmax(0,pmin(1,D_OPI_WATER))]
    dt[, D_OPI_CLIMATE := 100 * pmax(0,pmin(1,D_OPI_CLIMATE))]
    dt[, D_OPI_BIO := 100 * pmax(0,pmin(1,D_OPI_BIO))]
    dt[, D_OPI_LANDSCAPE := 100 * pmax(0,pmin(1,D_OPI_LANDSCAPE))]
    
   # calculate the integrative opportunity index (risk times impact)
    
      # weigh the importance given "distance to target"
      dt[,cfSOIL := wf(D_OPI_SOIL, type="score")]
      dt[,cfWAT := wf(D_OPI_WATER, type="score")]
      dt[,cfCLIM := wf(D_OPI_CLIMATE, type="score")]
      dt[,cfBIO := wf(D_OPI_BIO, type="score")]
      dt[,cfLAND := wf(D_OPI_LANDSCAPE, type="score")]
    
      # weighted mean
      dt[,D_OPI_TOT := (D_OPI_SOIL * cfSOIL + D_OPI_WATER * cfWAT + D_OPI_CLIMATE * cfCLIM +D_OPI_BIO * cfBIO + D_OPI_LANDSCAPE * cfLAND) / 
                       (cfSOIL + cfWAT + cfCLIM + cfBIO + cfLAND)]
      
  # order the fields
  setorder(dt, id)
  
  # rename the opportunity indexes to the final score
  setnames(dt,c('D_OPI_SOIL','D_OPI_WATER','D_OPI_CLIMATE','D_OPI_BIO','D_OPI_LANDSCAPE','D_OPI_TOT'),
              c('S_ER_SOIL','S_ER_WATER','S_ER_CLIMATE','S_ER_BIODIVERSITY','S_ER_LANDSCAPE','S_ER_TOT'))
  
  # round the values and get the field scores
  cols <- c('S_ER_SOIL','S_ER_WATER','S_ER_CLIMATE','S_ER_BIODIVERSITY','S_ER_LANDSCAPE','S_ER_TOT','S_ER_REWARD')
  dt <- dt[, c(cols) := lapply(.SD, round, digits = 0),.SDcols = cols]
  
  # update the field-reward with the farm-reward (in euro/ha)
  dt[,S_ER_REWARD := S_ER_REWARD + dt.farm$S_ER_REWARD / sum(B_AREA)]
  
  # extract value
  value <- dt[,mget(c('id',cols))]
   
  # return value
  return(value)
}
