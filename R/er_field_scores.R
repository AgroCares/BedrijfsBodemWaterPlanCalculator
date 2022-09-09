#' Calculate the total score of five opportunity indicators conform Ecoregelingen Scoring
#'
#' Estimate the potential to contribute to agronomic and environmental challenges in a region given aims for soil quality, water quality, climate, biodiversity and landscape
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BBWP (character) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_LU_ARABLE_ER (boolean) does the crop fall within the ER category "arable"
#' @param B_LU_PRODUCTIVE_ER (boolean) does the crop fall within the ER category "productive"
#' @param B_LU_CULTIVATED_ER (boolean) does the crop fall within the ER category "cultivated"
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param B_AREA (numeric) the area of the field (m2) 
#' @param measures (list) the measures planned / done per fields (measurement nr)
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'melkveehouderij','akkerbouw','vollegrondsgroente','boomteelt','bollen','veehouderij','overig')
#'    
#' @import data.table
#' @import stats
#'
#' @export
# calculate the opportunity indices for a set of fields
er_field_scores <- function(B_SOILTYPE_AGR, B_LU_BBWP, B_LU_BRP, B_AER_CBS,B_AREA,
                            B_LU_ARABLE_ER, B_LU_PRODUCTIVE_ER,B_LU_CULTIVATED_ER,
                            measures = NULL, sector){
  
  # add visual bindings
  value = . = eco_id = b_lu_brp = type = erscore = EG15 = EG22 = cf = EB1A = EB1B = EB1C = NULL
  B_AREA_RR = EB2 = EB3 = EB8 = EB9 = soiltype = urgency = indicator = farmid = NULL
  D_MEAS_BIO = D_MEAS_CLIM = D_MEAS_LAND = D_MEAS_SOIL = D_MEAS_WAT = D_MEAS_TOT = ec1 = ec2 = NULL
  S_ER_SOIL = S_ER_WATER = S_ER_CLIMATE = S_ER_BIODIVERSITY = S_ER_LANDSCAPE = S_ER_TOT = id = S_ER_REWARD = NULL
  
  code = choices = NULL
  
  # Load bbwp_parms
  bbwp_parms <- BBWPC::bbwp_parms
  
  # reformat B_AER_CBS
  B_AER_CBS <- bbwp_format_aer(B_AER_CBS)
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(B_LU_BBWP),length(B_LU_BRP),
                    length(B_AREA),length(B_AER_CBS),
                    length(B_LU_ARABLE_ER),length(B_LU_PRODUCTIVE_ER),length(B_LU_CULTIVATED_ER))
  
  # check inputs
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unlist(bbwp_parms[code == "B_SOILTYPE_AGR", choices]))
  checkmate::assert_subset(B_LU_BBWP, choices = unlist(bbwp_parms[code == "B_LU_BBWP", choices]))
  checkmate::assert_character(B_LU_BBWP, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unlist(bbwp_parms[code == "B_LU_BRP", choices]))
  checkmate::assert_integerish(B_LU_BRP, len = arg.length)
  checkmate::assert_logical(B_LU_ARABLE_ER,len = arg.length)
  checkmate::assert_logical(B_LU_PRODUCTIVE_ER,len = arg.length)
  checkmate::assert_logical(B_LU_CULTIVATED_ER,len = arg.length)
  
  # check and update the measure table
  dt.er.meas <- bbwp_check_meas(measures, eco = TRUE, score = TRUE)
  dt.meas.eco <- as.data.table(BBWPC::er_measures)
  
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
                   B_LU_BBWP = B_LU_BBWP,
                   B_LU_BRP = B_LU_BRP,
                   B_LU_ARABLE_ER = B_LU_ARABLE_ER, 
                   B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                   B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                   B_AER_CBS = B_AER_CBS,
                   B_AREA = B_AREA
                  )
  
  # columns with the Ecoregelingen ranks
  cols <- c('soil','water','biodiversity','climate','landscape')
  
  # add the generic farm score as baseline
  # this gives the averaged ER score based on the crops in crop rotation plan
  dt.farm <- er_croprotation(B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR,
                             B_LU_BBWP = dt$B_LU_BBWP,
                             B_LU_BRP = dt$B_LU_BRP,
                             B_LU_ARABLE_ER = dt$B_LU_ARABLE_ER,
                             B_LU_PRODUCTIVE_ER = dt$B_LU_PRODUCTIVE_ER,
                             B_LU_CULTIVATED_ER = dt$B_LU_CULTIVATED_ER,
                             B_AER_CBS = dt$B_AER_CBS,
                             B_AREA = dt$B_AREA,
                             measures = measures,
                             sector = sector)
  
  
  
  # calculate the change in opportunity indexes given the measures taken
  
  # column names for impact of measures on the five indexes (do not change order)
  # these are not yet converted to a 0-1 scale
    
    # set colnames for the impact of measures
    mcols <- c('D_MEAS_BIO', 'D_MEAS_CLIM', 'D_MEAS_LAND', 'D_MEAS_SOIL', 'D_MEAS_WAT','D_MEAS_TOT','S_ER_REWARD')
  
    # calculate the total score per indicator 
    if(nrow(dt.er.meas) > 0){
      
      # calculate
      dt.meas.impact <- er_meas_score(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                                      B_LU_BBWP = B_LU_BBWP,
                                      B_LU_BRP = B_LU_BRP,
                                      B_LU_ARABLE_ER = B_LU_ARABLE_ER,
                                      B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                                      B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
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
      dt[,c(mcols) := list(0,0,0,0,0,0,0)]
    }
    
  # what is the total score of farm and field based measures taken
  dt[, S_ER_SOIL := dt.farm$soil + D_MEAS_SOIL]
  dt[, S_ER_WATER := dt.farm$water + D_MEAS_WAT]
  dt[, S_ER_CLIMATE :=  dt.farm$climate + D_MEAS_CLIM]
  dt[, S_ER_BIODIVERSITY := dt.farm$biodiversity + D_MEAS_BIO]
  dt[, S_ER_LANDSCAPE :=  dt.farm$landscape + D_MEAS_LAND]
  dt[, S_ER_TOT := dt.farm$total + D_MEAS_TOT]
    
  # update the field-reward with the farm-reward (in euro/ha)
  dt[,S_ER_REWARD := S_ER_REWARD + dt.farm$S_ER_REWARD]
  
  # order the fields
  setorder(dt, id)
  
  # cols to include in output
  cols <- c('S_ER_SOIL','S_ER_WATER','S_ER_CLIMATE','S_ER_BIODIVERSITY','S_ER_LANDSCAPE','S_ER_REWARD','S_ER_TOT')
  
  # extract value
  value <- dt[,mget(c('id',cols))]
   
  # return value
  return(value)
}
