#' Rank the suitability of agronomic measures to improve soil and water management for a given field
#'
#' Estimate the score for agronomic measures to improve soil and water management on agricultural farms. 
#' And send an ordered list back of the most suitable measures.
#'
#' @param B_BT_AK (character) The type of soil
#' @param B_GT (character) The groundwater table class
#' @param B_DRAIN (boolean) is there tube drainage present in the field
#' @param D_SA_W (numeric) The fraction of the parcel that is surrounded by surface water
#' @param D_OPI_NGW (numeric) the opportunity index (risk x impact) for nitrate leaching to groundwater given field properties
#' @param D_OPI_NSW (numeric) the opportunity index (risk x impact) for nitrate leaching and runoff to surface water given field properties
#' @param D_OPI_PSW (numeric) the opportunity index (risk x impact) for phosphorus leaching and runoff to surface water given field properties
#' @param D_OPI_NUE (numeric) the opportunity index (risk x impact) to improve the efficiency of nitrogen and phosphorus fertilizer use given field properties
#' @param D_OPI_WB (numeric) the opportunity index (risk x impact) to improve the potential to buffer and store water and efficiently use water for plant growth given field properties
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'melkveehouderij','akkerbouw','vollegrondsgroente','boomteelt','bollen','veehouderij','overig')
#' 
#'   
#' @import data.table
#'
#' @export
# rank the measures given their effectiveness to improve the sustainability of the farm
bbwp_meas_rank <- function(B_BT_AK,B_GT,B_DRAIN,D_SA_W,
                           D_OPI_NGW,D_OPI_NSW,D_OPI_PSW,D_OPI_NUE,D_OPI_WB,
                           sector){
  
  
  # check length of the inputs
  arg.length <- max(length(D_OPI_NGW),length(D_OPI_NSW),length(D_OPI_PSW),length(D_OPI_NUE),
                    length(D_OPI_WB),length(B_BT_AK),length(B_GT),length(B_DRAIN),
                    length(D_SA_W))
  
  # check inputs
  checkmate::assert_subset(B_BT_AK, choices = c('duinzand','dekzand','zeeklei','rivierklei','maasklei',
                                                'dalgrond','moerige_klei','veen','loess'))
  checkmate::assert_subset(B_GT, choices = c('-', 'GtI','GtII','GtIIb','GtIII','GtIIIb','GtIV','GtV','GtVb','GtVI','GtVII','GtVIII'))
  checkmate::assert_logical(B_DRAIN)
  checkmate::assert_numeric(D_SA_W, lower = 0, upper = 100)
  checkmate::assert_numeric(D_OPI_NGW, lower = 0, upper = 100)
  checkmate::assert_numeric(D_OPI_NSW, lower = 0, upper = 100)
  checkmate::assert_numeric(D_OPI_PSW, lower = 0, upper = 100)
  checkmate::assert_numeric(D_OPI_NUE, lower = 0, upper = 100)
  checkmate::assert_numeric(D_OPI_WB, lower = 0, upper = 100)
  checkmate::assert_subset(sector, choices = c('diary', 'arable', 'tree_nursery', 'bulbs'))
  
  # collect data in one data.table
  dt <- data.table(id = 1:arg.length,
                   B_BT_AK = B_BT_AK,
                   B_GT = B_GT,
                   B_DRAIN = B_DRAIN,
                   D_SA_W = D_SA_W,
                   D_OPI_NGW = D_OPI_NGW,
                   D_OPI_NSW = D_OPI_NSW,
                   D_OPI_PSW = D_OPI_PSW,
                   D_OPI_NUE = D_OPI_NUE,
                   D_OPI_WB = D_OPI_WB,
                   value = NA_real_)
  
  # Load in the datasets with measures
  bbwp.meas <- getMeasures()
  bbwp.meas <- bbwp.meas[bwp_active == TRUE & bwp_level == "field", ]
  
  # Loop through each field
  list.preferred <- list()
  for (i in 1:nrow(dt)) {

    this.dt <- dt[i, ]
    
    # add measures to field properties
    this.dt <- cbind(this.dt,bbwp.meas)
    # Change back opi from oppurtunity to risk
    this.dt[, D_OPI_NGW := 100 - D_OPI_NGW]
    this.dt[, D_OPI_NSW := 100 - D_OPI_NSW]
    this.dt[, D_OPI_PSW := 100 - D_OPI_PSW]
    this.dt[, D_OPI_NUE := 100 - D_OPI_NUE]
    this.dt[, D_OPI_WB := 100 - D_OPI_WB]
    # add integral score for measure list given opportunity indexes
    this.dt[, value := (D_OPI_NGW * bwp_e_ngw + D_OPI_NSW * bwp_e_nsw + D_OPI_PSW * bwp_e_psw + D_OPI_NUE * bwp_e_nue + D_OPI_WB * bwp_e_wb) / 5]
    
    # reduce score with a cost / financial estimate
    this.dt[,value := value - bwp_e_kosten]
    
    # rank is zero when measures are not applicable given the farm type
    if('diary' %in% sector){
      this.dt[bwp_melkveehouderij == FALSE, value := value * 0.1]
    }
    if('arable' %in% sector){
      this.dt[bwp_akkerbouw == FALSE,value := value * 0.1]
    }
    #if('vollegrondsgroente' %in% sector){dt[tp_vgg == 0,value := value * 0.1]}
    if('tree_nursery' %in% sector){
      this.dt[bwp_boomteelt == FALSE, value := value * 0.1]
    }
    if('bulbs' %in% sector){
      this.dt[bwp_bollenteelt == FALSE, value := value * 0.1]
    }
    
    # rank is zero when measure is not applicable depending on groundwater levels
    this.dt[B_GT %in% c('GtVII','GtVIII') & bwp_droog == FALSE, value := value * 0.1]
    this.dt[!B_GT %in% c('GtVII','GtVIII') & bwp_nat == FALSE, value := value * 0.1]
    
    # rank is zero when measure is not applicable depending on presence of tube drains
    this.dt[B_DRAIN == TRUE & bwp_metdrainage == FALSE, value := 0]
    this.dt[B_DRAIN == FALSE & bwp_zonderdrainage == FALSE, value := 0]
    
    # rank is zero when measure is not applicable depending on soil type
    this.dt[grepl('klei',B_BT_AK) & bwp_klei == FALSE , value := 0]
    this.dt[grepl('zand|dal',B_BT_AK) & bwp_zand == FALSE , value := 0]
    this.dt[grepl('veen',B_BT_AK) & bwp_veen == FALSE , value := 0]
    
    # rank is zero when measure is not applicable depending on wet surroundings
    this.dt[D_SA_W < 0.1 & bwp_nietontwaterd == FALSE, value := value * 0.1]
    
    # sort the measures
    this.dt <- this.dt[order(-value)]
    
    # extract value
    preferred <- this.dt$bwp_id[1:5]
    
    list.preferred[[i]] <- preferred
  }
 
  # return value
  return(list.preferred)
}

#' Evaluate the contribution of agronomic measures to improve soil mand water anagement
#'
#' Estimate the score for agronomic measures taken to improve soil and water management on agricultural farms.
#'
#' @param B_BT_AK (character) The type of soil
#' @param B_GT (character) The groundwater table class
#' @param B_DRAIN (boolean) is there tube drainage present in the field
#' @param D_SA_W (numeric) The fraction of the parcel that is surrounded by surface water
#' @param D_OPI_NGW (numeric) the opportunity index (risk x impact) for nitrate leaching to groundwater given field properties
#' @param D_OPI_NSW (numeric) the opportunity index (risk x impact) for nitrate leaching and runoff to surface water given field properties
#' @param D_OPI_PSW (numeric) the opportunity index (risk x impact) for phosphorus leaching and runoff to surface water given field properties
#' @param D_OPI_NUE (numeric) the opportunity index (risk x impact) to improve the efficiency of nitrogen and phosphorus fertilizer use given field properties
#' @param D_OPI_WB (numeric) the opportunity index (risk x impact) to improve the potential to buffer and store water and efficiently use water for plant growth given field properties
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'melkveehouderij','akkerbouw','vollegrondsgroente','boomteelt','bollen','veehouderij','overig')
#' @param B_MEAS (list) the measures planned / done per fields (measurement nr)
#' 
#'   
#' @import data.table
#'
#' @export
# calculate the score for a list of measures for one or multiple fields
bbwp_meas_score <- function(B_BT_AK,B_GT,B_DRAIN,D_SA_W,
                            D_OPI_NGW,D_OPI_NSW,D_OPI_PSW,D_OPI_NUE,D_OPI_WB,
                            B_MEAS,sector){
  
  
  # check length of the inputs
  arg.length <- max(length(D_OPI_NGW),length(D_OPI_NSW),length(D_OPI_PSW),length(D_OPI_NUE),
                    length(D_OPI_WB),length(B_BT_AK),length(B_GT),length(B_DRAIN),
                    length(D_SA_W),length(B_MEAS))
  
  # Load in the datasets with measures
  bbwp.meas <- getMeasures()
  
  # check inputs
  checkmate::assert_subset(B_BT_AK, choices = c('duinzand','dekzand','zeeklei','rivierklei','maasklei',
                                                'dalgrond','moerige_klei','veen','loess'))
  checkmate::assert_subset(B_GT, choices = c('-', 'GtI','GtII','GtIIb','GtIII','GtIIIb','GtIV','GtV','GtVb','GtVI','GtVII','GtVIII'))
  checkmate::assert_logical(B_DRAIN,len = arg.length)
  checkmate::assert_numeric(D_SA_W, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_OPI_NGW, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_OPI_NSW, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_OPI_PSW, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_OPI_NUE, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_numeric(D_OPI_WB, lower = 0, upper = 1, len = arg.length)
  checkmate::assert_subset(sector, choices = c('diary', 'arable', 'tree_nursery', 'bulbs'))
  checkmate::assert_subset(unlist(B_MEAS), choices = c(bbwp.meas$bwp_id, NA))
  
  # collect data in one data.table
  dt <- data.table(id = 1:arg.length,
                   B_BT_AK = B_BT_AK,
                   B_GT = B_GT,
                   B_DRAIN = B_DRAIN,
                   D_SA_W = D_SA_W,
                   D_OPI_NGW = D_OPI_NGW,
                   D_OPI_NSW = D_OPI_NSW,
                   D_OPI_PSW = D_OPI_PSW,
                   D_OPI_NUE = D_OPI_NUE,
                   D_OPI_WB = D_OPI_WB,
                   B_MEAS = B_MEAS,
                   D_MEAS_NGW = NA_real_,
                   D_MEAS_NSW = NA_real_,
                   D_MEAS_PSE = NA_real_,
                   D_MEAS_NUE = NA_real_,
                   D_MEAS_WB = NA_real_,
                   D_MEAS_TOT = NA_real_
  )
  
   
  # update dt for each field with the relevant measures taken
  
    # make list to store
    dt.ext <- list()
  
    # make subsets per field # CANT HANDLA NA!
    for (i in dt$id){
      dt.ext[[i]] <-  cbind(
        dt[id==i],
        bbwp.meas[bwp_id %in% unlist(dt[id==i,B_MEAS])])
    }
    
    # rbindlist
    dt.ext <- rbindlist(dt.ext)
  
  # update dt with the selected measures
  dt <- dt.ext[,B_MEAS := NULL]
  
  # add impact score for measure per opportunity index
  dt[, D_MEAS_NGW := D_OPI_NGW * bwp_e_ngw]
  dt[, D_MEAS_NSW := D_OPI_NSW * bwp_e_nsw]
  dt[, D_MEAS_PSW := D_OPI_PSW * bwp_e_psw]
  dt[, D_MEAS_NUE := D_OPI_NUE * bwp_e_nue]
  dt[, D_MEAS_WB := D_OPI_WB * bwp_e_wb]
  dt[, D_MEAS_TOT := (D_MEAS_NGW + D_MEAS_NSW + D_MEAS_PSW + D_MEAS_NUE + D_MEAS_WB ) /  5 - bwp_e_kosten * 0.01]

  # columns to be adapted given applicability
  scols <- c('D_MEAS_NGW','D_MEAS_NSW','D_MEAS_PSW','D_MEAS_NUE','D_MEAS_WB','D_MEAS_TOT')
  
  # rank is zero when measures are not applicable given the farm type
  if('diary' %in% sector) {
    dt[bwp_melkveehouderij == FALSE, c(scols) := lapply(.SD,function(x) x * 0.1), .SDcols = scols]
  }
  if('arable' %in% sector) {
    dt[bwp_akkerbouw == FALSE,c(scols) := lapply(.SD,function(x) x * 0.1),.SDcols = scols]
  }
  #if('vollegrondsgroente' %in% sector){dt[tp_vgg == 0,c(scols) := lapply(.SD,function(x) x * 0.1),.SDcols = scols]}
  if('tree_nursery' %in% sector) {
    dt[bwp_boomteelt == FALSE, c(scols) := lapply(.SD,function(x) x * 0.1),.SDcols = scols]
  }
  if('bulbs' %in% sector) {
    dt[bwp_bollenteelt == FALSE,c(scols) := lapply(.SD,function(x) x * 0.1),.SDcols = scols]
  }
  
  # rank is zero when measure is not applicable depending on groundwater levels
  dt[B_GT %in% c('GtVII','GtVIII') & bwp_droog == FALSE, c(scols) := lapply(.SD,function(x) x * 0.1),.SDcols = scols]
  dt[!B_GT %in% c('GtVII','GtVIII') & bwp_nat == FALSE, c(scols) := lapply(.SD,function(x) x * 0.1),.SDcols = scols]
  
  # rank is zero when measure is not applicable depending on presence of tube drains
  dt[B_DRAIN == TRUE & bwp_metdrainage == FALSE, c(scols) := 0]
  dt[B_DRAIN == FALSE & bwp_zonderdrainage == FALSE, c(scols) := 0]
  
  # rank is zero when measure is not applicable depending on soil type
  dt[grepl('klei',B_BT_AK) & bwp_klei == FALSE , c(scols) := 0]
  dt[grepl('zand|dal',B_BT_AK) & bwp_zand == FALSE , c(scols) := 0]
  dt[grepl('veen',B_BT_AK) & bwp_veen == FALSE , c(scols) := 0]
  
  # rank is zero when measure is not applicable depending on wet surroundings
  dt[D_SA_W < 0.1 & bwp_nietontwaterd == FALSE, c(scols) := lapply(.SD,function(x) x * 0.1),.SDcols = scols]
  
  # calculate the total impact of measures on the five opportunity indexes (in units of effectiveness, from -2 to +2 per measure)
  dt.meas <- dt[,lapply(.SD,sum),.SDcols = scols,by='id']
  
  # replace NA values (when no measures are taken) by 0
  dt.meas[,c(scols) := lapply(.SD,function(x) fifelse(is.na(x),0,x)),.SDcols = scols]
  
  # it should be possible to fill all requirements with two very effective measures per field, equal to +4 points per field
  # so the change in score can increase from 0 to 1 when 4 effectiveness unit points via measures are collected
  # so each effectiveness unit point is equal to 0.25 score units
  dt.meas[,c(scols) := lapply(.SD,function(x) x * 0.25),.SDcols = scols]
  
  # order
  setorder(dt.meas,id)
  
  # extract value
  out <- dt.meas[,mget(scols)]
  
  # return value
  return(out)
}
