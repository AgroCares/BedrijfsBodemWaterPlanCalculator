#' Calculate the minimum required Ecoregelingen Scores on Farm level
#'
#' Estimate the  required score on farm level for soil quality, water quality, climate, biodiversity and landscape given soil type
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param D_AREA (numeric) the area of the field (\ m2 or \ ha) 
#' @param farmscore (numeric) the desired farm score for Ecoregeling (number / ha)
#'    
#' @import data.table
#'
#' @export
# calculate the desired Ecoregeling Score for a farm
er_farm_aim <- function(B_SOILTYPE_AGR, D_AREA, farmscore = 100){
  
  # add visual bindings
  . = type = soiltype = value.mis = value = farmid = NULL
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(D_AREA))
  
  # check inputs
  checkmate::assert_numeric(D_AREA, lower = 0, upper = 50000)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = c('duinzand','dekzand','zeeklei','rivierklei','maasklei',
                                                       'dalgrond','moerige_klei','veen','loess'))
  checkmate::assert_numeric(farmscore,lower = 0,upper = 1000, len = 1)
  
  # get internal table for minimum scores on farm level
  er_aim <- as.data.table(BBWPC::er_scoring)[type == 'aim'][,type := NULL]
  
  # make internal table
  dt <- data.table(id = 1:arg.length,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   D_AREA = D_AREA,
                   farmscore = farmscore)
  
  # add soil type
  dt[grepl('klei', B_SOILTYPE_AGR) , soiltype := 'klei']
  dt[grepl('zand|dal', B_SOILTYPE_AGR), soiltype := 'zand']
  dt[grepl('veen', B_SOILTYPE_AGR), soiltype := 'veen']
  dt[grepl('loess', B_SOILTYPE_AGR), soiltype := 'loess']
  
  # merge dt with er_aim
  dt <- merge(dt, er_aim,by='soiltype')
  
  # reshape table to estimate minimum socre per indicator on farm level
  dt <- melt(dt,id.vars = c('id','D_AREA','farmscore'),
             measure.vars = c('cf_soil', 'cf_water','cf_climate', 'cf_biodiversity','cf_landscape'),
             variable.name = 'indicator')
  
  dt[,value.mis := (1 - sum(value))/sum(value==0),by='id']
  dt[value == 0, value := value.mis]
  
  # weighted mean on farm level
  dt <- dt[,list(er_score = weighted.mean(farmscore * value,w = D_AREA)), by = c('indicator')]
  
  # add a farm id
  dt[,farmid := 1]
  
  # dcast the table to make selection easier
  out <- dcast(dt,farmid~indicator,value.var ='er_score')
  
  # update name to set target
  setnames(out,
           c('cf_soil', 'cf_water','cf_climate', 'cf_biodiversity','cf_landscape'),
           c('B_CT_SOIL', 'B_CT_WATER','B_CT_CLIMATE','B_CT_BIO','B_CT_LANDSCAPE'))
  
  # return
  return(out)
}