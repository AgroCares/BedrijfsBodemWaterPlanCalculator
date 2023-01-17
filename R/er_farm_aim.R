#' Calculate the minimum required Ecoregelingen Scores on Farm level
#'
#' Estimate the  required score on farm level for soil quality, water quality, climate, biodiversity and landscape given soil type. Unit is score per hectare.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_AREA (numeric) The area of the field (m2) 
#' @param medalscore (character) The desired medal score expressed as bronze, silver or gold 
#' @param farmscore (numeric) The desired total ER score on farm level
#' @param thresholds (boolean) The threshold of scores for medals bronze, silver and gold should be included in output (options: TRUE or FALSE)
#' 
#' @import data.table
#'
#' @export
# calculate the desired Ecoregeling Score for a farm
er_farm_aim <- function(B_SOILTYPE_AGR, B_AREA, medalscore = "gold", farmscore = NA_real_, thresholds = FALSE){
  
  # add visual bindings
  . = type = soiltype = value.mis = value = farmid = NULL
  code = value_min = value_max = choices = cf_farm_tot = cf_costs = cf_landscape = cf_water = NULL
  medalscores = s_er_farmtotal = s_er_costs = NULL
  
  # Load bbwp_parms
  bbwp_parms <- BBWPC::bbwp_parms
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(B_AREA))
  
  # check inputs
  checkmate::assert_numeric(B_AREA, lower = 10, upper = bbwp_parms[code == "B_AREA", value_max], len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unlist(bbwp_parms[code == "B_SOILTYPE_AGR", choices]))
  checkmate::assert_subset(medalscore, choices = c('bronze','silver','gold'))
  
  # get internal table for minimum scores on farm level
  er_aim <- as.data.table(BBWPC::er_scoring)[type == 'aim'][,type := NULL]
  
  # make internal table
  dt <- data.table(id = 1:arg.length,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_AREA = B_AREA,
                   farmscore = farmscore)

  # merge with all options medal score
  dt <- merge(dt,
              CJ(id=1:arg.length,medalscores = c('gold','silver','bronze')),
              by = 'id')
  
  # calculate minimum score for medals: score per ha
  dt[medalscores == "bronze",farmscore := 14]
  dt[medalscores == "silver",farmscore := 22]
  dt[medalscores == "gold",farmscore := 35]
  
  # add soil type
  dt[grepl('klei', B_SOILTYPE_AGR) , soiltype := 'klei']
  dt[grepl('zand|dal', B_SOILTYPE_AGR), soiltype := 'zand']
  dt[grepl('veen', B_SOILTYPE_AGR), soiltype := 'veen']
  dt[grepl('loess', B_SOILTYPE_AGR), soiltype := 'loess']
  
  # merge dt with er_aim
  dt <- merge(dt, er_aim,by='soiltype')
  
  # reshape table to estimate minimum score per indicator on farm level
  dt <- melt(dt,
             id.vars = c('id','B_AREA','farmscore','medalscores'),
             measure.vars = c('cf_soil', 'cf_water','cf_climate', 'cf_biodiversity','cf_landscape'),
             variable.name = 'indicator')
  
  # dt[,value.mis := (1 - sum(value)) / sum(value==0),by=c('id','medalscore')]
  # dt[value == 0, value := value.mis]
  
  # weighted mean on farm level
  dt <- dt[,list(er_score = weighted.mean(farmscore * value,w = B_AREA)), by = c('indicator','medalscores')]

  # select rows in dt that match the desired medal score  
  v = medalscore
  out.tgt <- dt[medalscores == v,]
  
  # dcast the table to make selection easier
  out.tgt <- dcast(out.tgt,medalscores~indicator,value.var = 'er_score')
  
  # add target costs and total farm score on farm level
  out.tgt[medalscore == "gold", c('B_CT_FARM_TOT','B_CT_COSTS') := list(35,200)]
  out.tgt[medalscore == "silver", c('B_CT_FARM_TOT','B_CT_COSTS') := list(22,100)]
  out.tgt[medalscore == "bronze", c('B_CT_FARM_TOT','B_CT_COSTS') := list(14,60)]
  
  # add a farm id
  out.tgt[,farmid := 1]
  
  # remove medalscore from table
  out.tgt[, medalscores := NULL]
  
  # update name to set target
  setnames(out.tgt,
           c('cf_soil', 'cf_water','cf_climate', 'cf_biodiversity','cf_landscape'),
           c('B_CT_SOIL', 'B_CT_WATER','B_CT_CLIMATE','B_CT_BIO','B_CT_LANDSCAPE')) 
  
  # set colorder
  setcolorder(out.tgt,'farmid')
  
  # round values
  out.tgt <- round(out.tgt,1)
  
  # return output if thresholds for medals are requested
  if(thresholds == TRUE){
    
    # dcast the table to make selection easier
    out.threshold <- dcast(dt,medalscores~indicator,value.var = 'er_score')  
    
    # add farm targets on farm level 
    out.threshold[medalscores == "bronze",s_er_farmtotal := 14]
    out.threshold[medalscores == "silver",s_er_farmtotal := 22]
    out.threshold[medalscores == "gold",s_er_farmtotal := 35]
    
    # add target costs on farm level
    out.threshold[medalscores == "gold", s_er_costs := (200/200)*100]
    out.threshold[medalscores == "silver", s_er_costs := (100/200)*100]
    out.threshold[medalscores == "bronze", s_er_costs := (60/200)*100]
    
    # round values 
    cols <- colnames(out.threshold)[grepl('er_costs',colnames(out.threshold))]
    out.threshold[,c(cols) := lapply(.SD,round,1),.SDcols = cols]
    
    # set threshold of golden medal for landscape to 0 and
    # remove thresholds of bronze and silver medal for landscape
    out.threshold[medalscores == "gold", cf_landscape := 0]
    out.threshold[medalscores == "silver", cf_landscape := NA_real_]
    out.threshold[medalscores == "bronze", cf_landscape := NA_real_]
 
    # if farm only includes peat soils, set threshold for water to 0.5 
    if( all(grepl("veen",B_SOILTYPE_AGR)) == TRUE){
    
    out.threshold[medalscores == "gold", cf_water := 0]
    out.threshold[medalscores == "silver", cf_water := NA_real_]
    out.threshold[medalscores == "bronze", cf_water := NA_real_]
    }
    
    # update name to set absolute thresholds
    setnames(out.threshold,
             c('cf_soil', 'cf_water','cf_climate', 'cf_biodiversity','cf_landscape'),
             c('s_er_soil', 's_er_water','s_er_climate','s_er_biodiversity','s_er_landscape')) 
    
    # round values
    cols <- colnames(out.threshold)[grepl('er_',colnames(out.threshold))]
    out.threshold[,c(cols) := lapply(.SD,round,1),.SDcols = cols]
    
    # restructure the threshold
    out.threshold.bronze <- out.threshold[medalscores=='bronze',mget(cols)]
    out.threshold.silver <- out.threshold[medalscores=='silver',mget(cols)]
    out.threshold.gold <- out.threshold[medalscores=='gold',mget(cols)]
    
    # rename the thresholds
    setnames(out.threshold.bronze,paste0(colnames(out.threshold.bronze),'_bronze'))
    setnames(out.threshold.silver,paste0(colnames(out.threshold.silver),'_silver'))
    setnames(out.threshold.gold,paste0(colnames(out.threshold.gold),'_gold'))
    
    # convert to list
    out.threshold.bronze <- as.list(out.threshold.bronze)
    out.threshold.silver <- as.list(out.threshold.silver)
    out.threshold.gold <- as.list(out.threshold.gold)
    
    # combine in one output object
    out.threshold <- c(out.threshold.bronze,
                       out.threshold.silver,
                       out.threshold.gold)
    
    # set missing values to NULL
    out.threshold[is.na(out.threshold)] <- list(NULL)
    
    # set output object with target and thresholds
    out.tgt <- out.threshold
    
  } 
  

  # return
  return(out.tgt)
}



