#' Calculate the minimum required Ecoregelingen Scores on Farm level
#'
#' Estimate the  required score on farm level for soil quality, water quality, climate, biodiversity and landscape given soil type. Unit is score per hectare.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_AREA (numeric) The area of the field (m2) 
#' @param medalscore (character) The desired medal score expressed as bronze, silver or gold 
#' @param farmscore (numeric) The desired total ER score on farm level
#' 
#' @import data.table
#'
#' @export
# calculate the desired Ecoregeling Score for a farm
er_farm_aim <- function(B_SOILTYPE_AGR, B_AREA, medalscore = "gold", farmscore = NA_real_, thresholds = TRUE){
  
  # add visual bindings
  . = type = soiltype = value.mis = value = farmid = NULL
  code = value_min = value_max = choices = NULL
  
  # Load bbwp_parms
  bbwp_parms <- BBWPC::bbwp_parms
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(B_AREA))
  
  # check inputs
  checkmate::assert_numeric(B_AREA, lower = bbwp_parms[code == "B_AREA", value_min], upper = bbwp_parms[code == "B_AREA", value_max], len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unlist(bbwp_parms[code == "B_SOILTYPE_AGR", choices]))
  checkmate::assert_subset(medalscore, choices = c('bronze','silver','gold'))
  
  # get internal table for minimum scores on farm level
  er_aim <- as.data.table(BBWPC::er_scoring)[type == 'aim'][,type := NULL]
  
  # make internal table
  dt1 <- data.table(id = 1:arg.length,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_AREA = B_AREA,
                   medalscore = "gold",
                   farmscore = farmscore)

  dt2 <- data.table(id = 1:arg.length,
                    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                    B_AREA = B_AREA,
                    medalscore = "silver",
                    farmscore = farmscore)
  
  dt3 <- data.table(id = 1:arg.length,
                    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                    B_AREA = B_AREA,
                    medalscore = "bronze",
                    farmscore = farmscore)
  
  dt <- rbind(dt1,dt2,dt3)
  
  # calculate minimum score for medals: score per ha
  # dt[medalscore == "bronze" & is.na(farmscore),farmscore := 14]
  # dt[medalscore == "silver" & is.na(farmscore),farmscore := 22]
  # dt[medalscore == "gold" & is.na(farmscore),farmscore := 35]
  
  # calculate minimum score for medals: score per ha
  dt[medalscore == "bronze",farmscore := 14]
  dt[medalscore == "silver",farmscore := 22]
  dt[medalscore == "gold",farmscore := 35]
  
  # add soil type
  dt[grepl('klei', B_SOILTYPE_AGR) , soiltype := 'klei']
  dt[grepl('zand|dal', B_SOILTYPE_AGR), soiltype := 'zand']
  dt[grepl('veen', B_SOILTYPE_AGR), soiltype := 'veen']
  dt[grepl('loess', B_SOILTYPE_AGR), soiltype := 'loess']
  
  # merge dt with er_aim
  dt <- merge(dt, er_aim,by='soiltype')
  
  # reshape table to estimate minimum score per indicator on farm level
  dt <- melt(dt,id.vars = c('id','B_AREA','farmscore','medalscore'),
             measure.vars = c('cf_soil', 'cf_water','cf_climate', 'cf_biodiversity','cf_landscape'),
             variable.name = 'indicator')
  
  # dt[,value.mis := (1 - sum(value)) / sum(value==0),by=c('id','medalscore')]
  # dt[value == 0, value := value.mis]
  
  # weighted mean on farm level
  dt <- dt[,list(er_score = weighted.mean(farmscore * value,w = B_AREA)), by = c('indicator','medalscore')]

  # select rows in dt that match the desired medal score  
  v = medalscore
  out.tgt <- dt[medalscore %in% v,]
  
  # dcast the table to make selection easier
  out.tgt <- dcast(out.tgt,medalscore~indicator,value.var = 'er_score')
  
  # calculate target score for farm total
  out.tgt <- out.tgt[, cf_farm_tot := sum(out.tgt[,-1])]
  
  # add target costs on farm level
  out.tgt <- out.tgt[medalscore == "gold", cf_costs := 175][medalscore == "gold",cf_farm_tot :=35]
  out.tgt <- out.tgt[medalscore == "silver", cf_costs := 100][medalscore == "silver",cf_farm_tot :=22]
  out.tgt <- out.tgt[medalscore == "bronze", cf_costs := 70][medalscore == "bronze",cf_farm_tot :=14]

  # add a farm id
  out.tgt[,farmid := 1]
  
  # remove medalscore from table
  out.tgt[, medalscore := NULL]
  
  # update name to set target
  setnames(out.tgt,
           c('cf_soil', 'cf_water','cf_climate', 'cf_biodiversity','cf_landscape','cf_farm_tot','cf_costs'),
           c('B_CT_SOIL', 'B_CT_WATER','B_CT_CLIMATE','B_CT_BIO','B_CT_LANDSCAPE','B_CT_FARM_TOT','B_CT_COSTS')) 
  
  # return output if thresholds for medals are requested
  if(thresholds == TRUE){

    # dcast the table to make selection easier
    out.threshold <- dcast(dt,medalscore~indicator,value.var = 'er_score')  
    
    # select columns
    cols <- c('cf_soil', 'cf_water','cf_climate', 'cf_biodiversity','cf_landscape')
    
    # calculate target score for farm total
    out.threshold <- out.threshold[, cf_farm_tot := rowSums(.SD), .SDcols = cols]
    
    # add target costs on farm level
    out.threshold <- out.threshold[medalscore == "gold", cf_costs := 175][medalscore == "gold",cf_farm_tot :=35]
    out.threshold <- out.threshold[medalscore == "silver", cf_costs := 100][medalscore == "silver",cf_farm_tot :=22]
    out.threshold <- out.threshold[medalscore == "bronze", cf_costs := 70][medalscore == "bronze",cf_farm_tot :=14]
    
    # update name to set absolute thresholds
    setnames(out.threshold,
             c('cf_soil', 'cf_water','cf_climate', 'cf_biodiversity','cf_landscape','cf_farm_tot','cf_costs'),
             c('B_CT_SOIL', 'B_CT_WATER','B_CT_CLIMATE','B_CT_BIO','B_CT_LANDSCAPE','B_CT_FARM_TOT','B_CT_COSTS')) 
   
    # set output object with target and thresholds
    out <- list(target = as.list(out.tgt),farm.thresholds = out.threshold)
    
    }

  # return output if thresholds for medals are not requested
  if(thresholds == FALSE){
  
    # set output object with targets only 
    out <- list(target = as.list(out.tgt))
  
    }
 
 
  # return
  return(out)
}



