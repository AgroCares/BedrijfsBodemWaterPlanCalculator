#' Calculate the Medal on Field and Farm level
#'
#' Estimate the medal reward for measures taken for soil quality, water quality, climate, biodiversity and landscape given soil type. Unit is score per hectare.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param S_ER_TOT (numeric) the Ecoregeling score for the integrative opportunity index for each field
#' @param S_ER_SOIL (numeric) the Ecoregeling scoring index for soil quality for each field
#' @param S_ER_WATER (numeric) the Ecoregeling scoring index for water quality for each field
#' @param S_ER_CLIMATE (numeric) the Ecoregeling scoring index for climate for each field
#' @param S_ER_BIODIVERSITY (numeric) the Ecoregeling scoring index for biodiversity for each field
#' @param S_ER_LANDSCAPE (numeric) the Ecoregeling scoring index for landscape for each field
#' @param S_ER_REWARD (numeric) The financial reward per field for taking Ecoregeling measures (euro / ha)
#' @param B_AREA (numeric) The area of the field (m2) 
#' @param type (character) option to return field or farm medal
#' 
#' @import data.table
#'
#' @export
# calculate the field and farm medal type
er_medal <- function(B_SOILTYPE_AGR, B_AREA, 
                     S_ER_TOT,S_ER_SOIL,S_ER_WATER,S_ER_CLIMATE,S_ER_BIODIVERSITY,S_ER_LANDSCAPE, 
                     S_ER_REWARD,type = 'field'){
  
  # add visual bindings
  . = medal = c_bronze = c_silver = c_gold = er_bronze = er_silver = er_gold = er_medal =  NULL
  B_CT_BIO = B_CT_CLIMATE = B_CT_WATER = B_CT_SOIL = B_CT_TOTAL = B_CT_LANDSCAPE = REWARD = NULL
  indicator = score = value = NULL
  code = value_min = value_max = choices = NULL
  
  # Load bbwp_parms
  bbwp_parms <- BBWPC::bbwp_parms
  
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR), length(B_AREA),
                    length(S_ER_TOT),length(S_ER_SOIL),length(S_ER_WATER),length(S_ER_CLIMATE),
                    length(S_ER_BIODIVERSITY),length(S_ER_LANDSCAPE)) 
  # check inputs
  checkmate::assert_numeric(S_ER_TOT, lower = 0, upper = 500, len = arg.length)
  checkmate::assert_numeric(S_ER_SOIL, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_WATER, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_CLIMATE, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_BIODIVERSITY, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_LANDSCAPE, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(B_AREA,lower = bbwp_parms[code == "B_AREA", value_min], upper = bbwp_parms[code == "B_AREA", value_max], len = arg.length)
  checkmate::assert_numeric(S_ER_REWARD, lower = 0, upper = 10000, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unlist(bbwp_parms[code == "B_SOILTYPE_AGR", choices]))
  
  # get internal tables for minimum scores on farm level
  er_aim.gold <- er_farm_aim(B_SOILTYPE_AGR, B_AREA, medalscore = "gold") 
  er_aim.gold[,B_CT_TOTAL := B_CT_SOIL + B_CT_WATER + B_CT_CLIMATE + B_CT_BIO + B_CT_LANDSCAPE]
  er_aim.gold[,REWARD := 175]
  er_aim.gold <- melt(er_aim.gold,id.vars = 'farmid',variable.name = 'indicator',value.name = 'er_gold')
  er_aim.gold[,indicator := gsub('B_CT_','',indicator)]
  er_aim.silver <- er_farm_aim(B_SOILTYPE_AGR, B_AREA, medalscore = "silver") 
  er_aim.silver[,B_CT_TOTAL := B_CT_SOIL + B_CT_WATER + B_CT_CLIMATE + B_CT_BIO + B_CT_LANDSCAPE]
  er_aim.silver[,REWARD := 110]
  er_aim.silver <- melt(er_aim.silver,id.vars = 'farmid',variable.name = 'indicator',value.name = 'er_silver')
  er_aim.silver[,indicator := gsub('B_CT_','',indicator)]
  er_aim.bronze <- er_farm_aim(B_SOILTYPE_AGR, B_AREA, medalscore = "bronze") 
  er_aim.bronze[,B_CT_TOTAL := B_CT_SOIL + B_CT_WATER + B_CT_CLIMATE + B_CT_BIO + B_CT_LANDSCAPE]
  er_aim.bronze[,REWARD := 70]
  er_aim.bronze <- melt(er_aim.bronze,id.vars = 'farmid',variable.name = 'indicator',value.name = 'er_bronze')
  er_aim.bronze[,indicator := gsub('B_CT_','',indicator)]
  
  # collect data in one data.table
  dt <- data.table(id = 1:arg.length,
                   SOIL = S_ER_SOIL,
                   WATER = S_ER_WATER,
                   CLIMATE = S_ER_CLIMATE,
                   BIO = S_ER_BIODIVERSITY,
                   LANDSCAPE = S_ER_LANDSCAPE,
                   TOTAL = S_ER_TOT,
                   REWARD = S_ER_REWARD,
                   B_AREA = B_AREA,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR
  )
  
  # melt the data.table
  dt <- melt(dt,
             id.vars = c('id','B_AREA','B_SOILTYPE_AGR'),
             variable.name = 'indicator')
  
  # add the criteria for gold, silver and bronze
  dt <- merge(dt,er_aim.gold[,.(indicator,er_gold)],by='indicator',all.x = TRUE)
  dt <- merge(dt,er_aim.silver[,.(indicator,er_silver)],by='indicator',all.x = TRUE)
  dt <- merge(dt,er_aim.bronze[,.(indicator,er_bronze)],by='indicator',all.x = TRUE)
  
  # estimate the absolute ER score
  dt[, score := fifelse(indicator == 'REWARD',value,value * er_gold * 0.01)]
 
  # set output depending on farm or field level
  if(type == 'field'){
    
    # indicator landscape passes the threshold for gold, silver, and bronze in any case
    cols <- c("er_gold", "er_silver", "er_bronze")
    dt[indicator == "LANDSCAPE", c(cols) := 0]
    
    # indicator water passes the threshold for gold, silver, and bronze always when soil type is "veen"
    dt[indicator == "WATER" & B_SOILTYPE_AGR == "veen", c(cols) := 0]

    # set checks for medals score per field
    dt[,c_gold := fifelse(score>=er_gold,1,0)] 
    dt[,c_silver := fifelse(score>=er_silver,1,0)]
    dt[,c_bronze := fifelse(score>=er_bronze,1,0)]
  
    # sum the requirement for the medal (total should be 7 per field)
    dt.field <- dt[,lapply(.SD,sum),.SDcols = c('c_gold','c_silver','c_bronze'),by= c('id','B_AREA')]
    
    # set the medal per field
    dt.field[c_bronze >= 7,medal := 'bronze']
    dt.field[c_silver >= 7, medal := 'silver']
    dt.field[c_gold >= 7, medal := 'gold']
    dt.field[is.na(medal), medal := 'none']
  
    # set output object
    out <- dt.field[,medal]
    
  } else {
    
    # estimate weighted mean score for full farm
    dt.farm <- dt[,lapply(.SD,weighted.mean,w = B_AREA),
                  .SDcols = c('score','er_gold','er_silver','er_bronze'), by = 'indicator']
    
    # indicator landscape passes the threshold for gold, silver, and bronze in any case
    cols <- c("er_gold", "er_silver", "er_bronze")
    dt.farm[indicator == "LANDSCAPE", c(cols) := 0]
    
    # indicator water passes the threshold for gold, silver, and bronze always when soil type is "veen"
    #dt.farm[indicator == "WATER" & B_SOILTYPE_AGR == "veen", c(cols) := 0]
    
    # set checks for medals score per field
    dt.farm[,c_gold := fifelse(score>=er_gold,1,0)]
    dt.farm[,c_silver := fifelse(score>=er_silver,1,0)]
    dt.farm[,c_bronze := fifelse(score>=er_bronze,1,0)]
    
    # sum the requirement for the medal (total should be 7 per field)
    dt.farm <- dt.farm[,lapply(.SD,sum),.SDcols = c('c_gold','c_silver','c_bronze')]
    
    # set the medal for farm
    dt.farm[c_bronze >= 7,medal := 'bronze']
    dt.farm[c_silver >= 7, medal := 'silver']
    dt.farm[c_gold >= 7, medal := 'gold']
    dt.farm[is.na(medal), medal := 'none']
    
    # set output object
    out <- dt.farm[,medal]
    
  }
  
  # return
  return(out)
}



