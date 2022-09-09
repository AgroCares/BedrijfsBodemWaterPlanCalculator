#' Calculate the Opportunity Index for the Ecoregeling method
#'
#' Estimate the contribution of individual fields to the desired scores for highest impact on farm level as well as the total farm score, weighted given the distance to target.
#' A high Ecoregeling score is indicative for the number of opportunities to improve soil quality, water quality, climate biodiversity and landscape.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param S_ER_SOIL (numeric) the Ecoregeling scoring index for soil quality for each field
#' @param S_ER_WATER (numeric) the Ecoregeling scoring index for water quality for each field
#' @param S_ER_CLIMATE (numeric) the Ecoregeling scoring index for climate for each field
#' @param S_ER_BIODIVERSITY (numeric) the Ecoregeling scoring index for biodiversity for each field
#' @param S_ER_LANDSCAPE (numeric) the Ecoregeling scoring index for landscape for each field
#' @param S_ER_REWARD (numeric) The financial reward per field for taking Ecoregeling measures (euro / ha)
#' @param B_AREA (numeric) the area of the field (m2) 
#' @param medalscore (character) The desired medal score expressed as bronze, silver or gold 
#'  
#' @import data.table
#'
#' @export
er_opi <- function(B_SOILTYPE_AGR, 
                   S_ER_SOIL,S_ER_WATER,S_ER_CLIMATE,S_ER_BIODIVERSITY,S_ER_LANDSCAPE, 
                   S_ER_REWARD, B_AREA,
                   medalscore){
  
  # Load bbwp_parms
  bbwp_parms <- BBWPC::bbwp_parms
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(S_ER_SOIL),length(S_ER_WATER),length(S_ER_CLIMATE),
                    length(S_ER_BIODIVERSITY),length(S_ER_LANDSCAPE),length(B_AREA))
  
  # check inputs
  checkmate::assert_numeric(S_ER_SOIL, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_WATER, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_CLIMATE, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_BIODIVERSITY, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_LANDSCAPE, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(B_AREA, lower = bbwp_parms[code == "B_AREA", value_min], upper = bbwp_parms[code == "B_AREA", value_max], len = arg.length)
  checkmate::assert_numeric(S_ER_REWARD, lower = 0, upper = 10000, len = arg.length)
  checkmate::assert_subset(medalscore, choices = c('bronze','silver','gold'))
  
  # Calculate the minimum required ER scores on Farm level for the desired medal
  dt.farm.aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, B_AREA = B_AREA, medalscore = medalscore)
  
  # collect data in one data.table
  dt <- data.table(id = 1:arg.length,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   S_ER_SOIL = S_ER_SOIL,
                   S_ER_WATER = S_ER_WATER,
                   S_ER_CLIMATE = S_ER_CLIMATE,
                   S_ER_BIODIVERSITY = S_ER_BIODIVERSITY,
                   S_ER_LANDSCAPE = S_ER_LANDSCAPE,
                   S_ER_REWARD = S_ER_REWARD,
                   B_AREA = B_AREA
                   )
  
  # count the total score per field (scores / ha)
  dt[, S_ER_TOT := S_ER_SOIL + S_ER_WATER + S_ER_CLIMATE + S_ER_BIODIVERSITY + S_ER_LANDSCAPE]
  
  # estimate the farm scores
  
    # melt the table to estimate farm score
    dt.farm <- melt(dt,id.vars = c('id','B_AREA','B_SOILTYPE_AGR'),
                    measure = patterns("S_ER"), 
                    value.name = 'S_ER',
                    variable.name = 'indicator')
    
    # adjust the current score in the case that there is no minimum needed
    # be aware, this is done after calculation of the total score
    dt.farm[B_SOILTYPE_AGR == 'veen' & indicator == 'S_ER_WATER', S_ER := dt.farm.aim$B_CT_WATER]
    dt.farm[indicator == 'S_ER_LANDSCAPE', S_ER := dt.farm.aim$B_CT_LANDSCAPE]
    
    # estimate the mean score (score / ha) per indicator for the whole farm
    dt.farm <- dt.farm[,list(farmid = 1, S_ER = weighted.mean(x = S_ER, w = B_AREA)),by='indicator']
    
    # the minimum costs required (euro / ha) required for the medals
    if(medalscore=='gold'){mcosts <- 175} else if(medalscore=='silver') {mcosts <- 100} else {mcosts <- 70}
    
    # add the aim
    dt.farm[indicator=='S_ER_SOIL', S_AIM := dt.farm.aim$B_CT_SOIL]
    dt.farm[indicator=='S_ER_WATER', S_AIM := dt.farm.aim$B_CT_WATER]
    dt.farm[indicator=='S_ER_CLIMATE', S_AIM := dt.farm.aim$B_CT_CLIMATE]
    dt.farm[indicator=='S_ER_BIODIVERSITY', S_AIM := dt.farm.aim$B_CT_BIO]
    dt.farm[indicator=='S_ER_LANDSCAPE', S_AIM := dt.farm.aim$B_CT_LANDSCAPE]
    dt.farm[indicator=='S_ER_TOT', S_AIM := sum(dt.farm.aim[,-1])]
    dt.farm[indicator=='S_ER_REWARD', S_AIM := mcosts]
    
    # estimate distance to target
    dt.farm[, D_OPI := round(pmax(0,pmin(100,S_ER * 100 / S_AIM)),0)]
    
    # set a weighing factor on the score per indicator
    dt.farm[, cfOPI := wf(D_OPI, type="score")]
    
    # weighted farm score
    dt.farm.score <- dt.farm[,weighted.mean(x = D_OPI, w = cfOPI)]
    dt.farm.score <- round(dt.farm.score)
    
    # get the distance to target for the five indicators
    dt.farm.ind.score <- dcast(dt.farm,farmid ~ indicator, value.var = 'D_OPI')
  
  # estimate the distance to target for 5 indicators per field
    
    # estimate the total contribution of a single field to the desired score on farm level
    dt[, D_OPI_SOIL := S_ER_SOIL * B_AREA / (dt.farm.aim$B_CT_SOIL * sum(B_AREA))]
    dt[, D_OPI_WATER := S_ER_WATER * B_AREA / (dt.farm.aim$B_CT_WATER * sum(B_AREA))]
    dt[, D_OPI_CLIMATE := S_ER_CLIMATE * B_AREA / (dt.farm.aim$B_CT_CLIMATE * sum(B_AREA))]
    dt[, D_OPI_BIO := S_ER_BIODIVERSITY * B_AREA / (dt.farm.aim$B_CT_BIO * sum(B_AREA))]
    dt[, D_OPI_LANDSCAPE := S_ER_LANDSCAPE * B_AREA / (dt.farm.aim$B_CT_LANDSCAPE * sum(B_AREA))]
    dt[, D_OPI_TOT := S_ER_TOT * B_AREA / (sum(dt.farm.aim[,-1]) * sum(B_AREA))]
    dt[, D_OPI_REWARD := S_ER_REWARD * B_AREA / (mcosts * sum(B_AREA))]
    
    # melt the table
    dt.field <- melt(dt,id.vars = c('id','B_AREA'),
               measure = patterns("D_OPI"), 
               value.name = 'D_OPI',
               variable.name = 'indicator')
   
    # contribution of a single field, optimized between 0 and 100
    dt.field[,D_OPI_SCORE := round(100 * pmax(0,pmin(1,D_OPI)),0)]
    
    # add a correction for the distance to target for reward (10%)
    dt.field[, D_OPI_SCORE := round((0.9 * D_OPI_SCORE) + (10 * dt.farm.ind.score$S_ER_REWARD * 0.01))]
    
    # dcast output
    dt.field <- dcast(dt.field,id~indicator, value.var = 'D_OPI_SCORE')
    
    # rename the column names to scores
    setnames(dt.field, 
             old = c('id' , 'D_OPI_SOIL', 'D_OPI_WATER', 'D_OPI_CLIMATE', 'D_OPI_BIO', 'D_OPI_LANDSCAPE', 'D_OPI_TOT', 'D_OPI_REWARD'),
             new = c("field_id","s_er_soil","s_er_water","s_er_climate","s_er_biodiversity","s_er_landscape","s_er_tot","s_er_reward"))
    
  # collect output
  out <- list(dt.field.ind.score = dt.field,
              dt.farm.ind.score = dt.farm.ind.score,
              dt.farm.score = dt.farm.score)
  
  # return value
  return(out)
  
}