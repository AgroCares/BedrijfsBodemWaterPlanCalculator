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
#' @import checkmate
#'
#' @export
er_opi <- function(B_SOILTYPE_AGR, 
                   S_ER_SOIL,S_ER_WATER,S_ER_CLIMATE,S_ER_BIODIVERSITY,S_ER_LANDSCAPE, 
                   S_ER_REWARD, B_AREA,
                   medalscore){
  
  # add visual bindings
  code = value_min = value_max = S_ER_TOT = patterns = indicator = choices = NULL
  S_ER = S_AIM = D_OPI = cfOPI = D_OPI_SOIL = D_OPI_WATER = D_OPI_CLIMATE = D_OPI_LANDSCAPE = D_OPI_BIO = D_OPI_TOT = D_OPI_REWARD = D_OPI_SCORE = NULL
  S_ER_FARM_TOT = D_FS = cfFS = D_OPI_FARM_TOT = NULL
  s_er_soil = s_er_water = s_er_climate = s_er_landscape = s_er_biodiversity = s_er_farm_tot = s_er_costs = s_er_tot = NULL
  
  
  # Load bbwp_parms
  bbwp_parms <- BBWPC::bbwp_parms
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(S_ER_SOIL),length(S_ER_WATER),length(S_ER_CLIMATE),
                    length(S_ER_BIODIVERSITY),length(S_ER_LANDSCAPE),length(B_AREA))
  
  # check inputs
  checkmate::assert_numeric(S_ER_REWARD, lower = 0, len = arg.length)
  checkmate::assert_numeric(S_ER_SOIL, lower = 0, len = arg.length)
  checkmate::assert_numeric(S_ER_WATER, lower = 0, len = arg.length)
  checkmate::assert_numeric(S_ER_CLIMATE, lower = 0, len = arg.length)
  checkmate::assert_numeric(S_ER_BIODIVERSITY, lower = 0, len = arg.length)
  checkmate::assert_numeric(S_ER_LANDSCAPE, lower = 0, len = arg.length)
  checkmate::assert_numeric(B_AREA, lower = 10, upper = bbwp_parms[code == "B_AREA", value_max], len = arg.length)
  checkmate::assert_character(B_SOILTYPE_AGR,len = arg.length)
  checkmate::assert_character(medalscore,len = 1)
  checkmate::assert_subset(medalscore, choices = c('bronze','silver','gold'))
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unlist(bbwp_parms[code == "B_SOILTYPE_AGR", choices]))
  
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
  
  # count the total score per field(scores / ha) 
  dt[, S_ER_FARM_TOT := pmax((S_ER_SOIL + S_ER_WATER + S_ER_CLIMATE + S_ER_BIODIVERSITY + S_ER_LANDSCAPE),0.001)]
  
  # estimate the farm scores
  
    # melt the table to estimate farm score
    dt.farm <- melt(dt,id.vars = c('id','B_AREA','B_SOILTYPE_AGR'),
                    measure = patterns("S_ER"),
                    value.name = 'S_ER',
                    variable.name = 'indicator')

    # estimate the mean score (score / ha) per indicator for the whole farm
    dt.farm <- dt.farm[,list(farmid = 1, S_ER = weighted.mean(x = S_ER, w = B_AREA)),by='indicator']

    # the minimum costs required (euro / ha) required for the medals
    if(medalscore=='gold'){mcosts <- 200} else if(medalscore=='silver') {mcosts <- 100} else {mcosts <- 60}

    # add the aim
    dt.farm[indicator=='S_ER_SOIL', S_AIM := dt.farm.aim$B_CT_SOIL]
    dt.farm[indicator=='S_ER_WATER', S_AIM := max(dt.farm.aim$B_CT_WATER,0.001)]
    dt.farm[indicator=='S_ER_CLIMATE', S_AIM := dt.farm.aim$B_CT_CLIMATE]
    dt.farm[indicator=='S_ER_BIODIVERSITY', S_AIM := dt.farm.aim$B_CT_BIO]
    dt.farm[indicator=='S_ER_LANDSCAPE', S_AIM := max(dt.farm.aim$B_CT_LANDSCAPE,0.001)]
    dt.farm[indicator=='S_ER_REWARD', S_AIM := mcosts]
    dt.farm[indicator=='S_ER_FARM_TOT', S_AIM :=  dt.farm.aim$B_CT_FARM_TOT]

    # estimate distance to target between 0-100
    dt.farm[, D_OPI := round(pmax(0,pmin(100,S_ER * 100 / S_AIM)),0)]

    # set a weighting factor on the score per indicator
    dt.farm[, cfOPI := wf(D_OPI, type="score")]

    # weighted farm score (based on distance to target, relative)
    dt.farm.score <- dt.farm[,weighted.mean(x = D_OPI, w = cfOPI)]
    dt.farm.score <- round(dt.farm.score)

    # get the farm mean scores for the five indicators, farm total and costs
    dt.farm.ind.score <- dcast(dt.farm,farmid ~ indicator, value.var = 'S_ER')
    dt.farm.ind.score[,c('farmid') := NULL]
    
    # rename reward column to costs 
    setnames(dt.farm.ind.score,"S_ER_REWARD","S_ER_COSTS")
    
    # round values
    cols <- colnames(dt.farm.ind.score)[grepl('S_ER',colnames(dt.farm.ind.score))]
    dt.farm.ind.score[,c(cols) := lapply(.SD,round,1),.SDcols = cols]
    
    # get the distance to target for the five indicators
    dt.farm.ind.opi <- dcast(dt.farm,farmid ~ indicator, value.var = 'D_OPI')
    
    # rename reward to costs
    setnames(dt.farm.ind.opi,"S_ER_REWARD","S_ER_COSTS")
    
  # estimate the distance to target for 5 indicators per field
  # dt.field is distance to target and will be implemented in the ecoregeling tool later, currently not used
    
    # estimate the total contribution of a single field to the desired score on farm level (add 0.001 to water and landscape indicators for in case these are zero)
    dt[, D_OPI_SOIL := S_ER_SOIL * B_AREA / (dt.farm.aim$B_CT_SOIL * sum(B_AREA))]
    dt[, D_OPI_WATER := S_ER_WATER * B_AREA / (max(dt.farm.aim$B_CT_WATER,0.001) * sum(B_AREA))]
    dt[, D_OPI_CLIMATE := S_ER_CLIMATE * B_AREA / (dt.farm.aim$B_CT_CLIMATE * sum(B_AREA))]
    dt[, D_OPI_BIO := S_ER_BIODIVERSITY * B_AREA / (dt.farm.aim$B_CT_BIO * sum(B_AREA))]
    dt[, D_OPI_LANDSCAPE := S_ER_LANDSCAPE * B_AREA / (max(dt.farm.aim$B_CT_LANDSCAPE,0.001) * sum(B_AREA))]
    dt[, D_OPI_FARM_TOT := S_ER_FARM_TOT * B_AREA / (dt.farm.aim$B_CT_FARM_TOT * sum(B_AREA))]
    dt[, D_OPI_REWARD := S_ER_REWARD * B_AREA / (mcosts * sum(B_AREA))]
    dt[, D_OPI_TOT := S_ER_FARM_TOT * B_AREA / (dt.farm.aim$B_CT_FARM_TOT * sum(B_AREA))]
    
    # melt the table
    dt.field <- melt(dt,id.vars = c('id','B_AREA'),
                        measure = patterns("D_OPI"), 
                        value.name = 'D_OPI',
                        variable.name = 'indicator')
   
    # contribution of a single field, optimized between 0 and 100
    dt.field[,D_OPI_SCORE := round(100 * pmax(0,pmin(1,D_OPI)),0)]
    
    # add a correction for the distance to target for reward (10%)
    dt.field[indicator != "D_OPI_FARM_TOT", D_OPI_SCORE := round((0.9 * D_OPI_SCORE) + (10 * dt.farm.ind.opi$S_ER_COSTS * 0.01))]
    
    # dcast output
    dt.field <- dcast(dt.field,id~indicator, value.var = 'D_OPI_SCORE')
    
    # rename the column names to scores
    setnames(dt.field, 
             old = c('id' , 'D_OPI_SOIL', 'D_OPI_WATER', 'D_OPI_CLIMATE', 'D_OPI_BIO', 
                     'D_OPI_LANDSCAPE', 'D_OPI_FARM_TOT', 'D_OPI_REWARD', 'D_OPI_TOT'),
             new = c("field_id","s_er_soil","s_er_water","s_er_climate","s_er_biodiversity",
                     "s_er_landscape","s_er_farm_tot","s_er_costs","s_er_tot"))
    
  # estimate the absolute fieldscores with maxima
    
    # set colnames dt to lower case
    setnames(dt, tolower(colnames(dt)))
    
    # set colnames S_ER_REWARD to s_er_costs
    setnames(dt, c("s_er_reward","id"),c("s_er_costs","field_id"))
    
    # set maximum for eco scores and total farm score on field level
    # dt[, s_er_soil := pmin(15,s_er_soil)]
    # dt[, s_er_water := pmin(15,s_er_water)]
    # dt[, s_er_climate := pmin(15,s_er_climate)]
    # dt[, s_er_biodiversity := pmin(15,s_er_biodiversity)]
    # dt[, s_er_landscape := pmin(1,s_er_landscape)]
    # dt[, s_er_farm_tot:= pmin(50,s_er_farm_tot)]
    
    # set maximum for s_er_costs at 200 and convert to percentage 
    dt[, s_er_costs := (pmin(200,s_er_costs)/200)*100]
  
    # set s_er_tot equal to s_er_tot calculated in dt.field (distance to target) 
    dt[, s_er_tot := dt.field$s_er_tot]  
    
    # select columns to used in output
    cols <- colnames(dt)[grepl('s_er|id',colnames(dt))]
    dt <- dt[, mget(cols)] 
    
    # round values
    dt[,c(cols) := lapply(.SD,round,1),.SDcols = cols]

  # collect output
  out <- list(
              # the relative contribution of a single field to the farm objective
              # dt.field.ind.score = dt.field
              # the absolute score per field
              dt.field.ind.score = dt,
              # the averaged farm score (mean scores / ha, mean costs / ha)
              dt.farm.ind.score = dt.farm.ind.score,
              # the BBWP score, being overall distance to target
              dt.farm.score = dt.farm.score,
              # the relative farm score per theme
              dt.farm.ind.opi = dt.farm.ind.opi)

  
  # return value
  return(out)
  
}