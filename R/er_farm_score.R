#' Calculate the total EcoRegelingen score of five opportunity indicators
#'
#' Estimate the potential to contribute to agronomic and environmental challenges in a region for a farm
#'
#' @param S_ER_SOIL (numeric) the Ecoregeling scoring index for soil quality for each field
#' @param S_ER_WATER (numeric) the Ecoregeling scoring index for water quality for each field
#' @param S_ER_CLIMATE (numeric) the Ecoregeling scoring index for climate for each field
#' @param S_ER_BIODIVERSITY (numeric) the Ecoregeling scoring index for biodiversity for each field
#' @param S_ER_LANDSCAPE (numeric) the Ecoregeling scoring index for landscape for each field
#' @param S_ER_REWARD (numeric) The financial reward per field for taking Ecoregeling measures (euro / ha)
#' @param B_AREA (numeric) the area of the field (m2) 
#'   
#' @import data.table
#'
#' @export
# calculate the opportunities for a set of fields
er_farm_score <- function(S_ER_SOIL,S_ER_WATER,S_ER_CLIMATE,S_ER_BIODIVERSITY,S_ER_LANDSCAPE, 
                          S_ER_REWARD, B_AREA){
  
  # add visual bindings
  code = value_min = value_max = S_ER_TOT = NULL
  
  # Load bbwp_parms
  bbwp_parms <- BBWPC::bbwp_parms
  
  # check length of the inputs
  arg.length <- max(length(S_ER_SOIL),length(S_ER_WATER),length(S_ER_CLIMATE),
                    length(S_ER_BIODIVERSITY),length(S_ER_LANDSCAPE),length(B_AREA))
  
  # check inputs
  checkmate::assert_numeric(S_ER_REWARD, lower = 0, len = arg.length)
  checkmate::assert_numeric(S_ER_SOIL, lower = 0, len = arg.length)
  checkmate::assert_numeric(S_ER_WATER, lower = 0, len = arg.length)
  checkmate::assert_numeric(S_ER_CLIMATE, lower = 0, len = arg.length)
  checkmate::assert_numeric(S_ER_BIODIVERSITY, lower = 0, len = arg.length)
  checkmate::assert_numeric(S_ER_LANDSCAPE, lower = 0, len = arg.length)
  checkmate::assert_numeric(B_AREA, lower = bbwp_parms[code == "B_AREA", value_min], upper = bbwp_parms[code == "B_AREA", value_max], len = arg.length)
  
  # collect data in one data.table
  dt <- data.table(id = 1:arg.length,
                   S_ER_SOIL = S_ER_SOIL,
                   S_ER_WATER = S_ER_WATER,
                   S_ER_CLIMATE = S_ER_CLIMATE,
                   S_ER_BIODIVERSITY = S_ER_BIODIVERSITY,
                   S_ER_LANDSCAPE = S_ER_LANDSCAPE,
                   S_ER_REWARD = S_ER_REWARD,
                   B_AREA = B_AREA
  )
  
  # add total score
  dt[, S_ER_TOT := S_ER_SOIL + S_ER_WATER + S_ER_CLIMATE + S_ER_BIODIVERSITY + S_ER_LANDSCAPE]
  
  # keep relevant columns 
  cols <- c('S_ER_SOIL','S_ER_WATER','S_ER_CLIMATE','S_ER_BIODIVERSITY','S_ER_LANDSCAPE','S_ER_TOT','S_ER_REWARD')
  dt <- dt[, mget(c('id',cols))]
  
  # calculate area weighted sum of the field indices
  dt <- dt[,lapply(.SD, stats::weighted.mean, w = B_AREA), .SDcols = cols]
  
  # Round the values
  dt <- dt[, lapply(.SD, round, digits = 1)]
  
  # return output
  return(dt)
}
