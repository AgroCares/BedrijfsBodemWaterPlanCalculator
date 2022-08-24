#' Calculate the total EcoRegelingen score of five opportunity indicators
#'
#' Estimate the potential to contribute to agronomic and environmental challenges in a region for a farm
#'
#' @param S_ER_TOT (numeric) the Ecoregeling score for the integrative opportunity index for each field
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
er_farm_score <- function(S_ER_TOT,S_ER_SOIL,S_ER_WATER,S_ER_CLIMATE,S_ER_BIODIVERSITY,S_ER_LANDSCAPE, 
                          S_ER_REWARD, B_AREA){
  
  code = value_min = value_max = NULL
  cfSOIL = cfWAT = cfCLIM = cfBIO = cfLAND = S_ER_TOT_WEIGHTED = NULL
  
  # Load bbwp_parms
  bbwp_parms <- BBWPC::bbwp_parms
  
  # check length of the inputs
  arg.length <- max(length(S_ER_TOT),length(S_ER_SOIL),length(S_ER_WATER),length(S_ER_CLIMATE),
                    length(S_ER_BIODIVERSITY),length(S_ER_LANDSCAPE),length(B_AREA))
  
  # check inputs
  checkmate::assert_numeric(S_ER_TOT, lower = 0, upper = 500, len = arg.length)
  checkmate::assert_numeric(S_ER_SOIL, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_WATER, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_CLIMATE, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_BIODIVERSITY, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_LANDSCAPE, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(B_AREA, lower = bbwp_parms[code == "B_AREA", value_min], upper = bbwp_parms[code == "B_AREA", value_max], len = arg.length)
  checkmate::assert_numeric(S_ER_REWARD, lower = 0, upper = 10000, len = arg.length)
  
  # collect data in one data.table
  dt <- data.table(id = 1:arg.length,
                   S_ER_SOIL = S_ER_SOIL,
                   S_ER_WATER = S_ER_WATER,
                   S_ER_CLIMATE = S_ER_CLIMATE,
                   S_ER_BIODIVERSITY = S_ER_BIODIVERSITY,
                   S_ER_LANDSCAPE = S_ER_LANDSCAPE,
                   S_ER_TOT = S_ER_TOT,
                   S_ER_REWARD = S_ER_REWARD,
                   B_AREA = B_AREA
  )
  
  # calculate the integrative opportunity index (risk times impact) (since the S_ER_TOT is the absolute total instead of the weighted total)
  
  # weigh the importance given "distance to target"
  dt[,cfSOIL := wf(S_ER_SOIL, type="score")]
  dt[,cfWAT := wf(S_ER_WATER, type="score")]
  dt[,cfCLIM := wf(S_ER_CLIMATE, type="score")]
  dt[,cfBIO := wf(S_ER_BIODIVERSITY, type="score")]
  dt[,cfLAND := wf(S_ER_LANDSCAPE, type="score")]
  
  # weighted mean
  dt[,S_ER_TOT_WEIGHTED := (S_ER_SOIL * cfSOIL + S_ER_WATER * cfWAT + S_ER_CLIMATE * cfCLIM + S_ER_BIODIVERSITY * cfBIO + S_ER_LANDSCAPE * cfLAND) / 
       (cfSOIL + cfWAT + cfCLIM + cfBIO + cfLAND)]
  
  # keep relevant columns 
  cols <- c('id','S_ER_SOIL','S_ER_WATER','S_ER_CLIMATE','S_ER_BIODIVERSITY','S_ER_LANDSCAPE','S_ER_TOT_WEIGHTED','S_ER_REWARD')
  dt <- dt[, mget(cols)]
  
  # rename the opportunity index in order that the output name of variable that will be returned stays the same
  setnames(dt,'S_ER_TOT_WEIGHTED','S_ER_TOT')
  
  # columns with the score of the opportunity indexes
  cols <- c('S_ER_SOIL','S_ER_WATER','S_ER_CLIMATE','S_ER_BIODIVERSITY','S_ER_LANDSCAPE','S_ER_TOT','S_ER_REWARD')
  
  # calculate area weighted sum of the field indices
  dt <- dt[,lapply(.SD, stats::weighted.mean, w = B_AREA), .SDcols = cols]
  
  # Round the values
  dt <- dt[, lapply(.SD, round, digits = 0)]
  
  # return output
  return(dt)
}
