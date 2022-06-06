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
#' @param D_AREA (numeric) the area of the field (\ m2 or \ ha) 
#'   
#' @import data.table
#'
#' @export
# calculate the opportunities for a set of fields
er_farm_score <- function(S_ER_TOT,S_ER_SOIL,S_ER_WATER,S_ER_CLIMATE,S_ER_BIODIVERSITY,S_ER_LANDSCAPE, 
                          D_AREA){
  
  # check length of the inputs
  arg.length <- max(length(S_ER_TOT),length(S_ER_SOIL),length(S_ER_WATER),length(S_ER_CLIMATE),
                    length(S_ER_BIODIVERSITY),length(S_ER_LANDSCAPE),length(D_AREA))
  
  # check inputs
  checkmate::assert_numeric(S_ER_TOT, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_SOIL, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_WATER, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_CLIMATE, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_BIODIVERSITY, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_LANDSCAPE, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(D_AREA, lower = 0, upper = 50000, len = arg.length)
  
  # collect data in one data.table
  dt <- data.table(id = 1:arg.length,
                   S_ER_SOIL = S_ER_SOIL,
                   S_ER_WATER = S_ER_WATER,
                   S_ER_CLIMATE = S_ER_CLIMATE,
                   S_ER_BIODIVERSITY = S_ER_BIODIVERSITY,
                   S_ER_LANDSCAPE = S_ER_LANDSCAPE,
                   S_ER_TOT = S_ER_TOT,
                   D_AREA = D_AREA
                  )
  
  # columns with the score of the opportunity indexes
  cols <- c('S_ER_TOT','S_ER_SOIL','S_ER_WATER','S_ER_CLIMATE','S_ER_BIODIVERSITY','S_ER_LANDSCAPE')
  
  # calculate area weigthed sum of the field indices
  dt <- dt[,lapply(.SD, stats::weighted.mean, w = D_AREA), .SDcols = cols]
  
  # Round the values
  dt<- dt[, lapply(.SD, round, digits = 0)]
  
  # add scores from farm measures taken???
  
  # return output
  return(dt)
}
