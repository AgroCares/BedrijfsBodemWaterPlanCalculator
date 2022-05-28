#' Calculate the total EcoRegelingen score of five opportunity indicators
#'
#' Estimate the potential to contribute to agronomic and environmental challenges in a region for a farm
#'
#' @param D_OPI_TOT (numeric) the Ecoregeling score for the integrative opportunity index for each field
#' @param D_OPI_SOIL (numeric) the Ecoregeling scoring index for soil quality for each field
#' @param D_OPI_WATER (numeric) the Ecoregeling scoring index for water quality for each field
#' @param D_OPI_CLIMATE (numeric) the Ecoregeling scoring index for climate for each field
#' @param D_OPI_BIO (numeric) the Ecoregeling scoring index for biodiversity for each field
#' @param D_OPI_LANDSCAPE (numeric) the Ecoregeling scoring index for landscape for each field
#' @param D_AREA (numeric) the area of the field (\ m2 or \ ha) 
#'   
#' @import data.table
#'
#' @export
# calculate the opportunities for a set of fields
er_farm_score <- function(D_OPI_TOT,D_OPI_SOIL,D_OPI_WATER,D_OPI_CLIMATE,D_OPI_BIO,D_OPI_LANDSCAPE, 
                          D_AREA){
  
  # check length of the inputs
  arg.length <- max(length(D_OPI_TOT),length(D_OPI_SOIL),length(D_OPI_WATER),length(D_OPI_CLIMATE),
                    length(D_OPI_BIO),length(D_OPI_LANDSCAPE),length(D_AREA))
  
  # check inputs
  checkmate::assert_numeric(D_OPI_TOT, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(D_OPI_SOIL, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(D_OPI_WATER, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(D_OPI_CLIMATE, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(D_OPI_BIO, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(D_OPI_LANDSCAPE, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(D_AREA, lower = 0, upper = 50000)
  
  # collect data in one data.table
  dt <- data.table(
    id = 1:arg.length,
    D_OPI_SOIL = D_OPI_SOIL,
    D_OPI_WATER = D_OPI_WATER,
    D_OPI_CLIMATE = D_OPI_CLIMATE,
    D_OPI_BIO = D_OPI_BIO,
    D_OPI_LANDSCAPE = D_OPI_LANDSCAPE,
    D_OPI_TOT = D_OPI_TOT,
    D_AREA = D_AREA
  )
  
  # columns with the score of the opportunity indexes
  cols <- c('D_OPI_TOT','D_OPI_SOIL','D_OPI_WATER','D_OPI_CLIMATE','D_OPI_BIO','D_OPI_LANDSCAPE')
  
  # calculate area weigthed sum of the field indices
  dt <- dt[,lapply(.SD, stats::weighted.mean, w = D_AREA), .SDcols = cols]
  
  # Round the values
  dt<- dt[, lapply(.SD, round, digits = 0)]
  
  # add scores from farm measures taken???
  
  # return output
  return(dt)
}
