#' Calculate the precipitation surplus
#'
#' This function calculates the precipitation surplus (in mm / ha) given the crop rotation plan. Is identical to OBIC function calc_psp but much faster to facilitate national and regional calculations
#'
#' @param ID (character) A field id
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, options: TRUE, FALSE)
#' @param M_GREEN_START (integer) Month in which the green manure is sown
#' @param M_GREEN_TERMINATE (integer) Month in which the green manure is terminated
#'
#' @examples
#' bln_calc_psp(ID = 1,B_LU_BRP = 265, M_GREEN = TRUE)
#' bln_calc_psp(ID = c(1,2,1,2),B_LU_BRP = c(265,1019,265,1019), M_GREEN = rep(TRUE,4))
#' bln_calc_psp(ID = c(1,2),B_LU_BRP = c(2014, 2767), M_GREEN = rep(TRUE,2),
#'              M_GREEN_START = c(10, 11), M_GREEN_TERMINATE = c(12, 3))
#'
#' @return
#' The estimated precipitation surplus (in mm / ha) depending on averaged precipitation and evaporation. A numeric value.
#'
#' @export
bln_calc_psp <- function(ID, B_LU_BRP, M_GREEN, M_GREEN_START = 10L, M_GREEN_TERMINATE = 3L){
  
  # set visual bindings
  crop_code = crop_name = crop_makkink = psp = A_PREC_MEAN = A_ET_MEAN = mcf = . = NULL
  year_wc = year_wc2 = oid = NULL
  
  # Check input
  arg.length <- max(length(B_LU_BRP), length(M_GREEN),length(ID),
                    length(M_GREEN_START), length(M_GREEN_TERMINATE))
  
  # check inputs
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(OBIC::crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_logical(M_GREEN,any.missing = FALSE, len = arg.length)
  checkmate::assert_integer(M_GREEN_START, lower = 8, upper = 12)
  checkmate::assert_integer(M_GREEN_TERMINATE)
  checkmate::assert_subset(M_GREEN_TERMINATE, choices = c(10, 11, 12, 1, 2, 3, 4))
  
  # Load in the datasets
  dt.weather <- OBIC::weather.obic
  crops.obic <- as.data.table(OBIC::crops.obic)
  crops.makkink <- as.data.table(OBIC::crops.makkink)
  setnames(crops.makkink,old = c('crop_makkink',1:12),new=c('crop_makkink',paste0('m',1:12)))
  
  # Collect input data in a table
  dt <- data.table(ID = ID,B_LU_BRP = B_LU_BRP,M_GREEN = M_GREEN,oid = 1:arg.length,
                   M_GREEN_START, M_GREEN_TERMINATE)
  
  # merge with obic crop
  dt <- merge(dt, crops.obic[, list(crop_code, crop_name, crop_makkink)], by.x = "B_LU_BRP", by.y = "crop_code")
  
  # merge with makkink fractions
  dt <- merge(dt, crops.makkink, by = "crop_makkink")
  
  # add year
  dt[,year:= 1:.N,by=ID]
  
  # Order by year
  setorder(dt,ID,year)
  
  # reshape
  dtm <- melt(dt, measure.vars = paste0('m',1:12),
              variable.name = 'month', value.name = 'mcf')
  setorder(dtm, oid, month)
  dtm[, month := as.numeric(gsub('m', '', month))]
  
  # change makking factor for green manure months end of the year
  dtm[M_GREEN == TRUE & mcf < 0.6 & # only overwrite makkink if there is no regular crop expected here
        month >= M_GREEN_START & # select months after start
        (!month == M_GREEN_TERMINATE| (month > M_GREEN_TERMINATE & month > M_GREEN_START)),# do not select months of termination of after termination and before new year
      mcf := 0.6]
  
  # change makking factor for green manure months beginning of the year
  dtm[M_GREEN == TRUE & mcf < 0.6 &
        M_GREEN_START > M_GREEN_TERMINATE & # if TERMINATE is smaller than START, TERMINATION happens in the new year
        month <= M_GREEN_TERMINATE,
      mcf := 0.6]
  
  # merge with weather
  dtm <- merge(dtm, dt.weather, by='month')
  
  # calculate precipitation surplus
  dtm[, psp := A_PREC_MEAN - A_ET_MEAN * mcf]
  
  # calculate the precipitation surplus per year
  out <- dtm[,list(psp = sum(psp)),by = c("oid")]
  
  # reset order to input order
  setorder(out,oid)
  
  # return value
  D_PSP <- out[,psp]
  
  # return
  return(D_PSP)
  
}