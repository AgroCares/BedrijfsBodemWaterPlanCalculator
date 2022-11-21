#' Calculate the crop rotation based total score for five opportunity indicators
#'
#' Estimate the actual contribution of crop rotation given aims for soil quality, water quality, climate, biodiversity and landscape
#'
#' @param meas_scores (boolean) is the pdf needed for function er_meas_scores
#' @param croprotation (boolean) is the pdf needed for function er_croprotation
#' @param dt.field.measures (datatable) field measures
#' @param dt.farm.measures (datatable) field measures

#'
#' @import data.table
#' @import stats
#'
#' @export
# calculate the opportunities for a set of fields
er_pdf <- function(croprotation,meas_scores,dt.field.measures,dt.farm.measures){
  
  # add visual bindings
  eco_id = type = fr_area = id = er_urgency = NULL

  
  
  
  
  # should pdf function be used in er_meas_scores? 
  #dt.field.measures is dt2 in er_meas_scores
  if(meas_scores = TRUE){
    
    # get applied measures that have a score or reward
    pdf.meas.field <- dt.field.measures[total > 0 | euro_ha > 0 | euro_farm >0,]
    
    # select measures, area and corresponding scores
    pdf.meas.field <- pdf.meas.field[!is.na(bbwp_id), c("bbwp_id","B_AREA","climate","soil","water","landscape","biodiversity","total")]
    
    # get measures summary 
    dt1 <- bbwp_measures[, c("summary","bbwp_id")]
    
    # merge measure summary with applied measures
    pdf.meas.field <- merge(pdf.meas.field,dt1, on = "bbwp_id")
    
    # convert area to ha
    pdf.meas.field <- pdf.meas.field[, B_AREA := B_AREA/10000]
    
    # add up scores and area if measures are applied on multiple fields
      # get total area of the measures applied on multiple fields
      pdf.meas.field <- pdf.meas.field[, B_AREA_tot := sum(B_AREA), by = "summary"]
      
      # get cols
      cols <- c('climate','soil','water','landscape','biodiversity','total')
      
      # calculate weighted mean of the scores
      pdf.meas.field <- pdf.meas.field[,lapply(.SD,weighted.mean,w = B_AREA), by = c("summary","bbwp_id","B_AREA_tot"),.SDcols = cols]
      
    # arrange table to right format
    pdf.meas.field <- pdf.meas.field[, bbwp_id := NULL]
    setcolorder(pdf.meas.field, c("summary"))
    
  }
    
  
  
  # should pdf function be used in er_croprotation? 
  #dt.field.measures is dt2 in er_croprotation
  #dt.farm.measures is dt4 in er_croprotation
  
  if(croprotation = TRUE){
    
    # get measures applied on field level
    pdf.field.meas.name <- dt.field.measures[total>0 | euro_ha > 0, c("id","bbwp_id")]
    
    # get measures summary 
    dt1 <- bbwp_measures[, c("summary","bbwp_id")]
    
    # merge measure summary with applied measures
    pdf.field.meas.name <- merge(pdf.field.meas.name,dt1, by = "bbwp_id")
    
    # get applied measures and corresponding scores and merge with names field measures
    pdf.field.measures <- merge(pdf.field.meas.name, dt.field.measures[!is.na(bbwp_id), c("bbwp_id","id","B_AREA","climate","soil","water","landscape","biodiversity","total")], by = c('id','bbwp_id'))
    
    # convert area to ha
    pdf.field.measures <- pdf.field.measures[, B_AREA := B_AREA/10000]
    
    # add up scores and area if measures are applied on multiple fields
      # get total area of the measures applied on multiple fields
      pdf.field.measures <- pdf.field.measures[, B_AREA_tot := sum(B_AREA), by = "summary"]
      
      # get cols
      cols <- c('climate','soil','water','landscape','biodiversity','total')
      
      # calculate weighted mean of the scores
      pdf.field.measures <- pdf.field.measures[,lapply(.SD,weighted.mean,w = B_AREA), by = c("summary","bbwp_id","B_AREA_tot"),.SDcols = cols]
      
    # arrange table to right format
    pdf.field.measures <- pdf.field.measures[, c('bbwp_id') := NULL]
    setcolorder(pdf.field.measures, c("summary"))
    
    # get measures applied on farm level
    pdf.farm.meas.name <- dt.farm.measures[total>0 | euro_farm > 0 | euro_ha > 0, c("bbwp_id")]
    
    # merge measure summary with applied measures
    pdf.farm.meas.name <- merge(pdf.farm.meas.name,dt1, by = "bbwp_id")
    
    # get applied measures and corresponding scores (in score per farm)
    pdf.farm.measures <- merge(pdf.farm.meas.name,dt.farm.measures[!is.na(bbwp_id), c("bbwp_id","climate","soil","water","landscape","biodiversity","total")], by = c('bbwp_id'))
    
    # convert farm scores to score per ha
    pdf.farm.measures <- pdf.field.farm.score[, c(cols):= lapply(.SD, function (x) x / (dt.farm$area_farm/10000)), .SDcols = cols]
    
    # arrange table to right format
    pdf.farm.measures <- pdf.farm.measures[, c('bbwp_id') := NULL]
    setcolorder(pdf.farm.measures, c("summary"))
  }
  
  
  # hier een tabel van  maken 
  
  pdf.meas.field
  
  pdf.field.measures
  
  pdf.farm.measures
  
  
  
  
  
  # return table, with 
  return(out)
  
}
  