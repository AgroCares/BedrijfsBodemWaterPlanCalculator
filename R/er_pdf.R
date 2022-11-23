#' Make table of measures that generated scores
#'
#' Present each measure that generated scores on field and/or farm level in table format with scores for soil quality, water quality, climate, biodiversity and landscape given soil type.
#'
#' @param measurescores (boolean) is the pdf requested in function er_meas_scores
#' @param croprotation (boolean) is the pdf requested in function er_croprotation
#' @param dt.field.measures (datatable) field measures
#' @param dt.farm.measures (datatable) farm measures
#' @param B_AREA (numeric) the area of the field (m2) 
#'
#' @import data.table
#'
#' @export
# calculate the opportunities for a set of fields
er_pdf <- function(croprotation,measurescores,dt.field.measures,dt.farm.measures,B_AREA){
  
  # add visual bindings
  . = dt1 = pdf.meas.field = pdf.field.meas.name = pdf.field.measures = level = NULL
  pdf.farm.meas.name = pdf.farm.measures = pdf.field.tot = area_farm = pdf.tot = NULL
  total = euro_ha = euro_farm = bbwp_id = bbwp_measures = B_AREA_tot = NULL
  
  # get internal measures table
  bbwp_measures <- as.data.table(BBWPC::bbwp_measures)
  
  # create table with field measures taken and corresponding scores from er_meas_score 
  if(measurescores == TRUE){
    
    # get applied measures that have a score or reward
    pdf.meas.field <- dt.field.measures[total > 0 | euro_ha > 0 | euro_farm >0,]
    
    # select measures, area and corresponding scores
    pdf.meas.field <- pdf.meas.field[!is.na(bbwp_id), c("bbwp_id","B_AREA","climate","soil","water","landscape","biodiversity","total")]
    
    # get measures summary 
    dt1 <- bbwp_measures[, c("summary","bbwp_id")]
    
    # merge measure summary with applied measures
    pdf.meas.field <- merge(pdf.meas.field,dt1, by = "bbwp_id")
    
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
    pdf.meas.field[, level := "field"]
    setcolorder(pdf.meas.field, c("level","summary"))
    
    # set output
    out <- pdf.meas.field
  } 
    
  
  # create table with field and farm measures taken and corresponding scores from er_croprotation
  if(croprotation == TRUE){
    
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
    pdf.field.measures[, level := "field"]
    setcolorder(pdf.field.measures, c("level","summary"))
    
    # get measures applied on farm level
    pdf.farm.meas.name <- dt.farm.measures[total>0 | euro_farm > 0 | euro_ha > 0, c("bbwp_id")]
    
    # merge measure summary with applied measures
    pdf.farm.meas.name <- merge(pdf.farm.meas.name,dt1, by = "bbwp_id")
    
    # get applied measures and corresponding scores (in score per farm)
    pdf.farm.measures <- merge(pdf.farm.meas.name,dt.farm.measures[!is.na(bbwp_id), c("bbwp_id","climate","soil","water","landscape","biodiversity","total")], by = c('bbwp_id'))
    
    # get total farm area in ha
    area_farm = (sum(B_AREA)/10000)
    
    # convert farm scores to score per ha
    pdf.farm.measures <- pdf.farm.measures[, c(cols):= lapply(.SD, function (x) x / (area_farm)), .SDcols = cols]
    
    # arrange table to right format
    pdf.farm.measures <- pdf.farm.measures[, c('bbwp_id') := NULL][, B_AREA_tot := area_farm]
    pdf.farm.measures[, level := "farm"]
    setcolorder(pdf.farm.measures, c("level","summary"))
    pdf.farm.measures[, c(cols) := round(.SD,1), .SDcols = cols]
   
    # bind farm and field scores
    pdf.farm.field <- rbind(pdf.field.measures,pdf.farm.measures)
    
    # set output
    out <- pdf.meas.field
  }
  
  # return table
  return(out)
  
}
  