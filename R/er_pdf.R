er_pdf


# get measures applied on field level
pdf.field.meas.name <- dt2[total>0 | euro_ha > 0, c("id","bbwp_id")]

# get measures summary 
dt7 <- bbwp_measures[, c("summary","bbwp_id")]

# merge measure summary with applied measures
pdf.field.meas.name <- merge(pdf.field.meas.name,dt7, by = "bbwp_id")

# get applied measures and corresponding scores
pdf.field.score <- dt2[!is.na(bbwp_id), c("bbwp_id","id","B_AREA","climate","soil","water","landscape","biodiversity","total")]

# merge field measure names with field measure scores
pdf.field.measures <- merge(pdf.field.meas.name, pdf.field.score, by = c('id','bbwp_id'))

# convert area to ha
pdf.field.measures <- pdf.field.measures[, B_AREA := B_AREA/10000]

# add up scores and area if measures are applied on multiple fields
pdf.field.measures <- pdf.field.measures[, B_AREA_tot := sum(B_AREA), by = "summary"]

# get cols
cols <- c('climate','soil','water','landscape','biodiversity','total')

# calculate weighted mean of the scores
pdf.field.measures <- pdf.field.measures[,lapply(.SD,weighted.mean,w = B_AREA), by = c("summary","bbwp_id","B_AREA_tot"),.SDcols = cols]

# arrange table to right format
pdf.field.measures <- pdf.field.measures[, c('bbwp_id') := NULL]
setcolorder(pdf.field.measures, c("summary"))

# get measures applied on farm level
# calculate the average ER score (points/ ha) for the whole farm due to measures taken
pdf.field.meas.name <- dt2[total>0 | euro_ha > 0, c("id","bbwp_id")]
#pdf.field.score <- dt2[total > 0 | euro_ha > 0,lapply(.SD,function(x) (x*B_AREA)/dt.farm$area_farm), .SDcols = cols]

