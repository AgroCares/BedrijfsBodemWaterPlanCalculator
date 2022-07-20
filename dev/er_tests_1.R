
# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# de bron data
B_SOILTYPE_AGR = c('dekzand')
B_GWL_CLASS = c('GtIII')
A_P_SG = c(12)
B_SLOPE_DEGREE = c(1.5)
B_AER_CBS = c('LG05')
B_LU_BBWP = c(2)
B_LU_ECO1 = c(F)
B_LU_ECO2 = c(F)
B_LU_ECO3 = c(F)
B_LU_ECO4 = c(F)
B_LU_ECO5 = c(T)
B_LU_ECO6 = c(F) 
B_LU_ECO7 = c(F)
B_LU_ECO8 = c(F)
B_LU_ECO9 = c(T)
B_LU_ECO10 = c(T)
M_DRAIN = c(TRUE)
D_SA_W = c(0.5)
B_AREA = c(10)
farmscore = 100
medalscore = "gold"
measures = measures
sector = c('dairy')
output = 'scores'
list1 = list2 = list3 = list()

for(i in 1:nrow(dt.measures)){

  # assume the measures are not for GLB / ANLB
  measures <- rbind(data.table(id = 1, dt.measures[i]))
  measures$bbwp_status <- 'noting'

  aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR,B_AREA = B_AREA, medalscore = medalscore)

  field <- er_field_scores(B_SOILTYPE_AGR, B_AER_CBS,B_AREA,
                           B_LU_BBWP,B_LU_ECO1,B_LU_ECO2, B_LU_ECO3, B_LU_ECO4, B_LU_ECO5, 
                           B_LU_ECO6, B_LU_ECO7,B_LU_ECO8, B_LU_ECO9,B_LU_ECO10,
                           B_CT_SOIL =aim$B_CT_SOIL, 
                           B_CT_WATER = aim$B_CT_WATER,
                           B_CT_CLIMATE = aim$B_CT_CLIMATE,
                           B_CT_BIO = aim$B_CT_BIO,
                           B_CT_LANDSCAPE = aim$B_CT_LANDSCAPE, 
                           measures = measures, sector)
  list1[[i]] <- copy(cbind(field,aim))
  
  # get original scores for crop rotation and field level of measures
  
    # check and update the measure table
    dt.er.meas <- bbwp_check_meas(measures, eco = TRUE, score = TRUE)
    
    # get internal table with importance of environmental challenges
    dt.er.scoring <- as.data.table(BBWPC::er_scoring)
    setnames(dt.er.scoring,gsub('cf_','',colnames(dt.er.scoring)))
    dt.er.urgency <- melt(dt.er.scoring[type=='urgency'],
                          id.vars='soiltype',
                          measure.vars = c('soil', 'water', 'climate',  'biodiversity', 'landscape'),
                          variable.name = 'indicator',
                          value.name = 'urgency')
    
    # collect data in one data.table
    dt <- data.table(id = 1:arg.length,
                     B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                     B_LU_BBWP = B_LU_BBWP,
                     B_LU_ECO1 = B_LU_ECO1,
                     B_LU_ECO2 = B_LU_ECO2,
                     B_LU_ECO3 = B_LU_ECO3,
                     B_LU_ECO4 = B_LU_ECO4,
                     B_LU_ECO5 = B_LU_ECO5,
                     B_LU_ECO6 = B_LU_ECO6,
                     B_LU_ECO7 = B_LU_ECO7,
                     B_LU_ECO8 = B_LU_ECO8,
                     B_LU_ECO9 = B_LU_ECO9,
                     B_LU_ECO10 = B_LU_ECO10,
                     B_AER_CBS = B_AER_CBS,
                     B_AREA = B_AREA,
                     B_CT_SOIL = B_CT_SOIL, 
                     B_CT_WATER = B_CT_WATER,
                     B_CT_CLIMATE = B_CT_CLIMATE,
                     B_CT_BIO = B_CT_BIO,
                     B_CT_LANDSCAPE = B_CT_LANDSCAPE
    )
    
    # columns with the Ecoregelingen ranks
    cols <- c('soil','water','biodiversity','climate','landscape')
    
    # add the generic farm score as baseline
    # this gives the averaged ER score based on the crops in crop rotation plan
    dt.farm <- er_croprotation(B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR,
                               B_LU_BBWP = dt$B_LU_BBWP,
                               B_LU_ECO1 = dt$B_LU_ECO1,
                               B_LU_ECO2 = dt$B_LU_ECO2,
                               B_LU_ECO3 = dt$B_LU_ECO3,
                               B_LU_ECO4 = dt$B_LU_ECO4,
                               B_LU_ECO5 = dt$B_LU_ECO5,
                               B_LU_ECO6 = dt$B_LU_ECO6,
                               B_LU_ECO7 = dt$B_LU_ECO7,
                               B_LU_ECO8 = dt$B_LU_ECO8,
                               B_LU_ECO9 = dt$B_LU_ECO9,
                               B_LU_ECO10 = dt$B_LU_ECO10,
                               B_AER_CBS = dt$B_AER_CBS,
                               B_AREA = dt$B_AREA,
                               B_CT_SOIL = dt$B_CT_SOIL,
                               B_CT_WATER = dt$B_CT_WATER,
                               B_CT_CLIMATE = dt$B_CT_CLIMATE,
                               B_CT_BIO = dt$B_CT_BIO,
                               B_CT_LANDSCAPE = dt$B_CT_LANDSCAPE,
                               measures = measures,
                               sector = sector)
  
    list2[[i]] <- copy(dt.farm)
    
    # calculate the change in opportunity indexes given the measures taken
    
    # column names for impact of measures on the five indexes (do not change order)
    # these are not yet converted to a 0-1 scale
    
    # set colnames for the impact of measures
    mcols <- c('D_MEAS_BIO', 'D_MEAS_CLIM', 'D_MEAS_LAND', 'D_MEAS_SOIL', 'D_MEAS_WAT','S_ER_REWARD')
    
    # calculate the total score per indicator 
    if(nrow(dt.er.meas) > 0){
      
      # calculate
      dt.meas.impact <- er_meas_score(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                                      B_LU_BBWP = B_LU_BBWP,
                                      B_LU_ECO1 = B_LU_ECO1,
                                      B_LU_ECO2 = B_LU_ECO2,
                                      B_LU_ECO3 = B_LU_ECO3,
                                      B_LU_ECO4 = B_LU_ECO4,
                                      B_LU_ECO5 = B_LU_ECO5,
                                      B_LU_ECO6 = B_LU_ECO6,
                                      B_LU_ECO7 = B_LU_ECO7,
                                      B_LU_ECO8 = B_LU_ECO8,
                                      B_LU_ECO9 = B_LU_ECO9,
                                      B_LU_ECO10 = B_LU_ECO10,
                                      B_AER_CBS = B_AER_CBS,
                                      B_AREA = B_AREA,
                                      measures = measures, 
                                      sector = sector)
      
      # merge with dt
      dt <- merge(dt,dt.meas.impact,by='id')
      
      # set NA to zero
      dt[,c(mcols) := lapply(.SD,function(x) fifelse(is.na(x),0,x)),.SDcols = mcols]
      
    } else {
      
      # set impact of management to zero when no measures are applied
      dt[,c(mcols) := list(0,0,0,0,0,0)]
    }
    
  list3[[i]] <- copy(dt)
  
  # print
  print(i)
}

# field scores and aim
dt1 <- rbindlist(list1)
# dt.farm score used within field score
dt2 <- rbindlist(list2)
# field scores
dt3 <- rbindlist(list3)
fwrite(dt1,'dev/er_fieldscores_1.csv')
fwrite(dt3,'dev/er_fieldscores_1uncor.csv')


