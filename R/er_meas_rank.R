#' Rank the suitability of agronomic measures to improve soil, climate, biodiversity and water management for a given field
#'
#' Estimate the Ecoregelingen score for agronomic measures to improve soil and water management on agricultural farms. 
#' And send an ordered list back of the most suitable measures.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BBWP (character) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_LU_ARABLE_ER (boolean) does the crop fall within the ER category "arable"
#' @param B_LU_PRODUCTIVE_ER (boolean) does the crop fall within the ER category "productive"
#' @param B_LU_CULTIVATED_ER (boolean) does the crop fall within the ER category "cultivated"
#' @param A_P_SG (numeric) 
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param M_DRAIN (boolean) is there tube drainage present in the field
#' @param B_AREA (numeric) the area of the field (m2) 
#' @param D_SA_W (numeric) The wet perimeter index of the field, fraction that field is surrounded by water
#' @param B_CT_SOIL (numeric) the target value for soil quality conform Ecoregeling scoring (score / ha)
#' @param B_CT_WATER (numeric) the target value for water quality conform Ecoregeling scoring (score / ha)
#' @param B_CT_CLIMATE (numeric) the target value for climate conform Ecoregeling scoring (score / ha)
#' @param B_CT_BIO (numeric) the target value for biodiversity conform Ecoregeling scoring (score / ha)
#' @param B_CT_LANDSCAPE (numeric) the target value for landscape quality conform Ecoregeling scoring (score / ha)
#' @param measures (data.table) table with the properties of the available measures
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'diary', 'arable', 'tree_nursery', 'bulbs')
#' 
#'   
#' @import data.table
#'
#' @export
# rank the measures given their effectiveness to improve the sustainability of the farm
er_meas_rank <- function(B_SOILTYPE_AGR, B_GWL_CLASS, A_P_SG, B_SLOPE_DEGREE, M_DRAIN, D_SA_W,
                         B_AREA,B_AER_CBS,
                         B_LU_BBWP,B_LU_BRP,
                         B_LU_ARABLE_ER, B_LU_PRODUCTIVE_ER,B_LU_CULTIVATED_ER,
                         B_CT_SOIL, B_CT_WATER,B_CT_CLIMATE,B_CT_BIO,B_CT_LANDSCAPE, 
                         measures, sector){
  
  # add visual bindings
  eco_id = type = fr_area = id = bbwp_id = NULL
  fsector = fdairy = dairy = farable = arable = ftree_nursery = tree_nursery = fbulbs = bulbs = NULL
  level = nc1 = nc2 = nc3 = nc4 = nc5 = nc6 = nc7 = nc8 = nc9 = nc10 = nc11 = nc12 = NULL
  soiltype = peat = clay = sand = silt = loess = ec1 = ec2 = NULL
  er_water = cf_water = er_soil = cf_soil = er_climate = cf_climate = er_biodiversity = cf_biodiversity = er_landscape = cf_landscape = NULL
  er_total = B_AREA_FARM = er_reward = er_total_scaled = er_soil_scaled = er_water_scaled = NULL
  er_climate_scaled = er_biodiversity_scaled = er_landscape_scaled= NULL
  er_euro_combi = er_euro_ha = er_euro_farm = er_reward_scaled = oid = NULL
  eco_app = b_lu_arable_er = b_lu_productive_er = b_lu_cultivated_er = NULL
  code = value_min = value_max = choices = NULL
  
  # Load bbwp_parms
  bbwp_parms <- BBWPC::bbwp_parms
  
  # derive a table with all possible field measurements
  dt.meas.av <- bbwp_check_meas(dt = NULL,eco = TRUE, score = FALSE)
  dt.meas.av <- dt.meas.av[level=='field']
  
  # get internal table which measures are applicable on which crops
  dt.meas.eco <- as.data.table(BBWPC::er_measures)
  
  # get internal table with importance of environmental challenges
  er_scoring <- as.data.table(BBWPC::er_scoring)
  er_urgency <- er_scoring[type=='urgency'][,type := NULL]
  er_aim <- er_scoring[type == 'aim'][,type := NULL]
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(B_LU_BBWP),length(B_LU_BRP),
                    length(A_P_SG),length(B_SLOPE_DEGREE),
                    length(M_DRAIN),length(D_SA_W),length(B_CT_SOIL),length(B_CT_WATER),length(B_CT_CLIMATE),length(B_AER_CBS),
                    length(B_CT_BIO),length(B_CT_LANDSCAPE),
                    length(B_LU_ARABLE_ER),length(B_LU_PRODUCTIVE_ER),length(B_LU_CULTIVATED_ER))
  
  # check inputs
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unlist(bbwp_parms[code == "B_SOILTYPE_AGR", choices]))
  checkmate::assert_character(B_SOILTYPE_AGR,len = arg.length)
  checkmate::assert_subset(B_LU_BBWP, choices = unlist(bbwp_parms[code == "B_LU_BBWP", choices]))
  checkmate::assert_character(B_LU_BBWP, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unlist(bbwp_parms[code == "B_LU_BRP", choices]))
  checkmate::assert_integerish(B_LU_BRP, len = arg.length)
  checkmate::assert_numeric(B_SLOPE_DEGREE,lower = bbwp_parms[code == "B_SLOPE_DEGREE", value_min], upper = bbwp_parms[code == "B_SLOPE_DEGREE", value_max],len = arg.length)
  checkmate::assert_logical(B_LU_ARABLE_ER,len = arg.length)
  checkmate::assert_logical(B_LU_PRODUCTIVE_ER,len = arg.length)
  checkmate::assert_logical(B_LU_CULTIVATED_ER,len = arg.length)
  
  
  # collect data in one data.table
  dt <- data.table(id = 1:arg.length,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_GWL_CLASS = B_GWL_CLASS,
                   B_AREA = B_AREA,
                   A_P_SG = A_P_SG,
                   B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                   B_LU_BBWP = B_LU_BBWP,
                   B_LU_BRP = B_LU_BRP,
                   B_LU_ARABLE_ER = B_LU_ARABLE_ER, 
                   B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                   B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                   B_AER_CBS = B_AER_CBS,
                   M_DRAIN = M_DRAIN,
                   B_CT_SOIL = B_CT_SOIL, 
                   B_CT_WATER = B_CT_WATER,
                   B_CT_CLIMATE = B_CT_CLIMATE,
                   B_CT_BIO = B_CT_BIO,
                   B_CT_LANDSCAPE = B_CT_LANDSCAPE
                  )
  
  # do check op groundwater class
  checkmate::assert_subset(B_GWL_CLASS, choices = unlist(bbwp_parms[code == 'B_GWL_CLASS', choices]))
  
  # add farm area
  dt[, B_AREA_FARM := sum(B_AREA)]
  
  # add the generic farm score as baseline
  # this gives the averaged ER score based on the crops in crop rotation plan and the farm measures taken
  dt.farm <- er_croprotation(B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR,
                             B_LU_BBWP = dt$B_LU_BBWP,
                             B_LU_BRP = dt$B_LU_BRP,
                             B_LU_ARABLE_ER = dt$B_LU_ARABLE_ER,
                             B_LU_PRODUCTIVE_ER = dt$B_LU_PRODUCTIVE_ER,
                             B_LU_CULTIVATED_ER = dt$B_LU_CULTIVATED_ER,
                             B_AER_CBS = dt$B_AER_CBS,
                             B_AREA = dt$B_AREA,
                             measures = measures,
                             sector = sector)
  
  # merge all measures to the given fields
  dt <- as.data.table(merge.data.frame(dt, dt.meas.av, all = TRUE))
  
    # only select measures at field level
    dt <- dt[level == 'field']
  
    # merge with the Ecoregeling ~ Measure list to evaluate applicability
    dt <- merge(dt,
                dt.meas.eco, 
                by = c('B_LU_BRP','eco_id'),
                all.x = TRUE)
    
    # remove crop measures when current crop is already used for farm score
    dt[!is.na(eco_app) & grepl('EB1$|EB2$|EB3$|EB8|EB9',eco_id), eco_app := 0]
    
    # add crop rotation measures when current crop is not applicable
    dt[is.na(eco_app) & grepl('EB1$|EB2$|EB3$|EB8|EB9',eco_id), eco_app := 1]
    
    # ensure that EG20 is always applicable
    dt[is.na(eco_app) & !grepl('EG20',eco_id),eco_app := 0]
    dt[is.na(eco_app) & grepl('EG20',eco_id), eco_app := 1]
    
  # rank is zero when measures are not applicable given the crop type
  
    # columns with the Ecoregelingen ranks and reward
    cols <- c('er_soil','er_water','er_biodiversity','er_climate','er_landscape','er_euro_ha', 'er_euro_farm')
    
    # set first all missing data impacts to 0
    dt[,c(cols) := lapply(.SD, function(x) fifelse(is.na(x),0,x)), .SDcols = cols]
    
    # lower the score when not applicable for given BBWP category
    dt[B_LU_BBWP == 'gras_permanent' & nc1 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'gras_tijdelijk' & nc2 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'rustgewas' & nc3 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'rooivrucht' & nc4 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'groenten' & nc5 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'bollensierteelt' & nc6 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'boomfruitteelt' & nc7 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'natuur' & nc8 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'mais' & nc9 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'randensloot' & nc10 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'vanggewas' & nc11 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 'eiwitgewas' & nc12 == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    
    # set the score to zero when not applicable for a given ER combined category
    dt[eco_app == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    
    # lower the score  when not applicable as arable/productive/cultivated measure
    dt[B_LU_ARABLE_ER  == TRUE & b_lu_arable_er  == 0 & !grepl('EG20',eco_id), c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_PRODUCTIVE_ER == TRUE & b_lu_productive_er == 0 & !grepl('EG20',eco_id), c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_CULTIVATED_ER  == TRUE & b_lu_cultivated_er == 0 & !grepl('EG20',eco_id), c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_ARABLE_ER  == FALSE & b_lu_arable_er  == 1 & !grepl('EG20',eco_id), c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_PRODUCTIVE_ER == FALSE & b_lu_productive_er == 1 & !grepl('EG20',eco_id), c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_CULTIVATED_ER  == FALSE & b_lu_cultivated_er == 1 & !grepl('EG20',eco_id), c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    
    # lower the score when the sector limits the applicability of measures
    
      # add columns for the farm sector
      fs0 <- c('fdairy','farable','ftree_nursery','fbulbs')
      fs1 <- paste0('f',sector)
      fs2 <- fs0[!fs0 %in% fs1]
      dt[,c(fs1) := 1]
      if(length(fs2) >= 1) { dt[,c(fs2) := 0] }
    
      # estimate whether sector allows applicability
      dt[, fsector := fdairy * dairy + farable * arable + ftree_nursery * tree_nursery + fbulbs * bulbs]
      
      # lower the score when measure is not applicable
      dt[fsector == 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
      
    # lower the score when the soil type limits the applicability of measures
    dt[grepl('klei', B_SOILTYPE_AGR) & clay == FALSE , c(cols) := 0]
    dt[grepl('zand|dal', B_SOILTYPE_AGR) & sand == FALSE , c(cols) := 0]
    dt[grepl('veen', B_SOILTYPE_AGR) & peat == FALSE , c(cols) := 0]
    dt[grepl('loess', B_SOILTYPE_AGR) & loess == FALSE , c(cols) := 0]
    
  # multiply by urgency
  
    # add soil type for political and environmental urgency
    dt[grepl('klei', B_SOILTYPE_AGR) , soiltype := 'klei']
    dt[grepl('zand|dal', B_SOILTYPE_AGR), soiltype := 'zand']
    dt[grepl('veen', B_SOILTYPE_AGR), soiltype := 'veen']
    dt[grepl('loess', B_SOILTYPE_AGR), soiltype := 'loess']
    
    # merge with urgency table
    dt <- merge(dt,er_urgency, by= 'soiltype')
    
    # multiply measurement score with urgency
    dt[, er_water := er_water * cf_water]
    dt[, er_soil := er_soil * cf_soil]
    dt[, er_climate := er_climate * cf_climate]
    dt[, er_biodiversity := er_biodiversity * cf_biodiversity]
    dt[, er_landscape := er_landscape * cf_landscape]
    
    # multiply measurement score by distance to target
    dt[, er_water := er_water * pmax(0.1,1 - dt.farm$water / B_CT_WATER)]
    dt[, er_soil := er_soil * pmax(0.1,1 - dt.farm$soil/B_CT_SOIL)]
    dt[, er_climate := er_climate * pmax(0.1,1 - dt.farm$climate / B_CT_CLIMATE)]
    dt[, er_biodiversity := er_biodiversity * pmax(0.1,1 - dt.farm$biodiversity / B_CT_BIO)]
    dt[, er_landscape := er_landscape * pmax(0.1,1 - dt.farm$landscape/max(B_CT_LANDSCAPE,0.001))]
    dt[, er_reward := dt.farm$S_ER_REWARD]
    
    # Calculate total measurement score given the distance to target
    dt[, er_total := (er_water + er_soil + er_climate + er_biodiversity + er_landscape) / 5]
  
    # add scaling for both scoring and reward, and combine both
    dt[, er_total_scaled := (er_total - min(er_total)) / (max(er_total) - min(er_total))]
    dt[, er_soil_scaled := (er_soil - min(er_soil)) / (max(er_soil) - min(er_soil))]
    dt[, er_water_scaled := (er_water - min(er_water)) / (max(er_water) - min(er_water))]
    dt[, er_climate_scaled := (er_climate - min(er_climate)) / (max(er_climate) - min(er_climate))]
    dt[, er_biodiversity_scaled := (er_biodiversity - min(er_biodiversity)) / (max(er_biodiversity) - min(er_biodiversity))]
    dt[, er_landscape_scaled := (er_landscape - min(er_landscape)) / (max(er_landscape) - min(er_landscape))]
    dt[, er_euro_combi := er_euro_ha + er_euro_farm / B_AREA_FARM ]
    dt[, er_reward_scaled := (er_euro_combi - min(er_euro_combi)) / (max(er_euro_combi) - min(er_euro_combi))]
    dt[, er_total_scaled := 0.8 * er_total_scaled + 0.2 * er_reward_scaled]
    dt[, er_water_scaled := 0.8 * er_water_scaled + 0.2 * er_reward_scaled]
    dt[, er_soil_scaled := 0.8 * er_soil_scaled + 0.2 * er_reward_scaled]
    dt[, er_climate_scaled := 0.8 * er_climate_scaled + 0.2 * er_reward_scaled]
    dt[, er_biodiversity_scaled := 0.8 * er_biodiversity_scaled + 0.2 * er_reward_scaled]
    dt[, er_landscape_scaled := 0.8 * er_landscape_scaled + 0.2 * er_reward_scaled]
    
    # set score from lowest rank to zero per conflicting measure
    dt[, oid := frank(-er_total_scaled, ties.method = 'first',na.last = 'keep'),by = c('id','bbwp_conflict')]
    dt[oid > 1, er_total_scaled := 0]
    dt[, oid := frank(-er_water_scaled, ties.method = 'first',na.last = 'keep'),by = c('id','bbwp_conflict')]
    dt[oid > 1, er_water_scaled := 0]
    dt[, oid := frank(-er_climate_scaled, ties.method = 'first',na.last = 'keep'),by = c('id','bbwp_conflict')]
    dt[oid > 1, er_climate_scaled := 0]
    dt[, oid := frank(-er_biodiversity_scaled, ties.method = 'first',na.last = 'keep'),by = c('id','bbwp_conflict')]
    dt[oid > 1, er_biodiversity_scaled := 0]
    dt[, oid := frank(-er_soil_scaled, ties.method = 'first',na.last = 'keep'),by = c('id','bbwp_conflict')]
    dt[oid > 1, er_soil_scaled := 0]
    dt[, oid := frank(-er_landscape_scaled, ties.method = 'first',na.last = 'keep'),by = c('id','bbwp_conflict')]
    dt[oid > 1, er_landscape_scaled := 0]
   
  # Loop through each field
  
  # define an empty list
  list.meas <- list()
  
  # select for each field the top5 measures per objective
  for (i in 1:arg.length) {
    
    # list to store output
    list.field <- list()
    
    # Get the overall top measures
    top_er_tot <- dt[id == i & er_total > 0, ][order(-er_total_scaled)][1:5,bbwp_id]
    
    # Get the top measures for soil quality
    top_er_soil <- dt[id == i & er_soil > 0, ][order(-er_soil_scaled)][1:5,bbwp_id]
    
    # Get the top measures for water quality
    top_er_water <- dt[id == i & er_water > 0, ][order(-er_water_scaled)][1:5,bbwp_id]
    
    # Get the top measures for climate
    top_er_climate <- dt[id == i & er_climate > 0, ][order(-er_climate_scaled)][1:5,bbwp_id]
    
    # Get the top measures for biodiversity
    top_er_biodiversity <- dt[id == i & er_biodiversity > 0, ][order(-er_biodiversity_scaled)][1:5,bbwp_id]
    
    # Get the top measures for landscape
    top_er_landscape <- dt[id == i & er_landscape > 0, ][order(-er_landscape_scaled)][1:5,bbwp_id]
    
    # Get the top measures for rewards 
    top_er_reward <- dt[id == i & er_reward_scaled > 0, ][order(-er_reward_scaled)][1:5,bbwp_id]
    
    # add them to list
    list.meas[[i]] <- data.table(id = i,
                                 top_er_tot = top_er_tot,
                                 top_er_soil = top_er_soil,
                                 top_er_water = top_er_water,
                                 top_er_climate = top_er_climate,
                                 top_er_biodiversity = top_er_biodiversity,
                                 top_er_landscape = top_er_landscape,
                                 top_er_reward = top_er_reward)
  }
  
  # retrieve output object
  out <- data.table::rbindlist(list.meas)
  
  # remove NA for cases that no measures are needed at all
  out <- unique(out)
  
  # return value
  return(out)
}
