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
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'melkveehouderij','akkerbouw','vollegrondsgroente','boomteelt','bollen','veehouderij','overig')
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param measures (list) the measures planned /done (measure nr)
#' 
#' @import data.table
#'
#' @export
# calculate the opportunities for a set of fields
er_farm_score <- function(S_ER_TOT,S_ER_SOIL,S_ER_WATER,S_ER_CLIMATE,S_ER_BIODIVERSITY,S_ER_LANDSCAPE, 
                          S_ER_REWARD, B_AREA, B_SOILTYPE_AGR, sector, B_AER_CBS, measures){
  
  # check length of the inputs
  arg.length <- max(length(S_ER_TOT),length(S_ER_SOIL),length(S_ER_WATER),length(S_ER_CLIMATE),
                    length(S_ER_BIODIVERSITY),length(S_ER_LANDSCAPE),length(B_AREA))
  
  # check inputs
  checkmate::assert_numeric(S_ER_TOT, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_SOIL, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_WATER, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_CLIMATE, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_BIODIVERSITY, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(S_ER_LANDSCAPE, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(B_AREA, lower = 0, upper = 500000000, len = arg.length)
  checkmate::assert_numeric(S_ER_REWARD, lower = 0, upper = 10000, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = c('duinzand','dekzand','zeeklei','rivierklei','maasklei',
                                                       'dalgrond','moerige_klei','veen','loess'))

  # reformat B_AER_CBS
  B_AER_CBS <- bbwp_format_aer(B_AER_CBS)
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(B_AREA),length(B_AER_CBS))
  
  # collect data in one data.table
  dt <- data.table(id = 1:arg.length,
                   S_ER_SOIL = S_ER_SOIL,
                   S_ER_WATER = S_ER_WATER,
                   S_ER_CLIMATE = S_ER_CLIMATE,
                   S_ER_BIODIVERSITY = S_ER_BIODIVERSITY,
                   S_ER_LANDSCAPE = S_ER_LANDSCAPE,
                   S_ER_TOT = S_ER_TOT,
                   S_ER_REWARD = S_ER_REWARD,
                   B_AREA = B_AREA,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   sector = sector,
                   B_AER_CBS = B_AER_CBS
                  )
  
  # keep fields with B_AREA in separate dt
  #dt0 <- dt[, c("id","B_AREA")]
  
  # check and update the measures table
  dt.meas.taken <- bbwp_check_meas(dt = measures, eco = TRUE, score = TRUE)
  
  # calculate the total score per indicator 
  if(nrow(dt.meas.taken) > 0){
  
  # filter farm measures
  dt.meas.taken <- dt.meas.taken[level == "farm",]
  
  # DEZE REGEL MOET ER STRAKS TUSSENUIT!!!!!
  setnames(dt.meas.taken,"er_profit","er_euro_farm")

  # keep fields without measures taken in separate dt
  dt1 <- merge(dt, dt.meas.taken, by='id', all = TRUE)
  dt1 <- dt1[is.na(eco_id),]
    
  # merge all measures to the given fields
  dt <- merge(dt,dt.meas.taken, by='id', all.y = TRUE)
  
  # add farm scores and profit for total farm
  
    # columns with the Ecoregelingen ranks and reward
    cols <- c('er_soil','er_water','er_biodiversity','er_climate','er_landscape','er_euro_farm')
  
    # set first all missing data impacts to 0
    dt[,c(cols) := lapply(.SD, function(x) fifelse(is.na(x),0,x)), .SDcols = cols]

    # add columns for the sector to which the farms belong
    fs0 <- c('fdairy','farable','ftree_nursery','fbulbs')
    fs1 <- paste0('f',sector)
    fs2 <- fs0[!fs0 %in% fs1]
    dt[,c(fs1) := 1]
    dt[,c(fs2) := 0]
    
    # estimate whether sector allows applicability
    dt[, fsector := fdairy * dairy + farable * arable + ftree_nursery * tree_nursery + fbulbs * bulbs]
    
    # adapt the score and profit to zero when measure is not applicable
    dt[fsector == 0, c(cols) := 0]
    
    # set the score and profit to zero when the measure is not applicable given soil type
    
    # adapt the score and profit when the soil type limits the applicability of measures
    dt[grepl('klei', B_SOILTYPE_AGR) & clay == 0 , c(cols) := 0]
    dt[grepl('zand|dal', B_SOILTYPE_AGR) & sand == 0 , c(cols) := 0]
    dt[grepl('veen', B_SOILTYPE_AGR) & peat == 0 , c(cols) := 0]
    dt[grepl('loess', B_SOILTYPE_AGR) & loess == 0 , c(cols) := 0]
    
    # multiply scores by (political) urgency
      
      # get urgency data
      dt.er.scoring <- as.data.table(BBWPC::er_scoring)
      setnames(dt.er.scoring,gsub('cf_','',colnames(dt.er.scoring)))
      dt.er.urgency <- melt(dt.er.scoring[type=='urgency'],
                            id.vars='soiltype',
                            measure.vars = c('soil', 'water', 'climate',  'biodiversity', 'landscape'),
                            variable.name = 'indicator',
                            value.name = 'urgency')
      # copy dt
      dt2 <- dt
      
      # first add soil type for political and environmental urgency
      dt2[grepl('klei', B_SOILTYPE_AGR) , soiltype := 'klei']
      dt2[grepl('zand|dal', B_SOILTYPE_AGR), soiltype := 'zand']
      dt2[grepl('veen', B_SOILTYPE_AGR), soiltype := 'veen']
      dt2[grepl('loess', B_SOILTYPE_AGR), soiltype := 'loess']
      
      # melt dt
      dt2 <- melt(dt,
                 id.vars = c('id','bbwp_id','eco_id','soiltype','bbwp_conflict'),
                 measure = patterns(erscore = "^er_"),
                 variable.name = 'indicator',
                 value.name = 'value')
      dt2[,indicator := gsub('er_', '',cols[indicator])]
      
      # merge with urgency table
      dt2 <- merge(dt2,dt.er.urgency, by= c('soiltype','indicator'),all.x = TRUE)
      
      # adapt the score based on urgency
      dt2[indicator != 'euro_farm', value := value * urgency]
      
      # reshape dt to wide format
      dt2 <- dcast(dt2, id + soiltype + bbwp_id + bbwp_conflict + eco_id ~ indicator, value.var = 'value')
    
    # merge selected columns of dt2 with dt
    dt2 <- dt2[, c("soiltype","bbwp_id","bbwp_conflict"):= NULL]
    dt <- merge(dt,dt2, by= c('id','eco_id'),all.x = TRUE)

    # add bbwp table for financial reward correction factor per AER
    dt.er.reward <- as.data.table(BBWPC::er_aer_reward)
      
    # merge with regional correction factor for the financial reward
    dt <- merge(dt,dt.er.reward[,.(statcode,er_cf)], by.x = 'B_AER_CBS',by.y = 'statcode')
   
    # correct financial reward for specific agricultural region
    dt <- dt[,euro_farm := euro_farm*er_cf]  
    
  # farm scale measures rewards and scoring (measures that have score/reward on farm scale: EG5A,5B,5C; EG6A,6B,6C,6D; EG12A,12B; EB10A,10B; EB14A,14B,14C)
  dt[,S_ER_REWARD := fifelse(grepl("^EG5|^EG6|^EG12|^EB10|^EB14",eco_id), S_ER_REWARD + euro_farm, S_ER_REWARD)]
  dt[,S_ER_SOIL := fifelse(grepl("^EG5|^EG6|^EG12|^EB10|^EB14",eco_id), S_ER_SOIL + soil, S_ER_SOIL)]
  dt[,S_ER_WATER := fifelse(grepl("^EG5|^EG6|^EG12|^EB10|^EB14",eco_id), S_ER_WATER + water, S_ER_WATER)]
  dt[,S_ER_CLIMATE := fifelse(grepl("^EG5|^EG6|^EG12|^EB10|^EB14",eco_id), S_ER_CLIMATE + climate, S_ER_CLIMATE)]
  dt[,S_ER_BIODIVERSITY := fifelse(grepl("^EG5|^EG6|^EG12|^EB10|^EB14",eco_id), S_ER_BIODIVERSITY + biodiversity, S_ER_BIODIVERSITY)]
  dt[,S_ER_LANDSCAPE := fifelse(grepl("^EG5|^EG6|^EG12|^EB10|^EB14",eco_id), S_ER_LANDSCAPE + landscape, S_ER_LANDSCAPE)]
  
  # extract value
  cols <- c('S_ER_SOIL','S_ER_WATER','S_ER_CLIMATE','S_ER_BIODIVERSITY','S_ER_LANDSCAPE','S_ER_TOT','S_ER_REWARD')
  dt <- dt[,mget(c('id',cols))]
  dt1 <- dt1[,mget(c('id',cols))]
  
  # add fields without measures again 
  dt <- rbind(dt,dt1)
  
  # sum measures score per field
  dt <- dt[, lapply(.SD, sum), by = "id"]
  
  # add B_AREA per field again
  #dt <- merge(dt,dt0, by = "id", all.x=TRUE)

  # order the fields
  setorder(dt, id)
  
  # calculate total reward and aggregated total farm score 
  dt.f<- dt[,lapply(.SD,sum), .SDcols = cols]

    # weigh the importance given "distance to target"
    dt.f[,cfSOIL := wf(S_ER_SOIL, type="score")]
    dt.f[,cfWAT := wf(S_ER_WATER, type="score")]
    dt.f[,cfCLIM := wf(S_ER_CLIMATE, type="score")]
    dt.f[,cfBIO := wf(S_ER_BIODIVERSITY, type="score")]
    dt.f[,cfLAND := wf(S_ER_LANDSCAPE, type="score")]
    
    # weighted mean
    dt.f[,S_ER_TOT := (S_ER_SOIL * cfSOIL + S_ER_WATER * cfWAT + S_ER_CLIMATE * cfCLIM + S_ER_BIODIVERSITY * cfBIO + S_ER_LANDSCAPE * cfLAND) /
         (cfSOIL + cfWAT + cfCLIM + cfBIO + cfLAND)]
    
    # round the values of the farm scores
    cols <- c('S_ER_SOIL','S_ER_WATER','S_ER_CLIMATE','S_ER_BIODIVERSITY','S_ER_LANDSCAPE','S_ER_TOT','S_ER_REWARD')
    dt.f <- dt.f[, c(cols) := lapply(.SD, round, digits = 0),.SDcols = cols]

    dt <- dt.f[,mget(cols)]
    
  
  } else {
    
    # set impact of management to zero when no measures are applied
    dt <- data.table(S_ER_SOIL = S_ER_SOIL,
                     S_ER_WATER = S_ER_WATER,
                     S_ER_CLIMATE = S_ER_CLIMATE,
                     S_ER_BIODIVERSITY = S_ER_BIODIVERSITY,
                     S_ER_LANDSCAPE = S_ER_LANDSCAPE,
                     S_ER_TOT = S_ER_TOT,
                     S_ER_REWARD = S_ER_REWARD
                     )
    
    # calculate total reward and aggregated total farm score 
    dt.f<- dt[,lapply(.SD,sum), .SDcols = cols]
    
    # weigh the importance given "distance to target"
    dt.f[,cfSOIL := wf(S_ER_SOIL, type="score")]
    dt.f[,cfWAT := wf(S_ER_WATER, type="score")]
    dt.f[,cfCLIM := wf(S_ER_CLIMATE, type="score")]
    dt.f[,cfBIO := wf(S_ER_BIODIVERSITY, type="score")]
    dt.f[,cfLAND := wf(S_ER_LANDSCAPE, type="score")]
    
    # weighted mean
    dt.f[,S_ER_TOT := (S_ER_SOIL * cfSOIL + S_ER_WATER * cfWAT + S_ER_CLIMATE * cfCLIM + S_ER_BIODIVERSITY * cfBIO + S_ER_LANDSCAPE * cfLAND) /
           (cfSOIL + cfWAT + cfCLIM + cfBIO + cfLAND)]
    
    # round the values of the farm scores
    cols <- c('S_ER_SOIL','S_ER_WATER','S_ER_CLIMATE','S_ER_BIODIVERSITY','S_ER_LANDSCAPE','S_ER_TOT','S_ER_REWARD')
    dt.f <- dt.f[, c(cols) := lapply(.SD, round, digits = 0),.SDcols = cols]
    
    dt <- dt.f[,mget(cols)]
    
  } 
    
  # return output
  return(dt)
}
