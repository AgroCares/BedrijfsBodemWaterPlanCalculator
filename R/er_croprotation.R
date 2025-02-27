#' Calculate the crop rotation based total score for five opportunity indicators
#'
#' Estimate the actual contribution of crop rotation given aims for soil quality, water quality, climate, biodiversity and landscape
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BBWP (character) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_LU_ARABLE_ER (boolean) does the crop fall within the ER category "arable"
#' @param B_LU_PRODUCTIVE_ER (boolean) does the crop fall within the ER category "productive"
#' @param B_LU_CULTIVATED_ER (boolean) does the crop fall within the ER category "cultivated"
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param B_AREA (numeric) the area of the field (m2) 
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'dairy', 'arable', 'tree_nursery', 'bulbs')
#' @param measures (list) The measures planned / done per fields
#' @param pdf (boolean) is there a pdf needed
#'
#' @import data.table
#' @import stats
#'
#' @export
# calculate the opportunities for a set of fields
er_croprotation <- function(B_SOILTYPE_AGR, B_AER_CBS,B_AREA,
                            B_LU_BBWP,B_LU_BRP, 
                            B_LU_ARABLE_ER, B_LU_PRODUCTIVE_ER,B_LU_CULTIVATED_ER,
                            measures, sector, pdf = FALSE){
  
  # add visual bindings
  . = eco_id = farmid = b_lu_brp = type = erscore = B_AREA_RR = indicator = NULL
  urgency = soiltype = er_profit = statcode = er_cf = id = value = reward = ec1 = ec2 = NULL
  level = nc1 = nc2 = nc3 = nc4 = nc5 = nc6 = nc7 = nc8 = nc9 = nc10 = nc11 = nc12 = NULL
  fsector = fdairy = dairy = farable = arable = ftree_nursery = tree_nursery = patterns = fbulbs = bulbs = NULL
  clay = sand = silt = peat = bbwp_id = B_AREA_REL = S_ER_REWARD = euro_farm = NULL
  fr_soil = er_reward = fr_soil = reward_cf = regio_factor = euro_ha = oid = water = soil = climate = biodiversity = landscape = climate = total = NULL
  er_total = er_climate = er_soil = er_water = er_landscape = er_biodiversity = NULL
  eco_app = b_lu_arable_er = b_lu_productive_er = b_lu_cultivated_er = NULL
  code = choices = cfr = B_IDX = k = combi = appl = area_appl = bbwp_status = NULL
  
  # Load bbwp_parms
  bbwp_parms <- BBWPC::bbwp_parms
  
  # reformat B_AER_CBS
  B_AER_CBS <- bbwp_format_aer(B_AER_CBS)
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(B_LU_BBWP),length(B_LU_BRP),length(B_AER_CBS),
                    length(B_LU_ARABLE_ER),length(B_LU_PRODUCTIVE_ER),length(B_LU_CULTIVATED_ER))
  
  # check inputs
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unlist(bbwp_parms[code == "B_SOILTYPE_AGR", choices]))
  checkmate::assert_subset(B_LU_BBWP, choices = unlist(bbwp_parms[code == "B_LU_BBWP", choices]))
  checkmate::assert_character(B_LU_BBWP, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unlist(bbwp_parms[code == "B_LU_BRP", choices]))
  checkmate::assert_integerish(B_LU_BRP, len = arg.length)
  checkmate::assert_logical(B_LU_ARABLE_ER,len = arg.length)
  checkmate::assert_logical(B_LU_PRODUCTIVE_ER,len = arg.length)
  checkmate::assert_logical(B_LU_CULTIVATED_ER,len = arg.length)

  # check and update the measure table
  dt.meas.farm <- bbwp_check_meas(dt = measures, eco = TRUE, score = TRUE)
  dt.meas.field <- bbwp_check_meas(dt = NULL, eco = TRUE, score = FALSE)
  dt.meas.eco <- as.data.table(BBWPC::er_measures)
  dt.meas.idx <- bbwp_check_meas(dt = NULL, eco = TRUE, score = FALSE)
    
  # subset measurement tables # Add EB18 here Gerard
  dt.meas.field <- dt.meas.field[grepl('EB1$|EB2$|EB3$|EB8|EB9',eco_id) & level == 'field',] # these are measures teel rustgewassen als hoofdteelt or teelt of either eiwitgewassen, meerjarige gewassen, diepwortelende gewassen, gewassen met gunstige spruit:wortel verhouding
  dt.meas.idx <- dt.meas.idx[grepl('EB10A',eco_id) & level == 'farm']
  dt.meas.farm <- dt.meas.farm[level == 'farm',]
 
  # add crop diversification index (EB10A) to farm measures table
  if(nrow(dt.meas.farm[grepl("EB10",eco_id)]) > 0){
    
    # replace EB10B or EB10C by EB10A
    ucols <- colnames(dt.meas.idx)
    dt.meas.farm[grepl('EB10',eco_id), c(ucols) := dt.meas.idx[,mget(ucols)]]
    
    # farm measures should be unique
    dt.meas.farm <- dt.meas.farm[!duplicated(eco_id)]
    
    } else { 
    
    # add crop EB10A to farm measures  
    dt.meas.farm <- rbind(dt.meas.farm,dt.meas.idx, use.names = TRUE, fill = TRUE)
    dt.meas.farm[eco_id == 'EB10A', id := fifelse(is.na(id),1,id)]
    dt.meas.farm[eco_id == 'EB10A', bbwp_status := 'Planned']
    
    # farm measures should be unique
    dt.meas.farm <- dt.meas.farm[!duplicated(eco_id)]
    } 
  
  # add bbwp table for financial reward correction factor per AER
  dt.er.reward <- as.data.table(BBWPC::er_aer_reward)
  
  # get internal table with importance of environmental challenges
  dt.er.scoring <- as.data.table(BBWPC::er_scoring)
  setnames(dt.er.scoring,gsub('cf_','',colnames(dt.er.scoring)))
  dt.er.urgency <- melt(dt.er.scoring[type=='urgency'],
                        id.vars='soiltype',
                        measure.vars = c('soil', 'water', 'climate',  'biodiversity', 'landscape'),
                        variable.name = 'indicator',
                        value.name = 'urgency')
  
  # add financial urgency
  dt.er.urgency[,c('euro_farm', 'euro_ha') := 1]
  
  # collect total areas on farm level
  dt.farm <- data.table(area_farm = sum(B_AREA),
                        area_arable = sum(B_AREA * B_LU_ARABLE_ER),
                        area_productive = sum(B_AREA * B_LU_PRODUCTIVE_ER),
                        area_cultivated = sum(B_AREA * B_LU_CULTIVATED_ER))
  
  # collect data in one data.table
  dt <- data.table(id = 1:arg.length,
                   B_AER_CBS = B_AER_CBS,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_LU_BBWP = B_LU_BBWP,
                   B_LU_BRP = B_LU_BRP,
                   B_LU_ARABLE_ER = B_LU_ARABLE_ER, 
                   B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                   B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                   B_AREA = B_AREA)
  
  # add regional correction value for price
  dt <- merge(dt,dt.er.reward[,.(statcode,reward_cf = er_cf)], 
              by.x = 'B_AER_CBS',by.y = 'statcode',all.x = TRUE)
  
  # add soil type for political and environmental urgency
  dt[grepl('klei', B_SOILTYPE_AGR) , soiltype := 'klei']
  dt[grepl('zand|dal', B_SOILTYPE_AGR), soiltype := 'zand']
  dt[grepl('veen', B_SOILTYPE_AGR), soiltype := 'veen']
  dt[grepl('loess', B_SOILTYPE_AGR), soiltype := 'loess']
  
  # merge with the measures taken
  dt.field <- dt[,as.list(dt.meas.field),by=names(dt)]
  
  # merge with the Ecoregeling ~ Measure list to evaluate applicability
  dt.field <- merge(dt.field,
                    dt.meas.eco, 
                    by = c('B_LU_BRP','eco_id'),
                    all.x = TRUE)
  dt.field[is.na(eco_app),eco_app := 0]
  
    # columns with the Ecoregelingen ranks and reward
    cols <- c('er_soil','er_water','er_biodiversity','er_climate','er_landscape','er_euro_ha', 'er_euro_farm')
     
    # set first all missing data impacts to 0
    dt.field[,c(cols) := lapply(.SD, function(x) fifelse(is.na(x),0,x)), .SDcols = cols]
  
    # set the score to zero when not applicable for given BBWP category
    dt.field[B_LU_BBWP == 'gras_permanent' & nc1 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'gras_tijdelijk' & nc2 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'rustgewas' & nc3 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'rooivrucht' & nc4 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'groenten' & nc5 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'bollensierteelt' & nc6 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'boomfruitteelt' & nc7 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'natuur' & nc8 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'mais' & nc9 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'randensloot' & nc10 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'vanggewas' & nc11 == 0, c(cols) := 0]
    dt.field[B_LU_BBWP == 'eiwitgewas' & nc12 == 0, c(cols) := 0]
    
    # set the score to zero when not applicable for a given ER combined category
    dt.field[eco_app == 0, c(cols) := 0]
   
    # set measures not applicable on arable, cultivated or productive land (only for measures that are crop rotation based)
    dt.field[B_LU_ARABLE_ER  == TRUE & b_lu_arable_er  == 0, c(cols) := 0]
    dt.field[B_LU_PRODUCTIVE_ER == TRUE & b_lu_productive_er == 0, c(cols) := 0]
    dt.field[B_LU_CULTIVATED_ER  == TRUE & b_lu_cultivated_er == 0, c(cols) := 0]
    dt.field[B_LU_ARABLE_ER  == FALSE & b_lu_arable_er  == 1, c(cols) := 0]
    dt.field[B_LU_PRODUCTIVE_ER == FALSE & b_lu_productive_er == 1, c(cols) := 0]
    dt.field[B_LU_CULTIVATED_ER  == FALSE & b_lu_cultivated_er == 1, c(cols) := 0]
    
    # add columns for the sector to which the farms belong
    fs0 <- c('fdairy','farable','ftree_nursery','fbulbs')
    fs1 <- paste0('f',sector)
    fs2 <- fs0[!fs0 %in% fs1]
    dt.field[,c(fs1) := 1]
    if(length(fs2) > 0){dt.field[,c(fs2) := 0]}
    
    # estimate whether sector allows applicability
    dt.field[, fsector := fdairy * dairy + farable * arable + ftree_nursery * tree_nursery + fbulbs * bulbs] 
    
    # adapt the score when measure is not applicable
    dt.field[fsector == 0, c(cols) := 0]
    
    # adapt the score when the soil type limits the applicability of measures
    dt.field[grepl('klei', B_SOILTYPE_AGR) & clay == FALSE , c(cols) := 0]
    dt.field[grepl('zand|dal', B_SOILTYPE_AGR) & sand == FALSE , c(cols) := 0]
    dt.field[grepl('veen', B_SOILTYPE_AGR) & peat == FALSE , c(cols) := 0]
    dt.field[grepl('loess', B_SOILTYPE_AGR) & loess == FALSE , c(cols) := 0]
    
    # columns to be updated
    cols.sel <- c('er_climate','er_soil','er_water','er_landscape','er_biodiversity','er_euro_ha')
    
    # measure EB1. Cultivate rustgewas on a field
    cols.ad1 <- c(3,3,3,0,1,0)
    cols.ad2 <- c(4,4,4,1,1,0)
    dt.field[, er_total := er_climate + er_soil + er_water + er_landscape + er_biodiversity]
    dt.field[bbwp_id == 'G54' & er_total > 0, B_AREA_REL := sum(B_AREA) * 100 / dt.farm$area_arable]
    dt.field[bbwp_id == 'G54' & er_total > 0 & B_AREA_REL <= 20, c(cols.sel) := 0]
    dt.field[bbwp_id == 'G54' & er_total > 0 & B_AREA_REL > 35 & B_AREA_REL <= 50, c(cols.sel) := Map('+',mget(cols.sel),cols.ad1)]
    dt.field[bbwp_id == 'G54' & er_total > 0 & B_AREA_REL > 50, c(cols.sel) := Map('+',mget(cols.sel),cols.ad2)]
    
    # multiply by (political) urgency
    
      # melt dt
      dt.field <- melt(dt.field,
               id.vars = c('id','bbwp_id','soiltype','bbwp_conflict','reward_cf','regio_factor'),
               measure.vars = patterns("^er_"),
               value.name = "erscore",
               variable.name = 'indicator')
      dt.field[,indicator := gsub('er_', '',indicator)]
    
    # merge with urgency table
    dt.field <- merge(dt.field,dt.er.urgency, by= c('soiltype','indicator'),all.x = TRUE)
    
    # adapt the score based on urgency
    dt.field[!grepl('euro',indicator), value := erscore * urgency]
    
    # add area
    dt.field <- merge(dt.field,dt[,.(id,B_AREA)],by='id')
    
    # dcast to add totals, to be used to update scores when measures are conflicting
    cols <- c('biodiversity', 'climate', 'landscape', 'soil','water','total')
    dt2 <- dcast(dt.field, id + soiltype + bbwp_id + bbwp_conflict + reward_cf + B_AREA + regio_factor ~ indicator, value.var = 'value')
    dt2[, total := biodiversity + climate + landscape + soil + water]
    dt2[, oid := frank(-total, ties.method = 'first',na.last = 'keep'),by = c('id','bbwp_conflict')]
    dt2[oid > 1, c(cols) := 0]
    
    # calculate the weighed average ER score (points/ ha) for the whole farm due to measures taken
    dt.field.score <- dt2[,lapply(.SD,function(x) sum(x*B_AREA)/dt.farm$area_farm), .SDcols = cols]
    dt.field.reward <- dt2[,list(er_reward = max(euro_ha[total>0],0,na.rm=T),
                                 B_AREA = B_AREA[1],
                                 reward_cf = reward_cf[1],
                                 regio_factor = regio_factor[1]),by=id]
    dt.field.reward[regio_factor== 1, er_reward := er_reward * reward_cf]
    dt.field.reward <- dt.field.reward[,list(er_reward = weighted.mean(x = er_reward,w=B_AREA))]
    
    # calculate the total score for farm measures
    dt.farm.soiltype <- dt[,list(fr_soil = sum(B_AREA) / sum(dt$B_AREA)),by = soiltype]
    dt.farm.urgency <- merge(dt.er.urgency[soiltype %in% dt$soiltype], dt.farm.soiltype,by= 'soiltype')
    dt.farm.urgency <- dt.farm.urgency[,list(urgency = weighted.mean(x = urgency,w = fr_soil)),by = indicator]
    
    # add farm measures when present
    if(nrow(dt.meas.farm) > 0){
      
      # columns with the Ecoregelingen ranks and reward
      cols <- c('er_soil','er_water','er_biodiversity','er_climate','er_landscape','er_euro_ha', 'er_euro_farm')
      
      # set first all missing data impacts to 0
      dt.meas.farm[,c(cols) := lapply(.SD, function(x) fifelse(is.na(x),0,x)), .SDcols = cols]
      
      # add columns for the sector to which the farms belong
      fs0 <- c('fdairy','farable','ftree_nursery','fbulbs')
      fs1 <- paste0('f',sector)
      fs2 <- fs0[!fs0 %in% fs1]
      dt.meas.farm[,c(fs1) := 1]
      if(length(fs2)>0){dt.meas.farm[,c(fs2) := 0]}
      
      # estimate whether sector allows applicability
      dt.meas.farm[, fsector := fdairy * dairy + farable * arable + ftree_nursery * tree_nursery + fbulbs * bulbs] 
      
      # adapt the score when measure is not applicable
      dt.meas.farm[fsector == 0, c(cols) := 0]
      
      # columns to be updated
      cols.sel <- c('er_climate','er_soil','er_water','er_landscape','er_biodiversity','er_euro_farm')
      
      # measure EB10: Crop diversification index
      cols.ad1 <- c(0,10,10,10,20,1000)
      cols.ad2 <- c(0,20,20,20,40,2000)
      dt.meas.farm[, er_total := er_climate + er_soil + er_water + er_landscape + er_biodiversity]
      
        # select only the unique crops that count for measure EB10
        crops <- unique(B_LU_BRP)
        crops <- sum(crops %in% dt.meas.eco[grepl('EB10',eco_id),B_LU_BRP], na.rm=TRUE) 
        
      dt.meas.farm[grepl("B189", bbwp_id) & er_total > 0, B_IDX := crops / (dt.farm$area_cultivated/10000)]
      dt.meas.farm[grepl("B189", bbwp_id) & er_total > 0 & B_IDX <= 0.05, c(cols.sel) := 0]
      dt.meas.farm[grepl("B189", bbwp_id) & er_total > 0 & B_IDX > 0.07 & B_IDX <= 0.10, c(cols.sel) := Map('+',mget(cols.sel),cols.ad1)]
      dt.meas.farm[grepl("B189", bbwp_id) & er_total > 0 & B_IDX > 0.10, c(cols.sel) := Map('+',mget(cols.sel),cols.ad2)]
      # dt.meas.farm[grepl("B189", bbwp_id) & er_total > 0 & B_IDX > 0.05 & B_IDX <= 0.07, c(cols.sel) := Map('+',mget(cols.sel), c(0, 25, 25, 50, 50, 1000))]
      # dt.meas.farm[grepl("B189", bbwp_id) & er_total > 0 & B_IDX > 0.07 & B_IDX <= 0.10, c(cols.sel) := Map('+',mget(cols.sel), c(0, 35, 35, 60, 70, 2000))]
      # dt.meas.farm[grepl("B189", bbwp_id) & er_total > 0 & B_IDX > 0.10                , c(cols.sel) := Map('+',mget(cols.sel), c(0, 45, 45, 70, 90, 3000))]

      # copy dt.meas.farm to be used later
      dt.region <- copy(dt.meas.farm)
      
      # farm measures do not have a field_id
      scols <- colnames(dt.meas.farm)[grepl('^er_|bbwp_id|bbwp_conflict',colnames(dt.meas.farm))]
      dt.meas.farm <- unique(dt.meas.farm[,mget(scols)])
      
      # multiply by (political) urgency
      dt3 <- melt(dt.meas.farm, 
                  id.vars = c('bbwp_id','bbwp_conflict'),
                  measure.vars = patterns("^er_"),
                  value.name = "erscore",
                  variable.name = 'indicator')
      dt3[,indicator := gsub('er_', '',indicator)]
      dt3 <- merge(dt3,dt.farm.urgency[,.(indicator,urgency)],by= 'indicator',all.x = T)
      
      # adapt the score based on urgency
      dt3[!grepl('euro',indicator), value := erscore * urgency]
      dt3[grepl('euro', indicator), value := erscore]
      
      # dcast to add totals, to be used to update scores when measures are conflicting
      cols <- c('biodiversity', 'climate', 'landscape', 'soil','water','total')
      dt4 <- dcast(dt3, bbwp_id + bbwp_conflict  ~ indicator, value.var = 'value')
      dt4[, total := biodiversity + climate + landscape + soil + water]
      dt4[, oid := frank(-total, ties.method = 'first',na.last = 'keep'),by = c('bbwp_conflict')]
      dt4[oid > 1, c(cols) := 0]
      
      # measure index crop diversification (score dependent on cultivated area)
      dt4[grepl('B189|B190|B191',bbwp_id), c(cols) := lapply(.SD, function (x) x * dt.farm$area_cultivated/dt.farm$area_farm), .SDcols = cols]

      # add correction reward
      dt5 <- merge(dt.region[,c("id","bbwp_id","regio_factor")], dt[,c("id","reward_cf")], by = "id")
      dt4 <- merge(dt4,dt5[, c("bbwp_id","regio_factor","reward_cf")], by = "bbwp_id")
      dt4[, cfr := fifelse(regio_factor == 1, reward_cf, 1)]

      # calculate the applicable area of farm level measures that apply on specific area of farm based on crop type and get score per farm
      # farm level measures that are applicable on specific area of farm based on crop type
      dt.ha <- dt4[grepl("B115|B116|B117|B118|B124|B125|B126|B127|B128|B129|B130|B133|B134|B135|B136", bbwp_id),]
      
      # check whether measures to be calculated are present
      if(nrow(dt.ha) > 0){
        
      # get eco_id of measures and merge with measures taken
      dt.measures <- as.data.table(BBWPC::bbwp_measures)
      dt.measures <- dt.measures[, c("bbwp_id","eco_id")]
      dt.ha <- merge(dt.ha,
                     dt.measures, by = "bbwp_id")
      
      # get eco_id's of measures
      eco.dt <- dt.ha[,c("eco_id")]
     
      # get crop types on farm and corresponding area
      crop.dt <- dt[, c("B_LU_BRP","B_AREA")]
     
      # get all combinations of B_LU_BRP and eco_id of farm measures taken
      combi.dt <- setkey(crop.dt[,c(k=1,.SD)],k)[eco.dt[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]
      
      # combine columns to be used for check on applicability
      combi.dt[, combi := paste0(B_LU_BRP,eco_id)]
      dt.meas.eco[, combi := paste0(B_LU_BRP,eco_id)]
      
      # set appl to 1 when combination of eco_id and crop exists
      appl.dt <- combi.dt[, appl := fifelse(combi %in% dt.meas.eco$combi,1,0)]
      
      # sum up area to which the measure applies per measure
      appl.dt <- appl.dt[appl ==1 , area_appl := sum(B_AREA)/10000, by = "eco_id"]
      
      # get per measures the area to which the measure applies
      appl.dt <- appl.dt[!is.na(area_appl), c("eco_id","area_appl")]
      appl.dt <- unique(appl.dt)
      
      # merge applicable area into original table
      dt.ha <- merge(dt.ha,appl.dt, by = "eco_id", all.x = T)
      
      # when measure has no applicable area, set area_appl to 0 and remove eco_id column
      dt.ha[, area_appl := fifelse(is.na(area_appl),0,area_appl)][,eco_id := NULL]
      
      # select cols
      #cols <- c('biodiversity', 'climate', 'landscape', 'soil','water','total','euro_ha')
      
      # get score and euro per farm for measures with specific application area (in score/farm)
      dt.ha[, c(cols) := lapply(.SD, function(x) x * area_appl), .SDcols = cols]
      dt.ha <- dt.ha[!is.na(euro_ha), euro_farm := euro_ha * area_appl][,euro_ha := 0]
      
      # remove area_appl column 
      dt.ha <- dt.ha[, area_appl := NULL]
      
      # rbind dt.ha with remaining measures of dt4
      dt4 <- rbind(dt.ha,dt4[!grepl("B115|B116|B117|B118|B124|B125|B126|B127|B128|B129|B130|B133|B134|B135|B136", bbwp_id)])
      
    }
      
      # sum total score (score per farm to score per ha)
      #cols <- c('biodiversity', 'climate', 'landscape', 'soil','water','total')
      dt6 <- dt4[,lapply(.SD,sum), .SDcols = cols]
      dt.farm.score <- dt6[, c(cols):= lapply(.SD, function (x) x / (dt.farm$area_farm/10000)) , .SDcols = cols]
      dt.farm.reward <- dt4[,list(er_reward = cfr * ( max(euro_ha[total>0],0) + max(euro_farm[total>0],0) / (dt.farm$area_farm/10000) ))][1]
      
      
    } else {
      
      dt.farm.score <- data.table(biodiversity = 0,
                                  climate = 0,
                                  landscape = 0,
                                  soil = 0,
                                  water = 0,
                                  total = 0)
      dt.farm.reward <- data.table(er_reward = 0)
    }
  
    # total score for farm measures and crop rotation
    out <- dt.farm.score + dt.field.score
    out[,farmid := 1]
    out[, total := biodiversity + climate + landscape + soil + water]
    out[,S_ER_REWARD := dt.farm.reward + dt.field.reward]
    setcolorder(out,'farmid') 
    
    # data table with measures applied on field and farm level and corresponding scores (in score/ha) to be used for pdf 
    if(pdf == TRUE){
      
      pdf <- er_pdf(croprotation = TRUE,
                    measurescores = FALSE,
                    dt.field.measures = dt2,
                    dt.farm.measures = dt4, 
                    B_AREA = B_AREA)
      out <- list(out = out, pdf = pdf)

    }
  
  # return the Ecoregelingen Score based on Crop Rotation and Farm Measures
  # output has units score / ha and euro/ha
  return(out)
}
