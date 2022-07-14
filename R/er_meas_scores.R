#' Evaluate the contribution of agronomic measures to improve soil mand water management
#'
#' Estimate the Ecoregeling score for agronomic measures taken to improve soil and water management on agricultural farms.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BBWP (numeric) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param B_AREA (numeric) the area of the field (m2)
#' @param B_LU_ECO1 (boolean) does the crop belong in Ecoregeling category 1
#' @param B_LU_ECO2 (boolean) does the crop belong in Ecoregeling category 2
#' @param B_LU_ECO3 (boolean) does the crop belong in Ecoregeling category 3
#' @param B_LU_ECO4 (boolean) does the crop belong in Ecoregeling category 4
#' @param B_LU_ECO5 (boolean) does the crop belong in Ecoregeling category 5
#' @param B_LU_ECO6 (boolean) does the crop belong in Ecoregeling category 6
#' @param B_LU_ECO7 (boolean) does the crop belong in Ecoregeling category 7
#' @param B_LU_ECO8 (boolean) does the crop fall within the category "arable"
#' @param B_LU_ECO9 (boolean) does the crop fall within the category "productive"
#' @param B_LU_ECO10 (boolean) does the crop fall within the category "cultivated"
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'dairy', 'arable', 'tree_nursery', 'bulbs')
#' @param measures (list) The measures planned / done per fields
#'   
#' @import data.table
#'
#' @export
# calculate the score for a list of measures for one or multiple fields
er_meas_score <- function(B_SOILTYPE_AGR, B_AER_CBS,B_AREA,
                          B_LU_BBWP,B_LU_ECO1,B_LU_ECO2, B_LU_ECO3, B_LU_ECO4, B_LU_ECO5, 
                          B_LU_ECO6, B_LU_ECO7,B_LU_ECO8, B_LU_ECO9,B_LU_ECO10,
                          measures, sector){
  
  # add visual bindings
  eco_id = type = fr_area = id = er_urgency = NULL
  fsector = fdairy = dairy = farable = arable = ftree_nursery = tree_nursery = fbulbs = bulbs = NULL
  crop_cat1 = crop_cat2 = crop_cat3 = crop_cat4 = crop_cat5 = crop_cat6 = crop_cat7 = crop_cat8 = crop_cat9 = NULL
  soiltype = peat = clay = sand = silt = loess = NULL
  patterns = indicator = erscore = urgency = S_ER_REWARD = value = NULL
  total = biodiversity = climate = landscape = soil = water = oid = NULL
  
  # reformat B_AER_CBS
  B_AER_CBS <- bbwp_format_aer(B_AER_CBS)
  
  # check on the inputs
  arg.length <- max(length(B_SOILTYPE_AGR), length(B_LU_BBWP),length(B_AER_CBS), length(B_LU_ECO1),
                    length(B_LU_ECO1),length(B_LU_ECO2),length(B_LU_ECO3),length(B_LU_ECO4),
                    length(B_LU_ECO5),length(B_LU_ECO6),length(B_LU_ECO7),length(B_LU_ECO8),
                    length(B_LU_ECO9),length(B_LU_ECO10))
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = c('duinzand','dekzand','zeeklei','rivierklei','maasklei',
                                                       'dalgrond','moerige_klei','veen','loess'))
  checkmate::assert_integerish(B_LU_BBWP, lower = 0, upper = 12,len = arg.length)
  checkmate::assert_character(B_SOILTYPE_AGR,len = arg.length)
  checkmate::assert_logical(B_LU_ECO1,len = arg.length)
  checkmate::assert_logical(B_LU_ECO2,len = arg.length)
  checkmate::assert_logical(B_LU_ECO3,len = arg.length)
  checkmate::assert_logical(B_LU_ECO4,len = arg.length)
  checkmate::assert_logical(B_LU_ECO5,len = arg.length)
  checkmate::assert_logical(B_LU_ECO6,len = arg.length)
  checkmate::assert_logical(B_LU_ECO7,len = arg.length)
  checkmate::assert_logical(B_LU_ECO8,len = arg.length)
  checkmate::assert_logical(B_LU_ECO9,len = arg.length)
  checkmate::assert_logical(B_LU_ECO10,len = arg.length)
  
  # get the measurement data.table
  dt.meas.taken <- bbwp_check_meas(dt = measures, eco = TRUE, score = TRUE)
      
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
                   B_AREA = B_AREA
                  )
  
  # collect total areas on farm level
  dt.farm <- data.table(area_farm = sum(B_AREA),
                        area_arable = sum(B_AREA * B_LU_ECO8),
                        area_productive = sum(B_AREA * B_LU_ECO9),
                        area_cultivated = sum(B_AREA * B_LU_ECO10))
  
  # merge all measures to the given fields
  dt <- merge(dt,dt.meas.taken,by = 'id',all=TRUE)
  
  # set scores to zero when measures are not applicable given the crop type
  
    # columns with the Ecoregelingen ranks and reward
    cols <- c('er_soil','er_water','er_biodiversity','er_climate','er_landscape','er_euro_ha', 'er_euro_farm')
    
    # set first all missing data impacts to 0
    dt[,c(cols) := lapply(.SD, function(x) fifelse(is.na(x),0,x)), .SDcols = cols]
    
    # set the score to zero when not applicable for given BBWP or ECO crop category
    dt[(B_LU_BBWP == 1 & nc1 == 0) | (B_LU_BBWP == 2 & nc2 == 0), c(cols) := 0]
    dt[(B_LU_BBWP == 3 & nc3 == 0) | (B_LU_BBWP == 4 & nc4 == 0), c(cols) := 0]
    dt[(B_LU_BBWP == 5 & nc5 == 0) | (B_LU_BBWP == 6 & nc6 == 0), c(cols) := 0]
    dt[(B_LU_BBWP == 7 & nc7 == 0) | (B_LU_BBWP == 8 & nc8 == 0), c(cols) := 0]
    dt[(B_LU_BBWP == 9 & nc9 == 0) | (B_LU_BBWP == 10 & nc10 == 0), c(cols) := 0]
    dt[(B_LU_BBWP == 11 & nc11 == 0) | (B_LU_BBWP == 12 & nc12 == 0), c(cols) := 0]
    dt[(B_LU_ECO1 == 1 & eco1 == 0) | (B_LU_ECO2 == 1 & eco2 == 0), c(cols) := 0]
    dt[(B_LU_ECO3 == 1 & eco3 == 0) | (B_LU_ECO4 == 1 & eco4 == 0), c(cols) := 0]
    dt[(B_LU_ECO5 == 1 & eco5 == 0) | (B_LU_ECO6 == 1 & eco6 == 0), c(cols) := 0]
    dt[B_LU_ECO7 == 1 & eco7 == 0, c(cols) := 0]

  # set the score and profit to zero when the measure is not applicable given sector or soil type
  
    # add columns for the sector to which the farms belong
    fs0 <- c('fdairy','farable','ftree_nursery','fbulbs')
    fs1 <- paste0('f',sector)
    fs2 <- fs0[!fs0 %in% fs1]
    dt[,c(fs1) := 1]
    dt[,c(fs2) := 0]
    
    # estimate whether sector allows applicability
    dt[, fsector := fdairy * dairy + farable * arable + ftree_nursery * tree_nursery + fbulbs * bulbs] 
    
    # adapt the score when measure is not applicable
    dt[fsector == 0, c(cols) := 0]
    
    # adapt the score when the soil type limits the applicability of measures
    dt[grepl('klei', B_SOILTYPE_AGR) & clay == FALSE , c(cols) := 0]
    dt[grepl('zand|dal', B_SOILTYPE_AGR) & sand == FALSE , c(cols) := 0]
    dt[grepl('veen', B_SOILTYPE_AGR) & peat == FALSE , c(cols) := 0]
    dt[grepl('loess', B_SOILTYPE_AGR) & loess == FALSE , c(cols) := 0]
    
    # set ER measures that are applicable on farm level to zero
    dt[level == 'farm' | er_euro_farm > 0, c(cols) := 0]
    
  # update score and rewards when there are conflicts in measures 
    
    # columns to select only scores or only reward
    cols.score <- c('er_climate', 'er_soil', 'er_water','er_landscape', 'er_biodiversity')
    cols.reward <- c('er_euro_ha','er_euro_farm')
    
    # adjust the score and reward when measure is applied as ANLB
    dt[acc_anlb == "none" & grepl('ANLB',bbwp_status), c(cols.score,cols.reward) := 0]
    dt[acc_anlb == "score only" & grepl('ANLB',bbwp_status), c(cols.reward) := 0]
    dt[acc_anlb == "reward only" & grepl('ANLB',bbwp_status), c(cols.score) := 0]
   
    # adjust the score and reward when measure is applied as GLMC
    dt[acc_glmc == "none" & grepl('GLMC',bbwp_status), c(cols.score,cols.reward) := 0]
    dt[acc_glmc == "score only" & grepl('GLMC',bbwp_status), c(cols.reward) := 0]
    dt[acc_glmc == "reward only" & grepl('GLMC',bbwp_status), c(cols.score) := 0]
    
  # there are field measures where the score depends on the farm properties
    
    # columns to be updated
    cols.sel <- c('er_climate','er_soil','er_water','er_landscape','er_biodiversity')
    
    # measure EB1. Cultivate rustgewas on a field
    cols.ad1 <- c(3,3,3,0,1)
    cols.ad2 <- c(4,4,4,1,1)
    dt[bbwp_id == 'G54', B_AREA_REL := sum(B_AREA) * 100 / dt.farm$area_arable]
    dt[bbwp_id == 'G54' & B_AREA_REL <= 20, c(cols.sel) := 0]
    dt[bbwp_id == 'G54' & B_AREA_REL > 35 & B_AREA_REL <= 50, c(cols.sel) := Map('+',mget(cols.sel),cols.ad1)]
    dt[bbwp_id == 'G54' & B_AREA_REL > 50, c(cols.sel) := Map('+',mget(cols.sel),cols.ad2)]
    
    # measure EB12. Green cultivation on the field
    cols.ad1 <- c(2,3,3,1,1)
    cols.ad2 <- c(5,6,6,1,2)
    dt[grepl('^EB12',eco_id),B_AREA_REL := sum(B_AREA) * 100 / dt.farm$area_farm]
    dt[grepl('^EB12',eco_id) & B_AREA_REL <= 51, c(cols.sel) := 0]
    dt[grepl('^EB12',eco_id) & B_AREA_REL > 71 & B_AREA_REL <= 85, c(cols.sel) := Map('+',mget(cols.sel),cols.ad1)]
    dt[grepl('^EB12',eco_id) & B_AREA_REL > 85, c(cols.sel) := Map('+',mget(cols.sel),cols.ad2)]
    
    # measure EB13. no till and reduced till
    cols.ad1 <- c(1,1,0,0,0)
    cols.ad2 <- c(2,2,0,0,0)
    dt[grepl('^EB13',eco_id), B_AREA_REL := sum(B_AREA) * 100 / dt.farm$area_arable]
    dt[grepl('^EB13',eco_id) & B_AREA_REL <= 50, c(cols.sel) := 0]
    dt[grepl('^EB13',eco_id) & B_AREA_REL > 65 & B_AREA_REL <= 80, c(cols.sel) := Map('+',mget(cols.sel),cols.ad1)]
    dt[grepl('^EB13',eco_id) & B_AREA_REL > 80, c(cols.sel) := Map('+',mget(cols.sel),cols.ad2)]

    # measure EG11. Apply treated manure from other stable systems
    cols.ad1 <- c(0,2,1,0,2)
    cols.ad2 <- c(0,5,2,0,3)
    dt[grepl('^EG11',eco_id),B_AREA_REL := sum(B_AREA) * 100 / dt.farm$area_farm]
    dt[grepl('^EG11',eco_id) & B_AREA_REL <= 10, c(cols.sel) := 0]
    dt[grepl('^EG11',eco_id) & B_AREA_REL > 25 & B_AREA_REL <= 50, c(cols.sel) := Map('+',mget(cols.sel),cols.ad1)]
    dt[grepl('^EG11',eco_id) & B_AREA_REL > 50, c(cols.sel) := Map('+',mget(cols.sel),cols.ad2)]
    
    # measure EG20. Not productive land
    cols.ad1 <- c(1,0,1,3,5)
    cols.ad2 <- c(2,0,2,6,10)
    dt[grepl('^EG20',eco_id),B_AREA_REL := sum(B_AREA) * 100 / dt.farm$area_farm]
    dt[grepl('^EG20',eco_id) & B_AREA_REL <= 1, c(cols.sel) := 0]
    dt[grepl('^EG20',eco_id) & B_AREA_REL > 3 & B_AREA_REL <= 5, c(cols.sel) := Map('+',mget(cols.sel),cols.ad1)]
    dt[grepl('^EG20',eco_id) & B_AREA_REL > 5, c(cols.sel) := Map('+',mget(cols.sel),cols.ad2)]
    
    # measure EG13. inzet baggerspuit (check na update maatregelentabel, EG13 kan 1 keer per perceel voorkomen)
    cols.ad1 <- c(0,0,5,0,5)
    dt[grepl('EG13',eco_id), B_AREA_REL := sum(B_AREA) / dt.farm$area_farm]
    dt[grepl('EG13',eco_id) & B_AREA_REL < 25, c(cols.score) := 0]
    dt[grepl('EG13',eco_id) & B_AREA_REL > 50, c(cols.sel) := Map('+',mget(cols.sel),cols.ad1)]

    # measure EG13. inzet baggerspuit (check na update maatregelentabel, EG13 kan 1 keer per perceel voorkomen)
    cols.ad1 <- c(0,2,2,1,5)
    dt[grepl('EG14',eco_id), B_AREA_REL := sum(B_AREA) / dt.farm$area_farm]
    dt[grepl('EG14',eco_id) & B_AREA_REL < 25, c(cols.score) := 0]
    dt[grepl('EG14',eco_id) & B_AREA_REL > 50, c(cols.sel) := Map('+',mget(cols.sel),cols.ad1)]
    
  # multiply by (political) urgency
  
    # first add soil type for political and environmental urgency
    dt[grepl('klei', B_SOILTYPE_AGR) , soiltype := 'klei']
    dt[grepl('zand|dal', B_SOILTYPE_AGR), soiltype := 'zand']
    dt[grepl('veen', B_SOILTYPE_AGR), soiltype := 'veen']
    dt[grepl('loess', B_SOILTYPE_AGR), soiltype := 'loess']
    
    # melt dt
    dt <- melt(dt,
               id.vars = c('id','bbwp_id','soiltype','bbwp_conflict'),
               measure = patterns(erscore = "^er_"),
               variable.name = 'indicator',
               value.name = 'value')
    dt[,indicator := gsub('er_', '',cols[indicator])]
    
    # merge with urgency table
    dt <- merge(dt,dt.er.urgency, by= c('soiltype','indicator'),all.x = TRUE)
    
  # adapt the score based on urgency
  dt[!grepl('euro',indicator), value := value * urgency]
    
  # dcast to add totals, to be used to update scores when measures are conflicting
  
    cols <- c('biodiversity', 'climate', 'landscape', 'soil','water','total')
    dt2 <- dcast(dt, id + soiltype + bbwp_id + bbwp_conflict ~ indicator, value.var = 'value')
    dt2[, total := biodiversity + climate + landscape + soil + water]
    dt2[, oid := frank(-total, ties.method = 'first',na.last = 'keep'),by = c('id','bbwp_conflict')]
    dt2[oid > 1, c(cols) := 0]
    
  # calculate the weighed average ER score (points/ ha) for the whole farm due to measures taken
  dt.field <- dt2[,lapply(.SD,sum), .SDcols = cols, by = 'id']
    
  # select total reward per field which is equal to the reward from the measure with the highest er_euro_ha (euro / ha)
  dt.reward <- dt[grepl('euro_ha',indicator),list(S_ER_REWARD = max(value,na.rm=T)),by = 'id']
  
  # add reward to the field
  dt.field <- merge(dt.field,dt.reward,by='id')
   
  # setnames
  setnames(dt.field,
           c('biodiversity', 'climate', 'landscape', 'soil','water','total'),
           c('D_MEAS_BIO', 'D_MEAS_CLIM', 'D_MEAS_LAND', 'D_MEAS_SOIL', 'D_MEAS_WAT','D_MEAS_TOT'))
  
  # order to ensure field order
  setorder(dt.field, id)
  
  # return value, scores and euros per hectare
  return(dt.field)
}
