#' Evaluate the contribution of agronomic measures to improve soil mand water management
#'
#' Estimate the Ecoregeling score for agronomic measures taken to improve soil and water management on agricultural farms.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BRP (numeric) The crop type (conform BRP coding, preferable the most frequent crop on the field)
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
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'diary', 'arable', 'tree_nursery', 'bulbs')
#' @param measures (list) The measures planned / done per fields
#'   
#' @import data.table
#'
#' @export
# calculate the score for a list of measures for one or multiple fields
er_meas_score <- function(B_SOILTYPE_AGR, B_LU_BRP,B_LU_BBWP,B_AER_CBS, measures, sector){
  
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
  arg.length <- max(length(B_SOILTYPE_AGR), length(B_LU_BRP),length(B_LU_BBWP),length(B_AER_CBS))
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = c('duinzand','dekzand','zeeklei','rivierklei','maasklei',
                                                       'dalgrond','moerige_klei','veen','loess'))
  checkmate::assert_integerish(B_LU_BRP, lower = 0, len = arg.length)
  checkmate::assert_integerish(B_LU_BBWP, lower = 0, upper = 9,len = arg.length)
  checkmate::assert_character(B_SOILTYPE_AGR,len = arg.length)
  
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
  dt <- data.table(
    id = 1:arg.length,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    B_LU_BRP = B_LU_BRP,
    B_LU_BBWP = B_LU_BBWP,
    B_LU_ECO1 = B_LU_ECO1,
    B_LU_ECO2 = B_LU_ECO2,
    B_LU_ECO3 = B_LU_ECO3,
    B_LU_ECO4 = B_LU_ECO4,
    B_LU_ECO5 = B_LU_ECO5,
    B_LU_ECO6 = B_LU_ECO6,
    B_LU_ECO7 = B_LU_ECO7,
    B_AER_CBS = B_AER_CBS,
    B_AREA = B_AREA
  )
  
  # merge all measures to the given fields
  dt <- merge(dt,dt.meas.taken,by = 'id',all=TRUE)
  
  # set scores to zero when measures are not applicable given the crop type
  
    # columns with the Ecoregelingen ranks and reward
    cols <- c('er_soil','er_water','er_biodiversity','er_climate','er_landscape','er_euro_ha', 'er_euro_farm')
    
    # set first all missing data impacts to 0
    dt[,c(cols) := lapply(.SD, function(x) fifelse(is.na(x),0,x)), .SDcols = cols]
    
    # set the score to zero when not applicable for given crop category
    # this includes also the measures that are taken on farm level
    # ACTION NEEDED 
    dt[B_LU_BBWP == 1 & crop_cat1 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 2 & crop_cat2 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 3 & crop_cat3 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 4 & crop_cat4 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 5 & crop_cat5 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 6 & crop_cat6 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 7 & crop_cat7 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 8 & crop_cat8 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 9 & crop_cat9 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 10 & crop_cat10 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 11 & crop_cat11 <= 0, c(cols) := 0]
    dt[B_LU_BBWP == 12 & crop_cat12 <= 0, c(cols) := 0]
    dt[B_LU_ECO1 == 1 & eco1 <= 0, c(cols) := 0]
    dt[B_LU_ECO2 == 1 & eco2 <= 0, c(cols) := 0]
    dt[B_LU_ECO3 == 1 & eco3 <= 0, c(cols) := 0]
    dt[B_LU_ECO4 == 1 & eco4 <= 0, c(cols) := 0]
    dt[B_LU_ECO5 == 1 & eco5 <= 0, c(cols) := 0]
    dt[B_LU_ECO6 == 1 & eco6 <= 0, c(cols) := 0]
    dt[B_LU_ECO7 == 1 & eco7 <= 0, c(cols) := 0]

  # set the score and profit to zero when the measure is not applicable given sector or soil type
  
    # add columns for the sector to which the farms belong
    fs0 <- c('fdairy','farable','ftree_nursery','fbulbs')
    fs1 <- paste0('f',sector)
    fs2 <- fs0[!fs0 %in% fs1]
    dt[,c(fs1) := 1]
    dt[,c(fs2) := 0]
    
    # estimate whether sector allows applicability
    dt[, fsector := fdairy * dairy + farable * arable + ftree_nursery * tree_nursery + fbulbs * bulbs] # dit werkt niet, generates NA for fsector
    
    # adapt the score when measure is not applicable
    dt[fsector == 0, c(cols) := 0]
    
    # adapt the score when the soil type limits the applicability of measures
    dt[grepl('klei', B_SOILTYPE_AGR) & clay == FALSE , c(cols) := 0]
    dt[grepl('zand|dal', B_SOILTYPE_AGR) & sand == FALSE , c(cols) := 0]
    dt[grepl('veen', B_SOILTYPE_AGR) & peat == FALSE , c(cols) := 0]
    dt[grepl('loess', B_SOILTYPE_AGR) & loess == FALSE , c(cols) := 0]
    
  # set the score and profit to zero when the measure is not applicable given the percentage area of the total farm to which the measure applies 
    
    # add filter for rustgewas (EB1) and estimate percentage rustgewassen
    dt1 <- dt[,cf := fifelse(grepl("^EB1",eco_id) & B_LU_BBWP == 3,1,0)] 
    dt1 <- unique(dt1, by= c("id","cf"))
    dt1 <- dt1[, .SD[which.max(abs(cf))], by=id]
    dt1 <- dt1[,B_AREA_RR := sum(B_AREA * cf) / sum(B_AREA)] 
    B_AREA_RR = dt1$B_AREA_RR[1]
    dt[, B_AREA_RR := B_AREA_RR]
    
    # adapt the score and profit for EB1 based on percentage area rustgewassen
    dt[B_AREA_RR < 21 & eco_id == 'EB1A', c(cols) := 0]
    dt[B_AREA_RR < 36 & eco_id == 'EB1B', c(cols) := 0]
    dt[B_AREA_RR < 50 & eco_id == 'EB1C', c(cols) := 0]

    # adapt the score and profit for kleinschalig landschap (EG22) en (EB25)
    dt[B_AREA > 2 & eco_id == 'EG22|EB25',  c(cols) := 0]
    
    # add filter for 'houtopstanden en water- en moeraselementen' (EG20) and estimate percentage houtopstanden en water- en moeraselementen
    dt1 <- dt[,cf := fifelse(grepl("^EG20",eco_id) & B_LU_BBWP == 8,1,0)] 
    dt1 <- unique(dt1, by= c("id","cf"))
    dt1 <- dt1[, .SD[which.max(abs(cf))], by=id]
    dt1 <- dt1[,B_AREA_N := sum(B_AREA * cf) / sum(B_AREA)] 
    B_AREA_N = dt1$B_AREA_N[1]
    dt[, B_AREA_N := B_AREA_N]
    
    # adapt the score and profit for EG20 based on percentage area nature (N) (houtwallen en water/moeraselementen) EG20
    dt[B_AREA_N < 5 & grepl("^EG20A",eco_id), c(cols) := 0]
    dt[B_AREA_N < 7.5 & grepl("^EG20B",eco_id), c(cols) := 0]
    dt[B_AREA_N < 9 & grepl("^EG20C",eco_id), c(cols) := 0]

    # set score and profit to 0 when a 'vanggewas' is cultivated for measure toepassen mengteelt (EB5)
    dt[grepl('^EB5',eco_id) & B_LU_BBWP == 11, c(cols) := 0]
    
  # set score to zero when measures is not applicable given Bouwland, Productief or Beteelbaar 
    
    # get internal table of BRP codes with data on Bouwland, Productief or Beteelbaar and rename these cols as preparation on merge
    dt.er.crops <- as.data.table(BBWPC::er_crops)
    setnames(dt.er.crops,c("bouwland","productive","Beteelbaar"),c("is_bouwland","is_productief","is_beteelbaar"))
    
    # merge dt with selected columns from dt.er.crops
    dt <- merge(dt,dt.er.crops[,c("b_lu_brp","is_bouwland","is_productief","is_beteelbaar")], by.x = "B_LU_BRP", by.y = "b_lu_brp", all.x = TRUE)
    dt[is_bouwland == 1 & Bouwland <= 0, c(cols):= 0]
    dt[is_productief == 1 & Productief <= 0, c(cols):= 0]
    dt[is_beteelbaar == 1 & Beteelbaar <= 0, c(cols):= 0]

    # calculate the reward per hectare while correcting for the specific agricultural region to which the measures applies
    dt[, er_euro_ha := er_euro_ha * er_cf]

  # update score and rewards when there are conflicts in measures 
    cols 
    dt[ACC_ANLB == "none", c(cols) := 0]
    dt[ACC_ANLB == "score only", c(cols) := 0]
    dt[ACC_ANLB == "reward only", c(cols) := 0]
    dt[ACC_ANLB == "both score and reward", c(cols) := 0]
    
    
    
    
    
    
    
    
    
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
  dt[indicator != 'er_euro_ha|er_euro_farm', value := value * urgency]
    
  # dcast to add totals, to be used to update scores when measures are conflicting
  
    cols <- c('biodiversity', 'climate', 'landscape', 'soil','water','total')
    dt2 <- dcast(dt, id + soiltype + bbwp_id + bbwp_conflict ~ indicator, value.var = 'value')
    dt2[, total := biodiversity + climate + landscape + soil + water]
    dt2[, oid := frank(-total, ties.method = 'first',na.last = 'keep'),by = c('id','bbwp_conflict')]
    dt2[oid > 1, c(cols) := 0]
    
  # calculate the weighed average ER score (points/ ha) for the whole farm due to measures taken
  dt.field <- dt2[,lapply(.SD,sum), .SDcols = cols, by = 'id']
    
  # select total reward per field which is equal to the reward from the measure with the highest er_euro_ha (euro / ha)
  dt.reward <- dt[indicator == 'er_euro_ha',list(S_ER_REWARD = max(value)),by = 'id']
  
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
