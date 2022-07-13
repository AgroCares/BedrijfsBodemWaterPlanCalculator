#' Evaluate the contribution of agronomic measures to improve soil mand water management
#'
#' Estimate the Ecoregeling score for agronomic measures taken to improve soil and water management on agricultural farms.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BRP (numeric) The crop type (conform BRP coding, preferable the most frequent crop on the field)
#' @param B_LU_BBWP (numeric) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'diary', 'arable', 'tree_nursery', 'bulbs')
#' @param measures (list) the measures planned / done per fields
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
    B_AER_CBS = B_AER_CBS
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
    dt[B_LU_ECO8 == 1 & eco8 <= 0, c(cols) := 0]
    
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
    
  # set the score and profit to zero when the measure is not applicable given the percentage area of the total farm to which the measure applies 
    
    # add filter for rustgewas (EB1) and estimate percentage rustgewassen
    dt[,cf := fifelse(grepl("^EB1",eco_id) & crop_cat3 == 1,1,0)]
    dt[,B_AREA_RR := sum(B_AREA * cf) / sum(B_AREA)] 
    
    # adapt the score and profit for EB1 based on percentage area rustgewassen
    dt[B_AREA_RR < 21 & eco_id == 'EB1A', c(cols) := 0]
    #dt[B_AREA_RR >= 21 & B_AREA_RR <= 35 & eco_id == 'EB1A', value := value + er_euro_ha]
    dt[B_AREA_RR < 36 & eco_id == 'EB1B', c(cols) := 0]
    #dt[B_AREA_RR >= 36 & B_AREA_RR <= 50 & eco_id == 'EB1B', value := value + er_euro_ha]
    dt[B_AREA_RR < 50 & eco_id == 'EB1C', c(cols) := 0]
    #dt[B_AREA_RR > 50 & eco_id == 'EB1C', value := value + er_euro_ha]
    
    # adapt the score and profit for kleinschalig landschap (EG22) en (EB25)
    #dt[B_AREA < 2 & eco_id == 'EG22|EB25', value := value + er_euro_ha]
    dt[B_AREA > 2 & eco_id == 'EG22|EB25',  c(cols) := 0]
    
    # add filter for 'houtopstanden en water- en moeraselementen' (EG20) and estimate percentage houtopstanden en water- en moeraselementen
    dt[,cf := fifelse(eco_id == "EG20" & crop_cat8 ==1,1,0)] 
    dt[,B_AREA_N := sum(B_AREA * cf) / sum(B_AREA)] 
    
    # adapt the score and profit for EG20 based on percentage area nature (N) (houtwallen en water/moeraselementen) EG20
    dt[B_AREA_N < 1 & grepl("^EG20A",eco_id), c(cols) := 0]
    #dt[B_AREA_N >= 1 & B_AREA_N <= 3.5 & eco_id == 'EG20A', value := value + er_euro_ha]
    dt[B_AREA_N < 3.5 & grepl("^EG20B",eco_id), c(cols) := 0]
    #dt[B_AREA_N >= 3.5 & B_AREA_N <= 5 & eco_id == 'EG20B', value := value + er_euro_ha]
    dt[B_AREA_N < 5 & grepl("^EG20C",eco_id), c(cols) := 0]
    #dt[B_AREA_N > 5 & eco_id == 'EG20C', value := value + er_euro_ha]

    # set score and profit to 0 when a 'vanggewas' is cultivated for measure toepassen mengteelt (EB5)
    dt[grepl('^EB5',eco_id) & crop_cat11 == 1, c(cols) := 0]
    
  # set score to zero when measures is not applicable given Bouwland, Productief or Beteelbaar 
    
    # get internal table of BRP codes with data on Bouwland, Productief or Beteelbaar and rename these cols as preparation on merge
    dt.er.crops <- as.data.table(BBWPC::er_crops)
    setnames(dt.er.crops,c("bouwland","productive","Beteelbaar"),c("is_bouwland","is_productief","is_beteelbaar"))
    
    # merge dt with selected columns from dt.er.crops
    dt <- merge(dt,dt.er.crops[,c("b_lu_brp","is_bouwland","is_productief","is_beteelbaar")], by.x = "B_LU_BRP", by.y = "b_lu_brp")
    dt[Bouwland == 1 & is_bouwland == 0, c(cols):= 0]
    dt[Productief != is_productief, c(cols):= 0]
    dt[Beteelbaar != is_beteelbaar, c(cols):= 0]
  
    
    
    #als Bouwland bij dt$B_LU_BRP niet gelijk is aan bouwland bij dezelfde B_LU_BRP, zet dan score op 0 (of in dit geval test op 2)
    test <- dt[Bouwland != (B_LU_BRP %in% dt.er.crops[b_lu_brp,bouwland]), test :=2]

  
               

    
    
    
    
    
    
    
    
    dt[B_LU_BRP %in% dt.er.crops[eco_id == "EB23",b_lu_brp] & eco_id == "EB23", ]
    
    # add kruidenrijke randen (EG15)
    #dt.fin[B_LU_BRP %in% dt.er.crops[eco_id == 'EG15',b_lu_brp] & eco_id == 'EG15', value := value + er_profit]
    
    # calculate the reward per hectare while correcting for the specific agricultural region to which the measures applies
    dt[, er_euro_ha := value * er_cf]
    
 
    
    # set score and profit to 0 when measure EB23 is not taken on Bouwland
    dt[Bouwland == 0 & grepl('^EB23',eco_id), c(cols) := 0][,value := value + 0]
    
    
    
    
    
    
  # reset score when ECO measure clashes with other ECO measure  
    # als van de maatregelen die worden ingestuurd de ACC_ER op 1 staat, maak dan een tabel met de maatregel in de rij en de maatregelen die in de list ACC_ER_LIST staan met bijbehorende scores
    # selecteer dan uit die tabel de hoogste score en de hoogste reward en voeg die toe bij de betreffende maatregel
    # zorg ervoor dat als een van deze maatregelen nog een keer komt dat die dan niet nogmaals wordt gerekend

    # filter the measures that accumulate and assign to each accumulation a unique ID number in order to do calculations per group
# 
#     dt1 <- dt[Accumulatie_ER_met_ER == 1,]
#     dt1 <- dt1[, acc.id := .I]
#   
#     # if there is an accumulation then do the following calculations
#     if(nrow(dt1)>0){
#    
#     #unlist cumulatie_list_ER, keep acc.id column
#     dt2 = dt1[ , .(acc.id = rep(acc.id, lengths(cumulatie_list_ER)), cumulatie_list_ER = unlist(cumulatie_list_ER))]
#     # eigenlijk krijg je dan:
#     dt2 = data.table(acc.id = c(1,2,2),
#                      cumulatie_list_ER = c("EG1D","EG3A","EG10B"))
#     
#     #merge 
#     dt2 = dt2[dt1[ , !'cumulatie_list_ER'], on = 'acc.id']  
#       
#     # als cumulatie_list_E voorkomt in eco_id kolom: dan maximum nemen van beide rijen
#     cols <- c('er_soil','er_water','er_biodiversity','er_climate','er_landscape','er_euro_ha', 'er_euro_farm')
#     test <- dt2[eco_id %in% cumulatie_list_ER & cumulatie_list_ER %in% eco_id, c(cols) := lapply(.SD,max), .SDcols = c(cols)]
#     
#     }
# 
#   # reset score when ECO measure clashes with ANLb   
#     
#     
    
    
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
  dt[indicator != 'profit', value := value * urgency]
    
  # dcast to add totals, to be used to update scores when measures are conflicting
  
    cols <- c('biodiversity', 'climate', 'landscape', 'soil','water','total')
    dt2 <- dcast(dt, id + soiltype + bbwp_id + bbwp_conflict ~ indicator, value.var = 'value')
    dt2[, total := biodiversity + climate + landscape + soil + water]
    dt2[, oid := frank(-total, ties.method = 'first',na.last = 'keep'),by = c('id','bbwp_conflict')]
    dt2[oid > 1, c(cols) := 0]
    
  # calculate the weighed average ER score (points/ ha) for the whole farm due to measures taken
  dt.field <- dt2[,lapply(.SD,sum), .SDcols = cols, by = 'id']
    
  # calculate total reward per field (euro / ha)
  dt.reward <- dt[indicator == 'profit',list(S_ER_REWARD = sum(value)),by = 'id']
  
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
