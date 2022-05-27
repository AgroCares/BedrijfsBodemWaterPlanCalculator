#' Rank the suitability of agronomic measures to improve soil, climate, biodiversity and water management for a given field
#'
#' Estimate the Ecoregelingen score for agronomic measures to improve soil and water management on agricultural farms. 
#' And send an ordered list back of the most suitable measures.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BRP (integer)
#' @param B_LU_BBWP (numeric) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param A_P_SG (numeric) 
#' @param B_SLOPE (boolean)
#' @param M_DRAIN (boolean) is there tube drainage present in the field
#' @param D_WP (numeric) The fraction of the parcel that is surrounded by surface water
#' @param B_CT_SOIL (numeric) the target value for soil quality conform Ecoregeling scoring
#' @param B_CT_WATER (numeric) the target value for water quality conform Ecoregeling scoring
#' @param B_CT_CLIMATE (numeric) the target value for climate conform Ecoregeling scoring
#' @param B_CT_BIO (numeric) the target value for biodiversity conform Ecoregeling scoring
#' @param B_CT_LANDSCAPE (numeric) the target value for landscape quality conform Ecoregeling scoring
#' @param available_measures (data.table) table with the properties of the available measures
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'diary', 'arable', 'tree_nursery', 'bulbs')
#' 
#'   
#' @import data.table
#'
#' @export
# rank the measures given their effectiveness to improve the sustainability of the farm
er_meas_rank <- function(B_SOILTYPE_AGR, B_LU_BBWP,B_GWL_CLASS, A_P_SG, B_SLOPE, B_LU_BRP, M_DRAIN, D_WP,
                         B_CT_SOIL, B_CT_WATER,B_CT_CLIMATE,B_CT_BIO,B_CT_LANDSCAPE, 
                         available_measures, sector){
  
  # add visual bindings
  eco_id = type = fr_area = D_AREA = id = NULL
  fsector = fdairy = dairy = farable = arable = ftree_nursery = tree_nursery = fbulbs = bulbs = NULL
  crop_cat1 = crop_cat2 = crop_cat3 = crop_cat4 = crop_cat5 = crop_cat6 = crop_cat7 = crop_cat8 = crop_cat9 = NULL
  soiltype = peat = clay = sand = silt = loess = NULL
  er_water = cf_water = er_soil = cf_soil = er_climate = cf_climate = er_biodiversity = cf_biodiversity = er_landscape = cf_landscape = NULL
  
  # derive a table with all possible measurements
  dt.meas.av <- bbwp_check_meas(available_measures,eco = TRUE, score = FALSE)
  
  # get internal table with importance of environmental challenges
  er_scoring <- as.data.table(BBWPC::er_scoring)
  er_urgency <- er_scoring[type=='urgency'][,type := NULL]
  er_aim <- er_scoring[type == 'aim'][,type := NULL]
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(B_LU_BRP),length(B_LU_BBWP),length(A_P_SG),length(B_SLOPE),
                    length(M_DRAIN),length(D_WP),length(B_CT_SOIL),length(B_CT_WATER),length(B_CT_CLIMATE),
                    length(B_CT_BIO),length(B_CT_LANDSCAPE))
  
  # check inputs
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = c('duinzand','dekzand','zeeklei','rivierklei','maasklei',
                                                       'dalgrond','moerige_klei','veen','loess'))
  checkmate::assert_integerish(B_LU_BRP, lower = 0, len = arg.length)
  checkmate::assert_integerish(B_LU_BBWP, lower = 0, upper = 9,len = arg.length)
  checkmate::assert_character(B_SOILTYPE_AGR,len = arg.length)
  
  # collect data in one data.table
  dt <- data.table(
    id = 1:arg.length,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    B_GWL_CLASS = B_GWL_CLASS,
    D_AREA = D_AREA,
    A_P_SG = A_P_SG,
    B_SLOPE = B_SLOPE,
    B_LU_BRP = B_LU_BRP,
    B_LU_BBWP = B_LU_BBWP,
    M_DRAIN = M_DRAIN,
    B_CT_SOIL = B_CT_SOIL, 
    B_CT_WATER = B_CT_WATER,
    B_CT_CLIMATE = B_CT_CLIMATE,
    B_CT_BIO = B_CT_BIO,
    B_CT_LANDSCAPE = B_CT_LANDSCAPE
  )

  # add the generic farm score as baseline
  # this gives the averaged ER score based on the crops in crop rotation plan
  dt.farm <- er_croprotation(B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR,
                             B_LU_BRP = dt$B_LU_BRP,
                             B_LU_BBWP = dt$B_LU_BBWP,
                             D_AREA = dt$D_AREA,
                             B_CT_SOIL = dt$B_CT_SOIL,
                             B_CT_WATER = dt$B_CT_WATER,
                             B_CT_CLIMATE = dt$B_CT_CLIMATE,
                             B_CT_BIO = dt$B_CT_BIO,
                             B_CT_LANDSCAPE = dt$B_CT_LANDSCAPE)
  
  # merge all measures to the given fields
  dt <- as.data.table(merge.data.frame(dt, dt.meas.av, all = TRUE))
  
  # rank is zero when measures are not applicable given the crop type
  
    # columns with the Ecoregelingen ranks
    cols <- c('er_soil','er_water','er_biodiversity','er_climate','er_landscape')
  
    # lower the score with 90% when not applicable for permanent grassland (cat 1), temporary grassland (cat 2),
    # cereals and catch crops (cat 3), tuber crops (cat 4), vegetables (cat 5), bulbs and flowers (cat 6)
    # tree nurseries and fruits (cat 7), nature areas and ditch borders (cat 8) and maize (cat 9)
    dt[B_LU_BBWP == 1 & crop_cat1 <= 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 2 & crop_cat2 <= 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 3 & crop_cat3 <= 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 4 & crop_cat4 <= 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 5 & crop_cat5 <= 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 6 & crop_cat6 <= 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 7 & crop_cat7 <= 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 8 & crop_cat8 <= 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
    dt[B_LU_BBWP == 9 & crop_cat9 <= 0, c(cols) := lapply(.SD,function(x) x * 0.1), .SDcols = cols]
  
    # lower the score when the sector limits the applicability of measures

      # add columns for the farm sector
      fs0 <- c('fdairy','farable','ftree_nursery','fbulbs')
      fs1 <- paste0('f',sector)
      fs2 <- fs0[!fs0 %in% fs1]
      dt[,c(fs1) := 1]
      dt[,c(fs2) := 0]
      
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
    dt[, er_water := er_water * (1 - dt.farm$water)]
    dt[, er_soil := er_soil * (1 - dt.farm$soil)]
    dt[, er_climate := er_climate * (1 - dt.farm$climate)]
    dt[, er_biodiversity := er_biodiversity * (1 - dt.farm$biodiversity)]
    dt[, er_landscape := er_landscape * (1 - dt.farm$landscape)]
    
    # Calculate total measurement score given the distance to target
    dt[, er_total := (er_water + er_soil + er_climate + er_biodiversity + er_landscape) / 5]
    
    # correct total measurement score given minimum scores
    
  # Loop through each field
  
    # define an empty list
    list.meas <- list()

    # select for each field the top5 measures per objective
    for (i in 1:arg.length) {
    
       # list to store output
       list.field <- list()
       
       # Get the overall top measures
       top.tot <- dt[id == i & er_total > 0, ][order(-er_total)][1:5,bbwp_id]
  
       # Get the top measures for soil quality
       top.soil <- dt[id == i & er_soil > 0, ][order(-er_soil)][1:5,bbwp_id]
       
       # Get the top measures for water quality
       top.water <- dt[id == i & er_water > 0, ][order(-er_water)][1:5,bbwp_id]
       
       # Get the top measures for climate
       top.climate <- dt[id == i & er_climate > 0, ][order(-er_climate)][1:5,bbwp_id]
       
       # Get the top measures for biodiversity
       top.biodiversity <- dt[id == i & er_biodiversity > 0, ][order(-er_biodiversity)][1:5,bbwp_id]
       
       # Get the top measures for landscape
       top.landscape <- dt[id == i & er_landscape > 0, ][order(-er_landscape)][1:5,bbwp_id]

       # add them to list
       list.meas[[i]] <- data.table(id = i,
                                    top.tot = top.tot,
                                    top.soil = top.soil,
                                    top.water = top.water,
                                    top.climate = top.climate,
                                    top.biodiversity = top.biodiversity,
                                    top.landscape = top.landscape)
   }
  
  # return value
  out <- data.table::rbindlist(list.meas)
   
  # return value
  return(out)
}

#' Evaluate the contribution of agronomic measures to improve soil mand water management
#'
#' Estimate the Ecoregeling score for agronomic measures taken to improve soil and water management on agricultural farms.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BRP (numeric) The crop type (conform BRP coding, preferable the most frequent crop on the field)
#' @param B_LU_BBWP (numeric) The BBWP category used for allocation of measures to BBWP crop categories
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'diary', 'arable', 'tree_nursery', 'bulbs')
#' @param measures (list) the measures planned / done per fields
#'   
#' @import data.table
#'
#' @export
# calculate the score for a list of measures for one or multiple fields
er_meas_score <- function(B_SOILTYPE_AGR, B_LU_BRP,B_LU_BBWP, measures, sector){
  
  # add visual bindings
  eco_id = type = fr_area = D_AREA = id = er_urgency = NULL
  fsector = fdairy = dairy = farable = arable = ftree_nursery = tree_nursery = fbulbs = bulbs = NULL
  crop_cat1 = crop_cat2 = crop_cat3 = crop_cat4 = crop_cat5 = crop_cat6 = crop_cat7 = crop_cat8 = crop_cat9 = NULL
  soiltype = peat = clay = sand = silt = loess = NULL
  patterns = indicator = erscore = urgency = NULL
  
  # check on the inputs
  arg.length <- max(length(B_SOILTYPE_AGR), length(B_LU_BRP),length(B_LU_BBWP))
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
    D_AREA = D_AREA
  )
  
  # merge all measures to the given fields
  dt <- merge(dt,dt.meas.taken,by = 'id',all=TRUE)
  
  # set scores to zero when measures are not applicable given the crop type
  
    # columns with the Ecoregelingen ranks
    cols <- c('er_soil','er_water','er_biodiversity','er_climate','er_landscape')
    
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
  
  # set the score to zero when the measure is not applicable
  
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
  
  # multiply by (political) urgency
  
    # first add soil type for political and environmental urgency
    dt[grepl('klei', B_SOILTYPE_AGR) , soiltype := 'klei']
    dt[grepl('zand|dal', B_SOILTYPE_AGR), soiltype := 'zand']
    dt[grepl('veen', B_SOILTYPE_AGR), soiltype := 'veen']
    dt[grepl('loess', B_SOILTYPE_AGR), soiltype := 'loess']
    
    # melt dt
    dt <- melt(dt,
               id.vars = c('id','bbwp_id','soiltype'),
               measure = patterns(erscore = "^er_"),
               variable.name = 'indicator',
               value.name = 'erscore')
    dt[,indicator := gsub('er_', '',cols[indicator])]
    
    # merge with urgency table
    dt <- merge(dt,dt.er.urgency, by= c('soiltype','indicator'))
    
  # calculate the weighed average ER score (points/ ha) for the whole farm due to measures taken
  dt.field <- dt[,list(erscore = sum(erscore * urgency)),by = c('id', 'indicator')]
  
  # dcast the output
  dt.field <- dcast(dt.field,id~indicator,value.var = "erscore")
  
  # setnames
  setnames(dt.field,
           c('biodiversity', 'climate', 'landscape', 'soil','water'),
           c('D_MEAS_BIO', 'D_MEAS_CLIM', 'D_MEAS_LAND', 'D_MEAS_SOIL', 'D_MEAS_WAT'))
  
  # order to ensure field order
  setorder(dt.field, id)
  
  # return value
  return(dt.field)
}
