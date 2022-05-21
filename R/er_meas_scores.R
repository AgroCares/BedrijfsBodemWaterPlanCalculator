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
#' @param D_OPI_NGW (numeric) the opportunity index (risk x impact) for nitrate leaching to groundwater given field properties
#' @param D_OPI_NSW (numeric) the opportunity index (risk x impact) for nitrate leaching and runoff to surface water given field properties
#' @param D_OPI_PSW (numeric) the opportunity index (risk x impact) for phosphorus leaching and runoff to surface water given field properties
#' @param D_OPI_NUE (numeric) the opportunity index (risk x impact) to improve the efficiency of nitrogen and phosphorus fertilizer use given field properties
#' @param D_OPI_WB (numeric) the opportunity index (risk x impact) to improve the potential to buffer and store water and efficiently use water for plant growth given field properties
#' @param available_measures (data.table) table with the properties of the available measures
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'diary', 'arable', 'tree_nursery', 'bulbs')
#' 
#'   
#' @import data.table
#'
#' @export
# rank the measures given their effectiveness to improve the sustainability of the farm
er_meas_rank <- function(B_SOILTYPE_AGR, B_LU_BBWP,B_GWL_CLASS,  A_P_SG, B_SLOPE, B_LU_BRP, M_DRAIN, D_WP,
                           D_OPI_NGW, D_OPI_NSW, D_OPI_PSW, D_OPI_NUE, D_OPI_WB,
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
  arg.length <- 2
  
  # check inputs
  
  # collect data in one data.table
  dt <- data.table(
    id = 1:arg.length,
    B_SOILTYPE_AGR = 'dekzand',
    B_GWL_CLASS = 'GtIII',
    D_AREA = c(15,0.8),
    A_P_SG = 0.15,
    B_SLOPE = 1.5,
    B_LU_BRP = c(265,2005),
    B_LU_BBWP = c(1,4),
    M_DRAIN = TRUE
  )

  # add farm scores
  
  
  # merge all measures to the given fields
  dt <- as.data.table(merge.data.frame(dt, dt.measures, all = TRUE))
  
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
  
    # add columns for the farm sector
    fs0 <- c('fdairy','farable','ftree_nursery','fbulbs')
    fs1 <- paste0('f',sector)
    fs2 <- fs0[!fs0 %in% fs1]
    dt[,c(fs1) := 1]
    dt[,c(fs2) := 0]
    
    setnames(dt,'diary','dairy')
    
    # lower the score when the sector limits the applicability of measures
    
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
  
  # correct for current crop rotation related scores
  
    
    # add here something to stimulate where biggest distance is...
   
  # add farm related scores (see document)
    
    
  # Loop through each field
  
    # define an empty list
    list.measures <- list()
  
      
    # select for each field the top5 measures per objective
    for (i in 1:arg.length) {
    
      # # list to store output
      # list.field <- list()
      # 
      # # Get the overall top measures
      # this.dt.tot <- dt[id == i & D_MEAS_TOT > 0, ]
      # top.tot <- this.dt.tot[order(-D_MEAS_TOT)]$bbwp_id[1:5]
      # list.field$top <- na.omit(top.tot)
      # 
  
  #   # Get the top nsw measures
  #   this.dt.ngw <- dt[id == i & D_MEAS_NGW > 0, ]
  #   top.ngw <- this.dt.ngw[order(-D_MEAS_NGW)]$bbwp_id[1:5]
  #   list.field$ngw <- data.table(
  #     top = 'ngw',
  #     measure = top.ngw,
  #     rank = 1:length(top.ngw)
  #   )
  #   
  #   # Get the top nsw measures
  #   this.dt.nsw <- dt[id == i & D_MEAS_NSW > 0, ]
  #   top.nsw <- this.dt.nsw[order(-D_MEAS_NSW)]$bbwp_id[1:5]
  #   list.field$nsw <- data.table(
  #     top = 'nsw',
  #     measure = top.nsw,
  #     rank = 1:length(top.nsw)
  #   )
  #   
  #   # Get the top psw measures
  #   this.dt.psw <- dt[id == i & D_MEAS_PSW > 0, ]
  #   top.psw <- this.dt.psw[order(-D_MEAS_PSW)]$bbwp_id[1:5]
  #   list.field$psw <- data.table(
  #     top = 'psw',
  #     measure = top.psw,
  #     rank = 1:length(top.psw)
  #   )
  #   
  #   # Get the top nue measures
  #   this.dt.nue <- dt[id == i & D_MEAS_NUE > 0, ]
  #   top.nue <- this.dt.nue[order(-D_MEAS_NUE)]$bbwp_id[1:5]
  #   list.field$nue <- data.table(
  #     top = 'nue',
  #     measure = top.nue,
  #     rank = 1:length(top.nue)
  #   )
  #   
  #   # Get the top wb measures
  #   this.dt.wb <- dt[id == i & D_MEAS_WB > 0, ]
  #   top.wb <- this.dt.psw[order(-D_MEAS_WB)]$bbwp_id[1:5]
  #   list.field$wb <- data.table(
  #     top = 'wb',
  #     measure = top.wb,
  #     rank = 1:length(top.wb)
  #   )
  #   
  #   list.measures[[i]] <- stats::na.omit(rbindlist(list.field))
   }
  # 
  # # return value
   dt.measures <- data.table::rbindlist(list.measures)
   return(dt.measures)
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
  checkmate::assert_data_table(measures)
  checkmate::assert_true('bbwp_id' %in% colnames(measures))
  checkmate::assert_true('id' %in% colnames(measures))
  
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
               id.vars = c('id','bbwp_id','soiltype','D_AREA'),
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
  
  # order to ensure field order
  setorder(dt.field, id)
  
  # return value
  return(dt.field)
}
