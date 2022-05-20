#' Calculate the total score of five opportunity indicators conform Ecoregelingen Scoring
#'
#' Estimate the potential to contribute to agronomic and environmental challenges in a region given aims for soil quality, water quality, climate, biodiversity and landscape
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BRP (integer)
#' @param B_LU_BBWP (numeric) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_CT_SOIL (numeric) the target value for soil quality conform Ecoregeling scoring
#' @param B_CT_WATER (numeric) the target value for water quality conform Ecoregeling scoring
#' @param B_CT_CLIMATE (numeric) the target value for climate conform Ecoregeling scoring
#' @param B_CT_BIO (numeric) the target value for biodiversity conform Ecoregeling scoring
#' @param B_CT_LANDSCAPE (numeric) the target value for landscape quality conform Ecoregeling scoring
#' @param D_AREA (numeric) the area of the field (\ m2 or \ ha) 
#' @param measures (list) the measures planned / done per fields (measurement nr)
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'melkveehouderij','akkerbouw','vollegrondsgroente','boomteelt','bollen','veehouderij','overig')
#'    
#' @import data.table
#' @import stats
#'
#' @export
# calculate the opportunities for a set of fields
er_field_scores <- function(B_SOILTYPE_AGR, B_LU_BRP, B_LU_BBWP,
                            D_AREA,
                            B_CT_SOIL, B_CT_WATER,B_CT_CLIMATE,B_CT_BIO,B_CT_LANDSCAPE, 
                            measures, sector){
  
  # add visual bindings
 value = . = eco_id = b_lu_brp = type = erscore = EG15 = EG22 = cf = EB1A = EB1B = EB1C = NULL
 D_AREA_RR = EB2 = EB3 = EB8 = EB9 = soiltype = urgency = indicator = farmid = NULL
 D_OPI_SOIL = D_OPI_WATER = D_OPI_CLIMATE = D_OPI_BIO = D_OPI_LANDSCAPE = NULL
  
  # check length of the inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(B_LU_BRP),length(B_LU_BBWP))
  
  # check inputs
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = c('duinzand','dekzand','zeeklei','rivierklei','maasklei',
                                                       'dalgrond','moerige_klei','veen','loess'))
  checkmate::assert_integerish(B_LU_BRP, lower = 0, len = arg.length)
  checkmate::assert_integerish(B_LU_BBWP, lower = 0, len = arg.length)
  checkmate::assert_numeric(B_CT_SOIL, lower = 0, upper = 1000, min.len = 1)
  checkmate::assert_numeric(B_CT_WATER, lower = 0, upper = 1000, min.len = 1)
  checkmate::assert_numeric(B_CT_CLIMATE, lower = 0, upper = 1000,min.len = 1)
  checkmate::assert_numeric(B_CT_BIO, lower = 0, upper = 1000, min.len = 1)
  checkmate::assert_numeric(B_CT_LANDSCAPE, lower = 0, upper = 1000,min.len = 1)
  checkmate::assert_list(measures)
  
  # add bbwp table for crop rotation related measures
  dt.er.farm <- as.data.table(BBWPC::er_farm_measure)
  dt.er.farm <- dcast(dt.er.farm,indicator~measure,value.var = 'er_score')
  
  # add bwwp table for crop lists relevant for ecoregeling
  dt.er.crops <- as.data.table(BBWPC::er_crops)
  dt.er.crops <- dt.er.crops[,.(eco_id,b_lu_brp)]
  
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
    D_AREA = D_AREA,
    B_CT_SOIL = B_CT_SOIL, 
    B_CT_WATER = B_CT_WATER,
    B_CT_CLIMATE = B_CT_CLIMATE,
    B_CT_BIO = B_CT_BIO,
    B_CT_LANDSCAPE = B_CT_LANDSCAPE,
  )
  
  # columns with the Ecoregelingen ranks
  cols <- c('soil','water','biodiversity','climate','landscape')
  
  # add the generic farm score as baseline
  
    # make a local copy
    dt.farm <- copy(dt)
    
    # start with zero points
    dt.farm[, c(cols) := list(0,0,0,0,0)]
    
    # melt the data.table to simplify addition of basic ER points
    dt.farm <- melt(dt.farm, 
                    id.vars = c('id','B_SOILTYPE_AGR','B_LU_BRP','D_AREA'),
                    measure.vars = cols,
                    variable.name = 'indicator',
                    value.name = 'm0')
    
    # merge dt.farm with the farm measures
    dt.farm <- merge(dt.farm,dt.er.farm,by='indicator')
    
    # apply filters and selections
    
      # start value
      dt.farm[,erscore:=0]
    
      # add kruidenrijke randen (EG15)
      dt.farm[B_LU_BRP %in% dt.er.crops[eco_id=='EG15',b_lu_brp], erscore := erscore + EG15]
      
      # add kleinschalig landschap (EG22)
      dt.farm[D_AREA < 2, erscore := erscore + EG22]
    
      # add filter for rustgewas (EB1)
      dt.farm[,cf := fifelse(B_LU_BRP %in% dt.er.crops[eco_id=='EB1',b_lu_brp],1,0)]
      
      # add percentage rustgewassen (EB1)
      dt.farm[,D_AREA_RR := sum(D_AREA * cf) / sum(D_AREA)]
      dt.farm[D_AREA_RR > 20 & D_AREA_RR <= 30, erscore := erscore + EB1A]
      dt.farm[D_AREA_RR > 30 & D_AREA_RR <= 40, erscore := erscore + EB1B]
      dt.farm[D_AREA_RR > 40, erscore := erscore + EB1C]
      
      # add eiwitgewassen (EB2)
      dt.farm[B_LU_BRP %in% dt.er.crops[eco_id=='EB2',b_lu_brp], erscore := erscore + EB2]
      
      # add meerjarige gewassen (EB3)
      dt.farm[B_LU_BRP %in% dt.er.crops[eco_id=='EB3',b_lu_brp], erscore := erscore + EB3]
      
      # add diepwortelende gewassen (EB8)
      dt.farm[B_LU_BRP %in% dt.er.crops[eco_id=='EB8',b_lu_brp], erscore := erscore + EB8]
      
      # teelt van gewassen met een gunstige wortel-spruit (EB9)
      dt.farm[B_LU_BRP %in% dt.er.crops[eco_id=='EB9',b_lu_brp], erscore := erscore + EB9]
      
      # add soil type for political and environmental urgency
      dt.farm[grepl('klei', B_SOILTYPE_AGR) , soiltype := 'klei']
      dt.farm[grepl('zand|dal', B_SOILTYPE_AGR), soiltype := 'zand']
      dt.farm[grepl('veen', B_SOILTYPE_AGR), soiltype := 'veen']
      dt.farm[grepl('loess', B_SOILTYPE_AGR), soiltype := 'loess']
 
      # merge with soil specific urgency table
      dt.farm <- merge(dt.farm,dt.er.urgency, by= c('indicator','soiltype'))
      
      # calculate the weighed average ER score (points/ ha) for the whole farm due to crop rotation 
      dt.farm <- dt.farm[,list(erscore = weighted.mean(erscore * urgency, D_AREA)),by = indicator]
      
      # add a farm id
      dt.farm[,farmid := 1]
  
      # dcast the table to make selection easier
      dt.farm <- dcast(dt.farm,farmid~indicator,value.var ='erscore')
      
  # what is the opportunity to contribute to environmental challenges
    
    # in theory is that maximum, since there are not yet measures applied
    # so the OPI is difference between 1 and the current farm score derived from crop rotation
    dt[, D_OPI_SOIL := pmax(0,1 - dt.farm$soil / B_CT_SOIL)]
    dt[, D_OPI_WATER :=  pmax(0,1 - dt.farm$water / B_CT_WATER)]
    dt[, D_OPI_CLIMATE :=  pmax(0,1 - dt.farm$climate / B_CT_CLIMATE)]
    dt[, D_OPI_BIO :=  pmax(0,1 - dt.farm$biodiversity / B_CT_BIO)]
    dt[, D_OPI_LANDSCAPE :=  pmax(0,1 - dt.farm$landscape / B_CT_LANDSCAPE)]
      
    B_CT_SOIL = B_CT_WATER = B_CT_CLIMATE = B_CT_BIO = B_CT_LANDSCAPE = 5
    
  # calculate the change in opportunity indexes given the measures taken
  
  # column names for impact of measures on the five indexes (do not change order)
  mcols <- c('D_MEAS_NGW', 'D_MEAS_NSW', 'D_MEAS_PSW', 'D_MEAS_NUE', 'D_MEAS_WB', 'D_MEAS_TOT')
  
  # # estimate these indexes
  # nr.measures <- length(Filter(function(x) dim(x)[1] > 0, measures))
  # if (nr.measures > 0 ) {
  #   dt[,c(mcols) := bbwp_meas_score(
  #     B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR,
  #     B_GWL_CLASS = dt$B_GWL_CLASS,
  #     A_P_SG = dt$A_P_SG,
  #     B_SLOPE = dt$B_SLOPE,
  #     B_LU_BRP = dt$B_LU_BRP,
  #     M_DRAIN = dt$M_DRAIN,
  #     D_WP = dt$D_WP,
  #     D_OPI_NGW = dt$D_OPI_NGW,
  #     D_OPI_NSW = dt$D_OPI_NSW,
  #     D_OPI_PSW = dt$D_OPI_PSW,
  #     D_OPI_NUE = dt$D_OPI_NUE,
  #     D_OPI_WB = dt$D_OPI_WB,
  #     measures = measures,
  #     sector = sector
  #   )]
  # } else {
  #   dt[, c('D_MEAS_NGW','D_MEAS_NSW','D_MEAS_PSW','D_MEAS_NUE','D_MEAS_WB','D_MEAS_TOT') := 0]
  # }
  # 
  # 
  # # update the field score with measures
  # dt[,D_OPI_NGW := 1 - pmax(0, D_OPI_NGW - D_MEAS_NGW)]
  # dt[,D_OPI_NSW := 1 - pmax(0, D_OPI_NSW - D_MEAS_NSW)]
  # dt[,D_OPI_PSW := 1 - pmax(0, D_OPI_PSW - D_MEAS_PSW)]
  # dt[,D_OPI_NUE := 1 - pmax(0, D_OPI_NUE - D_MEAS_NUE)]
  # dt[,D_OPI_WB :=  1 - pmax(0, D_OPI_WB - D_MEAS_WB)]
  # 
  # # Convert form 0-1 to 0-100
  # dt[,D_OPI_NGW := 100 * D_OPI_NGW]
  # dt[,D_OPI_NSW := 100 * D_OPI_NSW]
  # dt[,D_OPI_PSW := 100 * D_OPI_PSW]
  # dt[,D_OPI_NUE := 100 * D_OPI_NUE]
  # dt[,D_OPI_WB :=  100 * D_OPI_WB]
  # 
  # # calculate the integrative opportunity index (risk times impact)
  # dt[,D_OPI_TOT := (D_OPI_NGW * wf(D_OPI_NGW, type="score") + D_OPI_NSW * wf(D_OPI_NSW, type="score") + D_OPI_PSW * wf(D_OPI_PSW, type="score") + D_OPI_NUE * wf(D_OPI_NUE, type="score") + D_OPI_WB * wf(D_OPI_WB, type="score")) /
  #      (wf(D_OPI_NGW, type="score") + wf(D_OPI_NSW, type="score") +  wf(D_OPI_PSW, type="score") +  wf(D_OPI_NUE, type="score") +  wf(D_OPI_WB, type="score"))]
  # 
  # # order the fields
  # setorder(dt, id)
  # 
  # # extract value
  # value <- dt[,mget(c('D_OPI_NGW','D_OPI_NSW','D_OPI_PSW','D_OPI_NUE','D_OPI_WB','D_OPI_TOT'))]
  # 
  # # Round the values
  # value <- value[, lapply(.SD, round, digits = 0)]
  
  # return value
  return(value)
}
