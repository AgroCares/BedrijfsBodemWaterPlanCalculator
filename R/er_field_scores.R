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
#'
#' @export
# calculate the opportunities for a set of fields
er_field_scores <- function(B_SOILTYPE_AGR, B_LU_BRP, B_LU_BBWP,
                            D_AREA,
                            B_CT_SOIL, B_CT_WATER,B_CT_CLIMATE,B_CT_BIO,B_CT_LANDSCAPE, 
                            measures, sector){
  
  # add visual bindings
 
  
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
  
  # what is the opportunity to contribute to environmental challenges
  # in theory is that maximum, since there are not yet measures applied
  dt[, D_OPI_SOIL := 1]
  dt[, D_OPI_WATER := 1]
  dt[, D_OPI_CLIMATE := 1]
  dt[, D_OPI_BIO := 1]
  dt[, D_OPI_LANDSCAPE := 1]
  
  # columns with the Ecoregelingen ranks
  cols <- c('er_fsoil','er_fwat','er_fbio','er_fclim','er_fland')
  
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
    
    # add basic ER points when borders are available
    dt.fm <- data.table(indicator = cols, 
                        m1 = c(0,10,0,20,10), # EG15
                        m2 = c(0,0,3,7,10), # EG22
                        m3a = c(5,1,2,1,1), # EB1
                        m3b = c(6,1,3,1,1), # EB1
                        m3c = c(7,1,4,1,1), # EB1
                        m4 = c(3,0,3,5,3), # EB2
                        m5 = c(4,2,2,2,0), # EB3
                        m6 = c(5,5,0,0,0) # EB8
                        )
    
    # B_LU_BRP van rustgewassen
    crops.dr <- c(235,236,1921,238,944,3512,246,3506,1922,1923,666,258,664,
                  3807,237,3519,233,234,381,314,3523,3736,1037,247,799,3524,
                  516,382,1022,2652,266)
    
    # B_LU_BRP van diepwortelende gewassen
    crops.dw <- c(3502,3503,3504,511,428,944,3512,246,799,1922,1923,666,258,516,664,3807,3519,3736,1036,1037)
    
    # B_LU_BRP of eiwitgewassen
    crops.ew <- c(242,853,854,311,308,244,241,663,258,665,2747,2751,2779,799,3524,258)
    
    # B_LU_BRP van meerjarige gewassen
    crops.mj <- c(265,266,799,258,516)
    if('dairy' %in% sector){crops.mj <- crops.mj[crops.mj != 265]}
    
    # B_LU_BRP van gewassen gunstige wortel-spruitverhouding
    crops.ws <- c(516,944,3736)
    
    # merge dt.farm with the farm measures
    dt.farm <- merge(dt.farm,dt.fm,by='indicator')
    
    # apply filters and selections
    
      # start value
      dt.farm[,erscore:=0]
    
      # add kruidenrijke randen (EG15)
      dt.farm[B_LU_BRP %in% c(333,334,370,337), erscore := erscore + m1]
      
      # add kleinschalig landschap (EG22)
      dt.farm[D_AREA < 2, erscore := erscore + m2]
    
      # add filter for rustgewas
      dt.farm[,cf := fifelse(B_LU_BRP %in% crops.dr,1,0)]
      
      # add percentage rustgewassen (EB1)
      dt.farm[,D_AREA_RR := sum(D_AREA * cf) / sum(D_AREA)]
      dt.farm[D_AREA_RR > 20 & D_AREA_RR <= 30, erscore := erscore + m3a]
      dt.farm[D_AREA_RR > 30 & D_AREA_RR <= 40, erscore := erscore + m3b]
      dt.farm[D_AREA_RR > 40, erscore := erscore + m3c]
      
      # add eiwitgewassen (EB2)
      dt.farm[B_LU_BRP %in%crops.ew, erscore := erscore + m4]
      
      # add meerjarige gewassen (EB3)
      dt.farm[B_LU_BRP %in%crops.mj, erscore := erscore + m5]
      
      # add diepwortelende gewassen (EB8)
      dt.farm[B_LU_BRP %in% crops.dw, erscore := erscore + m6]
      
      # teelt van gewassen met een gunstige wortel-spruit (EB9)
      dt.farm[B_LU_BRP %in% crops.ws, erscore := erscore + m7]
      
  # add list of measures
  
  # calculate the change in opportunity indexes given the measures taken
  
  # column names for impact of measures on the five indexes (do not change order)
  mcols <- c('D_MEAS_NGW', 'D_MEAS_NSW', 'D_MEAS_PSW', 'D_MEAS_NUE', 'D_MEAS_WB', 'D_MEAS_TOT')
  
  # estimate these indexes
  nr.measures <- length(Filter(function(x) dim(x)[1] > 0, measures))
  if (nr.measures > 0 ) {
    dt[,c(mcols) := bbwp_meas_score(
      B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR,
      B_GWL_CLASS = dt$B_GWL_CLASS,
      A_P_SG = dt$A_P_SG,
      B_SLOPE = dt$B_SLOPE,
      B_LU_BRP = dt$B_LU_BRP,
      M_DRAIN = dt$M_DRAIN,
      D_WP = dt$D_WP,
      D_OPI_NGW = dt$D_OPI_NGW,
      D_OPI_NSW = dt$D_OPI_NSW,
      D_OPI_PSW = dt$D_OPI_PSW,
      D_OPI_NUE = dt$D_OPI_NUE,
      D_OPI_WB = dt$D_OPI_WB,
      measures = measures,
      sector = sector
    )]
  } else {
    dt[, c('D_MEAS_NGW','D_MEAS_NSW','D_MEAS_PSW','D_MEAS_NUE','D_MEAS_WB','D_MEAS_TOT') := 0]
  }
  
  
  # update the field score with measures
  dt[,D_OPI_NGW := 1 - pmax(0, D_OPI_NGW - D_MEAS_NGW)]
  dt[,D_OPI_NSW := 1 - pmax(0, D_OPI_NSW - D_MEAS_NSW)]
  dt[,D_OPI_PSW := 1 - pmax(0, D_OPI_PSW - D_MEAS_PSW)]
  dt[,D_OPI_NUE := 1 - pmax(0, D_OPI_NUE - D_MEAS_NUE)]
  dt[,D_OPI_WB :=  1 - pmax(0, D_OPI_WB - D_MEAS_WB)]
  
  # Convert form 0-1 to 0-100
  dt[,D_OPI_NGW := 100 * D_OPI_NGW]
  dt[,D_OPI_NSW := 100 * D_OPI_NSW]
  dt[,D_OPI_PSW := 100 * D_OPI_PSW]
  dt[,D_OPI_NUE := 100 * D_OPI_NUE]
  dt[,D_OPI_WB :=  100 * D_OPI_WB]
  
  # calculate the integrative opportunity index (risk times impact)
  dt[,D_OPI_TOT := (D_OPI_NGW * wf(D_OPI_NGW, type="score") + D_OPI_NSW * wf(D_OPI_NSW, type="score") + D_OPI_PSW * wf(D_OPI_PSW, type="score") + D_OPI_NUE * wf(D_OPI_NUE, type="score") + D_OPI_WB * wf(D_OPI_WB, type="score")) /
       (wf(D_OPI_NGW, type="score") + wf(D_OPI_NSW, type="score") +  wf(D_OPI_PSW, type="score") +  wf(D_OPI_NUE, type="score") +  wf(D_OPI_WB, type="score"))]
  
  # order the fields
  setorder(dt, id)
  
  # extract value
  value <- dt[,mget(c('D_OPI_NGW','D_OPI_NSW','D_OPI_PSW','D_OPI_NUE','D_OPI_WB','D_OPI_TOT'))]
  
  # Round the values
  value <- value[, lapply(.SD, round, digits = 0)]
  
  # return value
  return(value)
}
