#' Calculate the Ecoregeling scores on field and farm level
#'
#' Estimate the potential to contribute to agronomic and environmental challenges in a region for a farm and assess the impact of farm measures taken.
#' A high Ecoregeling score is indicative for the number of opportunities to improve soil quality, water quality, climate biodiversity and landscape.
#'
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_LU_BBWP (character) The BBWP category used for allocation of measures to BBWP crop categories
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_LU_ARABLE_ER (boolean) does the crop fall within the ER category "arable"
#' @param B_LU_PRODUCTIVE_ER (boolean) does the crop fall within the ER category "productive"
#' @param B_LU_CULTIVATED_ER (boolean) does the crop fall within the ER category "cultivated"
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param A_P_SG (numeric) The P-saturation index (\%)
#' @param D_SA_W (numeric) The wet perimeter index of the field, fraction that field is surrounded by water
#' @param B_AREA (numeric) the area of the field (m2) 
#' @param M_DRAIN (boolean) is there tube drainage present in the field
#' @param farmscore (numeric) The desired total ER score on farm level
#' @param measures (data.table) The measures planned / done per fields
#' @param sector (string) a vector with the farm type given the agricultural sector (options: options: 'diary', 'arable', 'tree_nursery', 'bulbs')
#' @param output (string) a vector specifying the output type of the function. Options: scores, measures 
#' @param medalscore (character) The desired medal score expressed as bronze, silver or gold 
#'  
#' @import data.table
#' @import OBIC
#'
#' @export
ecoregeling <- function(B_SOILTYPE_AGR, B_LU_BRP,B_LU_BBWP,
                        B_GWL_CLASS, B_SLOPE_DEGREE,B_AER_CBS,
                        B_LU_ARABLE_ER, B_LU_PRODUCTIVE_ER,B_LU_CULTIVATED_ER,
                        A_P_SG,D_SA_W, B_AREA,M_DRAIN, farmscore, 
                        measures, sector, output = 'scores', medalscore = 'gold'){
  
  # add visual bindings
  S_ER_TOT = S_ER_SOIL = S_ER_WATER = S_ER_CLIMATE = S_ER_BIODIVERSITY = S_ER_LANDSCAPE = S_ER_REWARD = NULL
  medal = NULL
  
  # check wrapper inputs that are not checked in the bbwp functions
  checkmate::assert_character(output)
  checkmate::assert_subset(output,choices = c('scores','measures'))
  checkmate::assert_subset(sector, choices = c('dairy', 'arable', 'tree_nursery', 'bulbs'))
  
  # reformat B_AER_CBS and B_LU_BBWP
  B_AER_CBS <- bbwp_format_aer(B_AER_CBS)
  
  # Calculate the minimum required ER scores on Farm level for the desired medal
  dt.farm.aim <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                             B_AREA = B_AREA, 
                             medalscore = medalscore)
   
  # Calculate the aggregated ER scores per field
  dt.fields <- er_field_scores(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                               B_LU_BBWP = B_LU_BBWP,
                               B_LU_BRP = B_LU_BRP,
                               B_LU_ARABLE_ER = B_LU_ARABLE_ER, 
                               B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                               B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                               B_AREA = B_AREA,
                               B_AER_CBS = B_AER_CBS,
                               measures = measures, 
                               sector = sector)

  # Calculate the ER farm score
  dt.farm <- er_farm_score(S_ER_SOIL = dt.fields$S_ER_SOIL,
                           S_ER_WATER = dt.fields$S_ER_WATER,
                           S_ER_CLIMATE = dt.fields$S_ER_CLIMATE,
                           S_ER_BIODIVERSITY = dt.fields$S_ER_BIODIVERSITY,
                           S_ER_LANDSCAPE = dt.fields$S_ER_LANDSCAPE,
                           S_ER_REWARD = dt.fields$S_ER_REWARD,
                           B_AREA = B_AREA)
 
  # estimate the medal on farm level
  dt.farm[, medal := er_medal(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                              S_ER_TOT = dt.fields$S_ER_TOT,
                              S_ER_SOIL = dt.fields$S_ER_SOIL,
                              S_ER_WATER = dt.fields$S_ER_WATER,
                              S_ER_CLIMATE = dt.fields$S_ER_CLIMATE,
                              S_ER_BIODIVERSITY = dt.fields$S_ER_BIODIVERSITY,
                              S_ER_LANDSCAPE = dt.fields$S_ER_LANDSCAPE,
                              S_ER_REWARD = dt.fields$S_ER_REWARD,
                              B_AREA = B_AREA, type = 'farm')]
  
  # correct total reward in dt.farm after medal is awarded
  dt.farm[medal == "bronze", S_ER_REWARD := 70]
  dt.farm[medal == "silver", S_ER_REWARD := 110]
  dt.farm[medal == "gold", S_ER_REWARD := 175]
  dt.farm[medal == "none", S_ER_REWARD := 0]

  # estimate the opportunity index for farm and field
  dt.opi <- er_opi(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   S_ER_SOIL = dt.fields$S_ER_SOIL,
                   S_ER_WATER = dt.fields$S_ER_WATER,
                   S_ER_CLIMATE = dt.fields$S_ER_CLIMATE,
                   S_ER_BIODIVERSITY = dt.fields$S_ER_BIODIVERSITY,
                   S_ER_LANDSCAPE = dt.fields$S_ER_LANDSCAPE,
                   S_ER_REWARD = dt.fields$S_ER_REWARD,
                   B_AREA = B_AREA,
                   medalscore = medalscore)
  
  # return output when preferred measures are requested
  if(output == 'measures'){
    
    # Retreive the best measures to improve
    dt.meas <- er_meas_rank(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                            B_GWL_CLASS = B_GWL_CLASS,
                            A_P_SG = A_P_SG,
                            B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                            B_LU_BBWP = B_LU_BBWP,
                            B_LU_BRP = B_LU_BRP,
                            B_LU_ARABLE_ER = B_LU_ARABLE_ER, 
                            B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                            B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                            B_AER_CBS = B_AER_CBS,
                            M_DRAIN = M_DRAIN,
                            D_SA_W = D_SA_W,
                            B_AREA = B_AREA,
                            B_CT_SOIL = dt.farm.aim$B_CT_SOIL, 
                            B_CT_WATER = dt.farm.aim$B_CT_WATER,
                            B_CT_CLIMATE = dt.farm.aim$B_CT_CLIMATE,
                            B_CT_BIO = dt.farm.aim$B_CT_BIO,
                            B_CT_LANDSCAPE = dt.farm.aim$B_CT_LANDSCAPE,
                            measures = measures,
                            sector = sector
                            )
    
   
    # convert dt.meas to a splitted list
    out <- split(dt.meas,by='id',keep.by = FALSE)
    
    # covnert each list again to a list
    out <- lapply(out,function(x) as.list(na.omit(x)))
    
    # set output object
    out <- data.table(field_id = sort(unique(dt.meas$id)),
                      measures = out)
    
  }
  
  # return output when BBWP field and farm scores are requested
  if(output == 'scores'){
    
    # copy the opportunity indexes on field level (given their contribution to farm score)
    # 90% of score is for the indicator, 10% for the farm reward
    out.field <- copy(dt.opi$dt.field.ind.score)
    
    # add the farm medal to the field
    out.field[, s_er_medal := dt.farm$medal]
    
    # copy the farm output and set to lower case
    out.farm <- copy(dt.farm)
    setnames(out.farm, tolower(colnames(out.farm)))
    
    # rename medal 
    setnames(out.farm,'medal','s_er_medal')
    
    # set output object
    out <- list(farm = as.list(out.farm),fields = out.field)
    
  }
  
  
  # return output
  return(out)
  
}
