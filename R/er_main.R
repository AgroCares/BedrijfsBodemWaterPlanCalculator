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
#' @param pdf (boolean) add table with summary of all measures taken for pdf. Options: TRUE or FALSE
#'  
#' @import data.table
#' @import OBIC
#'
#' @export
ecoregeling <- function(B_SOILTYPE_AGR, B_LU_BRP,B_LU_BBWP,
                        B_GWL_CLASS, B_SLOPE_DEGREE,B_AER_CBS,
                        B_LU_ARABLE_ER, B_LU_PRODUCTIVE_ER,B_LU_CULTIVATED_ER,
                        A_P_SG,D_SA_W, B_AREA,M_DRAIN, farmscore, 
                        measures, sector, output = 'scores', medalscore = 'gold', pdf = FALSE){
  
  # add visual bindings
  S_ER_TOT = S_ER_SOIL = S_ER_WATER = S_ER_CLIMATE = S_ER_BIODIVERSITY = S_ER_LANDSCAPE = S_ER_REWARD = NULL
  medal = s_er_medal = field_id = s_er_reward = s_er_tot = s_er_costs = NULL
  s_er_soil = s_er_water = s_er_climate = s_er_biodiversity = s_er_landscape = s_er_farm_tot = NULL 
  
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
   
  # Calculate the thresholds required for medal evaluation
  dt.farm.thresholds <- er_farm_aim(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                                    B_AREA = B_AREA, 
                                    medalscore = medalscore,thresholds = TRUE)
  
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

  # Calculate the ER farm score (in mean scores per ha)
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
  dt.farm[medal == "bronze", S_ER_REWARD := 60]
  dt.farm[medal == "silver", S_ER_REWARD := 100]
  dt.farm[medal == "gold", S_ER_REWARD := 200]
  dt.farm[medal == "none", S_ER_REWARD := 0]

  # estimate the opportunity index for farm and field
  
  # dt.field.ind.score gives the relative contribution of a single field to the farm objective
  # dt.farm.ind.score gives the averaged farm score (mean scores / ha, mean costs / ha)
  # dt.farm.ind.opi gives the distance to target for the five indicators on farm level
  # dt.farm.score gives the BBWP score, being overall distance to target
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
    
    # convert each list again to a list
    out <- lapply(out,function(x) as.list(na.omit(x)))
    
    # set output object
    out <- data.table(field_id = sort(unique(dt.meas$id)),
                      measures = out)
    
  }
  
  # return output when BBWP field and farm scores are requested
  if(output == 'scores'){
    
    # copy the opportunity indexes on field level (given their contribution to farm score)
    # 90% of score is for the indicator, 10% for the farm reward
    
    if (TRUE){
      
      # select field output where bars reflect contribution to desired total farm score
      out.field <- copy(dt.opi$dt.field.ind.score)
      
      
    } else {
      
      # reset field scores to be similar to the farm score
      out.field <- copy(dt.opi$dt.farm.ind.score)[rep(1,max(dt.fields$id))]
      out.field[,field_id := .I]
      setnames(out.field, tolower(colnames(out.field)))
      setcolorder(out.field,
                  c("field_id","s_er_soil","s_er_water","s_er_climate","s_er_biodiversity",
                    "s_er_landscape","s_er_costs","s_er_farm_tot","s_er_tot")) 

    }
    
    # add the farm medal to the field
    out.field[, s_er_medal := dt.farm$medal]
    
    # add reward corresponding with medal to the field
    out.field[, s_er_reward := dt.farm$S_ER_REWARD]
    
    # set column order for field scores
    setcolorder(out.field,
                c("field_id","s_er_soil","s_er_water","s_er_climate","s_er_biodiversity",
                  "s_er_landscape","s_er_costs","s_er_farm_tot","s_er_medal","s_er_reward","s_er_tot")) 
    
    # copy the farm output and set to lower case
    out.farm <- copy(dt.opi$dt.farm.ind.score)
    setnames(out.farm, tolower(colnames(out.farm)))
    
    # add medal and reward
    out.farm[, s_er_medal := dt.farm$medal]
    out.farm[, s_er_reward := dt.farm$S_ER_REWARD]
    out.farm[, s_er_tot := dt.opi$dt.farm.score]
    
    # set maximum for s_er_costs at 200 and convert to percentage 
    out.farm[, s_er_costs := (pmin(200,s_er_costs)/200)*100]
    
    # set maximum for eco scores and total farm scores on farm level
    out.farm[, s_er_soil := pmin(dt.farm.thresholds$s_er_soil_gold,s_er_soil)] 
    out.farm[, s_er_water := pmin(dt.farm.thresholds$s_er_water_gold,s_er_water)]
    out.farm[, s_er_climate := pmin(dt.farm.thresholds$s_er_climate_gold,s_er_climate)]
    out.farm[, s_er_biodiversity := pmin(dt.farm.thresholds$s_er_biodiversity_gold,s_er_biodiversity)]
    out.farm[, s_er_landscape := pmin(1,s_er_landscape)]
    out.farm[, s_er_farm_tot:= pmin(dt.farm.thresholds$s_er_farmtotal_gold,s_er_farm_tot)]
    
    # add thresholds
    out <- list(farm = c(as.list(out.farm),dt.farm.thresholds),
                fields = out.field)
    
    }
  
  # make table for pdf with summary of all measures taken 
  if(pdf == TRUE){
    
    # table 1: cropping plan
    # get field and crop info
    pdf.1 <- data.table(B_LU_BRP = B_LU_BRP,
                        B_AREA = B_AREA)
    
    # sum up area of same crops 
    pdf.1[, B_AREA := sum(B_AREA), by = "B_LU_BRP"]
    pdf.1 <- unique(pdf.1, by = "B_LU_BRP")
    
    # get corresponding crops names 
    dt.cropname <- as.data.table(pandex::b_lu_brp[, c('B_LU_BRP','B_LU_NAME')])
    
    # merge crop code with crop name
    pdf.1 <- merge(pdf.1,
                   dt.cropname, by = "B_LU_BRP")
    
    # set format of table 1
    pdf.1[, B_LU_BRP := NULL][, B_AREA := B_AREA/10000]
    setcolorder(pdf.1, c("B_LU_NAME","B_AREA"))
    
    # table 2: scores per theme (in %)
    # get relative scores
    pdf.2 <- copy(dt.opi$dt.farm.ind.opi)
    setnames(pdf.2,gsub("S_ER_","",colnames(pdf.2)))
    setnames(pdf.2,colnames(pdf.2),tolower(colnames(pdf.2)))
    pdf.2 <- pdf.2[,c("soil","water","climate","biodiversity","landscape")]
    
    # table 3: financial reward
    # get medal and area info
    pdf.3 <- data.table(medal = out$farm$s_er_medal,
                        reward = out$farm$s_er_reward,
                        tot_area = sum(B_AREA))
    
    # combine medal and reward
    pdf.3[, pb := paste0(medal," (",reward, " euro)"), by = .I]
    
    # set area to ha and calculate compensation for total farm
    pdf.3[, tot_area := tot_area/10000][, compensation := reward * tot_area]
    
    # keep relevant columns
    pdf.3 <- pdf.3[,c("pb","tot_area","compensation")]
    
    # setnames
    setnames(pdf.3,c('prestatie bedrijf','bedrijfsoppervlakte','vergoeding'))  
    
    
    # table 4: field and farm measures with scores per measure
    # get scores and the table with field measures that generated scores 
    pdf.field <- er_meas_score(B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                               B_LU_BBWP = B_LU_BBWP,
                               B_LU_BRP = B_LU_BRP,
                               B_LU_ARABLE_ER = B_LU_ARABLE_ER,
                               B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                               B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                               B_AER_CBS = B_AER_CBS,
                               B_AREA = B_AREA,
                               measures = measures, 
                               sector = sector,
                               pdf = TRUE)
    
    # select the table with field measures that generated scores as input for pdf 
    pdf.field <- pdf.field$pdf
    
    # get scores and the table with field and farm measures that generated scores 
    pdf.farm.field <- er_croprotation(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                      B_LU_BBWP = B_LU_BBWP,
                                      B_LU_BRP = B_LU_BRP,
                                      B_LU_ARABLE_ER = B_LU_ARABLE_ER,
                                      B_LU_PRODUCTIVE_ER = B_LU_PRODUCTIVE_ER,
                                      B_LU_CULTIVATED_ER = B_LU_CULTIVATED_ER,
                                      B_AER_CBS = B_AER_CBS,
                                      B_AREA = B_AREA,
                                      measures = measures,
                                      sector = sector,
                                      pdf = TRUE)
    
    # select the table with field and farm measures that generated scores as input for pdf 
    pdf.farm.field <- pdf.farm.field$pdf
    
    # table with all field and farm measures and corresponding scores
    pdf.4 <- rbind(pdf.field,pdf.farm.field)
    
    # arrange order table columns
    setcolorder(pdf.4, c("level","summary","B_AREA_tot","climate","soil","water","landscape","biodiversity","total"))
    setnames(pdf.4,c('niveau','maatregel','oppervlakte','klim','bod','wat','land','bio','totaal'))
    
    # table 5: total score of field measures and total score of farm measures 
    cols <- c('oppervlakte','klim','bod','wat','land','bio')
    pdf.5 <- pdf.4[,lapply(.SD,function(x) weighted.mean(x,w = as.numeric(oppervlakte))),.SDcols = cols,by=.(niveau)]
    pdf.5 <- rbind(pdf.5,data.table(niveau='totaal',t(colSums(pdf.5[,-1]))))
    pdf.5[niveau=='total',oppervlakte :=  sum(B_AREA)]
    
    # table 6: score aim per theme for field and farm and medal thresholds per theme
    # get scores per field
    pdf.6a <- copy(dt.fields)
    
    # rearrange table 6a
    setnames(pdf.6a,gsub("S_ER_","",colnames(pdf.6a)))
    setnames(pdf.6a,colnames(pdf.6a),tolower(colnames(pdf.6a)))
    pdf.6a <- pdf.6a[, c("id","climate","soil","water","landscape","biodiversity","tot")]
    pdf.6a[, id := paste0("veld ",id)]
    
    # round values in table 6a
    cols <- c("climate","soil","water","landscape","biodiversity","tot")
    pdf.6a[, c(cols) := round(.SD,1), .SDcols = cols]
    #### MOET VELDSCORE WORDEN AFGETOPT??? --> dat moet dan op basis van aim/threshold per field, maar daar is nog geen output van uit er.farm.aim.
    
    # get farm scores per theme
    pdf.6b <- copy(out.farm)
    
    # arrange format table 6b
    pdf.6b[, id := "behaald bedrijf"]
    setnames(pdf.6b,gsub("s_er_","",colnames(pdf.6b)))
    pdf.6b <- pdf.6b[, c("id","climate","soil","water","landscape","biodiversity","farm_tot")]
    setnames(pdf.6b,"farm_tot","tot")
    
    # get medal thresholds
    pdf.6c <- as.data.table(dt.farm.thresholds)
    
    # melt table
    pdf.6c <- melt(pdf.6c, 
                 measure.vars = patterns("climate","soil","water","landscape","biodiversity","tot"),
                 variable.name = "medal",
                 value.name = c("climate","soil","water","landscape","biodiversity","tot"))
    
    # arrange format table 6c
    pdf.6c <- pdf.6c[, c("medal","climate","soil","water","landscape","biodiversity","tot")]
    pdf.6c[, medal := as.character(medal)]
    pdf.6c[, medal := fifelse(medal == "1","drempelwaarde brons",medal)][, medal := fifelse(medal == "2","drempelwaarde zilver",medal)][, medal := fifelse(medal == "3","drempelwaarde goud",medal)]
    setnames(pdf.6c,"medal","id")
    
    # set missing landscape threshold to 0
    pdf.6c[, landscape := fifelse(is.na(landscape),0,landscape)]

    # bind tables 6a, 6b, 6c
    pdf.6 <- rbind(pdf.6a,pdf.6b,pdf.6c)
    
    # set names pdf.6
    setnames(pdf.6,c('omschrijving','klim','bod','wat','land','bio','totaal'))
    
    # table 7 midpoints field

    r7_title = 'locatie van het perceel'
    r6_title = 'Puntenscore doel op perceels- en bedrijfsniveau en per prestatieniveau'
    r5_title = 'Maatregelen (totaal) op perceels- en bedrijfsniveau'
    r4_title = 'Maatregelen (uitgesplitst) op perceels- en bedrijfsniveau'
    r3_title = 'Vergoedingen EcoRegeling'
    r2_title = 'Ecoregeling puntenscore'
    r1_title = 'Gegevens bouwplan bedrijf'
    
    
    # add pdf table to output when pdf is requested
    out <-list(r1 = pdf.1,
               r1_title = r1_title,
               r2 = pdf.2,
               r2_title = r2_title,
               r3 = pdf.3,
               r3_title = r3_title,
               r4 = pdf.4,
               r4_title = r4_title,
               r5 = pdf.5,
               r5_title = r5_title,
               r6 = pdf.6,
               r6_title = r6_title,
               r7 = NULL,
               r7_title = r7_title)
  }

  # return output
  return(out)
  
}
