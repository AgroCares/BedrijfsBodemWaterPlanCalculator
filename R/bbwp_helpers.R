#' Calculate the chance that soil property is lower than the measured value given the variation in a catchment
#'
#' assuming a normal distributed density function. The chance can be used as a relative ranking position of the field given the variation inside the catchment.
#'
#' @param smean (numeric) The mean value for a numeric soil property in a given catchment
#' @param ssd (numeric) The variation (standard deviation) among a numeric soil property in a given catchment
#' @param svalue (numeric) The measured value of a soil property of a field inside a catchment
#' 
#'
#' @export
# calculate the ranking position of one or multiple field given a normal distributed soil property in a single catchment
cdf_rank <- function(smean,ssd,svalue){
    
    # set lower and maximum limit for range
    range.lower <- min(svalue,smean - 3 * ssd)
    range.upper <- max(svalue,smean + 3 * ssd)
    
    # set range
    range <- sort(c(seq(range.lower,range.upper,length.out = 1000),svalue))
    
    # make cumulative density function with pnorm
    cdf <- stats::pnorm(range, smean, ssd)
    
    # exctract the probability that a value is below given svalue
    out <- cdf[range %in% svalue]
    
    # return likelyhood
    return(out)
  }
  

#' Helper function to weight and correct the risk and scores
#' 
#' @param x The risk or score value to be weighted
#' @param type Use the weighing function for indicators or score
#' @param penalty (boolean) the option to apply a penalty for high risk BBWP field indicators
#' 
#' @export
wf <- function(x, type = "indicators", penalty = TRUE) {
  
  if (type == "indicators" & penalty == TRUE) {
    
    y <- 1 / (1 - x + 0.2)
    
  } else if (type == "score" & penalty == TRUE) {
    
    y <- 1 / (x * 0.01 + 0.2)
    
  } else if(penalty == FALSE){
    
    y = 1
  }
  
  return(y)
}

#' Helper function to check, update and extend the table with measurements
#' 
#' @param dt (data.table) a data.table containing the bbwp measures with or without properties
#' @param eco (boolean) get only the measures for Ecoregeling (with a given eco_id)
#' @param score (boolean) is check done for scoring of subsets of full list of measures
#' 
#' @export
bbwp_check_meas <- function(dt,eco = FALSE, score = TRUE){
  
  # add visual bindings
  eco_id = bbwp_id = NULL
  
  # get internal table with measures
  dt.measures <- as.data.table(BBWPC::bbwp_measures)
  
  # perform general checks
  
    # check format of the measures given
    checkmate::assert_data_table(dt,null.ok = TRUE)
  
    # check if function argument eco is boolean
    checkmate::assert_logical(eco,len = 1)
  
    # check argument score is boolean
    checkmate::assert_logical(score,len = 1)
    
  # select from internal table only the Ecoregeling measures
  if(eco == TRUE){dt.measures <- dt.measures[!is.na(eco_id)]}
  
  # select the relevant columns as output
  cols.use <- colnames(dt.measures)[!grepl('summary|descr|url|mok|boot',colnames(dt.measures))]
    
  # adapt measurements table when input is given
    
    # select the internal BBWP table when dt is missing and required output is list of measures
    if(is.null(dt) & score == FALSE){dt <- dt.measures}
    
    # set score to zero when no measures are given as input
    if(is.null(dt) & score == TRUE){dt <- data.table(id = 1, bbwp_id = 'NONE',bbwp_status = 'NONE')}
    
    # do checks on table with measures
    if(nrow(dt) > 0){
      
      # check if bbwp-id is present (unique per measure)
      checkmate::assert_true('bbwp_id' %in% colnames(dt))
         
      # which columns are missing in dt
      cols.miss <- unique(c('bbwp_id',colnames(dt.measures)[!colnames(dt.measures) %in% colnames(dt)]))
      
      # merge measurement properties with the input list of available measures
      dt <- merge(dt, dt.measures[,mget(cols.miss)],by='bbwp_id',allow.cartesian = TRUE)
      
    }
    
  # add also missing measures when function requires full list
  if(score == FALSE){
    
      # perform check
      dt.miss <- dt.measures[!bbwp_id %in% dt$bbwp_id]
      
      # add missing ones to dt
      dt <- rbind(dt,dt.miss,fill = TRUE)
      
  } else {
      
    # check if farm-id is present
    checkmate::assert_true('id' %in% colnames(dt))
    
    # check if measure-status is present (unique per measure)
    checkmate::assert_true('bbwp_status' %in% colnames(dt))
    
    # add 'id' and 'status' to cols.use
    cols.use <- unique(c('id','bbwp_status', cols.use))
  }
  
  # set all scoring, applicability and effects to zero when data is missing
    

  
  # get relevant column to be converted
    scols <- colnames(dt)[grepl('^nsw|^ngw|^psw|^p_|^n_|^effect|^er|^regio',colnames(dt))]
    
    # set columns to numeric
    dt[,c(scols) := lapply(.SD, as.numeric), .SDcols = scols]
    
    # update the columns
    dt[,c(scols) := lapply(.SD, function(x) fifelse(is.na(x),0,x)), .SDcols = scols]
    
    # select these columns
    dt <- dt[,mget(cols.use)]
    
  # return output
  return(dt)
    
  
}


#' Helper function to check and update the groundwater class
#' 
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' 
#' @export
bbwp_check_gt <- function(B_GWL_CLASS,B_AER_CBS){

  # check length of inputs
  arg.length <- max(length(B_GWL_CLASS),length(B_AER_CBS))
  
  # check inputs
  checkmate::assert_character(B_GWL_CLASS,len = arg.length)
  checkmate::assert_character(B_AER_CBS,len = arg.length)
  
  # make internal table
  dt <- data.table(B_GWL_CLASS = B_GWL_CLASS,
                   B_AER_CBS = B_AER_CBS)
  
  # remove suffixes
  dt[, B_GWL_CLASS := gsub("b|a|u|s", "", B_GWL_CLASS)]
  
  # replace unknowns 
  dt[B_GWL_CLASS == 'unknown', B_GWL_CLASS := '-']
  
  # change unknown to dry for Limburg
  dt[B_GWL_CLASS == "-" & grepl('lg14|limburg',tolower(B_AER_CBS)), B_GWL_CLASS := "VIII"]
  
  # change unknown to Gt II in other provinces
  dt[, B_GWL_CLASS := gsub("-", "II", B_GWL_CLASS)] 
  
  # add Gt to the class
  dt[!grepl('^Gt|^-',B_GWL_CLASS), B_GWL_CLASS := paste0("Gt", B_GWL_CLASS)]
  
  # checkmate test for the allowed values for B_GWL_CLASS
  checkmate::assert_subset(dt$B_GWL_CLASS, 
                           choices = c('-', 'GtI','GtII','GtII','GtIII','GtIII','GtIV', 'GtV','GtVI','GtVII','GtVIII'))
  # get output
  out <- dt[,B_GWL_CLASS]
  
  # return output
  return(out)
}

#' Convert possible B_AER_CBS values to standardized values
#' 
#' This function formats information of Agricultural Economic Region so it can be understood by other functions
#' 
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' 
#' @import data.table
#' 
#' @examples 
#' bbwp_format_aer(c("LG13","LG12"))
#' bbwp_format_aer(c("LG13","LG12",'Rivierengebied'))
#' 
#' @return 
#' A standardized B_AER_CBS value as required for the BBWP functions. A character string.
#' 
#' @export
bbwp_format_aer <- function(B_AER_CBS) {
  
  # convert UTF-8 encoded strings to latin1 if required
  if('UTF-8' %in% Encoding(B_AER_CBS)) {
    B_AER_CBS <- iconv(B_AER_CBS, from = '', to = 'latin1')
  }
  
  # options for B_AER_CBS
  aer.text <- c('Zuid-Limburg','Zuidelijk Veehouderijgebied','Zuidwest-Brabant',
                'Zuidwestelijk Akkerbouwgebied','Rivierengebied','Hollands/Utrechts Weidegebied',
                'Waterland en Droogmakerijen','Westelijk Holland','IJsselmeerpolders',
                'Centraal Veehouderijgebied','Oostelijk Veehouderijgebied','Noordelijk Weidegebied',
                'Veenkoloni\xEBn en Oldambt', "Veenkolonien en Oldambt",
                'Bouwhoek en Hogeland')
  
  # options for B_AER_CBS
  aer.code <- c("LG14","LG13","LG12","LG11","LG10","LG09","LG08","LG07","LG06","LG05","LG04","LG03","LG02","LG02","LG01")
  
  # all input options
  aer.all <- c(aer.text,aer.code)
  
  # Check if B_GT values are appropriate
  checkmate::assert_subset(B_AER_CBS, empty.ok = FALSE, choices = aer.all)
  
  # which of the input values are database codes
  var.sel <- match(B_AER_CBS,aer.text,nomatch = 0)
  
  # replace numeric values with strings
  B_AER_CBS[B_AER_CBS %in% aer.text] <- aer.code[var.sel]
  
  # Return B_AER_CBS
  return(B_AER_CBS)
}
  
#' Convert possible B_SC_WENR values to standardized values
#' 
#' This function formats information of subsoil compaction so it can be understood by BBWP functions
#' 
#' @param B_SC_WENR (character) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006)
#' 
#' @import data.table
#' 
#' @examples 
#' bbwp_format_sc_wenr(c("1","5"))
#' bbwp_format_sc_wenr(c("Zeer beperkt","Water",'4'))
#' 
#' @return 
#' A standardized B_SC_WENR value as required for the BBWP functions. A character string.
#' 
#' @export
bbwp_format_sc_wenr <- function(B_SC_WENR) {
  
  # ensure B_SC_WENR is a character
  B_SC_WENR <- as.character(B_SC_WENR)
  
  # options for B_SC_WENR
  bsc.text <- c('Zeer beperkt','Beperkt','Matig','Groot','Zeer groot','Beperkt door veenlagen','Van nature dicht',
                'Glastuinbouw, niet beoordeeld','Bebouwing en infrastructuur','Water')
  
  # options for B_SC_WENR
  bsc.code <- c(1,2,3,4,5,10,11,401,901,902)
  
  # all input options
  bsc.all <- c(bsc.text,bsc.code)
  
  # Check if B_GT values are appropriate
  checkmate::assert_subset(B_SC_WENR, empty.ok = FALSE, choices = bsc.all)
  
  # which of the input values are database codes
  var.sel <- match(B_SC_WENR,bsc.text,nomatch = 0)
  
  # replace numeric values with strings
  B_SC_WENR[B_SC_WENR %in% bsc.text] <- bsc.code[var.sel]
  
  # convert to an integer
  B_SC_WENR <- as.integer(B_SC_WENR)
  
  # Return B_SC_WENR
  return(B_SC_WENR)
}

#' Filter measures applicable for the field
#' 
#' This function selects, from a set of all measures, measures which are applicable for the field based on the field properties
#' 
#' @param bbwp_id (character) The unique BBWP measure id based on the van Gerven number of the measure, related to the study by Van Gerven et al. (2020)
#' @param B_LU_BBWP (character) The BBWP category used for allocation of measures to BBWP crop categories
#' @param sector (string) a vector with the farm type given the agricultural sector (options: 'diary', 'arable', 'tree_nursery', 'bulbs')
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#' @param M_DRAIN (boolean) is there tube drainage present in the field
#' @param dt.measures (data table) A table of measures with properties. This should include:
#' bbwp_id, nc1, nc2, nc3, nc4, nc5, nc6, nc7, nc8, nc9, nc10, nc11, nc12,
#' dairy, arable, tree_nursery, bulbs, clay, sand, peat, loess, nodrains
#' 
#' @import data.table
#' 
#' @examples
#' 
#' @return 
#' app (integer) whether the measure is applicable for the field (1) or not (0)
#' 
#' @export
bbwp_meas_filter <-function(bbwp_id,
                            B_LU_BBWP, 
                            sector,  
                            B_SOILTYPE_AGR, 
                            B_SLOPE_DEGREE, 
                            M_DRAIN, 
                            dt.measures){

  # check length of the inputs
  arg.length <- max(length(B_LU_BBWP), length(sector), length(B_SOILTYPE_AGR), length(B_SLOPE_DEGREE),
                    length(M_DRAIN),length(bbwp_id))
  
  # collect data in one data.table
  dt <- data.table(id = 1:arg.length,
                   bbwp_id = bbwp_id,
                   B_LU_BBWP = B_LU_BBWP,
                   sector = sector,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                   M_DRAIN = M_DRAIN
                   )
  
  # merge measure properties
  # this should include: 
  # nc1, nc2, nc3, nc4, nc5, nc6, nc7, nc8, nc9, nc10, nc11, nc12,
  # dairy, arable, tree_nursery, bulbs,
  # clay, sand, peat, loess,
  # nodrains
  dt <- merge(dt, dt.measures, by = "bbwp_id", all.x = T)
  
  
  # Assign zero when measures are not applicable given the crop type
  cols <- c('app_psw','app_nsw', 'app_ngw','app_wb','app_nue')
  
  # set first 1 for all
  dt[,app := 1]
  
  # set the score to zero when not applicable for given crop category
  dt[B_LU_BBWP == 'gras_permanent' & nc1 == 0, app := app * 0.1]
  dt[B_LU_BBWP == 'gras_tijdelijk' & nc2 == 0, app := app * 0.1]
  dt[B_LU_BBWP == 'rustgewas' & nc3 == 0, app := app * 0.1]
  dt[B_LU_BBWP == 'rooivrucht' & nc4 == 0, app := app * 0.1]
  dt[B_LU_BBWP == 'groenten' & nc5 == 0, app := app * 0.1]
  dt[B_LU_BBWP == 'bollensierteelt' & nc6 == 0, app := app * 0.1]
  dt[B_LU_BBWP == 'boomfruitteelt' & nc7 == 0, app := app * 0.1]
  dt[B_LU_BBWP == 'natuur' & nc8 == 0, app := app * 0.1]
  dt[B_LU_BBWP == 'mais' & nc9 == 0, app := app * 0.1]
  dt[B_LU_BBWP == 'randensloot' & nc10 == 0, app := app * 0.1]
  dt[B_LU_BBWP == 'vanggewas' & nc11 == 0, app := app * 0.1]
  dt[B_LU_BBWP == 'eiwitgewas' & nc12 == 0, app := app * 0.1]
  
  # set the score to zero when the measure is not applicable for the sector
  if('sector' %in% colnames(dt)){
    
    # sector correction for desk studies where sector is available / added per field
    dt[,c('fdairy','farable','ftree_nursery','fbulbs') := 1]
    dt[sector == 'dairy', c('ftree_nursery','farable','fbulbs') := 0]
    dt[sector == 'arable', c('ftree_nursery','fdairy','fbulbs') := 0]
    dt[sector == 'bulbs', c('ftree_nursery','fdairy','farable') := 0]
    dt[sector == 'tree_nursery', c('fbulbs','fdairy','farable') := 0]
    
  } else {
    
    fs0 <- c('fdairy','farable','ftree_nursery','fbulbs')
    fs1 <- paste0('f',sector)
    fs2 <- fs0[!fs0 %in% fs1]
    dt[,c(fs1) := 1]
    if(length(fs2) >= 1){ dt[,c(fs2) := 0] }
  }
  
  # estimate whether sector allows applicability
  dt[, fsector := fdairy * dairy + farable * arable + ftree_nursery * tree_nursery + fbulbs * bulbs]
  
  # adapt the score when measure is not applicable
  dt[fsector == 0, app := 0]
  
  # adapt the score when the soil type limits the applicability of measures
  dt[grepl('klei', B_SOILTYPE_AGR) & clay == FALSE , app := 0]
  dt[grepl('zand|dal', B_SOILTYPE_AGR) & sand == FALSE , app := 0]
  dt[grepl('veen', B_SOILTYPE_AGR) & peat == FALSE , app := 0]
  dt[grepl('loess', B_SOILTYPE_AGR) & loess == FALSE , app := 0]
  
  # adapt the score for slope dependent
  dt[B_SLOPE_DEGREE <= 2 & bbwp_id == 'G21',app := 0]
  
  # zuiveren drainage alleen als er ook drains zijn
  dt[M_DRAIN == FALSE & nodrains == TRUE, app := 0]
  
  # order
  setorder(dt, id)
  
  return(dt[, app])
}
