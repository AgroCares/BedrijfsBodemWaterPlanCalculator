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