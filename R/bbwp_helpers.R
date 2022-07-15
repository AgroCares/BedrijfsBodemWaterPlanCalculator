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
#' 
#' @export
wf <- function(x, type = "indicators") {
  
  if (type == "indicators") {
    
    y <- 1 / (1 - x + 0.2)
    
  } else if (type == "score") {
    
    y <- 1 / (x * 0.01 + 0.2)
    
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
  
  # set all scoring, applicabilility and effects to zero when data is missing
    
    # get relevant colums to be converted
    scols <- colnames(dt)[grepl('^nsw|^ngw|^psw|^p_|^n_|^effect|^er',colnames(dt))]
    
    # update the columns
    dt[,c(scols) := lapply(.SD, function(x) fifelse(is.na(x),0,x)), .SDcols = scols]
    
    # select these columns
    dt <- dt[,mget(cols.use)]
    
  # return output
  return(dt)
    
  
}


#' Helper function to check the LSW table 
#' 
#' If there is no input LSW, the LSW properties are derived form the oow_id given 
#' IF available as geopackage, the lon/lat can be used to select LSW properties
#' 
#' @param LSW (data.table) The input LSW table with `oow_id` as identifier
#' @param lsw.sf (sf object) a geopackage with the LSW properties, crs 4326
#' @param a_lon (numeric) Longitude of the field (required if no LSW is submitted)
#' @param a_lat (numeric) Latitude of the field (required if no LSW is submitted)
#' 
#' @import data.table
#' @import sf
#' 
#' @details 
#' Due to high memory use, the spatial LSW gpkg is moved to "dev" directory of the package.
#' 
#' @export
bbwp_check_lsw <- function(LSW, a_lat = NULL, a_lon = NULL,lsw.sf = NULL){
  
  # add visual bindings
  id = NULL
  
  # check inputs
  checkmate::assert_data_table(LSW)
  
  # which columns need to be present in LSW
  cols <- c('n_rt','p_cc','p_al','p_wa','p_sg','fe_ox','al_ox','clay_mi','sand_mi','silt_mi',
            'som_loi','ro_r','sa_w')
  cols <- c(paste0('mean_',cols),paste0('sd_',cols))
  
  # retrieve properties when geopackage and lon-lat of fields are given
  if(!is.null(lsw.sf)){
    
    # length of inputs
    arg.length <- max(length(a_lat),length(a_lon))
    
    # check properties of the spatial object
    checkmate::assert_choice(st_crs(lsw.sf)$input,choices = c('EPSG:4326'))
    checkmate::assert_class(lsw.sf,classes = c('sf'))
    checkmate::assertDataFrame(lsw.sf, nrows = arg.length)
    checkmate::assert_logical('oow_id' %in% colnames(lsw.sf))
    checkmate::assert_logical('geom|geometry' %in% colnames(lsw.sf))
    
    # check inputs lon and lat
    checkmate::assert_numeric(a_lon, lower = 3.3, upper = 7.3, len = arg.length)
    checkmate::assert_numeric(a_lat, lower = 50.5, upper = 53.5, len = arg.length)
      
    # load internal LSW
    lsw.sf <- st_as_sf(lsw.sf)
    
    # make sf object of field location(s)
    loc <- sf::st_sf(geom = st_sfc(st_multipoint(matrix(c(a_lon,a_lat),ncol=2))), crs = 4326)
    
    # crop lsw.sf by an extend slightly bigger than the points
    suppressWarnings(lsw.crop <- st_crop(lsw.sf,st_bbox(loc) + c(-0.005,-0.0025,0.005,0.0025)))
      
    # intersect with the package lsw object
    suppressWarnings(dt <- as.data.table(st_intersection(loc, lsw.crop)))
    
    # add id in the same order as the input
    dt[,id := 1:arg.length]
    
  }
  
  # retrieve properties when only LSW_id is given
  if(ncol(LSW) <= 2 & 'oow_id' %in% colnames(LSW)){ 
    
    # load internal table
    lsw <- as.data.table(BBWPC::lsw)
    
    # check subset of LSW_id
    checkmate::assert_numeric(LSW$oow_id)
    checkmate::assert_subset(LSW$oow_id, choices = lsw$oow_id)
    
    # make internal data.table
    dt <- data.table(id = 1: nrow(LSW),
                     oow_id = LSW$oow_id)
    
    # merge with properties
    dt <- merge(dt, lsw, by = 'oow_id')
    
    # sort the data.table given the input order of lsw_id
    setorder(dt,id)
    
    }
  
  # check and update the LSW properties send in as table
  if(ncol(LSW) > 25){
    
    # make internal copy
    dt <- copy(LSW)
    
    # check format
    checkmate::assert_data_table(dt, min.cols = 26)
    
    # remove database prefix (for case that input originates from NMI-API)
    setnames(dt,gsub('LSW\\.','',colnames(dt)))
    
    # convert old element names for the case that they are present
    setnames(dt, 
             old = c('mean_p_vg','sd_p_vg','mean_os_gv','sd_os_gv','mean_wp','sd_wp'),
             new = c('mean_p_sg','sd_p_sg','mean_som_loi','sd_som_loi','mean_sa_w','sd_sa_w'),
             skip_absent = TRUE)
    
    # relevant columns that need to be present
    cols <- c('oow_nitrogen','oow_phosphate','oow_id','oow_name','oow_source','geom',cols)
    
    # check colnames
    checkmate::assert_subset(colnames(dt),choices = cols)
    
    # add id in the same order as the input
    dt[,id := 1:.N]
  }
  
  # if no output, take the averaged one of internal LSW table
  if(nrow(dt) == 0){
      
    # load internal table
    lsw <- as.data.table(BBWPC::lsw)
    
    # take the median mean and SD value of all LSW properties
    dt <- lsw[,c(cols) := lapply(.SD,median),.SDcols = cols]
      
    # add id in the same order as the input
    dt[,id := 1:.N]
  }
    
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
  
