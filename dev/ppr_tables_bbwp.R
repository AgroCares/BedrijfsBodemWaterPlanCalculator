# load packages
require(data.table);library(usethis)

# -- prepare measures table -----

  # load measures table
  bbwp_measures <- fread('dev/measures.csv', encoding = 'UTF-8')
  bbwp_measures[bbwp_measures == ''] <- NA

  # setcolorder
  setcolorder(bbwp_measures,'bbwp_id')
  
  # set effect values to 0 when NA
  scols <- colnames(bbwp_measures)[grepl('^nsw|^ngw|^psw|^p_|^n_|^effect|^er|^regio',colnames(bbwp_measures))]
  bbwp_measures[,c(scols) := lapply(.SD, as.numeric), .SDcols = scols]
  bbwp_measures[,c(scols) := lapply(.SD,function(x) fifelse(is.na(x),0,x)),.SDcols = scols]

  # create new crop categories in new columns bbwp, eco1 and eco2
  
    # set BBWP categories where a measure can be applied
    
    # cat1 is permanent grassland, cat2 tijdelijk grasland, graszaad etc
    bbwp_measures[,nc1:= fifelse(c1==1,1,0)]
    bbwp_measures[,nc2:= fifelse(c2==1,1,0)]
    
    # cat3 is rustgewassen (niet gras), cat4 is rooivrucht
    bbwp_measures[,nc3:= fifelse(c3==1,1,0)]
    bbwp_measures[,nc4:= fifelse(c4==1,1,0)]
    
    # cat5 is groenten, cat6 zijn bloembollen en sierteelt
    bbwp_measures[,nc5:= fifelse(c5==1,1,0)]
    bbwp_measures[,nc6:= fifelse(c6==1,1,0)]
    
    # cat7 is boomteelt, fruit, etc, cat8 is natuur
    bbwp_measures[,nc7:= fifelse(c7==1,1,0)]
    bbwp_measures[,nc8:= fifelse(c8==1,1,0)] 
    
    # cat9 is mais, cat10 is (kruidenrijke) randen / sloten
    bbwp_measures[,nc9:= fifelse(c9==1,1,0)] 
    bbwp_measures[,nc10:= fifelse(c10==1,1,0)]
    
    # cat11 is vanggewas, cat12 is eiwitgewas
    bbwp_measures[,nc11:= fifelse(c11==1,1,0)]
    bbwp_measures[,nc12:= fifelse(c12==1,1,0)]
    
    # update names
    setnames(bbwp_measures, 
             old = c('bouwland', 'productief', 'beteelbaar'), 
             new = c('b_lu_arable_er','b_lu_productive_er','b_lu_cultivated_er'))
  
    # remove duplicated columns
    bbwp_measures[,c(paste0('c',1:12)) := NULL]
    
  # update the measure categories
    
    # set the ecoregeling array
    bbwp_measures[klimaat == 1, categories := "klimaat"]
    bbwp_measures[bodemkwaliteit == 1 & !is.na(categories), categories := paste0(categories,"||bodemkwaliteit"),by = .I]
    bbwp_measures[bodemkwaliteit == 1 & is.na(categories), categories := "bodemkwaliteit"]
    bbwp_measures[waterkwaliteit == 1 & !is.na(categories), categories := paste0(categories,"||waterkwaliteit"),by = .I]
    bbwp_measures[waterkwaliteit == 1 & is.na(categories), categories := "waterkwaliteit"]
    bbwp_measures[biodiversiteit == 1 & !is.na(categories), categories := paste0(categories,"||biodiversiteit"),by = .I]
    bbwp_measures[biodiversiteit == 1 & is.na(categories), categories := "biodiversiteit"]
    bbwp_measures[landschap == 1 & !is.na(categories), categories := paste0(categories,"||landschap"),by = .I]
    bbwp_measures[landschap == 1 & is.na(categories), categories := "landschap"]
    bbwp_measures[!is.na(eco_id), categories := paste0(categories,"||Ecoregeling")]
    
    # add the bbwp category
    bbwp_measures[!is.na(categories),categories := paste0(categories,"||",category),by = .I]
    bbwp_measures[is.na(categories),categories := category]
    
  # save measures as bbwp table
  use_data(bbwp_measures, overwrite = TRUE)
  
# -- prepare table for which ER measures can be used on which crops ---

  # load in csv  
  er_measures <- fread('dev/eco_brp.csv', encoding = 'UTF-8')
  
  # remove brp codes that do not occur in pandex
  er_measures <- er_measures[B_LU_BRP < 7000 & B_LU_BRP != 305,]
  
  # add a column with applicability
  er_measures[, eco_app := 1]
  
  # save measures as bbwp table
  use_data(er_measures, overwrite = TRUE)
  
    
# -- prepare ecoregeling objectives ---
  
  # load in csv
  er_scoring <- as.data.table(fread('dev/220519 ecorelingen opgave.csv',dec=','))
  
  # save measures as bbwp table
  use_data(er_scoring, overwrite = TRUE)
  
# -- prepare table for scores per farm-measure ---
  
  # load in csv
  er_farm_measure <- as.data.table(fread('dev/220517 farm measures.csv',dec=','))
  
  # save measures as bbwp table
  use_data(er_farm_measure, overwrite = TRUE)
  
# -- prepare crop specific tables for Ecoregelingen ---
  
  er_crops <- pandex::b_lu_brp[,.(B_LU_BRP, B_LU_NAME, B_LU_BBWP, B_LU_ARABLE_ER, B_LU_PRODUCTIVE_ER, B_LU_CULTIVATED_ER)]
  
  # save measures as bbwp table
  use_data(er_crops, overwrite = TRUE)
  fwrite(er_crops, 'dev/er_crops.csv', quote = TRUE)
  
# -- prepare correction factors for financial reward per Agricultural Economic Region for Ecoregelingen ---
  
  # load in csv
  er_aer_reward <- as.data.table(fread('dev/220519 ecoregeling reward weging.csv',dec=','))
  
  # convert UTF-8 encoded strings to latin1 if required
  if('UTF-8' %in% Encoding(er_aer_reward$statname)) {
    er_aer_reward$statname <- iconv(er_aer_reward$statname, from = '', to = 'latin1')
  }
  # save measures as bbwp table
  use_data(er_aer_reward, overwrite = TRUE)
  
  
# -- prepare LSW table
  
  
  # library(sf); library(DBI); library(RPostgres)
  
  # Connect to DB
  con <- dbConnect(
    RPostgres::Postgres(),
    host = '127.0.0.1',
    user = rstudioapi::askForPassword("user"),
    dbname = 'nmi'
  )
  
  # read latest lsw polygones with opgaves
  st_lsw <- st_read(con,  Id(schema = "lookup", table = "oppervlaktewateropgave")) |> setDT()
  
  # read mean and sd for lsw (should use fread, but didn't work)
  prop_lsw <- st_read(con,  Id(schema = "lookup", table = "oppervlaktewateropgave_distribution_properties")) |> setDT()

  # merge both tables
  lsw <- merge(st_lsw, prop_lsw, by = 'oow_id')
  
  # make lsw as sf object
  lsw <- st_as_sf(lsw)
  
  # cast to polygon (otherwise later errors due to multipolygon)
  lsw <- st_cast(lsw, 'MULTIPOLYGON')
  
  # convert to data.table
  lsw <- as.data.table(lsw)
  
  # convert old element names for the case that they are present
  setnames(lsw, 
           old = c('mean_p_vg','sd_p_vg','mean_os_gv','sd_os_gv'),
           new = c('mean_p_sg','sd_p_sg','mean_som_loi','sd_som_loi'),
           skip_absent = TRUE)
  
  # convert to sf object again and save in dev
  lsw <- st_as_sf(lsw)
  # st_write(lsw,'dev/lswproperties.gpkg')
  
  # convert to dlata.table and remove geometry
  lsw <- as.data.table(lsw)
  lsw[,geom := NULL]
  
  # save lsw data
  use_data(lsw, overwrite = T,compress='xz')
  