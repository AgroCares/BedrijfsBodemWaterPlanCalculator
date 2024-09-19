# load packages
require(data.table);library(usethis)

# -- prepare measures table -----

# load measures table
# Table was made in Github Repo 'NMI-DATA_script/bbwp/ppr_bbwp_measures.R'
# This is the table used for < version 2.3.0  
bbwp_measures <- fread('dev/bbwp_measures.csv', encoding = 'UTF-8')

# save measures as bbwp table
use_data(bbwp_measures, overwrite = TRUE)


# -- prepare measures table (with landscape category)-----

# load updated measure table, which includes weighing factor for 5 landscape category and effect_wb was updated
# (for hydrological module, made for project 2044.N.24)
# The table was made in Github Repository "NMI-DATA_scripts"
# (https://github.com/AgroCares/NMI-DATA_scripts/blob/main/watersysteem/bbwp_hydrologische_module/bbwp_hydro_meas.R)
# This is the table userd for >= version 2.3.0
bbwp_measures <- fread('dev/bbwp_measures2.csv', encoding = 'UTF-8')

# Overwrite bbwp measure table
use_data(bbwp_measures, overwrite = TRUE)
  
  
# -- prepare table for which ER measures can be used on which crops ----

  # load in csv  
  er_measures <- fread('dev/eco_brp.csv', encoding = 'UTF-8')
  
  # remove brp codes that do not occur in pandex
  er_measures <- er_measures[B_LU_BRP < 7000 & B_LU_BRP != 305,]
  
  # add a column with applicability
  er_measures[, eco_app := 1]
  
  # save measures as bbwp table
  use_data(er_measures, overwrite = TRUE)
  
    
# -- prepare ecoregeling objectives ----
  
  # load in csv
  er_scoring <- as.data.table(fread('dev/220519 ecorelingen opgave.csv',dec=','))
  
  # save measures as bbwp table
  use_data(er_scoring, overwrite = TRUE)
  
# -- prepare table for scores per farm-measure ----
  
  # load in csv
  er_farm_measure <- as.data.table(fread('dev/220517 farm measures.csv',dec=','))
  
  # save measures as bbwp table
  use_data(er_farm_measure, overwrite = TRUE)
  
# -- prepare crop specific tables for Ecoregelingen ----
  
  er_crops <- pandex::b_lu_brp[,.(B_LU_BRP, B_LU_NAME, B_LU_BBWP, B_LU_ARABLE_ER, B_LU_PRODUCTIVE_ER, B_LU_CULTIVATED_ER)]
  
  # save measures as bbwp table
  use_data(er_crops, overwrite = TRUE)
  fwrite(er_crops, 'dev/er_crops.csv', quote = TRUE)
  
# -- prepare correction factors for financial reward per Agricultural Economic Region for Ecoregelingen ----
  
  # load in csv
  er_aer_reward <- as.data.table(fread('dev/220519 ecoregeling reward weging.csv',dec=','))
  
  # convert UTF-8 encoded strings to latin1 if required
  if('UTF-8' %in% Encoding(er_aer_reward$statname)) {
    er_aer_reward$statname <- iconv(er_aer_reward$statname, from = '', to = 'latin1')
  }
  # save measures as bbwp table
  use_data(er_aer_reward, overwrite = TRUE)
  
  
# -- prepare LSW table ----
  
  
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
  