# load packages
require(data.table);library(usethis)

# -- prepare measures table -----

  # load measures table (under construction)
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
    
    # add ecoregeling categories
    
      # eco1 includes: natuur; (kruidenrijke) rand; vanggewas; wortelspruit gewas; rooivruchten (voorjaar); maiskolvenschroot;
      bbwp_measures[,eco1:= fifelse((nc3==1|nc4==1|nc9==1|nc10==1|nc11==1) & (c11==1|c15==1|c17==1|c21==1),1,0)]
    
      # eco2 includes: rooivruchten (najaar); mais; groenbemesters; sloten langs grasland; 
      bbwp_measures[,eco2:= fifelse((nc4==1|nc9==1|nc10==1|nc11==1) & (c16==1|c19==1),1,0)]
      
      # eco3 includes: sloten langs grasland of bouwland; groenebraak;
      bbwp_measures[,eco3:= fifelse((nc8==1|nc10==1) & (c20==1|c24==1),1,0)] 
    
      # eco4 includes: eiwitgewas; heg,haag,struweel; akkerranden,keverbanken;
      bbwp_measures[,eco4:= fifelse((nc8==1|nc10==1|nc12==1) & (c12==1|c22==1|c29),1,0)]
      
      # eco5 includes: voedergewas; overig hout;
      bbwp_measures[,eco5:= fifelse((nc1==1|nc2==1|nc9==1|nc8==1|nc12==1) & (c18==1|c23==1),1,0)] 
    
      # eco6 includes: meerjarig gewas; riet,poelen; bufferstrook langs bouwland;
      bbwp_measures[,eco6:= fifelse((nc3==1|nc8==1|nc10==1) & (c14==1|c25==1|c28)==1,1,0)]
      
      # eco7 includes: diepwortelend; natte teelten; vogelgranen en arenstripper;
      bbwp_measures[,eco7:= fifelse((nc3==1|nc8==1|nc11==1) & (c13==1|c26==1|c27==1),1,0)]
      
    # setnames
    setnames(bbwp_measures,old = c('bouwland','productief','beteelbaar'),new = c('eco8','eco9','eco10'))
    
    # columns to remove
    cols.rem <- paste0('c',1:29)
    
    # remove columns
    bbwp_measures[,c(cols.rem):= NULL]
 
  # save measures as bbwp table
  use_data(bbwp_measures, overwrite = TRUE)
  
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
  
  # load in csv with crop list
  er_crops <- fread('dev/er_crops.csv', encoding = 'UTF-8')
  er_crops[er_crops == ''] <- NA
  
  # transform old cropcategories in new categories
  # eco1 includes: natuur; (kruidenrijke) rand; vanggewas; wortelspruit gewas; rooivruchten (voorjaar); maiskolvenschroot;
  # eco2 includes: rooivruchten (najaar); mais; groenbemesters; sloten langs grasland; 
  # eco3 includes: sloten langs grasland of bouwland; groenebraak;
  # eco4 includes: eiwitgewas; heg,haag,struweel; akkerranden,keverbanken;
  # eco5 includes: voedergewas; overig hout;
  # eco6 includes: meerjarig gewas; riet,poelen; bufferstrook langs bouwland;
  # eco7 includes: diepwortelend; natte teelten; granen;
  # example: luzerne is C3 (rustgewas,bbwp), C11 (vanggewas,eco1), C12 (eiwt,eco4), C13 (diepwortelend,exo7), C14 (meerjarig, eco6),C18 (voedergewas,eco5)
  er_crops[,nc1:= fifelse(crop_cat1==1,1,0)]
  er_crops[,nc2:= fifelse(crop_cat2==1,1,0)]
  er_crops[,nc3:= fifelse(crop_cat3==1,1,0)]
  er_crops[,nc4:= fifelse(crop_cat4==1,1,0)]
  er_crops[,nc5:= fifelse(crop_cat5==1,1,0)]
  er_crops[,nc6:= fifelse(crop_cat6==1,1,0)]
  er_crops[,nc7:= fifelse(crop_cat7==1,1,0)]
  er_crops[,nc8:= fifelse(crop_cat8==1,1,0)] 
  er_crops[,nc9:= fifelse(crop_cat9==1,1,0)] 
  er_crops[,nc10:= fifelse(c10==1,1,0)] 
  er_crops[,nc11:= fifelse(c11==1,1,0)] 
  er_crops[,nc12:= fifelse(c12==1,1,0)] 
  er_crops[,eco1:= fifelse((nc3==1|nc4==1|nc9==1|nc10==1|nc11==1) & (c11==1|c15==1|c17==1|c21==1),1,0)]
  er_crops[,eco2:= fifelse((nc4==1|nc9==1|nc10==1|nc11==1) & (c16==1|c19==1),1,0)]
  er_crops[,eco3:= fifelse((nc8==1|nc10==1) & (c20==1|c24==1),1,0)] 
  er_crops[,eco4:= fifelse((nc8==1|nc10==1|nc12==1) & (c12==1|c22==1|c29),1,0)]
  er_crops[,eco5:= fifelse((nc1==1|nc2==1|nc9==1|nc8==1|nc12==1) & (c18==1|c23==1),1,0)] 
  er_crops[,eco6:= fifelse((nc3==1|nc8==1|nc10==1) & (c14==1|c25==1|c28)==1,1,0)]
  er_crops[,eco7:= fifelse((nc3==1|nc8==1|nc11==1) & (c13==1|c26==1|c27==1),1,0)]
  
  # each BBWP category in one column
  er_crops[nc1==1, B_LU_BBWP := 'gras_permanent']
  er_crops[nc2==1, B_LU_BBWP := 'gras_tijdelijk']
  er_crops[nc3==1, B_LU_BBWP := 'rustgewas']
  er_crops[nc4==1, B_LU_BBWP := 'rooivrucht']
  er_crops[nc5==1, B_LU_BBWP := 'groenten']
  er_crops[nc6==1, B_LU_BBWP := 'bollensierteelt']
  er_crops[nc7==1, B_LU_BBWP := 'boomfruitteelt']
  er_crops[nc8==1, B_LU_BBWP := 'natuur']
  er_crops[nc9==1, B_LU_BBWP := 'mais']
  er_crops[nc10==1, B_LU_BBWP := 'randensloot']
  er_crops[nc11==1, B_LU_BBWP := 'vanggewas']
  er_crops[nc12==1, B_LU_BBWP := 'eiwitgewas']
  
  # remove columns not needed
  cols.rem <- c(paste0('crop_cat',1:9),paste0('c',10:29),paste0('nc',1:12),'SUM')
  
  # keep relevant columns and remove rows without B_LU_BRP code
  er_crops[,c(cols.rem):= NULL]

  # reset to boolean
  cols <- colnames(er_crops)[grepl('eco',colnames(er_crops))]
  er_crops[,c(cols) := lapply(.SD,function(x) fifelse(x==1,TRUE,FALSE)),.SDcols = cols]
  er_crops[,B_LU_NAME := NULL]
  
  # rename
  setnames(er_crops, 
           old = c('bouwland','productive','beteelbaar'),
           new = c('eco8','eco9','eco10'))
  
  setcolorder(er_crops,c('B_LU_BRP','B_LU_NAME','B_LU_BBWP',paste0('eco',1:10)))
  
  # rename columns
  setnames(er_crops,old = paste0('eco',1:10),new = paste0('B_LU_ECO',1:10))
  
  # save measures as bbwp table
  use_data(er_crops, overwrite = TRUE)
  
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
  