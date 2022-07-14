# load packages
require(data.table);require(readxl);library(usethis)

# -- prepare measures table -----

  # load measures table (under construction)
  bbwp_measures <- as.data.table(read_excel("dev/ER_puntenregeling_aan_te_vullen_220706.xlsx"),sheet= '220706_fulldb_v6')
 
  # setcolorder
  setcolorder(bbwp_measures,'bbwp_id')
  
  # ensure utft8 in text
  Encoding(bbwp_measures$description) <- 'latin1'
  bbwp_measures$description <- iconv(bbwp_measures$description, 'latin1', 'UTF-8')
  
  # set effect values to 0 when NA
  scols <- colnames(bbwp_measures)[grepl('^nsw|^ngw|^psw|^p_|^n_|^effect|^er',colnames(bbwp_measures))]
  bbwp_measures[,c(scols) := lapply(.SD,function(x) fifelse(is.na(x),0,x)),.SDcols = scols]
  
  # create new crop categories in new columns bbwp, eco1 and eco2
    
    # definition of the eco categories 
    # eco1 includes: natuur; (kruidenrijke) rand; vanggewas; wortelspruit gewas; rooivruchten (voorjaar); maiskolvenschroot;
    # eco2 includes: rooivruchten (najaar); mais; groenbemesters; sloten langs grasland; bufferstrook langs bouwland;
    # eco3 includes: sloten langs grasland of bouwland; groenebraak;
    # eco4 includes: eiwitgewas; heg,haag,struweel; akkerranden,keverbanken;
    # eco5 includes: rustgewassen (niet grassen); voedergewas; overig hout;
    # eco6 includes: meerjarig gewas; riet,poelen;
    # eco7 includes: diepwortelend; natte teelten; granen;
  
    # definition of BBWP categories
  
    # set BBWP categories (crops can not occur in multiple BBWP categories)
    bbwp_measures[,nc1:= fifelse(c1==1,1,0)]
    bbwp_measures[,nc2:= fifelse(c2==1,1,0)]
    bbwp_measures[,nc3:= fifelse(c3==1,1,0)]
    bbwp_measures[,nc4:= fifelse(c3==1,1,0)]
    bbwp_measures[,nc5:= fifelse(c5==1,1,0)]
    bbwp_measures[,nc6:= fifelse(c6==1,1,0)]
    bbwp_measures[,nc7:= fifelse(c7==1,1,0)]
    bbwp_measures[,nc8:= fifelse(c8==1,1,0)] 
    bbwp_measures[,nc9:= fifelse(c9==1,1,0)] 
    bbwp_measures[,tmp1:= nc1+nc2+nc3+nc4+nc5+nc6+nc7+nc8+nc9]
    bbwp_measures[,nc3:= fifelse(tmp1==0 & (c15==1|c27==1|c14==1),1,0)]
    bbwp_measures[,nc4:= fifelse(tmp1==0 & (c17==1),1,0)]
    bbwp_measures[,nc8:= fifelse(tmp1==0 & (c22==1|c25==1|c24==1|c23==1|c26==1),1,0)]
    bbwp_measures[,nc9:= fifelse(tmp1==0 & (c21==1),1,0)]
    bbwp_measures[,tmp2:= nc1+nc2+nc3+nc4+nc5+nc6+nc7+nc8+nc9]
    bbwp_measures[,nc10:= fifelse(tmp1==0 & tmp2==0 & (c10==1|c28==1|c29==1|c20==1|c19==1),1,0)]
    bbwp_measures[,nc11:= fifelse(tmp1==0 & tmp2==0 & (c11==1|c16==1|c13==1),1,0)]
    bbwp_measures[,nc12:= fifelse(tmp1==0 & tmp2==0 & nc11==0 & (c12==1|c18==1),1,0)]
    
    # add ecoregling categories
    bbwp_measures[,eco1:= fifelse(c8==1|c10==1|c11==1|c15==1|c21==1|c17==1,1,0)]
    bbwp_measures[,eco2:= fifelse(c4==1|c9==1|c16==1|c19==1,1,0)]
    bbwp_measures[,eco3:= fifelse(c20==1|c24==1,1,0)] 
    bbwp_measures[,eco4:= fifelse(c12==1|c22==11|c29==1,1,0)]
    bbwp_measures[,eco5:= fifelse(c23==1|c18==1,1,0)] 
    bbwp_measures[,eco6:= fifelse(c14==1|c25==1|c28==1,1,0)]
    bbwp_measures[,eco7:= fifelse(c26==1|c27==1|c29==1|c13==1,1,0)]
     
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
  er_crops <- as.data.table(read_excel("dev/b_lu_brp_GR220707_decast_for_client.xlsx"))
  
  # transform old cropcategories in new categories
  # eco1 includes: natuur; (kruidenrijke) rand; vanggewas; wortelspruit gewas; rooivruchten (voorjaar); maiskolvenschroot;
  # eco2 includes: rooivruchten (najaar); mais; groenbemesters; sloten langs grasland; bufferstrook langs bouwland;
  # eco3 includes: sloten langs grasland of bouwland; groenebraak;
  # eco4 includes: eiwitgewas; heg,haag,struweel; akkerranden,keverbanken;
  # eco5 includes: rustgewassen (niet grassen); voedergewas; overig hout;
  # eco6 includes: meerjarig gewas; riet,poelen;
  # eco7 includes: diepwortelend; natte teelten; granen;
  er_crops[,nc1:= fifelse(crop_cat1==1,1,0)]
  er_crops[,nc2:= fifelse(crop_cat2==1,1,0)]
  er_crops[,nc3:= fifelse(crop_cat3==1,1,0)]
  er_crops[,nc4:= fifelse(crop_cat4==1,1,0)]
  er_crops[,nc5:= fifelse(crop_cat5==1,1,0)]
  er_crops[,nc6:= fifelse(crop_cat6==1,1,0)]
  er_crops[,nc7:= fifelse(crop_cat7==1,1,0)]
  er_crops[,nc8:= fifelse(crop_cat8==1,1,0)] 
  er_crops[,nc9:= fifelse(crop_cat9==1,1,0)] 
  er_crops[,tmp1:= nc1+nc2+nc3+nc4+nc5+nc6+nc7+nc8+nc9]
  er_crops[,nc3:= fifelse(tmp1==0 & (c15==1|c27==1|c14==1),1,0)]
  er_crops[,nc4:= fifelse(tmp1==0 & (c17==1),1,0)]
  er_crops[,nc8:= fifelse(tmp1==0 & (c22==1|c25==1|c24==1|c23==1|c26==1),1,0)]
  er_crops[,nc9:= fifelse(tmp1==0 & (c21==1),1,0)]
  er_crops[,tmp2:= nc1+nc2+nc3+nc4+nc5+nc6+nc7+nc8+nc9]
  er_crops[,nc10:= fifelse(tmp1==0 & tmp2==0 & (c10==1|c28==1|c29==1|c20==1|c19==1),1,0)]
  er_crops[,nc11:= fifelse(tmp1==0 & tmp2==0 & (c11==1|c16==1|c13==1),1,0)]
  er_crops[,nc12:= fifelse(tmp1==0 & tmp2==0 & nc11==0 & (c12==1|c18==1),1,0)]
  er_crops[,eco1:= fifelse(crop_cat8==1|c10==1|c11==1|c15==1|c21==1|c17==1,1,0)]
  er_crops[,eco2:= fifelse(crop_cat4==1|crop_cat9==1|c16==1|c19==1,1,0)]
  er_crops[,eco3:= fifelse(c20==1|c24==1,1,0)] 
  er_crops[,eco4:= fifelse(c12==1|c22==11|c29==1,1,0)]
  er_crops[,eco5:= fifelse(c23==1|c18==1,1,0)] 
  er_crops[,eco6:= fifelse(c14==1|c25==1|c28==1,1,0)]
  er_crops[,eco7:= fifelse(c26==1|c27==1|c29==1|c13==1,1,0)]
  
  #keep relevant columns and remove rows without B_LU_BRP code
  er_crops[,c(3:31,35,45:46):= NULL]
  er_crops[complete.cases(B_LU_BRP),]
  
  # rename cols
  setnames(er_crops,c("nc1","nc2","nc3","nc4","nc5","nc6","nc7","nc8","nc9","nc10","nc11","nc12"),c("crop_cat1","crop_cat2","crop_cat3","crop_cat4","crop_cat5","crop_cat6","crop_cat7","crop_cat8","crop_cat9","crop_cat10","crop_cat11","crop_cat12"))
  
  # load in csv with measures and brp codes of crops to which the measure applies
  m_brp <- as.data.table(fread('dev/er_croplist_supplemented_versie aangevuld AB.csv',dec=','))
  
  # merge measures table and crop table with new categories
  er_crops <- merge(m_brp,er_crops, by.x = "b_lu_brp", by.y = "B_LU_BRP")
  
  # remove duplicate columns
  er_crops[, B_LU_NAME := NULL]
  
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
  st_write(lsw,'dev/lswproperties.gpkg')
  
  # convert to data.table and remove geometry
  lsw <- as.data.table(lsw)
  lsw[,geom := NULL]
  
  # save lsw data
  use_data(lsw, overwrite = T,compress='xz')
  