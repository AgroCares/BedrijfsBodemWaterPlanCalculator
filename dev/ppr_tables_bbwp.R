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
    
  # save measures as bbwp table
  use_data(bbwp_measures, overwrite = TRUE)
  
# -- prepare table for which ER measures can be used on which crops ---

  # load in csv  
  er_measures <- fread('dev/eco_brp.csv', encoding = 'UTF-8')
  
  # BRP ids for nature, buffer zones
  cols <- c(863,7002,7003,1936,1940,2617,2619,2621,2622,2626,2628,2630,2631,2640,2641,2642,2643,2644,2636,2638,2620,2639,2618,2624,2625,2629,2637)
  
  # get right brp codes for each measure
  er_measures <- er_measures[!eco_id %in% c("EG11","EG10A","EG10B","EG22","EB4A","EB4B","EB5A","EB5B","EB10A","EB10B","EB10C","EB12","EB13A","EB13B","EB14A","EB14B","EB14C","EB15","EB18","EB19")]
  d1 <- er_crops[, .(B_LU_BRP)][, eco_id := "EG11"]
  d2 <- er_crops[!B_LU_BRP %in% c(343,cols), .(B_LU_BRP)][, eco_id := "EG10A"]
  d3 <- er_crops[!B_LU_BRP %in% c(343,cols), .(B_LU_BRP)][, eco_id := "EG10B"]
  d4 <- er_crops[productive ==1, .(B_LU_BRP)][, eco_id := "EG22"]
  d5 <- er_crops[bouwland ==1 | B_LU_BRP %in% c(cols), .(B_LU_BRP)][, eco_id := "EB4A"]
  d6 <- er_crops[bouwland ==1 | B_LU_BRP %in% c(cols), .(B_LU_BRP)][, eco_id := "EB4B"]
  d7 <- er_crops[bouwland ==1, .(B_LU_BRP)][, eco_id := "EB5A"]
  d8 <- er_crops[bouwland ==1, .(B_LU_BRP)][, eco_id := "EB5B"]
  d9 <- er_crops[productive ==1, .(B_LU_BRP)][, eco_id := "EB10A"]
  d9a <- er_crops[productive ==1, .(B_LU_BRP)][, eco_id := "EB10B"]
  d9b <- er_crops[productive ==1, .(B_LU_BRP)][, eco_id := "EB10C"]
  d10 <- er_crops[bouwland ==1 | B_LU_BRP %in% c(3501, 3502, 3503, 3504, 3510, 3505, 3506, 801, 3507, 3508, 3509, 428, 3512, 670, 3500, 
                                                 3511, 3515, 799, 3524, 663, 258, 3514, 426, 3807, 800, 3808, 3517, 3518, 3519, 3520, 
                                                 3522, 3523, 3513, 802, 803, 515, 669), .(B_LU_BRP)][, eco_id := "EB12"]
  d11 <- er_crops[bouwland ==1, .(B_LU_BRP)][, eco_id := "EB13A"]
  d12 <- er_crops[bouwland ==1, .(B_LU_BRP)][, eco_id := "EB13B"]
  d13 <- er_crops[bouwland ==1, .(B_LU_BRP)][, eco_id := "EB14A"]
  d14 <- er_crops[bouwland ==1, .(B_LU_BRP)][, eco_id := "EB14B"]
  d15 <- er_crops[bouwland ==1, .(B_LU_BRP)][, eco_id := "EB14C"]
  d16 <- er_crops[bouwland ==1, .(B_LU_BRP)][, eco_id := "EB15"]
  d17 <- er_crops[bouwland ==1, .(B_LU_BRP)][, eco_id := "EB18"]
  d18 <- er_crops[bouwland ==1, .(B_LU_BRP)][, eco_id := "EB19"]
  
  # bind all tables together
  setnames(er_measures,"brp_code","B_LU_BRP")
  all <- rbind(er_measures,d1,d2,d3,d4,d5,d6,d7,d8,d9,d9a,d9b,d10,d11,d12,d13,d14,d15,d16,d17,d18)
  
  # remove codes higher than 7000 
  all <- all[B_LU_BRP < 7000,]

  # overwrite table
  er_measures <- copy(all)
  
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
  
  # load in csv with crop list
  er_crops <- fread('dev/er_crops.csv', encoding = 'UTF-8')
  er_crops[er_crops == ''] <- NA
  
  # transform old cropcategories in new categories
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

  # reorder
  setcolorder(er_crops,c('B_LU_BRP','B_LU_NAME','B_LU_BBWP'))
  
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
  