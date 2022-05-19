# load packages
require(data.table); require(readxl);library(usethis)


# -- prepare measures table -----

  # load in excel with measures
  bbwp_measures <- as.data.table(read_xlsx('dev/220517 measures total versie 4.xlsx'))
  
  # setcolorder
  setcolorder(bbwp_measures,'bbwp_id')
  
  Encoding(bbwp_measures$description) <- 'latin1'
  bbwp_measures$description <- iconv(bbwp_measures$description, 'latin1', 'UTF-8')
  
  # save measures as bbwp table
  use_data(bbwp_measures, overwrite = TRUE)
  
# -- prepare ecoregeling objectives ---
  
  # load in csv
  er_scoring <- as.data.table(fread('dev/220519 ecorelingen opgave.csv',dec=','))
  
  # save measures as bbwp table
  use_data(er_scoring, overwrite = TRUE)
  