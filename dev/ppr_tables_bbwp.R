# prepare measures table
require(data.table); require(readxl)

# BBWP version 1.0

  # load in excel with measures
  d1 <- read_xlsx('dev/200728 measures total.xlsx',sheet='maatregelen_final')
  
  # convert to data.table
  d1 <- as.data.table(d1)
  
  # save measures as bbwp table
  # save(d1,file='data/bbwp_measures.rda')
  # saveRDS(d1,'data/bbwp_measures.rds')

  # prepare questionaire
  
  # load in excel with measures
  d1 <- read_xlsx('dev/200814 questionaire.xlsx',sheet=1)
  
  # convert to data.table
  d1 <- as.data.table(d1)
  
  #  answers to lower case
  d1[,antwoord := tolower(antwoord)]
  
  # add unique id
  d1[,id := paste0(id_quest,'_',id_sub,'_',id_answ)]
  
  # setcolorder
  setcolorder(d1,'id')
  
  # save measures as bbwp table
  # save(d1,file='data/bbwp_questionaire.rda')
  # saveRDS(d1,'data/bbwp_questionaire.rds')

# BBWP version 2.0
  
  # load in excel with measures
  d1 <- fread('dev/211015 bbwp_measures_v2.csv')
  
  # setcolorder
  setcolorder(d1,'bbwp_id')
  
  # save measures as bbwp table
  save(d1,file='data/bbwp_measures.rda')
  
  