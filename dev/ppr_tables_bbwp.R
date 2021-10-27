# prepare measures table
require(data.table); require(readxl)
library(usethis)

# load in excel with measures
bbwp_measures <- fread('dev/211015 bbwp_measures_v2.csv')

# setcolorder
setcolorder(bbwp_measures,'bbwp_id')

# save measures as bbwp table
use_data(bbwp_measures)
  