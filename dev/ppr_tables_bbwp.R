# prepare measures table
require(data.table); require(readxl)
library(usethis)

# load in excel with measures
bbwp_measures <- as.data.table(read_xlsx('dev/220517 measures total versie 4.xlsx'))

# setcolorder
setcolorder(bbwp_measures,'bbwp_id')

Encoding(bbwp_measures$description) <- 'latin1'
bbwp_measures$description <- iconv(bbwp_measures$description, 'latin1', 'UTF-8')

# save measures as bbwp table
use_data(bbwp_measures, overwrite = TRUE)
  

