
# default input for testing
B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei')
B_GWL_CLASS = c('GtIII', 'GtI', 'GtV')
B_AER_CBS = c('LG05','LG14','LG02')
A_P_SG = c(0.4, 0.8, 1)
B_SLOPE_DEGREE = c(1.5,4,1.5)
B_AER_CBS = c('LG05','LG14','LG02')
B_LU_BBWP = rep('gras_permanent',3)
B_LU_BRP = rep(265,3)
B_LU_ARABLE_ER = c(F,F,F)
B_LU_PRODUCTIVE_ER = c(T,T,T)
B_LU_CULTIVATED_ER = c(T,T,T)
M_DRAIN = c(TRUE, FALSE, TRUE)
D_SA_W = c(0, 0.5, 1)
B_AREA = c(100,80,2.5)
measures = NULL
farmscore = 100
sector = c('dairy', 'arable')
output = 'scores'
medalscore = 'gold'


# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# make measurement list for 2 of the 4 fields
measures <- rbind(data.table(id = 1, dt.measures[grepl('B172',bbwp_id)]),
                  data.table(id = 3, dt.measures[grepl('B189',bbwp_id)]))
measures$bbwp_status <- 'given for ANLB'


# run example with any measures taken
test <- ecoregeling(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
                    B_GWL_CLASS = c('GtIII', 'GtI', 'GtV'),
                    A_P_SG = c(0.4, 0.8, 1),
                    B_SLOPE_DEGREE = c(1.5,4,1.5),
                    B_LU_BBWP = c('gras_permanent','gras_permanent','gras_permanent'),
                    B_LU_BRP = c(265,265,265),
                    B_LU_ARABLE_ER = c(F,F,F),
                    B_LU_PRODUCTIVE_ER = c(T,T,T),
                    B_LU_CULTIVATED_ER = c(T,T,T),
                    B_AER_CBS = c('LG05','LG14','LG02'),
                    M_DRAIN = c(TRUE, FALSE, TRUE),
                    D_SA_W = c(0, 0.5, 1),
                    B_AREA = c(100,80,2.5),
                    farmscore = 100,
                    measures = measures,
                    sector = c('dairy', 'arable'),
                    output = 'scores'
)

test
