
require(testthat)

  # # default input for testing
  # B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen')
  # B_LU_BRP = c(265,2005,256,259)
  # B_LU_BBWP = c(1,4,4,9)
  # B_GWL_CLASS = 'GtIII' 
  # A_P_SG = rep(25,4) 
  # B_SLOPE_DEGREE = rep(2.5,4)
  # M_DRAIN = rep(TRUE,4)
  # D_SA_W = rep(0.5,4)
  # D_OPI_NGW = c(0,0.1, 0.5, 1)
  # D_OPI_NSW = c(0,0.1, 0.5, 1)
  # D_OPI_PSW = c(0,0.1, 0.5, 1) 
  # D_OPI_NUE = c(0,0.1, 0.5, 1)  
  # D_OPI_WB = c(0, 0.1,0.5, 1) 
  # measures = NULL
  # sector = 'dairy'

# run example 1 without any measures taken
test <- bbwp_meas_score(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                        B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                        B_GWL_CLASS = 'GtIII', 
                        B_AER_CBS = c('LG05','LG14','LG03','LG02'),
                        A_P_SG = rep(25,4) ,
                        B_SLOPE_DEGREE = rep(2.5,4),
                        M_DRAIN = rep(TRUE,4),
                        D_SA_W = rep(0.5,4),
                        D_OPI_NGW = c(0,0.1, 0.5, 1),
                        D_OPI_NSW = c(0,0.1, 0.5, 1),
                        D_OPI_PSW = c(0,0.1, 0.5, 1),
                        D_OPI_NUE = c(0,0.1, 0.5, 1),  
                        D_OPI_WB = c(0, 0.1,0.5, 1), 
                        measures = NULL,
                        sector = 'dairy',
                        B_LS_HYDROCAT = 'flanken'
                        )

test_that("check bbwp_meas_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      id = 1:4,
      D_MEAS_NGW = rep(0,4),
      D_MEAS_NSW = rep(0,4),
      D_MEAS_PSW = rep(0,4),
      D_MEAS_NUE = rep(0,4),
      D_MEAS_WB = rep(0,4),
      D_MEAS_TOT = rep(0,4)
    ),
    tolerance = 0.01,
    ignore_attr = TRUE)
})

# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# make measurement list for 2 of the 4 fields
measures <- rbind(data.table(id = 1, dt.measures[grepl('B189|G50|G3|B137|B172|G84',bbwp_id)]),
                  data.table(id = 3, dt.measures[grepl('B135|G84|B118|G58|B146',bbwp_id)]))
measures$bbwp_status <- 'given for ANLB'

# run example 2 without any measures taken
test <- bbwp_meas_score(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                      B_LU_BBWP =c('gras_permanent','rooivrucht','rooivrucht','mais'),
                      B_GWL_CLASS = 'GtIII', 
                      A_P_SG = rep(25,4) ,
                      B_SLOPE_DEGREE = rep(2.5,4),
                      B_AER_CBS = c('LG05','LG14','LG03','LG02'),
                      M_DRAIN = rep(TRUE,4),
                      D_SA_W = rep(0.5,4),
                      D_OPI_NGW = c(0,0.1, 0.5, 1),
                      D_OPI_NSW = c(0.2,0.1, 0.5, 1),
                      D_OPI_PSW = c(0.8,0.1, 0.5, 1),
                      D_OPI_NUE = c(0.33,0.1, 0.5, 1),  
                      D_OPI_WB = c(0.9, 0.1,0.5, 1), 
                      measures = measures,
                      sector = c('dairy','arable'),
                      B_LS_HYDROCAT = 'flanken'
)


test_that("check bbwp_meas_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      id = 1:4,
      D_MEAS_NGW = c(0.0125,0,3.8,0),
      D_MEAS_NSW = c(0.3,0,3.8,0),
      D_MEAS_PSW = c(3.75,0,0,0),
      D_MEAS_NUE = c(96250001,0,2,0),
      D_MEAS_WB = c(1,0,10,0),
      D_MEAS_TOT = c(19250001,0,4,0)
    ),
    tolerance = 1,
    ignore_attr = TRUE)
})







