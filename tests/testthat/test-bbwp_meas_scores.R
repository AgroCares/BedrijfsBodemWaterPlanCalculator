
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
                        B_LU_BRP = c(265,2005,256,259),
                        B_LU_BBWP = c(1,4,4,9),
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
                        sector = 'dairy'
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
measures <- rbind(data.table(id = 1, dt.measures[c(2,5,18,28,32,3,38,43,62)]),
                  data.table(id = 3, dt.measures[c(7,21,30,46,5)]))

# run example 2 without any measures taken
test <- bbwp_meas_score(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                      B_LU_BRP = c(265,2005,256,259),
                      B_LU_BBWP = c(1,4,4,9),
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
                      sector = c('dairy','arable')
)


test_that("check bbwp_meas_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      id = 1:4,
      D_MEAS_NGW = c(0,0,0.25,0),
      D_MEAS_NSW = c(0.0125,0.,0.125,0),
      D_MEAS_PSW = c(0,0,0,0),
      D_MEAS_NUE = c(0.0825,0,0.1458,0),
      D_MEAS_WB = c(0.18675,0,0.0625,0),
      D_MEAS_TOT = c(0.05635,0,0.11667,0)
    ),
    tolerance = 1,
    ignore_attr = TRUE)
})







