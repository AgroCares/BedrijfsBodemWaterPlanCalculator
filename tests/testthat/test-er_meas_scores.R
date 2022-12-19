
require(testthat)

# default input for testing
  B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen')
  B_LU_BBWP = rep('gras_permanent',4)
  B_LU_BRP = rep(265,4)
  B_LU_ARABLE_ER = c(T,T,T,T)
  B_LU_PRODUCTIVE_ER = c(T,T,T,T)
  B_LU_CULTIVATED_ER = c(T,T,T,T)
  B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland")
  B_AREA = c(450000,180000,8000,60000)
  measures = NULL
  sector = 'dairy'

# run example 1 without any measures taken
test <- er_meas_score(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                      B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','rooivrucht'),
                      B_LU_BRP = c(265,2741,2741,6660),
                      B_LU_ARABLE_ER = c(T,T,T,T),
                      B_LU_PRODUCTIVE_ER = c(T,T,T,T),
                      B_LU_CULTIVATED_ER = c(T,T,T,T),
                      B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland"),
                      B_AREA = c(450000,180000,8000,60000),
                      measures = NULL,
                      sector = 'dairy'
                      )

test_that("check er_meas_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      id = 1:4,
      D_MEAS_BIO = rep(0,4),
      D_MEAS_CLIM = rep(0,4),
      D_MEAS_LAND = rep(0,4),
      D_MEAS_SOIL = rep(0,4),
      D_MEAS_WAT = rep(0,4),
      D_MEAS_TOT = rep(0,4),
      reward = rep(0,4)
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
test <- er_meas_score(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                      B_LU_BBWP = rep('rooivrucht',4),
                      B_LU_BRP = rep(2741,4),
                      B_LU_ARABLE_ER = c(T,T,T,T),
                      B_LU_PRODUCTIVE_ER = c(T,T,T,T),
                      B_LU_CULTIVATED_ER = c(T,T,T,T),
                      B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland"),
                      B_AREA = c(450000,180000,8000,60000),
                      measures = measures,
                      sector = c('dairy','arable')
                      )


test_that("check er_meas_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      id = 1:4,
      D_MEAS_BIO = c(31,0,35,0),
      D_MEAS_CLIM = c(14,0,0,0),
      D_MEAS_LAND = c(5,0,10,0),
      D_MEAS_SOIL = c(7,0,22,0),
      D_MEAS_WAT = c(26,0,25,0),
      D_MEAS_TOT = c(93,0,108,0),
      reward = c(100,0,0,0)
    ),
    tolerance = 1,
    ignore_attr = TRUE)
})







