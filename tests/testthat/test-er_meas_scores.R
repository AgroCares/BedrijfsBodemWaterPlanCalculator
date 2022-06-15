
require(testthat)

# default input for testing
  # B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen')
  # B_LU_BRP = c(265,265,265,265)
  # B_LU_BBWP = c(1,1,1,1)
  # B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland")
  # B_AREA = c(45,18,0.8,6)
  # B_CT_SOIL = 20
  # B_CT_WATER = 15
  # B_CT_CLIMATE = 8
  # B_CT_BIO = 24
  # B_CT_LANDSCAPE = 20
  # measures = NULL
  # sector = 'dairy'

# run example 1 without any measures taken
test <- er_meas_score(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                      B_LU_BRP = c(265,2005,256,259),
                      B_LU_BBWP = c(1,4,4,9),
                      B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland"),
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
measures <- rbind(data.table(id = 1, dt.measures[c(2,5,18,28,32,3,38,43,62)]),
                  data.table(id = 3, dt.measures[c(7,21,30,46,5)]))

# run example 2 without any measures taken
test <- er_meas_score(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                      B_LU_BRP = c(265,265,265,265),
                      B_LU_BBWP = c(1,1,1,1),
                      B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland"),
                      measures = measures,
                      sector = c('dairy','arable')
                      )


test_that("check er_meas_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      id = 1:4,
      D_MEAS_BIO = c(3.75,0,11,0),
      D_MEAS_CLIM = c(11,0,11,0),
      D_MEAS_LAND = c(4,0,12,0),
      D_MEAS_SOIL = c(11,0,0,0),
      D_MEAS_WAT = c(12,0,8.75,0),
      D_MEAS_TOT = c(41.75,0,42.75,0),
      reward = c(715,0,950,0)
    ),
    tolerance = 1,
    ignore_attr = TRUE)
})







