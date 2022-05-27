
require(testthat)

# default input for testing
  # B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen')
  # B_LU_BRP = c(265,265,265,265)
  # B_LU_BBWP = c(1,1,1,1)
  # D_AREA = c(45,18,0.8,6)
  # B_CT_SOIL = 20
  # B_CT_WATER = 15
  # B_CT_CLIMATE = 8
  # B_CT_BIO = 24
  # B_CT_LANDSCAPE = 20
  # measures = NULL
  # sector = 'dairy'

# run example 1 without any measures taken
test <- er_meas_rank(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                      B_LU_BRP = c(265,2005,256,259),
                      B_LU_BBWP = c(1,4,4,9),
                      B_GWL_CLASS = 'GtIII', 
                      A_P_SG = 25, 
                      B_SLOPE = 2.5,
                      M_DRAIN = TRUE,
                      D_WP = 0.5,
                      D_AREA = c(45,18,0.8,6),
                      B_CT_SOIL = 15, 
                      B_CT_WATER = 5,
                      B_CT_CLIMATE = 8,
                      B_CT_BIO = 24,
                      B_CT_LANDSCAPE = 11, 
                      measures = NULL,
                      sector = 'dairy'
                        )

test_that("check er_meas_rank", {
  expect_equal(
    object = dim(test),
    expected = c(20,7),
    tolerance = 0.01)
})

test_that("check er_meas_rank", {
  expect_equal(
    object = colnames(test),
    expected = c('id',"top.tot","top.soil","top.water","top.climate","top.biodiversity","top.landscape"),
    tolerance = 0.01)
})

test_that("check er_meas_rank", {
  expect_equal(
    object = test$top.tot[c(1,4,9,15)],
    expected = c("B156",'G60','G60','B156'),
    tolerance = 0.01)
})

# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# make measurement list for 2 of the 4 fields
measures <- rbind(data.table(id = 1, dt.measures[c(2,5,18,28,32,3,38,43,62)]),
                  data.table(id = 3, dt.measures[c(7,21,30,46,5)]))

# run example 2 without any measures taken
test <- er_meas_rank(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                     B_LU_BRP = c(265,2005,256,259),
                     B_LU_BBWP = c(1,4,4,9),
                     B_GWL_CLASS = 'GtIII', 
                     A_P_SG = 25, 
                     B_SLOPE = 2.5,
                     M_DRAIN = TRUE,
                     D_WP = 0.5,
                     D_AREA = c(45,18,0.8,6),
                     B_CT_SOIL = 15, 
                     B_CT_WATER = 5,
                     B_CT_CLIMATE = 8,
                     B_CT_BIO = 24,
                     B_CT_LANDSCAPE = 11, 
                     measures = measures,
                     sector = 'dairy'
)

test_that("check er_meas_rank", {
  expect_equal(
    object = dim(test),
    expected = c(20,7),
    tolerance = 0.01)
})

test_that("check er_meas_rank", {
  expect_equal(
    object = colnames(test),
    expected = c('id',"top.tot","top.soil","top.water","top.climate","top.biodiversity","top.landscape"),
    tolerance = 0.01)
})

test_that("check er_meas_rank", {
  expect_equal(
    object = test$top.tot[c(1,4,9,15)],
    expected = c("B156",'G60','G60','B156'),
    tolerance = 0.01)
})





