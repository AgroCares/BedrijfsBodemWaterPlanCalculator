
require(testthat)

# default input for testing
  # B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen')
  # B_LU_BRP = c(265,265,265,265)
  # B_LU_BBWP = c(1,1,1,1)
  # B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland')
  # D_AREA = c(45,18,0.8,6)
  # B_CT_SOIL = 20
  # B_CT_WATER = 15
  # B_CT_CLIMATE = 8
  # B_CT_BIO = 24
  # B_CT_LANDSCAPE = 20
  # measures = NULL
  # sector = 'dairy'

# run example 1 without any measures taken
test <- er_croprotation(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                        B_LU_BRP = c(265,2005,256,259),
                        B_LU_BBWP = c(1,4,4,9),
                        B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland'),
                        D_AREA = c(45,18,0.8,6),
                        B_CT_SOIL = 15, 
                        B_CT_WATER = 5,
                        B_CT_CLIMATE = 8,
                        B_CT_BIO = 24,
                        B_CT_LANDSCAPE = 11
                        )

test_that("check er_croprotation", {
  expect_equal(
    object = dim(test),
    expected = c(1,7),
    tolerance = 0.01)
})

test_that("check er_croprotation", {
  expect_equal(
    object = colnames(test),
    expected = c('farmid','biodiversity','climate','landscape','soil','water','reward'),
    tolerance = 0.01)
})

test_that("check er_croprotation", {
  expect_equal(
    object = as.numeric(test),
    expected = c(1,1.65,1.37,0.11,2.58,1.93,45512.1),
    tolerance = 0.1,
    ignore_attr = TRUE)
})

# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# make measurement list for 2 of the 4 fields
measures <- rbind(data.table(id = 1, dt.measures[c(2,5,18,28,32,3,38,43,62)]),
                  data.table(id = 3, dt.measures[c(7,21,30,46,5)]))

# run example 2 without any measures taken
test <- er_croprotation(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                     B_LU_BRP = c(265,2005,256,259),
                     B_LU_BBWP = c(1,4,4,9),
                     B_AER_CBS =  c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland'),
                     D_AREA = c(4.5,18,0.8,60),
                     B_CT_SOIL = 1.5, 
                     B_CT_WATER = 50,
                     B_CT_CLIMATE = 18,
                     B_CT_BIO = 24,
                     B_CT_LANDSCAPE = 21
                     )

test_that("check er_croprotation", {
  expect_equal(
    object = dim(test),
    expected = c(1,7),
    tolerance = 0.01)
})

test_that("check er_croprotation", {
  expect_equal(
    object = colnames(test),
    expected = c('farmid','biodiversity','climate','landscape','soil','water','reward'),
    tolerance = 0.01)
})

test_that("check er_croprotation", {
  expect_equal(
    object = as.numeric(test),
    expected = c(1,0.16,0.18,0.10,0.22,0.16,4551.2),
    tolerance = 0.1,
    ignore_attr = TRUE)
})





