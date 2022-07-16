
require(testthat)

# default input for testing
  B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen')
  B_LU_BBWP = c(1,1,1,1)
  B_LU_ECO1 = c(F,F,F,F)
  B_LU_ECO2 = c(F,F,F,F)
  B_LU_ECO3 = c(F,F,F,F)
  B_LU_ECO4 = c(F,F,F,F)
  B_LU_ECO5 = c(T,T,T,T)
  B_LU_ECO6 = c(F,F,F,F)  
  B_LU_ECO7 = c(F,F,F,F)
  B_LU_ECO8 = c(T,T,T,T)
  B_LU_ECO9 = c(T,T,T,T)
  B_LU_ECO10 = c(T,T,T,T)
  B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland')
  B_AREA = c(45,18,0.8,6)
  erf <- er_farm_aim(B_SOILTYPE_AGR,B_AREA)
  B_CT_SOIL = erf$B_CT_SOIL
  B_CT_WATER = erf$B_CT_WATER
  B_CT_CLIMATE = erf$B_CT_CLIMATE
  B_CT_BIO = erf$B_CT_BIO
  B_CT_LANDSCAPE = erf$B_CT_LANDSCAPE
  measures = NULL
  sector = 'dairy'

  
# run example 1 without any measures taken
test <- er_croprotation(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                        B_LU_BBWP = c(1,4,4,9),
                        B_LU_ECO1 = c(F,F,F,F),
                        B_LU_ECO2 = c(F,F,F,F),
                        B_LU_ECO3 = c(F,F,F,F),
                        B_LU_ECO4 = c(F,F,F,F),
                        B_LU_ECO5 = c(T,T,T,T),
                        B_LU_ECO6 = c(F,F,F,F) , 
                        B_LU_ECO7 = c(F,F,F,F),
                        B_LU_ECO8 = c(T,T,T,T),
                        B_LU_ECO9 = c(T,T,T,T),
                        B_LU_ECO10 = c(T,T,T,T),
                        B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland'),
                        B_AREA = c(45,18,0.8,6),
                        B_CT_SOIL = B_CT_SOIL, 
                        B_CT_WATER = B_CT_WATER,
                        B_CT_CLIMATE = B_CT_CLIMATE,
                        B_CT_BIO = B_CT_BIO,
                        B_CT_LANDSCAPE = B_CT_LANDSCAPE,
                        measures = NULL,
                        sector = 'dairy'
                        )

test_that("check er_croprotation", {
  expect_equal(
    object = dim(test),
    expected = c(1,8),
    tolerance = 0.01)
})

test_that("check er_croprotation", {
  expect_equal(
    object = colnames(test),
    expected = c('farmid','biodiversity','climate','landscape','soil','water','total','S_ER_REWARD'),
    tolerance = 0.01)
})

test_that("check er_croprotation", {
  expect_equal(
    object = as.numeric(test),
    expected = c(1,4.988,0.043,1.065,4.625,5.56,17.28,0),
    tolerance = 0.1,
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
test <- er_croprotation(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                        B_LU_BBWP = c(1,4,4,9),
                        B_LU_ECO1 = c(F,F,F,F),
                        B_LU_ECO2 = c(F,F,F,F),
                        B_LU_ECO3 = c(F,F,F,F),
                        B_LU_ECO4 = c(F,F,F,F),
                        B_LU_ECO5 = c(T,T,T,T),
                        B_LU_ECO6 = c(F,F,F,F) , 
                        B_LU_ECO7 = c(F,F,F,F),
                        B_LU_ECO8 = c(T,T,T,T),
                        B_LU_ECO9 = c(T,T,T,T),
                        B_LU_ECO10 = c(T,T,T,T),
                        B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland'),
                        B_AREA = c(45,18,0.8,6),
                        B_CT_SOIL = B_CT_SOIL, 
                        B_CT_WATER = B_CT_WATER,
                        B_CT_CLIMATE = B_CT_CLIMATE,
                        B_CT_BIO = B_CT_BIO,
                        B_CT_LANDSCAPE = B_CT_LANDSCAPE,
                        measures = measures,
                        sector = 'dairy'
                     )

test_that("check er_croprotation", {
  expect_equal(
    object = dim(test),
    expected = c(1,8),
    tolerance = 0.01)
})

test_that("check er_croprotation", {
  expect_equal(
    object = colnames(test),
    expected = c('farmid','biodiversity','climate','landscape','soil','water','total','S_ER_REWARD'),
    tolerance = 0.01)
})

test_that("check er_croprotation", {
  expect_equal(
    object = as.numeric(test),
    expected = c(1,67.35,9.39,58.55,33.53,52.81,222,113.4),
    tolerance = 0.1,
    ignore_attr = TRUE)
})





