
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
  B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland")
  B_AREA = c(45,18,0.8,6)
  measures = NULL
  sector = 'dairy'

# run example 1 without any measures taken
test <- er_meas_score(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
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
                      B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland"),
                      B_AREA = c(45,18,0.8,6),
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
                      B_LU_BBWP = c(1,1,1,1),
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
                      B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland"),
                      B_AREA = c(45,18,0.8,6),
                      measures = measures,
                      sector = c('dairy','arable')
                      )


test_that("check er_meas_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      id = 1:4,
      D_MEAS_BIO = c(2.5,0,0,0),
      D_MEAS_CLIM = c(6,0,0,0),
      D_MEAS_LAND = c(10,0,0,0),
      D_MEAS_SOIL = c(2,0,0,0),
      D_MEAS_WAT = c(0,0,0,0),
      D_MEAS_TOT = c(20.5,0,0,0),
      reward = c(0,0,0,0)
    ),
    tolerance = 1,
    ignore_attr = TRUE)
})







