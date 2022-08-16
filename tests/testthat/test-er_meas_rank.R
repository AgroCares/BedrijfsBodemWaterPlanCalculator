
require(testthat)

  # default input for testing
  B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen')
  B_LU_BBWP = rep('gras_permanent',4)
  B_LU_BRP = rep(265,4)
  B_LU_ARABLE_ER = c(F,F,F,F)
  B_LU_PRODUCTIVE_ER = c(T,T,T,T)
  B_LU_CULTIVATED_ER = c(T,T,T,T)
  B_GWL_CLASS = rep('GtIII',4)
  B_AREA = c(45,18,0.8,6)
  B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland")
  B_SLOPE_DEGREE = rep(2.5,4)
  M_DRAIN = rep(TRUE,4)
  A_P_SG = rep(25,4)
  erf <- er_farm_aim(B_SOILTYPE_AGR,B_AREA)
  B_CT_SOIL = erf$B_CT_SOIL
  B_CT_WATER = erf$B_CT_WATER
  B_CT_CLIMATE = erf$B_CT_CLIMATE
  B_CT_BIO = erf$B_CT_BIO
  B_CT_LANDSCAPE = erf$B_CT_LANDSCAPE
  D_SA_W = 100
  measures = NULL
  sector = 'dairy'

# run example 1 without any measures taken
test <- er_meas_rank(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                     B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                     B_LU_BRP = c(265,2741,2741,259),
                     B_LU_ARABLE_ER = c(F,T,T,T),
                     B_LU_PRODUCTIVE_ER = c(T,T,T,T),
                     B_LU_CULTIVATED_ER = c(T,T,T,T),
                     B_GWL_CLASS = 'GtIII', 
                     A_P_SG = 25, 
                     B_SLOPE_DEGREE = rep(2.5,4),
                     B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland"),
                     M_DRAIN = TRUE,
                     D_SA_W = 0.5,
                     B_AREA = c(45,18,0.8,6),
                     B_CT_SOIL = B_CT_SOIL, 
                     B_CT_WATER = B_CT_WATER,
                     B_CT_CLIMATE = B_CT_CLIMATE,
                     B_CT_BIO = B_CT_BIO,
                     B_CT_LANDSCAPE = B_CT_LANDSCAPE,
                     measures = NULL,
                     sector = 'dairy'
                    )

test_that("check er_meas_rank", {
  expect_equal(
    object = dim(test),
    expected = c(20,8),
    tolerance = 0.01)
})

test_that("check er_meas_rank", {
  expect_equal(
    object = colnames(test),
    expected = c('id',"top_er_tot","top_er_soil","top_er_water","top_er_climate","top_er_biodiversity","top_er_landscape","top_er_reward"),
    tolerance = 0.01)
})

test_that("check er_meas_rank", {
  expect_equal(
    object = test$top_er_tot[c(1,4,9,15)],
    expected = c("B162",'B114','B166','G20'),
    tolerance = 0.01)
})

# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# make measurement list for 2 of the 4 fields
measures <- rbind(data.table(id = 1, dt.measures[grepl('B189|G50|G3|B137|B172|G84',bbwp_id)]),
                  data.table(id = 3, dt.measures[grepl('B135|G84|B118|G58|B146',bbwp_id)]))
measures$bbwp_status <- 'given for ANLB'

# run example 2 without any measures taken
test <- er_meas_rank(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                     B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                     B_LU_BRP = c(265,2741,2741,259),
                     B_LU_ARABLE_ER = c(F,T,T,T),
                     B_LU_PRODUCTIVE_ER = c(T,T,T,T),
                     B_LU_CULTIVATED_ER = c(T,T,T,T),
                     B_GWL_CLASS = 'GtIII', 
                     A_P_SG = 25, 
                     B_SLOPE_DEGREE = rep(2.5,4),
                     B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland"),
                     M_DRAIN = TRUE,
                     D_SA_W = 0.5,
                     B_AREA = c(45,18,0.8,6),
                     B_CT_SOIL = B_CT_SOIL, 
                     B_CT_WATER = B_CT_WATER,
                     B_CT_CLIMATE = B_CT_CLIMATE,
                     B_CT_BIO = B_CT_BIO,
                     B_CT_LANDSCAPE = B_CT_LANDSCAPE,
                     measures = measures,
                     sector = 'dairy'
)

test_that("check er_meas_rank", {
  expect_equal(
    object = dim(test),
    expected = c(20,8),
    tolerance = 0.01)
})

test_that("check er_meas_rank", {
  expect_equal(
    object = colnames(test),
    expected = c('id',"top_er_tot","top_er_soil","top_er_water","top_er_climate","top_er_biodiversity","top_er_landscape","top_er_reward"),
    tolerance = 0.01)
})

test_that("check er_meas_rank", {
  expect_equal(
    object = test$top_er_tot[c(1,4,8,15)],
    expected = c("B162", "B114", "B145", "G20"),
    tolerance = 0.01)
})






