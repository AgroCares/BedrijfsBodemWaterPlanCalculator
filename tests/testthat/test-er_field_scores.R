
require(testthat)

# default input for testing

# crops: permanent gras, rustgewas, rooivrucht, mais
  B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen')
  B_LU_BBWP = c(1,3,4,9)
  B_LU_ECO1 = c(F,F,F,F)
  B_LU_ECO2 = c(F,F,T,T)
  B_LU_ECO3 = c(F,F,F,F)
  B_LU_ECO4 = c(F,F,F,F)
  B_LU_ECO5 = c(T,T,F,T)
  B_LU_ECO6 = c(F,F,F,F)  
  B_LU_ECO7 = c(F,T,F,F)
  B_LU_ECO8 = c(T,T,T,T)
  B_LU_ECO9 = c(T,T,T,T)
  B_LU_ECO10 = c(T,T,T,T)
  B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland")
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
test <- er_field_scores(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                        B_LU_BBWP = B_LU_BBWP,
                        B_LU_ECO1 = B_LU_ECO1,
                        B_LU_ECO2 = B_LU_ECO2,
                        B_LU_ECO3 = B_LU_ECO3,
                        B_LU_ECO4 = B_LU_ECO4,
                        B_LU_ECO5 = B_LU_ECO5,
                        B_LU_ECO6 = B_LU_ECO6,
                        B_LU_ECO7 = B_LU_ECO7,
                        B_LU_ECO8 = B_LU_ECO8,
                        B_LU_ECO9 = B_LU_ECO9,
                        B_LU_ECO10 = B_LU_ECO10,
                        B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland"),
                        B_AREA = c(45,18,0.8,6),
                        B_CT_SOIL = B_CT_SOIL, 
                        B_CT_WATER = B_CT_WATER,
                        B_CT_CLIMATE = B_CT_CLIMATE,
                        B_CT_BIO = B_CT_BIO,
                        B_CT_LANDSCAPE = B_CT_LANDSCAPE,
                        measures = NULL,
                        sector = 'dairy'
                        )

test_that("check er_field_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      id = 1:4,
      S_ER_SOIL = rep(11,4),
      S_ER_WATER = rep(12,4),
      S_ER_CLIMATE = rep(5,4),
      S_ER_BIODIVERSITY = rep(13,4),
      S_ER_LANDSCAPE = rep(4,4),
      S_ER_TOT = rep(8,4),
      S_ER_REWARD = rep(0,4)
    ),
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
test <- er_field_scores(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                        B_LU_BBWP = B_LU_BBWP,
                        B_LU_ECO1 = B_LU_ECO1,
                        B_LU_ECO2 = B_LU_ECO2,
                        B_LU_ECO3 = B_LU_ECO3,
                        B_LU_ECO4 = B_LU_ECO4,
                        B_LU_ECO5 = B_LU_ECO5,
                        B_LU_ECO6 = B_LU_ECO6,
                        B_LU_ECO7 = B_LU_ECO7,
                        B_LU_ECO8 = B_LU_ECO8,
                        B_LU_ECO9 = B_LU_ECO9,
                        B_LU_ECO10 = B_LU_ECO10,
                        B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland"),
                        B_AREA = c(45,18,0.8,6),
                        B_CT_SOIL = B_CT_SOIL, 
                        B_CT_WATER = B_CT_WATER,
                        B_CT_CLIMATE = B_CT_CLIMATE,
                        B_CT_BIO = B_CT_BIO,
                        B_CT_LANDSCAPE = B_CT_LANDSCAPE,
                        measures = measures,
                        sector = c('dairy','arable')
)


test_that("check er_field_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      id = 1:4,
      S_ER_SOIL = rep(100,4),
      S_ER_WATER = rep(100,4),
      S_ER_CLIMATE = rep(100,4),
      S_ER_BIODIVERSITY = rep(100,4),
      S_ER_LANDSCAPE = rep(100,4),
      S_ER_TOT = rep(100,4),
      S_ER_REWARD = rep(153.4,4)
    ),
    tolerance = 1,
    ignore_attr = TRUE)
})



# make measurement list for 2 of the 4 fields
measures <- rbind(data.table(id = 1, dt.measures[grepl('G3',bbwp_id)]),
                  data.table(id = 3, dt.measures[grepl('G58',bbwp_id)]),
                  data.table(id = 3, dt.measures[grepl('B191',bbwp_id)]))
measures$bbwp_status <- 'given for ANLB'
measures[,c('description', 'url') := NULL]

# run example 2 without any measures taken
test <- er_field_scores(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                        B_LU_BBWP = B_LU_BBWP,
                        B_LU_ECO1 = B_LU_ECO1,
                        B_LU_ECO2 = B_LU_ECO2,
                        B_LU_ECO3 = B_LU_ECO3,
                        B_LU_ECO4 = B_LU_ECO4,
                        B_LU_ECO5 = B_LU_ECO5,
                        B_LU_ECO6 = B_LU_ECO6,
                        B_LU_ECO7 = B_LU_ECO7,
                        B_LU_ECO8 = B_LU_ECO8,
                        B_LU_ECO9 = B_LU_ECO9,
                        B_LU_ECO10 = B_LU_ECO10,
                        B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland"),
                        B_AREA = c(45,18,0.8,6),
                        B_CT_SOIL = B_CT_SOIL, 
                        B_CT_WATER = B_CT_WATER,
                        B_CT_CLIMATE = B_CT_CLIMATE,
                        B_CT_BIO = B_CT_BIO,
                        B_CT_LANDSCAPE = B_CT_LANDSCAPE,
                        measures = measures,
                        sector = c('dairy','arable')
)


test_that("check er_field_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      id = 1:4,
      S_ER_SOIL = rep(100,4),
      S_ER_WATER = rep(100,4),
      S_ER_CLIMATE = rep(4,4),
      S_ER_BIODIVERSITY = rep(100,4),
      S_ER_LANDSCAPE = rep(100,4),
      S_ER_TOT = rep(46,4),
      S_ER_REWARD = rep(89,4)
    ),
    tolerance = 1,
    ignore_attr = TRUE)
})





