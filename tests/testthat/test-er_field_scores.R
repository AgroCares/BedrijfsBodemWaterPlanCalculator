
require(testthat)

# default input for testing

  # crops: permanent gras, rustgewas, rooivrucht, mais
  # B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen')
  # B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais')
  # B_LU_BRP = c(265,2741,2741,259)
  # B_LU_ARABLE_ER = c(T,T,T,T)
  # B_LU_PRODUCTIVE_ER = c(T,T,T,T)
  # B_LU_CULTIVATED_ER = c(T,T,T,T)
  # B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland")
  # B_AREA = c(450000,180000,8000,60000)
  # measures = NULL
  # sector = 'dairy'

# run example 1 without any measures taken
test <- er_field_scores(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                        B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                        B_LU_BRP = c(265,2741,2741,259),
                        B_LU_ARABLE_ER = c(T,T,T,T),
                        B_LU_PRODUCTIVE_ER = c(T,T,T,T),
                        B_LU_CULTIVATED_ER = c(T,T,T,T),
                        B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland"),
                        B_AREA = c(450000,180000,8000,60000),
                        measures = NULL,
                        sector = 'dairy'
                        )

test_that("check er_field_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      id = 1:4,
      S_ER_SOIL = rep(0,4),
      S_ER_WATER = rep(0,4),
      S_ER_CLIMATE = rep(0,4),
      S_ER_BIODIVERSITY = rep(0,4),
      S_ER_LANDSCAPE = rep(0,4),
      S_ER_REWARD = rep(0,4),
      S_ER_TOT = rep(0,4)
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
                        B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                        B_LU_BRP = c(265,2741,2741,259),
                        B_LU_ARABLE_ER = c(T,T,T,T),
                        B_LU_PRODUCTIVE_ER = c(T,T,T,T),
                        B_LU_CULTIVATED_ER = c(T,T,T,T),
                        B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland"),
                        B_AREA = c(450000,180000,8000,60000),
                        measures = measures,
                        sector = c('dairy','arable')
)


test_that("check er_field_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      id = 1:4,
      S_ER_SOIL = c(29,29,36,29),
      S_ER_WATER = c(54,54,61,54),
      S_ER_CLIMATE = c(11.5,11.5,11.5,11.5),
      S_ER_BIODIVERSITY = c(65,65,70,65),
      S_ER_LANDSCAPE =c(58,58,63,58),
      S_ER_TOT = rep(165356,4),
      S_ER_REWARD = c(217,217,241,217)
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
                        B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                        B_LU_BRP = c(265,2741,2741,259),
                        B_LU_ARABLE_ER = c(T,T,T,T),
                        B_LU_PRODUCTIVE_ER = c(T,T,T,T),
                        B_LU_CULTIVATED_ER = c(T,T,T,T),
                        B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland"),
                        B_AREA = c(450000,180000,8000,60000),
                        measures = measures,
                        sector = c('dairy','arable')
)


test_that("check er_field_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      id = 1:4,
      S_ER_SOIL = rep(52,4),
      S_ER_WATER = rep(63,4),
      S_ER_CLIMATE = rep(0,4),
      S_ER_BIODIVERSITY = rep(112,4),
      S_ER_LANDSCAPE = rep(0,4),
      S_ER_TOT = rep(0,4),
      S_ER_REWARD = c(301,301,301,301)
    ),
    tolerance = 1,
    ignore_attr = TRUE)
})





