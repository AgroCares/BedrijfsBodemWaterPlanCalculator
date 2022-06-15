
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
test <- er_field_scores(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                        B_LU_BRP = c(265,2005,256,259),
                        B_LU_BBWP = c(1,4,4,9),
                        B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland"),
                        B_AREA = c(45,18,0.8,6),
                        B_CT_SOIL = 20,
                        B_CT_WATER = 15,
                        B_CT_CLIMATE = 8,
                        B_CT_BIO = 24,
                        B_CT_LANDSCAPE = 20,
                        measures = NULL,
                        sector = 'dairy'
                        )

test_that("check er_field_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      id = 1:4,
      S_ER_SOIL = rep(13,4),
      S_ER_WATER = rep(13,4),
      S_ER_CLIMATE = rep(17,4),
      S_ER_BIODIVERSITY = rep(7,4),
      S_ER_LANDSCAPE = rep(1,4),
      S_ER_TOT = rep(9,4),
      S_ER_REWARD = rep(652.04,4)
    ),
    tolerance = 0.01)
})

# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# make measurement list for 2 of the 4 fields
measures <- rbind(data.table(id = 1, dt.measures[c(2,5,18,28,32,3,38,43)]),
                  data.table(id = 3, dt.measures[c(7,21,30,46,5)]))

# run example 2 without any measures taken
test <- er_field_scores(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                        B_LU_BRP = c(265,265,265,265),
                        B_LU_BBWP = c(1,1,1,1),
                        B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12","Westelijk Holland"),
                        B_AREA = c(45,18,0.8,6),
                        B_CT_SOIL = 20,
                        B_CT_WATER = 15,
                        B_CT_CLIMATE = 8,
                        B_CT_BIO = 24,
                        B_CT_LANDSCAPE = 20,
                        measures = measures,
                        sector = c('dairy','arable')
)


test_that("check er_field_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      id = 1:4,
      S_ER_SOIL = c(33,23,61,23),
      S_ER_WATER = c(49,19,60,19),
      S_ER_CLIMATE = c(65,27,100,27),
      S_ER_BIODIVERSITY = c(21,11,23,11),
      S_ER_LANDSCAPE = c(11,1,76,1),
      S_ER_TOT = c(29,13,55,13),
      S_ER_REWARD = c(1346.6,946.58,1146.58,946.58)
    ),
    tolerance = 1,
    ignore_attr = TRUE)
})







