
# #default inputs for testing
  
  # S_ER_TOT = seq(0, 10, 2.5)
  # S_ER_SOIL = seq(0, 10, 2.5)
  # S_ER_WATER = seq(0, 10, 2.5)
  # S_ER_CLIMATE = seq(0, 10, 2.5)
  # S_ER_BIODIVERSITY = seq(0, 10, 2.5)
  # S_ER_LANDSCAPE = seq(0, 10, 2.5)
  # S_ER_REWARD = c(250,4650,213,15,680)
  # B_AREA = rep(1,5)
  # B_SOILTYPE_AGR <- rep('dekzand',5)

# run example without any measures taken  
test <- er_opi(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen','dekzand'),
               S_ER_SOIL = seq(0, 10, 2.5),
               S_ER_WATER = seq(0, 10, 2.5),
               S_ER_CLIMATE = seq(0, 10, 2.5),
               S_ER_BIODIVERSITY = seq(0, 10, 2.5),
               S_ER_LANDSCAPE = seq(0, 10, 2.5),
               S_ER_REWARD = c(250,4650,213,15,680),
               B_AREA = rep(10000,5),
               medalscore = 'gold'
               )

test_that("check er_opi", {
  expect_equal(
    names(test),
    expected = c("dt.field.ind.score" ,"dt.farm.ind.score" , "dt.farm.score"),
    tolerance = 0.01)
})

test_that("check er_opi", {
  expect_equal(
    names(test$dt.field.ind.score),
    expected = c('field_id','s_er_soil','s_er_water','s_er_climate','s_er_biodiversity','s_er_landscape','s_er_costs','s_er_farm_tot','s_er_tot'),
    tolerance = 0.01)
})

test_that("check er_opi", {
  expect_equal(
    names(test$dt.farm.ind.score),
    expected = c('S_ER_SOIL' ,'S_ER_WATER', 'S_ER_CLIMATE', 'S_ER_BIODIVERSITY' ,'S_ER_LANDSCAPE', 'S_ER_COSTS' ,'S_ER_FARM_TOT'),
    tolerance = 0.01)
})

test_that("check er_opi", {
  expect_equal(
    test$dt.farm.score,
    expected = 77,
    tolerance = 0.01)
})

test_that("check er_opi", {
  expect_equal(
    test$dt.field.ind.score$s_er_biodiversity,
    expected = c(0,2.5,5.0,7.5,10),
    tolerance = 0.01)
})
