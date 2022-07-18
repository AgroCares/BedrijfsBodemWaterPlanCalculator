
# #default inputs for testing
  
  # S_ER_TOT = seq(0, 10, 2.5)
  # S_ER_SOIL = seq(0, 10, 2.5)
  # S_ER_WATER = seq(0, 10, 2.5)
  # S_ER_CLIMATE = seq(0, 10, 2.5)
  # S_ER_BIODIVERSITY = seq(0, 10, 2.5)
  # S_ER_LANDSCAPE = seq(0, 10, 2.5)
  # S_ER_REWARD = c(250,4650,213,15,680)
  # B_AREA = rep(1,5)


# run example without any measures taken  

test_that("check er_farm_score", {
  expect_equal(
    er_farm_score(
      S_ER_TOT = seq(0, 10, 2.5),
      S_ER_SOIL = seq(0, 10, 2.5),
      S_ER_WATER = seq(0, 10, 2.5),
      S_ER_CLIMATE = seq(0, 10, 2.5),
      S_ER_BIODIVERSITY = seq(0, 10, 2.5),
      S_ER_LANDSCAPE = seq(0, 10, 2.5),
      S_ER_REWARD = c(250,4650,213,15,680),
      B_AREA = rep(1,5)
    ),
    expected = data.table(
      S_ER_SOIL = 5,
      S_ER_WATER = 5,
      S_ER_CLIMATE = 5,
      S_ER_BIODIVERSITY = 5,
      S_ER_LANDSCAPE = 5,
      S_ER_TOT = 5,
      S_ER_REWARD = 1162
    ),
    tolerance = 0.01)
})

