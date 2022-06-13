
  # default inputs for testing
  # S_ER_TOT = seq(0, 10, 2.5)
  # S_ER_SOIL = seq(0, 10, 2.5)
  # S_ER_WATER = seq(0, 10, 2.5)
  # S_ER_CLIMATE = seq(0, 10, 2.5)
  # S_ER_BIO = seq(0, 10, 2.5)
  # S_ER_LANDSCAPE = seq(0, 10, 2.5)
  # reward = c(250,4650,213,15,680)
  # D_AREA = 1


test_that("check er_farm_score", {
  expect_equal(
    er_farm_score(
      S_ER_TOT = seq(0, 10, 2.5),
      S_ER_SOIL = seq(0, 10, 2.5),
      S_ER_WATER = seq(0, 10, 2.5),
      S_ER_CLIMATE = seq(0, 10, 2.5),
      S_ER_BIODIVERSITY = seq(0, 10, 2.5),
      S_ER_LANDSCAPE = seq(0, 10, 2.5),
      reward = c(250,4650,213,15,680),
      D_AREA = rep(1,5)
    ),
    expected = data.table(
      S_ER_TOT = 5,
      S_ER_SOIL = 5,
      S_ER_WATER = 5,
      S_ER_CLIMATE = 5,
      S_ER_BIODIVERSITY = 5,
      S_ER_LANDSCAPE = 5,
      reward = 1162
    ),
    tolerance = 0.01)
})



test_that("check er_farm_score", {
  expect_equal(
    er_farm_score(
      S_ER_TOT = seq(0, 100, 25),
      S_ER_SOIL = seq(0, 10, 2.5),
      S_ER_WATER = seq(0, 100, 25),
      S_ER_CLIMATE = seq(0, 10, 2.5),
      S_ER_BIODIVERSITY = seq(0, 10, 2.5),
      S_ER_LANDSCAPE = seq(0, 10, 2.5),
      reward = c(250,4650,213,15,680),
      D_AREA = c(15,0.8,45,95,12)
    ),
    expected = data.table(
      S_ER_TOT = 63,
      S_ER_SOIL = 6,
      S_ER_WATER = 63,
      S_ER_CLIMATE = 6,
      S_ER_BIODIVERSITY = 6,
      S_ER_LANDSCAPE = 6,
      reward = 159
    ),
    tolerance = 0.01)
})
