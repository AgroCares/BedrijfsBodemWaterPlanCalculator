
  # default inputs for testing
  # D_OPI_TOT = seq(0, 10, 2.5)
  # D_OPI_SOIL = seq(0, 10, 2.5)
  # D_OPI_WATER = seq(0, 10, 2.5)
  # D_OPI_CLIMATE = seq(0, 10, 2.5)
  # D_OPI_BIO = seq(0, 10, 2.5)
  # D_OPI_LANDSCAPE = seq(0, 10, 2.5)
  # D_AREA = 1


test_that("check er_farm_score", {
  expect_equal(
    er_farm_score(
      D_OPI_TOT = seq(0, 10, 2.5),
      D_OPI_SOIL = seq(0, 10, 2.5),
      D_OPI_WATER = seq(0, 10, 2.5),
      D_OPI_CLIMATE = seq(0, 10, 2.5),
      D_OPI_BIO = seq(0, 10, 2.5),
      D_OPI_LANDSCAPE = seq(0, 10, 2.5),
      D_AREA = rep(1,5)
    ),
    expected = data.table(
      D_OPI_TOT = 5,
      D_OPI_SOIL = 5,
      D_OPI_WATER = 5,
      D_OPI_CLIMATE = 5,
      D_OPI_BIO = 5,
      D_OPI_LANDSCAPE = 5
    ),
    tolerance = 0.01)
})



test_that("check er_farm_score", {
  expect_equal(
    er_farm_score(
      D_OPI_TOT = seq(0, 100, 25),
      D_OPI_SOIL = seq(0, 10, 2.5),
      D_OPI_WATER = seq(0, 100, 25),
      D_OPI_CLIMATE = seq(0, 10, 2.5),
      D_OPI_BIO = seq(0, 10, 2.5),
      D_OPI_LANDSCAPE = seq(0, 10, 2.5),
      D_AREA = c(15,0.8,45,95,12)
    ),
    expected = data.table(
      D_OPI_TOT = 63,
      D_OPI_SOIL = 6,
      D_OPI_WATER = 63,
      D_OPI_CLIMATE = 6,
      D_OPI_BIO = 6,
      D_OPI_LANDSCAPE = 6
    ),
    tolerance = 0.01)
})
