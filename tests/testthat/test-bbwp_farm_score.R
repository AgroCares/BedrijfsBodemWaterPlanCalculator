test_that("check bbwp_farm_score", {
  expect_equal(
    bbwp_farm_score(
      D_OPI_TOT = seq(0, 10, 2.5),
      D_OPI_NGW = seq(0, 10, 2.5),
      D_OPI_NSW = seq(0, 10, 2.5),
      D_OPI_PSW = seq(0, 10, 2.5),
      D_OPI_NUE = seq(0, 10, 2.5),
      D_OPI_WB = seq(0, 10, 2.5),
      D_AREA = c(10,10,10,10,10)
    ),
    expected = data.table(
      D_OPI_TOT = 5,
      D_OPI_NGW = 5,
      D_OPI_NSW = 5,
      D_OPI_PSW = 5,
      D_OPI_NUE = 5,
      D_OPI_WB = 5
    ),
    tolerance = 0.01)
})

test_that("check bbwp_farm_score", {
  expect_equal(
    bbwp_farm_score(
      D_OPI_TOT = seq(0, 100, 25),
      D_OPI_NGW = seq(0, 85, length.out = 5),
      D_OPI_NSW = seq(85, 0, length.out = 5),
      D_OPI_PSW = seq(0, 25, length.out = 5),
      D_OPI_NUE = seq(0, 2.5, length.out = 5),
      D_OPI_WB = seq(100, 25, length.out = 5),
      D_AREA = c(100,8,2.5,0.6,80)
    ),
    expected = data.table(
      D_OPI_TOT = 44,
      D_OPI_NGW = 37,
      D_OPI_NSW = 48,
      D_OPI_PSW = 11,
      D_OPI_NUE = 1,
      D_OPI_WB = 67
    ),
    tolerance = 0.01)
})
