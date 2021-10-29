test_that("check bbwp_farm_score", {
  expect_equal(
    bbwp_farm_score(
      D_OPI_TOT = seq(0, 1, 0.25),
      D_OPI_NGW = seq(0, 1, 0.25),
      D_OPI_NSW = seq(0, 1, 0.25),
      D_OPI_PSW = seq(0, 1, 0.25),
      D_OPI_NUE = seq(0, 1, 0.25),
      D_OPI_WB = seq(0, 1, 0.25),
      D_AREA = 1
    ),
    expected = data.table(
      D_OPI_TOT = 0,
      D_OPI_NGW = 0,
      D_OPI_NSW = 0,
      D_OPI_PSW = 0,
      D_OPI_NUE = 0,
      D_OPI_WB = 0
    ),
    tolerance = 0.01)
})
