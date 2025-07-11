test_that("check bbwp_farm_score", {
  expect_equal(
    bbwp_farm_score(
      S_BBWP_TOT = seq(0, 10, 2.5),
      S_BBWP_NGW = seq(0, 10, 2.5),
      S_BBWP_NSW = seq(0, 10, 2.5),
      S_BBWP_PSW = seq(0, 10, 2.5),
      S_BBWP_NUE = seq(0, 10, 2.5),
      S_BBWP_WB = seq(0, 10, 2.5),
      S_BBWP_GW = seq(0, 10, 2.5),
      B_AREA = c(10,10,10,10,10)
    ),
    expected = data.table(
      S_BBWP_TOT = 5,
      S_BBWP_NGW = 5,
      S_BBWP_NSW = 5,
      S_BBWP_PSW = 5,
      S_BBWP_NUE = 5,
      S_BBWP_WB = 5,
      S_BBWP_GW = 5
    ),
    tolerance = 0.01)
})

test_that("check bbwp_farm_score", {
  expect_equal(
    bbwp_farm_score(
      S_BBWP_TOT = seq(0, 100, 25),
      S_BBWP_NGW = seq(0, 85, length.out = 5),
      S_BBWP_NSW = seq(85, 0, length.out = 5),
      S_BBWP_PSW = seq(0, 25, length.out = 5),
      S_BBWP_NUE = seq(0, 2.5, length.out = 5),
      S_BBWP_WB = seq(100, 25, length.out = 5),
      S_BBWP_GW = seq(100, 25, length.out = 5),
      B_AREA = c(100,8,2.5,0.6,80)
    ),
    expected = data.table(
      S_BBWP_TOT = 44,
      S_BBWP_NGW = 37,
      S_BBWP_NSW = 48,
      S_BBWP_PSW = 11,
      S_BBWP_NUE = 1,
      S_BBWP_WB = 67,
      S_BBWP_GW = 67
    ),
    tolerance = 0.01)
})
