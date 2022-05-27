
require(testthat)


# run example 1
test <- bbwp_check_meas(dt = NULL,eco = FALSE, score = FALSE)

test_that("check bbwp_check_meas", {
  expect_equal(
    object = dim(test),
    expected = c(139,47),
    tolerance = 0.01)
})


# run example 2
test <- bbwp_check_meas(dt = NULL,eco = TRUE, score = FALSE)

test_that("check bbwp_check_meas", {
  expect_equal(
    object = dim(test),
    expected = c(67,47),
    tolerance = 0.01)
})

# run example 3
test <- bbwp_check_meas(dt = NULL,eco = TRUE, score = TRUE)

test_that("check bbwp_check_meas", {
  expect_equal(
    object = dim(test),
    expected = c(0,48),
    tolerance = 0.01)
})

# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# make measurement list for 2 of the 4 fields
measures <- rbind(data.table(id = 1, dt.measures[c(2,5,18,28,32,3,38,43,62)]),
                  data.table(id = 3, dt.measures[c(7,21,30,46,5)]))

# run example 4
test <- bbwp_check_meas(dt = measures,eco = TRUE, score = TRUE)

test_that("check bbwp_check_meas", {
  expect_equal(
    object = dim(test),
    expected = c(14,48),
    tolerance = 0.01)
})

# run example 4
test <- bbwp_check_meas(dt = measures,eco = TRUE, score = FALSE)

test_that("check bbwp_check_meas", {
  expect_equal(
    object = dim(test),
    expected = c(68,47),
    tolerance = 0.01)
})


