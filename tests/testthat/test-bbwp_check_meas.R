
require(testthat)


# run example 1
test <- bbwp_check_meas(dt = NULL,eco = FALSE, score = FALSE)

test_that("check bbwp_check_meas", {
  expect_equal(
    object = dim(test),
    expected = c(169,65),
    tolerance = 0.01)
})


# run example 2
test <- bbwp_check_meas(dt = NULL,eco = TRUE, score = FALSE)

test_that("check bbwp_check_meas", {
  expect_equal(
    object = dim(test),
    expected = c(94,65),
    tolerance = 0.01)
})

# run example 3
test <- bbwp_check_meas(dt = NULL,eco = TRUE, score = TRUE)

test_that("check bbwp_check_meas", {
  expect_equal(
    object = dim(test),
    expected = c(0,67),
    tolerance = 0.01)
})

# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# make measurement list for 2 of the 4 fields
measures <- rbind(data.table(id = 1, dt.measures[grepl('B189|G50|G3|B137|B172|G84',bbwp_id)]),
                  data.table(id = 3, dt.measures[grepl('B135|G84|B118|G58|B146',bbwp_id)]))
measures$bbwp_status <- 'given for ANLB'

# run example 4
test <- bbwp_check_meas(dt = measures,eco = TRUE, score = TRUE)

test_that("check bbwp_check_meas", {
  expect_equal(
    object = dim(test),
    expected = c(11,67),
    tolerance = 0.01)
})

# run example 4
test <- bbwp_check_meas(dt = measures,eco = TRUE, score = FALSE)

test_that("check bbwp_check_meas", {
  expect_equal(
    object = dim(test),
    expected = c(95,65),
    tolerance = 0.01)
})


