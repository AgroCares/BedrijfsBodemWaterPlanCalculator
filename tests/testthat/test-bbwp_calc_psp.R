test_that("bbwp_calc_psp() works without start and termination arguments", {
  expect_equal(
    object = bbwp_calc_psp(B_LU_BRP = 2014, M_GREEN = TRUE),
    expected = 410,
    tolerance = 1
  )
  # two years 
  expect_equal(
    object = bbwp_calc_psp(B_LU_BRP = c(2014, 233), M_GREEN = c(TRUE, FALSE)),
    expected = c(410, 417),
    tolerance = 1
  )
  # two fields
  expect_equal(
    object = bbwp_calc_psp(B_LU_BRP = c(2014, 233), M_GREEN = c(TRUE, FALSE)),
    expected = c(410, 417),
    tolerance = 1
  )
})

test_that("bbwp_calc_psp() works with specified green manure start and termination dates", {
  expect_equal(
    object = bbwp_calc_psp(B_LU_BRP = 2014, M_GREEN = TRUE, 
                           M_GREEN_START = 9L, M_GREEN_TERMINATE = 3L),
    expected = 384,
    tolerance = 1
  )
  
  expect_equal(
    object = bbwp_calc_psp(B_LU_BRP = c(6794, 6794) , M_GREEN = c(TRUE, FALSE), 
                           M_GREEN_START = 8L, M_GREEN_TERMINATE = 4L),
    expected = c(573, 645),
    tolerance = 1
  )
})