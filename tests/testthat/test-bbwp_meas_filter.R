library(testthat)

dt.measures <- as.data.table(BBWPC::bbwp_measures)

test_that("check bbwp_meas_filter", {
  expect_equal(
    object = bbwp_meas_filter(B_LU_BBWP = 'gras_permanent', 
                              sector = 'dairy', 
                              B_SOILTYPE_AGR = 'dekzand',  
                              B_SLOPE_DEGREE = 1.5,  
                              M_DRAIN = FALSE,  
                              bbwp_id =  c("G11aBWP4", "B132", "G19", "G21"), 
                              dt.measures),
    expected = c(1, 1, 0, 0),
    tolerance = 0.01)
})