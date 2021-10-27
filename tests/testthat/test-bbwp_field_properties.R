# test functions for helper function cdf_rank
test_that("estimation cdf_rank", {
  expect_equal(
    cdf_rank(25,12,seq(1,30,5)),
    expected = c(0.02275013,0.05667275,0.12167250,0.22662735,0.36944134,0.36944134,0.53320675),
    tolerance = 0.01)
})

bbwp_field_properties(
  B_BT_AK = 'dekzand',
  B_LU_BRP = 265,
  B_GT = 'GtIII',
  B_OV_WENR = 4, 
  B_HELP_WENR = 'AZW1AwF',
  A_CLAY_MI = 15,
                      A_SAND_MI = 45,
                      A_SILT_MI = 40,
                      A_OS_GV = 5, 
                      A_N_RT = 4200,
                      A_FE_OX = 500, 
                      A_AL_OX = 150, 
                      A_P_CC = 5, 
                      A_P_AL = 65,
                      A_P_WA = 52, 
                      A_P_VG = 38,
                      D_SA_W = 0.5, 
                      D_RO_R = 0.5,
                      B_LSW = 0.5)