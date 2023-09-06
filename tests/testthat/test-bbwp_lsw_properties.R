# test preparation of LSW

test_that("check bbwp_lsw_properties", {
  expect_equal(
    object = bbwp_lsw_properties(B_LSW_ID=1:2),
    expected = data.table(B_LSW_ID = 1:2, 
                          B_SOM_LOI = 8.65,
                          B_CLAY_MI = 15.8,
                          B_SAND_MI = 60.5,
                          B_SILT_MI = 23.71,
                          B_N_RT = 3834,
                          B_P_AL = 49,
                          B_P_CC = 2.71,
                          B_P_WA = 40,
                          B_P_SG = 22, 
                          B_FE_OX = 83,
                          B_AL_OX = 40,
                          B_SA_W = 0.47,
                          B_RO_R = 0.5,
                          B_SOM_LOI_SD = 6.67,
                          B_CLAY_MI_SD = 13.45,
                          B_SAND_MI_SD = 23.5,
                          B_SILT_MI_SD = 11.7,
                          B_N_RT_SD = 2928,
                          B_P_AL_SD = 13.5,
                          B_P_CC_SD = 1.51,
                          B_P_WA_SD = 15.6,
                          B_P_SG_SD = 14, 
                          B_FE_OX_SD = 59,
                          B_AL_OX_SD = 19,
                          B_SA_W_SD = 0.33,
                          B_RO_R_SD = 0.3
                          ),
    tolerance = 0.01)
})

