test_that("bbwp_wat_groundwater_recharge works", {
  
  # test for various crops on river clay
  expect_equal(
    bbwp_wat_groundwater_recharge(
      B_LU_BRP = c(233,259,2014,308),
      B_SC_WENR = rep(11,4),
      B_GWL_CLASS = rep('VI',4),
      M_DRAIN = rep(TRUE,4),
      A_CLAY_MI = rep(20,4),
      A_SAND_MI = rep(15,4),
      A_SILT_MI = rep(10,4),
      A_SOM_LOI = c(2,3,5,8),
      M_GREEN = rep(FALSE,4)
    ),
    expected = c(0.68,0.71,0.78,0.79),
    tolerance = 0.1
  )
  
  # test for various crops on loess
  expect_equal(
    bbwp_wat_groundwater_recharge(
      B_LU_BRP = c(233,259,2014,308),
      B_SC_WENR = rep(11,4),
      B_GWL_CLASS = rep('II',4),
      M_DRAIN = rep(TRUE,4),
      A_CLAY_MI = rep(20,4),
      A_SAND_MI = rep(15,4),
      A_SILT_MI = rep(60,4),
      A_SOM_LOI = c(2,3,15,18),
      M_GREEN = rep(FALSE,4)
    ),
    expected = c(0.69,0.71,0.80,0.80),
    tolerance = 0.1
  )
  
})

test_that("M_DRAIN affect GWL class IIIb and IV but not others", {
  expect_false(
    all(bbwp_wat_groundwater_recharge(
      B_LU_BRP = c(233,259,2014,308),
      B_SC_WENR = rep(1,4),
      B_GWL_CLASS = rep('IV',4),
      M_DRAIN = rep(TRUE,4),
      A_CLAY_MI = rep(20,4),
      A_SAND_MI = rep(15,4),
      A_SILT_MI = rep(10,4),
      A_SOM_LOI = rep(5, 4),
      M_GREEN = rep(FALSE,4)
    ) == bbwp_wat_groundwater_recharge(
      B_LU_BRP = c(233,259,2014,308),
      B_SC_WENR = rep(1,4),
      B_GWL_CLASS = rep('IV',4),
      M_DRAIN = rep(FALSE,4),
      A_CLAY_MI = rep(20,4),
      A_SAND_MI = rep(15,4),
      A_SILT_MI = rep(10,4),
      A_SOM_LOI = rep(5, 4),
      M_GREEN = rep(FALSE,4)
    )
  ))

  expect_false(
    all(bbwp_wat_groundwater_recharge(
      B_LU_BRP = c(233,259,2014,308),
      B_SC_WENR = rep(11,4),
      B_GWL_CLASS = rep('IIIb',4),
      M_DRAIN = rep(TRUE,4),
      A_CLAY_MI = rep(20,4),
      A_SAND_MI = rep(15,4),
      A_SILT_MI = rep(10,4),
      A_SOM_LOI = c(2,3,5,8),
      M_GREEN = rep(FALSE,4)
    ) == bbwp_wat_groundwater_recharge(
      B_LU_BRP = c(233,259,2014,308),
      B_SC_WENR = rep(11,4),
      B_GWL_CLASS = rep('IIIb',4),
      M_DRAIN = rep(FALSE,4),
      A_CLAY_MI = rep(20,4),
      A_SAND_MI = rep(15,4),
      A_SILT_MI = rep(10,4),
      A_SOM_LOI = c(2,3,5,8),
      M_GREEN = rep(FALSE,4)
    )
  ))
  
  expect_equal(
    object = bbwp_wat_groundwater_recharge(
      B_LU_BRP = rep(233, 11),
      B_SC_WENR = rep(11, 11),
      B_GWL_CLASS = c(paste0('', c('I', 'II', 'IIb', 'III', 'IIIa','V', 'Va', 'Vb',
                                     'VI', 'VII', 'VIII'))),
      M_DRAIN = rep(FALSE, 11),
      A_CLAY_MI = rep(20, 11),
      A_SAND_MI = rep(15, 11),
      A_SILT_MI = rep(10, 11),
      A_SOM_LOI = rep(5, 11),
      M_GREEN = rep(FALSE, 11)
    ),
    expected = bbwp_wat_groundwater_recharge(
      B_LU_BRP = rep(233, 11),
      B_SC_WENR = rep(11, 11),
      B_GWL_CLASS = c(paste0('', c('I', 'II', 'IIb', 'III', 'IIIa','V', 'Va', 'Vb',
                                     'VI', 'VII', 'VIII'))),
      M_DRAIN = rep(TRUE, 11),
      A_CLAY_MI = rep(20, 11),
      A_SAND_MI = rep(15, 11),
      A_SILT_MI = rep(10, 11),
      A_SOM_LOI = rep(5, 11),
      M_GREEN = rep(FALSE, 11)
    )
  )
  
  # check that functions that wrap bbwp_wat_groundwater_recharge keep the b suffix intact
  LSW = data.table(B_LSW_ID = 1:2,
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
                   B_RO_R_SD = 0.3)
  
  drainT <- bbwp(B_SOILTYPE_AGR = c('dekzand', 'loess'),
                 B_GWL_CLASS = c('IIIb', 'IV'),
                 B_SC_WENR = c(4, 2),
                 B_HELP_WENR = c('AZW1AwF', 'AZW1AwF'),
                 B_AER_CBS = c("Bouwhoek en Hogeland","LG14"),
                 A_P_SG = c(0.4, 0.8),
                 A_CLAY_MI = c(15, 5),
                 A_SAND_MI = c(45, 65),
                 A_SILT_MI = c(40, 30),
                 A_SOM_LOI = c(5, 15) ,
                 A_N_RT = c(4200, 1000),
                 A_FE_OX = c(500, 500),
                 A_AL_OX = c(150, 150),
                 A_P_CC = c(5, 1),
                 A_P_AL = c(65, 5),
                 A_P_WA = c(52, 5),
                 B_SLOPE_DEGREE = c(1.5,4),
                 M_DRAIN = c(TRUE, TRUE), # With these GWL_CLASSES, M_DRAIN should affect the WB score
                 D_SA_W = c(0, 0.5),
                 D_RO_R = c(0.5, 0),
                 B_AREA = c(100,80),
                 B_GWP = c(TRUE, FALSE),
                 B_AREA_DROUGHT = c(FALSE, FALSE),
                 B_CT_PSW = c(0, 25),
                 B_CT_NSW = c(0, 50),
                 B_CT_PSW_MAX = 0.5,
                 B_CT_NSW_MAX = 5.0,
                 measures = NULL,
                 sector = c('dairy', 'arable'),
                 output = 'scores',
                 B_LSW_ID = 1:2,
                 LSW = LSW,
                 M_GREEN = c(FALSE, FALSE),
                 B_LU_BRP = c(265, 2014)
  )
  drainF <- bbwp(B_SOILTYPE_AGR = c('dekzand', 'loess'),
                 B_GWL_CLASS = c('IIIb', 'IV'),
                 B_SC_WENR = c(4, 2),
                 B_HELP_WENR = c('AZW1AwF', 'AZW1AwF'),
                 B_AER_CBS = c("Bouwhoek en Hogeland","LG14"),
                 A_P_SG = c(0.4, 0.8),
                 A_CLAY_MI = c(15, 5),
                 A_SAND_MI = c(45, 65),
                 A_SILT_MI = c(40, 30),
                 A_SOM_LOI = c(5, 15) ,
                 A_N_RT = c(4200, 1000),
                 A_FE_OX = c(500, 500),
                 A_AL_OX = c(150, 150),
                 A_P_CC = c(5, 1),
                 A_P_AL = c(65, 5),
                 A_P_WA = c(52, 5),
                 B_SLOPE_DEGREE = c(1.5,4),
                 M_DRAIN = c(FALSE, FALSE), # With these GWL_CLASSES, M_DRAIN should affect the WB score
                 D_SA_W = c(0, 0.5),
                 D_RO_R = c(0.5, 0),
                 B_AREA = c(100,80),
                 B_GWP = c(TRUE, FALSE),
                 B_AREA_DROUGHT = c(FALSE, FALSE),
                 B_CT_PSW = c(0, 25),
                 B_CT_NSW = c(0, 50),
                 B_CT_PSW_MAX = 0.5,
                 B_CT_NSW_MAX = 5.0,
                 measures = NULL,
                 sector = c('dairy', 'arable'),
                 output = 'scores',
                 B_LSW_ID = 1:2,
                 LSW = LSW,
                 M_GREEN = c(FALSE, FALSE),
                 B_LU_BRP = c(265, 2014)
  )
  expect_false(drainT$farm$s_bbwp_tot == drainF$farm$s_bbwp_tot)
  expect_false(drainT$farm$s_bbwp_wb == drainF$farm$s_bbwp_wb)
  expect_true(drainT$farm$s_bbwp_ngw == drainF$farm$s_bbwp_ngw)
  expect_true(drainT$farm$s_bbwp_nsw == drainF$farm$s_bbwp_nsw)
  expect_true(drainT$farm$s_bbwp_psw == drainF$farm$s_bbwp_psw)
  expect_true(drainT$farm$s_bbwp_nue == drainF$farm$s_bbwp_nue)
})
