require(testthat)
#require(data.table)
require(BBWPC)
  # # default input for testing
  # B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei')
  # B_GWL_CLASS = c('GtIII', 'GtI', 'GtV')
  # B_SC_WENR = c(4, 2,2)
  # B_HELP_WENR = c('AZW1AwF', 'AZW1AwF','AZW1AwF')
  # B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12")
  # A_P_SG = c(0.4, 0.8, 1)
  # A_CLAY_MI = c(15, 5,8)
  # A_SAND_MI = c(45, 65,15)
  # A_SILT_MI = c(40, 30,45)
  # A_SOM_LOI = c(5, 15,3)
  # A_N_RT = c(4200, 1000,3000)
  # A_FE_OX = c(500, 500,800)
  # A_AL_OX = c(150, 150,145)
  # A_P_CC = c(5, 1,6)
  # A_P_AL = c(65, 5,40)
  # A_P_WA = c(52, 5,45)
  # B_SLOPE_DEGREE = c(1.5,4,1.5)
  # B_LU_BBWP = c('gras_permanent','rooivrucht','gras_permanent')
  # M_DRAIN = c(TRUE, FALSE, TRUE)
  # D_SA_W = c(0, 0.5, 1)
  # D_RO_R = c(0.5, 0,0.9)
  # B_AREA = c(100,80,2.5)
  # B_GWP = c(TRUE, FALSE, TRUE)
  # B_AREA_DROUGHT = c(TRUE, FALSE, TRUE)
  # B_CT_PSW = c(0, 25, 50)
  # B_CT_NSW = c(0, 50, 100)
  # B_CT_PSW_MAX = 0.5
  # B_CT_NSW_MAX = 5.0
  # measures = NULL
  # sector = c('dairy', 'arable')
  # output = 'scores'
  # 
  LSW = data.table(B_LSW_ID = 1:3,
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
  # B_LSW_ID <- LSW$B_LSW_ID

# run example 1 without any measures taken
    test_that("check bbwp without any measures", {
      test <- bbwp(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
                   B_GWL_CLASS = c('GtIII', 'GtI', 'GtV'),
                   B_SC_WENR = c(4, 2,2),
                   B_HELP_WENR = c('AZW1AwF', 'AZW1AwF','AZW1AwF'),
                   B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12"),
                   A_P_SG = c(0.4, 0.8, 1),
                   A_CLAY_MI = c(15, 5,8),
                   A_SAND_MI = c(45, 65,15),
                   A_SILT_MI = c(40, 30,45),
                   A_SOM_LOI = c(5, 15,3) ,
                   A_N_RT = c(4200, 1000,3000),
                   A_FE_OX = c(500, 500,800),
                   A_AL_OX = c(150, 150,145),
                   A_P_CC = c(5, 1,6),
                   A_P_AL = c(65, 5,40),
                   A_P_WA = c(52, 5,45),
                   B_SLOPE_DEGREE = c(1.5,4,1.5),
                   M_DRAIN = c(TRUE, FALSE, TRUE),
                   D_SA_W = c(0, 0.5, 1),
                   D_RO_R = c(0.5, 0,0.9),
                   B_AREA = c(100,80,2.5),
                   B_GWP = c(TRUE, FALSE, TRUE),
                   B_AREA_DROUGHT = c(TRUE, FALSE, TRUE),
                   B_CT_PSW = c(0, 25, 50),
                   B_CT_NSW = c(0, 50, 100),
                   B_CT_PSW_MAX = 0.5,
                   B_CT_NSW_MAX = 5.0,
                   measures = NULL,
                   sector = c('dairy', 'arable', 'dairy'),
                   output = 'scores',
                   B_LSW_ID = 1:3,
                   LSW = LSW,
                   M_GREEN = c(FALSE, TRUE, FALSE),
                   B_LU_BRP = c(265, 2014, 265)
      )
      
      # checks on format and output values
      expect_equal(
        object = names(test),
        expected = c('farm','fields'))
      
      expect_equal(
        object = colnames(test$fields),
        expected = c("s_bbwp_ngw", "s_bbwp_nsw", "s_bbwp_psw", "s_bbwp_nue", "s_bbwp_wb" , "s_bbwp_tot", "field_id"))
      
      expect_equal(
        object = test$fields$s_bbwp_tot,
        expected = c(68, 37, 29),
        tolerance = 0.01)
      
      expect_equal(
        object = test$fields$s_bbwp_ngw,
        expected = c(72, 83, 61),
        tolerance = 0.01)
      
      expect_equal(
        object = test$fields$s_bbwp_nsw,
        expected = c(98 , 19, 18),
        tolerance = 0.01)
      
      expect_equal(
        object = test$fields$s_bbwp_psw,
        expected = c(98, 28, 10),
        tolerance = 0.01)
      
      expect_equal(
        object = test$fields$s_bbwp_nue,
        expected = c(31, 65, 30),
        tolerance = 0.01)
      
       expect_equal(
         object = test$fields$s_bbwp_wb,
         expected = c(83, 32, 80),
         tolerance = 0.01) 
      
      expect_equal(
        object = as.numeric(unlist(test$farm)),
        expected = c(54 ,77 ,62 ,66 ,46 , 61),
        tolerance = 0.01)
    })


# run example 2 without any measures taken
    test_that("check bbwp without any measures", {
      test <- bbwp(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
                   B_GWL_CLASS = c('GtIII', 'GtI', 'GtV'),
                   B_SC_WENR = c(4, 2,2),
                   B_HELP_WENR = c('AZW1AwF', 'AZW1AwF','AZW1AwF'),
                   B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12"),
                   A_P_SG = c(0.4, 0.8, 1),
                   A_CLAY_MI = c(15, 5,8),
                   A_SAND_MI = c(45, 65,15),
                   A_SILT_MI = c(40, 30,45),
                   A_SOM_LOI = c(5, 15,3) ,
                   A_N_RT = c(4200, 1000,3000),
                   A_FE_OX = c(500, 500,800),
                   A_AL_OX = c(150, 150,145),
                   A_P_CC = c(5, 1,6),
                   A_P_AL = c(65, 5,40),
                   A_P_WA = c(52, 5,45),
                   B_SLOPE_DEGREE = c(1.5,4,1.5),
                   M_DRAIN = c(TRUE, FALSE, TRUE),
                   D_SA_W = c(0, 0.5, 1),
                   D_RO_R = c(0.5, 0,0.9),
                   B_AREA = c(100,80,2.5),
                   B_GWP = c(TRUE, FALSE, TRUE),
                   B_AREA_DROUGHT = c(TRUE, FALSE, TRUE),
                   B_CT_PSW = c(0, 25, 50),
                   B_CT_NSW = c(0, 50, 100),
                   B_CT_PSW_MAX = 0.5,
                   B_CT_NSW_MAX = 5.0,
                   measures = NULL,
                   sector = c('dairy', 'arable'),
                   output = 'measures',
                   B_LSW_ID = 1:3,
                   LSW = LSW,
                   M_GREEN = c(FALSE, TRUE, FALSE),
                   B_LU_BRP = c(265, 2014, 265)
      )
      # run tests on format and output values
      
      expect_equal(
        object = names(test$measures[[1]]),
        expected = c("top_bbwp_tot","top_bbwp_ngw","top_bbwp_nsw","top_bbwp_psw","top_bbwp_wb", "top_bbwp_nue"))
      
      expect_equal(
        object = test$measures[[1]]$top_bbwp_tot,
        expected = c("G36","B132", "G53", "G25", "G11aBWP4"))
    })



# run example 3 without any measures taken
  test_that("check bbwp with measures", {
    # get internal table with measures
    dt.measures <- as.data.table(BBWPC::bbwp_measures)
    dt.measures <- dt.measures[!is.na(eco_id)]
    
    # make measurement list for 2 of the 4 fields
    measures <- rbind(data.table(id = 1, dt.measures[grepl('B189|G50|G3|B137|B172|G84',bbwp_id)]),
                      data.table(id = 3, dt.measures[grepl('B135|G84|B118|G58|B146',bbwp_id)]))
    measures$bbwp_status <- 'given for ANLB'
    
    
    test <- bbwp(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
                 B_GWL_CLASS = c('GtIII', 'GtI', 'GtV'),
                 B_SC_WENR = c(4, 2,2),
                 B_HELP_WENR = c('AZW1AwF', 'AZW1AwF','AZW1AwF'),
                 B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12"),
                 A_P_SG = c(0.4, 0.8, 1),
                 A_CLAY_MI = c(15, 5,8),
                 A_SAND_MI = c(45, 65,15),
                 A_SILT_MI = c(40, 30,45),
                 A_SOM_LOI = c(5, 15,3) ,
                 A_N_RT = c(4200, 1000,3000),
                 A_FE_OX = c(500, 500,800),
                 A_AL_OX = c(150, 150,145),
                 A_P_CC = c(5, 1,6),
                 A_P_AL = c(65, 5,40),
                 A_P_WA = c(52, 5,45),
                 B_SLOPE_DEGREE = c(1.5,4,1.5),
                 M_DRAIN = c(TRUE, FALSE, TRUE),
                 D_SA_W = c(0, 0.5, 1),
                 D_RO_R = c(0.5, 0,0.9),
                 B_AREA = c(100,80,2.5),
                 B_GWP = c(TRUE, FALSE, TRUE),
                 B_AREA_DROUGHT = c(TRUE, FALSE, TRUE),
                 B_CT_PSW = c(0, 25, 50),
                 B_CT_NSW = c(0, 50, 100),
                 B_CT_PSW_MAX = 0.5,
                 B_CT_NSW_MAX = 5.0,
                 measures = measures,
                 sector = c('dairy', 'arable'),
                 output = 'scores',
                 B_LSW_ID = 1:3,
                 LSW = LSW,
                 M_GREEN = c(FALSE, TRUE, FALSE),
                 B_LU_BRP = c(265, 2014, 265)
    )
    
    
    # run tests on format and output values
    expect_equal(
      object = test$fields$s_bbwp_tot,
      expected = c(88, 37, 64),
      tolerance = 0.01)
    
    expect_equal(
      object = as.numeric(unlist(test$farm)),
      expected = c(65, 85 ,63 ,67 ,72 ,62),
      tolerance = 0.01)
  })

  
  # run tests on format and output values
  test_that("check bbwp works without measures and no LSW supplied", {
    # run example 3 without any measures taken and LSW equal to NULL
    expect_warning( # warning is expected as LSW is not supplied and used Dutch average values
      test <- bbwp(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
                   B_GWL_CLASS = c('GtIII', 'GtI', 'GtV'),
                   B_SC_WENR = c(4, 2,2),
                   B_HELP_WENR = c('AZW1AwF', 'AZW1AwF','AZW1AwF'),
                   B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12"),
                   A_P_SG = c(0.4, 0.8, 1),
                   A_CLAY_MI = c(15, 5,8),
                   A_SAND_MI = c(45, 65,15),
                   A_SILT_MI = c(40, 30,45),
                   A_SOM_LOI = c(5, 15,3) ,
                   A_N_RT = c(4200, 1000,3000),
                   A_FE_OX = c(500, 500,800),
                   A_AL_OX = c(150, 150,145),
                   A_P_CC = c(5, 1,6),
                   A_P_AL = c(65, 5,40),
                   A_P_WA = c(52, 5,45),
                   B_SLOPE_DEGREE = c(1.5,4,1.5),
                   M_DRAIN = c(TRUE, FALSE, TRUE),
                   D_SA_W = c(0, 0.5, 1),
                   D_RO_R = c(0.5, 0,0.9),
                   B_AREA = c(100,80,2.5),
                   B_GWP = c(TRUE, FALSE, TRUE),
                   B_AREA_DROUGHT = c(TRUE, FALSE, TRUE),
                   B_CT_PSW = c(0, 25, 50),
                   B_CT_NSW = c(0, 50, 100),
                   B_CT_PSW_MAX = 0.5,
                   B_CT_NSW_MAX = 5.0,
                   measures = NULL,
                   sector = c('dairy', 'arable'),
                   output = 'scores',
                   B_LSW_ID = 1:3,
                   LSW = NULL,
                   M_GREEN = c(FALSE, TRUE, FALSE),
                   B_LU_BRP = c(265, 2014, 265))
    )
    
    expect_equal(
      object = names(test),
      expected = c('farm','fields'))
    
    expect_equal(
      object = colnames(test$fields),
      expected =c("s_bbwp_ngw", "s_bbwp_nsw", "s_bbwp_psw", "s_bbwp_nue", "s_bbwp_wb" , "s_bbwp_tot", "field_id"))
    
    expect_equal(
      object = test$fields$s_bbwp_tot,
      expected = c(68 , 37 , 29),
      tolerance = 0.01)
    
    expect_equal(
      object = as.numeric(unlist(test$farm)),
      expected = c(54 ,77 ,62 ,66 ,46 ,61),
      tolerance = 0.01)
  })
  
  
# example with high PSW loss risk
  # run example 1 without any measures taken
  test <- bbwp(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
               B_GWL_CLASS = c('GtI', 'GtI', 'GtI'),
               B_SC_WENR = c(4, 4,4),
               B_HELP_WENR = c('AZW1AwF', 'AZW1AwF','AZW1AwF'),
               B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12"),
               A_P_SG = c(50, 80, 65),
               A_CLAY_MI = c(15, 5,8),
               A_SAND_MI = c(45, 65,15),
               A_SILT_MI = c(40, 30,45),
               A_SOM_LOI = c(5, 15,3) ,
               A_N_RT = c(4200, 1000,3000),
               A_FE_OX = c(500, 500,800),
               A_AL_OX = c(150, 150,145),
               A_P_CC = c(15, 10,16),
               A_P_AL = c(165, 150,140),
               A_P_WA = c(100, 100,100),
               B_SLOPE_DEGREE = c(2,4,2),
               M_DRAIN = c(TRUE, FALSE, TRUE),
               D_SA_W = c(1.0, 1.0, 1),
               D_RO_R = c(.9, 0.9,0.9),
               B_AREA = c(100,80,2.5),
               B_GWP = c(TRUE, FALSE, TRUE),
               B_AREA_DROUGHT = c(TRUE, FALSE, TRUE),
               B_CT_PSW = c(1.0, 1.0, 1.0),
               B_CT_NSW = c(5, 5, 5),
               B_CT_PSW_MAX = 0.5,
               B_CT_NSW_MAX = 5.0,
               measures = NULL,
               sector = c('dairy', 'arable'),
               output = 'scores',
               B_LSW_ID = 1:3,
               LSW = LSW,
               M_GREEN = c(FALSE, TRUE, FALSE),
               B_LU_BRP = c(256, 2014, 256)
  )
  
  test_that("check bbwp with high PSW loss risk", {
    expect_equal(
      object = as.numeric(unlist(test$farm)),
      expected = c(20, 86, 8, 6, 32, 29),
      tolerance = 0.01)
  })
  
  
  # example with high nitrate leaching risk
  # run example 1 without any measures taken
  test <- bbwp(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
               B_GWL_CLASS = c('GtVI', 'GtVI', 'GtVI'),
               B_SC_WENR = c(1, 1,1),
               B_HELP_WENR = c('AZW1AwF', 'AZW1AwF','AZW1AwF'),
               B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12"),
               A_P_SG = c(5, 5, 5),
               A_CLAY_MI = c(15, 5,8),
               A_SAND_MI = c(45, 65,15),
               A_SILT_MI = c(40, 30,45),
               A_SOM_LOI = c(5, 15,3) ,
               A_N_RT = c(4200, 1000,3000),
               A_FE_OX = c(500, 500,800),
               A_AL_OX = c(150, 150,145),
               A_P_CC = c(1.5, 1.0,1.6),
               A_P_AL = c(16, 15,14),
               A_P_WA = c(10, 10,10),
               B_SLOPE_DEGREE = c(1.2,1.4,1.2),
               M_DRAIN = c(TRUE, FALSE, TRUE),
               D_SA_W = c(1.0, 1.0, 1),
               D_RO_R = c(.9, 0.9,0.9),
               B_AREA = c(50,50,50),
               B_GWP = c(TRUE, TRUE, TRUE),
               B_AREA_DROUGHT = c(TRUE, TRUE, TRUE),
               B_CT_PSW = c(.10, .10, .10),
               B_CT_NSW = c(0.5, 0.5, 0.5),
               B_CT_PSW_MAX = 0.5,
               B_CT_NSW_MAX = 5.0,
               measures = NULL,
               sector = c('dairy', 'arable'),
               output = 'scores',
               B_LSW_ID = 1:3,
               LSW = LSW,
               M_GREEN = c(FALSE, TRUE, FALSE),
               B_LU_BRP = c(2014, 2014, 2014)
  )
  
  test_that("check bbwp with high nitrate leaching risk and no measures", {
    expect_equal(
      object = as.numeric(unlist(test$farm)),
      expected = c(33, 24, 55, 49, 82, 9),
      tolerance = 0.01)
  })
  
  # run example with low regional targets for region
  test <- bbwp(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
               B_GWL_CLASS = c('GtI', 'GtI', 'GtI'),
               B_SC_WENR = c(4, 4,4),
               B_HELP_WENR = c('AZW1AwF', 'AZW1AwF','AZW1AwF'),
               B_AER_CBS = c("Bouwhoek en Hogeland","LG14","LG12"),
               A_P_SG = c(50, 80, 65),
               A_CLAY_MI = c(15, 5,8),
               A_SAND_MI = c(45, 65,15),
               A_SILT_MI = c(40, 30,45),
               A_SOM_LOI = c(5, 15,3) ,
               A_N_RT = c(4200, 1000,3000),
               A_FE_OX = c(500, 500,800),
               A_AL_OX = c(150, 150,145),
               A_P_CC = c(15, 10,16),
               A_P_AL = c(165, 150,140),
               A_P_WA = c(100, 100,100),
               B_SLOPE_DEGREE = c(2,4,2),
               M_DRAIN = c(TRUE, FALSE, TRUE),
               D_SA_W = c(1.0, 1.0, 1),
               D_RO_R = c(.9, 0.9,0.9),
               B_AREA = c(100,80,2.5),
               B_GWP = c(TRUE, FALSE, TRUE),
               B_AREA_DROUGHT = c(TRUE, FALSE, TRUE),
               B_CT_PSW = c(0.1, 0.1, 0.1),
               B_CT_NSW = c(0.5, 0.5, 0.5),
               B_CT_PSW_MAX = 0.5,
               B_CT_NSW_MAX = 5.0,
               measures = NULL,
               sector = c('dairy', 'arable'),
               output = 'scores',
               B_LSW_ID = 1:3,
               LSW = LSW,
               M_GREEN = c(FALSE, TRUE, FALSE),
               B_LU_BRP = c(2014, 2014, 2014)
  )
  
  test_that("check bbwp with low regional targets", {
    expect_equal(
      object = as.numeric(unlist(test$farm)),
      expected = c(43, 86, 49, 43, 32, 29),
      tolerance = 0.01)
  })
  
  test_that('B_SLOPE and B_SLOPE_DEGREE can both be used but at leatst one needs to be given', {
    # missing both degree parameters
    expect_error(bbwp(
      B_SOILTYPE_AGR = c('dekzand'),
      B_GWL_CLASS = c('GtIII'),
      B_SC_WENR = c(4),
      B_HELP_WENR = c('AZW1AwF'),
      B_AER_CBS = c("LG12"),
      A_P_SG = c(0.4),
      A_CLAY_MI = c(15),
      A_SAND_MI = c(45),
      A_SILT_MI = c(40),
      A_SOM_LOI = c(5) ,
      A_N_RT = c(4200),
      A_FE_OX = c(500),
      A_AL_OX = c(150),
      A_P_CC = c(5),
      A_P_AL = c(65),
      A_P_WA = c(52),
      M_DRAIN = c(TRUE),
      D_SA_W = c(0),
      D_RO_R = c(0.5),
      B_AREA = c(100),
      B_GWP = c(TRUE),
      B_AREA_DROUGHT = c(TRUE),
      B_CT_PSW = c(0),
      B_CT_NSW = c(0),
      B_CT_PSW_MAX = 0.5,
      B_CT_NSW_MAX = 5.0,
      measures = NULL,
      sector = c('dairy'),
      output = 'scores',
      B_LSW_ID = 1,
      LSW = NULL,
      M_GREEN = FALSE,
      B_LU_BRP = 265
    ))
  })
  
  test_that('Warnings are given when using deprecated B_SLOPE instead of B_SLOPE_DEGREE',{
    expect_warning(bbwp(
      B_SLOPE = c(TRUE),
      B_SOILTYPE_AGR = c('dekzand'),
      B_GWL_CLASS = c('GtIII'),
      B_SC_WENR = c(4),
      B_HELP_WENR = c('AZW1AwF'),
      B_AER_CBS = c("LG12"),
      A_P_SG = c(0.4),
      A_CLAY_MI = c(15),
      A_SAND_MI = c(45),
      A_SILT_MI = c(40),
      A_SOM_LOI = c(5) ,
      A_N_RT = c(4200),
      A_FE_OX = c(500),
      A_AL_OX = c(150),
      A_P_CC = c(5),
      A_P_AL = c(65),
      A_P_WA = c(52),
      M_DRAIN = c(TRUE),
      D_SA_W = c(0),
      D_RO_R = c(0.5),
      B_AREA = c(100),
      B_GWP = c(TRUE),
      B_AREA_DROUGHT = c(TRUE),
      B_CT_PSW = c(0),
      B_CT_NSW = c(0),
      B_CT_PSW_MAX = 0.5,
      B_CT_NSW_MAX = 5.0,
      measures = NULL,
      sector = c('dairy'),
      output = 'scores',
      B_LSW_ID = 1,
      LSW = LSW[1],
      M_GREEN = FALSE,
      B_LU_BRP = 265
    ))
  })
  
  