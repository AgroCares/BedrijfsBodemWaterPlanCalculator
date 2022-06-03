require(testthat)

  # # default input for testing
  # B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei')
  # B_GWL_CLASS = c('GtIII', 'GtI', 'GtV')
  # B_SC_WENR = c(4, 2,2)
  # B_HELP_WENR = c('AZW1AwF', 'AZW1AwF','AZW1AwF')
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
  # B_SLOPE = c(1.5,4,1.5)
  # B_LU_BRP = c(265, 1932, 266)
  # B_LU_BBWP = c(1,4,1)
  # M_DRAIN = c(TRUE, FALSE, TRUE)
  # D_WP = c(0, 0.5, 1)
  # D_RO_R = c(0.5, 0,0.9)
  # D_AREA = c(100,80,2.5)
  # B_GWP = c(TRUE, FALSE, TRUE)
  # B_AREA_DROUGHT = c(TRUE, FALSE, TRUE)
  # B_CT_PSW = c(0, 25, 50)
  # B_CT_NSW = c(0, 50, 100)
  # B_CT_PSW_MAX = 0.5
  # B_CT_NSW_MAX = 5.0
  # measures = NULL
  # sector = c('dairy', 'arable')
  # output = 'scores'

  LSW = data.table(sd_wp = rep(0.27, 3),
                   mean_wp = 0.33,
                   sd_n_rt = 371.4,
                   sd_p_al = 10.6,
                   sd_p_cc = 1.34,
                   sd_p_sg = 7.92,
                   sd_p_wa = 12.5,
                   sd_ro_r = 0.305,
                   sd_al_ox = 3.99,
                   sd_fe_ox = 6.25,
                   mean_n_rt = 1599.9,
                   mean_p_al=  63.2,
                   mean_p_cc =3.27,
                   mean_p_sg = 59.5,
                   mean_p_wa = 56.781,
                   mean_ro_r = 0.471,
                   mean_al_ox = 36.401,
                   mean_fe_ox = 27.877,
                   sd_clay_mi = 0.583,
                   sd_sand_mi = 5.93,
                   sd_silt_mi = 4.158,
                   sd_som_loi = 0.799,
                   mean_clay_mi = 2.00,
                   mean_sand_mi = 83.96,
                   mean_silt_mi = 12.81,
                   mean_som_loi = 3.92
  )
# run example 1 without any measures taken
test <- bbwp(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
              B_GWL_CLASS = c('GtIII', 'GtI', 'GtV'),
              B_SC_WENR = c(4, 2,2),
              B_HELP_WENR = c('AZW1AwF', 'AZW1AwF','AZW1AwF'),
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
              B_SLOPE = c(1.5,4,1.5),
              B_LU_BRP = c(265, 1932, 266),
              B_LU_BBWP = c(1,4,1),
              M_DRAIN = c(TRUE, FALSE, TRUE),
              D_WP = c(0, 0.5, 1),
              D_RO_R = c(0.5, 0,0.9),
              D_AREA = c(100,80,2.5),
              B_GWP = c(TRUE, FALSE, TRUE),
              B_AREA_DROUGHT = c(TRUE, FALSE, TRUE),
              B_CT_PSW = c(0, 25, 50),
              B_CT_NSW = c(0, 50, 100),
              B_CT_PSW_MAX = 0.5,
              B_CT_NSW_MAX = 5.0,
              measures = NULL,
              sector = c('dairy', 'arable'),
             output = 'scores',
             LSW = LSW
            )

    # run tests on format and output values
    test_that("check bbwp", {
      expect_equal(
        object = names(test),
        expected = c('farm','fields'))
    })
    
    test_that("check bbwp", {
      expect_equal(
        object = colnames(test$fields),
        expected = c("s_bbwp_ngw", "s_bbwp_nsw", "s_bbwp_psw", "s_bbwp_nue", "s_bbwp_wb" , "s_bbwp_tot", "field_id"))
    })
    
    test_that("check bbwp", {
      expect_equal(
        object = test$fields$s_bbwp_tot,
        expected = c(35,17,8),
        tolerance = 0.01)
    })
    
    test_that("check bbwp", {
      expect_equal(
        object = as.numeric(unlist(test$farm)),
        expected = c(27,42,54,54,24,92),
        tolerance = 0.01)
    })


# run example 2 without any measures taken
test <- bbwp(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
             B_GWL_CLASS = c('GtIII', 'GtI', 'GtV'),
             B_SC_WENR = c(4, 2,2),
             B_HELP_WENR = c('AZW1AwF', 'AZW1AwF','AZW1AwF'),
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
             B_SLOPE = c(1.5,4,1.5),
             B_LU_BRP = c(265, 1932, 266),
             B_LU_BBWP = c(1,4,1),
             M_DRAIN = c(TRUE, FALSE, TRUE),
             D_WP = c(0, 0.5, 1),
             D_RO_R = c(0.5, 0,0.9),
             D_AREA = c(100,80,2.5),
             B_GWP = c(TRUE, FALSE, TRUE),
             B_AREA_DROUGHT = c(TRUE, FALSE, TRUE),
             B_CT_PSW = c(0, 25, 50),
             B_CT_NSW = c(0, 50, 100),
             B_CT_PSW_MAX = 0.5,
             B_CT_NSW_MAX = 5.0,
             measures = NULL,
             sector = c('dairy', 'arable'),
             output = 'measures',
             LSW = LSW
)

    # run tests on format and output values
    test_that("check bbwp", {
      expect_equal(
        object = names(test$measures[[1]]),
        expected = c("top_bbwp_tot","top_bbwp_ngw","top_bbwp_nsw","top_bbwp_psw","top_bbwp_wb", "top_bbwp_nue"))
    })
    
    test_that("check bbwp", {
      expect_equal(
        object = test$measures[[1]]$top_bbwp_tot,
        expected = c("G17","G16", "G18", "G19", "G88"))
    })



# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(effect_psw)]

# make measurement list for 2 of the 4 fields
measures <- rbind(data.table(id = 1, dt.measures[c(2,5,18,28,32,3,38,43,62)]),
                  data.table(id = 3, dt.measures[c(7,21,30,46,5)]))

# run example 3 without any measures taken
test <- bbwp(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
             B_GWL_CLASS = c('GtIII', 'GtI', 'GtV'),
             B_SC_WENR = c(4, 2,2),
             B_HELP_WENR = c('AZW1AwF', 'AZW1AwF','AZW1AwF'),
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
             B_SLOPE = c(1.5,4,1.5),
             B_LU_BRP = c(265, 1932, 266),
             B_LU_BBWP = c(1,4,1),
             M_DRAIN = c(TRUE, FALSE, TRUE),
             D_WP = c(0, 0.5, 1),
             D_RO_R = c(0.5, 0,0.9),
             D_AREA = c(100,80,2.5),
             B_GWP = c(TRUE, FALSE, TRUE),
             B_AREA_DROUGHT = c(TRUE, FALSE, TRUE),
             B_CT_PSW = c(0, 25, 50),
             B_CT_NSW = c(0, 50, 100),
             B_CT_PSW_MAX = 0.5,
             B_CT_NSW_MAX = 5.0,
             measures = measures,
             sector = c('dairy', 'arable'),
             output = 'scores',
             LSW = LSW
)

  # run tests on format and output values
  test_that("check bbwp", {
    expect_equal(
      object = test$fields$s_bbwp_tot,
      expected = c(100,17,8),
      tolerance = 0.01)
  })
  
  test_that("check bbwp", {
    expect_equal(
      object = as.numeric(unlist(test$farm)),
      expected = c(62,92,55,55,72,96),
      tolerance = 0.01)
  })