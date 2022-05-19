test_that("check bbwp_meas_rank", {
  expect_equal(
    bbwp_meas_rank(
      B_SOILTYPE_AGR = c('loess', 'dekzand', 'rivierklei'), 
      B_GWL_CLASS = c('-', 'GtI', 'GtV'),  
      A_P_SG = c(0.1, 0.5, 1),
      B_SLOPE = c(TRUE, FALSE, FALSE), 
      B_LU_BRP = c(265, 1932, 266), 
      M_DRAIN = c(FALSE, TRUE, TRUE), 
      D_WP = c(0, 0.5, 1),
      D_OPI_NGW = c(0, 0.5, 1), 
      D_OPI_NSW = c(0, 0.5, 1), 
      D_OPI_PSW = c(0, 0.5, 1), 
      D_OPI_NUE = c(0, 0.5, 1),  
      D_OPI_WB = c(0, 0.5, 1), 
      available_measures = rbindlist(list(
        list(
          clay = TRUE,
          peat = TRUE,
          sand = TRUE,
          bulbs = TRUE, 
          diary = TRUE,
          level = "field",
          loess =  TRUE,
          active = TRUE,
          arable = TRUE,
          bbwp_id = "X1",
          effect_wb = 0.25,
          psw_bulbs = 0,
          effect_ngw = 0.5,
          effect_nsw = 0.5,
          effect_nue = 0.5, 
          effect_psw =  0,
          nsw_drains = 0,
          nsw_gwl_low = 0,
          psw_noslope = 0,
          psw_psg_low = 0,
          effect_costs = 1,
          nsw_gwl_high = 0,
          nsw_nodrains = 0,
          psw_psg_high = 0,
          tree_nursery = FALSE,
          ngw_grassland = 0,
          psw_psg_medium = 0, 
          vegetables_outdoor = FALSE
        ),
        list(
          clay = FALSE,
          peat = FALSE,
          sand = FALSE,
          bulbs = FALSE,
          diary = FALSE,
          level = "field",
          loess = FALSE,
          active = TRUE,
          arable = FALSE,
          bbwp_id = 'X2',
          effect_wb = -0.25,
          psw_bulbs = 0.5,
          effect_ngw = 0,
          effect_nsw = 0,
          effect_nue = 0,
          effect_psw = 1,
          nsw_drains = 1,
          nsw_gwl_low = 1,
          psw_noslope = 1,
          psw_psg_low = 1,
          effect_costs = 0,
          nsw_gwl_high = 1,
          nsw_nodrains = 1,
          psw_psg_high = 1,
          tree_nursery = TRUE,
          ngw_grassland = 1,
          psw_psg_medium = 1,
          vegetables_outdoor = TRUE
        )
      )),
      sector = c('diary', 'arable')
    ),
    expected = data.table(
      top = rep(c('total', 'ngw', 'nsw', 'nue'), 3),
      measure = 'X1',
      rank = 1
    ),
    tolerance = 0.01)
})

test_that("check bbwp_meas_rank", {
  expect_equal(
    hoi <- bbwp_meas_score(
      B_SOILTYPE_AGR = c('loess', 'dekzand', 'rivierklei'), 
      B_GWL_CLASS = c('-', 'GtI', 'GtV'),  
      A_P_SG = c(0.1, 0.5, 1),
      B_SLOPE = c(TRUE, FALSE, FALSE), 
      B_LU_BRP = c(265, 1932, 266), 
      M_DRAIN = c(FALSE, TRUE, TRUE), 
      D_WP = c(0, 0.5, 1),
      D_OPI_NGW = c(0, 0.5, 1), 
      D_OPI_NSW = c(0, 0.5, 1), 
      D_OPI_PSW = c(0, 0.5, 1), 
      D_OPI_NUE = c(0, 0.5, 1),  
      D_OPI_WB = c(0, 0.5, 1), 
      measures = rbindlist(list(
        list(
          id = 1,
          clay = TRUE,
          peat = TRUE,
          sand = TRUE,
          bulbs = TRUE, 
          diary = TRUE,
          level = "field",
          loess =  TRUE,
          active = TRUE,
          arable = TRUE,
          bbwp_id = "X1",
          effect_wb = 0.25,
          psw_bulbs = 0,
          effect_ngw = 0.5,
          effect_nsw = 0.5,
          effect_nue = 0.5, 
          effect_psw =  0,
          nsw_drains = 0,
          nsw_gwl_low = 0,
          psw_noslope = 0,
          psw_psg_low = 0,
          effect_costs = 1,
          nsw_gwl_high = 0,
          nsw_nodrains = 0,
          psw_psg_high = 0,
          tree_nursery = FALSE,
          ngw_grassland = 0,
          psw_psg_medium = 0, 
          vegetables_outdoor = FALSE
        ),
        list(
          id = 2,
          clay = FALSE,
          peat = FALSE,
          sand = FALSE,
          bulbs = FALSE,
          diary = FALSE,
          level = "field",
          loess = FALSE,
          active = TRUE,
          arable = FALSE,
          bbwp_id = 'X2',
          effect_wb = -0.25,
          psw_bulbs = 0.5,
          effect_ngw = 0,
          effect_nsw = 0,
          effect_nue = 0,
          effect_psw = 1,
          nsw_drains = 1,
          nsw_gwl_low = 1,
          psw_noslope = 1,
          psw_psg_low = 1,
          effect_costs = 0,
          nsw_gwl_high = 1,
          nsw_nodrains = 1,
          psw_psg_high = 1,
          tree_nursery = TRUE,
          ngw_grassland = 1,
          psw_psg_medium = 1,
          vegetables_outdoor = TRUE
        )
      )),
      sector = c('diary', 'arable')
    ),
    expected = data.table(
      D_MEAS_NGW = c(0, 0, 0),
      D_MEAS_NSW = c(0, 0, 0),
      D_MEAS_PSW = c(0, 0, 0),
      D_MEAS_NUE = c(0, 0, 0),
      D_MEAS_WB = c(0, 0, 0),
      D_MEAS_TOT = c(0, 0, 0)
    ),
    tolerance = 0.01)
})