
  # # default inputs for testing
  # B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei') 
  # B_GWL_CLASS = c('-', 'GtI', 'GtV')
  # A_P_SG = c(0.4, 0.8, 1)
  # B_SLOPE = c(1.5,4,1.5)
  # B_LU_BRP = c(265, 1932, 266)
  # B_LU_BBWP = c(1,4,1)
  # M_DRAIN = c(TRUE, FALSE, TRUE)
  # D_WP = c(0, 0.5, 1)
  # D_RISK_NGW = c(0, 0.5 ,1) 
  # D_RISK_NSW = c(0, 0.5, 1)
  # D_RISK_PSW = c(0, 0.5, 1)
  # D_RISK_NUE = c(0, 0.5, 1)
  # D_RISK_WB= c(0, 0.5, 1)
  # B_GWP = c(TRUE, FALSE, TRUE) 
  # B_AREA_DROUGHT = c(TRUE, FALSE, TRUE)
  # B_CT_PSW = c(0, 25, 50)
  # B_CT_NSW = c(0, 50, 100) 
  # B_CT_PSW_MAX = 0.5
  # B_CT_NSW_MAX = 5.0
  # measures = NULL
  # sector = c('dairy', 'arable')


# test 1 with no measures applied
test <- bbwp_field_scores(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'), 
                          B_GWL_CLASS = c('-', 'GtI', 'GtV'), 
                          A_P_SG = c(0.4, 0.8, 1), 
                          B_SLOPE =  c(1.5,4,1.5),
                          B_LU_BRP = c(265, 1932, 266), 
                          B_LU_BBWP = c(1,4,1),
                          M_DRAIN = c(TRUE, FALSE, TRUE), 
                          D_WP = c(0, 0.5, 1), 
                          D_RISK_NGW = c(0, 0.5 ,1), 
                          D_RISK_NSW = c(0, 0.5, 1),
                          D_RISK_PSW = c(0, 0.5, 1), 
                          D_RISK_NUE = c(0, 0.5, 1), 
                          D_RISK_WB= c(0, 0.5, 1),
                          B_GWP = c(TRUE, FALSE, TRUE), 
                          B_AREA_DROUGHT = c(TRUE, FALSE, TRUE),
                          B_CT_PSW = c(0, 25, 50),
                          B_CT_NSW = c(0, 50, 100), 
                          B_CT_PSW_MAX = 0.5, 
                          B_CT_NSW_MAX = 5.0, 
                          measures = NULL,
                          sector = c('dairy', 'arable')
                          )


test_that("check bbwp_field_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      S_BBWP_NGW = c(98, 53, 2),
      S_BBWP_NSW = c(99, 0, 0),
      S_BBWP_PSW = c(99, 0, 0),
      S_BBWP_NUE = c(99, 53, 3),
      S_BBWP_WB = c(98, 53, 2),
      S_BBWP_TOT = c(99, 16,1)
    ),
    tolerance = 0.01)
})


# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(effect_psw)]

# make measurement list for 2 of the 4 fields
measures <- rbind(data.table(id = 1, dt.measures[c(2,5,18,28,32,3,38,43,62)]),
                  data.table(id = 3, dt.measures[c(7,21,30,46,5)]))

# test 2 with  measures applied
test <- bbwp_field_scores(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'), 
                          B_GWL_CLASS = c('-', 'GtI', 'GtV'), 
                          A_P_SG = c(0.4, 0.8, 1), 
                          B_SLOPE =  c(1.5,4,1.5),
                          B_LU_BRP = c(265, 1932, 266), 
                          B_LU_BBWP = c(1,4,1),
                          M_DRAIN = c(TRUE, FALSE, TRUE), 
                          D_WP = c(0, 0.5, 1), 
                          D_RISK_NGW = c(0, 0.5 ,1), 
                          D_RISK_NSW = c(0, 0.5, 1),
                          D_RISK_PSW = c(0, 0.5, 1), 
                          D_RISK_NUE = c(0, 0.5, 1), 
                          D_RISK_WB= c(0, 0.5, 1),
                          B_GWP = c(TRUE, FALSE, TRUE), 
                          B_AREA_DROUGHT = c(TRUE, FALSE, TRUE),
                          B_CT_PSW = c(0, 25, 50),
                          B_CT_NSW = c(0, 50, 100), 
                          B_CT_PSW_MAX = 0.5, 
                          B_CT_NSW_MAX = 5.0, 
                          measures = measures,
                          sector = c('dairy', 'arable')
)


test_that("check bbwp_field_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      S_BBWP_NGW = c(100,53,2),
      S_BBWP_NSW = c(100,0,0),
      S_BBWP_PSW = c(100,0,0),
      S_BBWP_NUE = c(100,53,3),
      S_BBWP_WB = c(100,53,2),
      S_BBWP_TOT = c(100,16,1)
    ),
    tolerance = 0.01)
})

