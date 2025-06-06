
  ## default inputs for testing
  # B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei')
  # B_GWL_CLASS = c('-', 'GtI', 'GtV')
  # A_P_SG = c(0.4, 0.8, 1)
  # B_SLOPE_DEGREE = c(1.5,4,1.5)
  # B_LU_BRP = c(265, 1932, 266)
  # B_LU_BBWP =c('gras_permanent','rooivrucht','gras_permanent')
  # M_DRAIN = c(TRUE, FALSE, TRUE)
  # D_SA_W = c(0, 0.5, 1)
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
                          A_P_CC = c(0.4, 0.8, 1), 
                          A_P_AL = c(40,80,100),
                          B_SLOPE_DEGREE =  c(1.5,4,1.5),
                          B_AER_CBS = c('LG05','LG14','LG03'),
                          B_LU_BBWP = c('gras_permanent','rooivrucht','gras_permanent'),
                          M_DRAIN = c(TRUE, FALSE, TRUE), 
                          D_SA_W = c(0, 0.5, 1), 
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
                          sector = c('dairy', 'arable'),
                          B_LS_HYDROCAT = 'flanken'
                          )


test_that("check bbwp_field_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      S_BBWP_NGW = c(98,67,4),
      S_BBWP_NSW = c(99,46,4),
      S_BBWP_PSW = c(99,46,4),
      S_BBWP_NUE = c(98,60,28),
      S_BBWP_WB = c(97,60,4),
      S_BBWP_TOT = c(98,55,6)
    ),
    tolerance = 0.01)
})


# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# make measurement list for 2 of the 4 fields
measures <- rbind(data.table(id = 1, dt.measures[grepl('B189|G50|G3|B137|B172|G84',bbwp_id)]),
                  data.table(id = 3, dt.measures[grepl('B135|G84|B118|G58|B146',bbwp_id)]))
measures$bbwp_status <- 'given for ANLB'


# test 2 with  measures applied
test <- bbwp_field_scores(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'), 
                          B_GWL_CLASS = c('-', 'GtI', 'GtV'), 
                          A_P_CC = c(0.4, 0.8, 1), 
                          A_P_AL = c(40,80,100),
                          B_SLOPE_DEGREE =  c(1.5,4,1.5),
                          B_LU_BBWP = c('gras_permanent','rooivrucht','gras_permanent'),
                          B_AER_CBS = c('LG05','LG14','LG03'),
                          M_DRAIN = c(TRUE, FALSE, TRUE), 
                          D_SA_W = c(0, 0.5, 1), 
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
                          sector = c('dairy', 'arable'),
                          B_LS_HYDROCAT = 'flanken'
)


test_that("check bbwp_field_scores", {
  expect_equal(
    object = test,
    expected = data.table(
      S_BBWP_NGW = c(99,67,58),
      S_BBWP_NSW = c(99,46,40),
      S_BBWP_PSW = c(99,46,34),
      S_BBWP_NUE = c(99,60,100),
      S_BBWP_WB = c(97,60,16),
      S_BBWP_TOT = c(99,55,39)
    ),
    tolerance = 0.01)
})

