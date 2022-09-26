require(testthat)

  # # default input for testing
  # B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei')
  # B_GWL_CLASS = c('GtIII', 'GtI', 'GtV')
  # B_AER_CBS = c('LG05','LG14','LG02')
  # A_P_SG = c(0.4, 0.8, 1)
  # B_SLOPE_DEGREE = c(1.5,4,1.5)
  # B_AER_CBS = c('LG05','LG14','LG02')
  # B_LU_BBWP = rep('gras_permanent',3)
  # B_LU_BRP = rep(265,3)
  # B_LU_ARABLE_ER = c(F,F,F)
  # B_LU_PRODUCTIVE_ER = c(T,T,T)
  # B_LU_CULTIVATED_ER = c(T,T,T)
  # M_DRAIN = c(TRUE, FALSE, TRUE)
  # D_SA_W = c(0, 0.5, 1)
  # B_AREA = c(100,80,2.5)
  # measures = NULL
  # farmscore = 100
  # sector = c('dairy', 'arable')
  # output = 'scores'
  # medalscore = 'gold'

# run example 1 without any measures taken
test <- ecoregeling(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
                    B_GWL_CLASS = c('GtIII', 'GtI', 'GtV'),
                    A_P_SG = c(0.4, 0.8, 1),
                    B_SLOPE_DEGREE = c(1.5,4,1.5),
                    B_AER_CBS = c('LG05','LG14','LG02'),
                    B_LU_BBWP = c('gras_permanent','rooivrucht','gras_permanent'),
                    B_LU_BRP = c(265,2741,259),
                    B_LU_ARABLE_ER = c(F,T,T),
                    B_LU_PRODUCTIVE_ER = c(T,T,T),
                    B_LU_CULTIVATED_ER = c(T,T,T),
                    M_DRAIN = c(TRUE, FALSE, TRUE),
                    D_SA_W = c(0, 0.5, 1),
                    B_AREA = c(100,80,2.5),
                    farmscore = 100,
                    measures = NULL,
                    sector = c('dairy', 'arable'),
                    output = 'scores'
                   )


  # run tests on format and output values
  test_that("check ecoregeling", {
    expect_equal(
      object = names(test),
      expected = c('farm','fields','farm_thresholds'))
  })
  
  test_that("check ecoregeling", {
    expect_equal(
      object = colnames(test$fields),
      expected = c("field_id","s_er_soil","s_er_water","s_er_climate","s_er_biodiversity",
                   "s_er_landscape","s_er_costs","s_er_farm_tot","s_er_medal","s_er_reward","s_er_tot"))
  })
  
  test_that("check ecoregeling", {
    expect_equal(
      object = test$fields$s_er_farm_tot,
      expected = c(0,0,0),
      tolerance = 0.01)
  })
  
  test_that("check ecoregeling", {
    expect_equal(
      object = as.character(unlist(test$farm)),
      expected = c(0,0,0,0,0,0,0,'none',0,0),
      tolerance = 0.01)
  })

  # run example 1 with all soil types and without any measures taken
  test <- ecoregeling(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','duinzand',
                                         'zeeklei','maasklei','dalgrond','moerige_klei',
                                         'veen'),
                      B_GWL_CLASS = c('GtIII', 'GtI', 'GtV','GtVI','GtV','GtV','GtVI','GtIII','GtI'),
                      A_P_SG = c(0.4, 0.8, 1,15,20,24,36,28,5),
                      B_SLOPE_DEGREE = c(1.5,4,1.5,1,rep(1.25,4)),
                      B_AER_CBS = c('LG05','LG14','LG02','LG03','LG05','LG07','LG11','LG06','LG12'),
                      B_LU_BBWP = c('gras_permanent','gras_tijdelijk','rustgewas','rooivrucht','groenten',
                                    'bollensierteelt','boomfruitteelt','natuur','mais'),
                      B_LU_BRP = c(265,266,233,2741,672,174,212,662,259),
                      B_LU_ARABLE_ER = c(F,F,T,T,T,F,F,F,T),
                      B_LU_PRODUCTIVE_ER = c(T,T,T,T,T,T,T,F,T),
                      B_LU_CULTIVATED_ER = c(T,T,T,T,T,T,T,F,T),
                      M_DRAIN = c(T,F,T,T,T,T,T,T,F),
                      D_SA_W = c(0, 0.5, 1,rep(0.3,6)),
                      B_AREA = c(100,80,2.5,5,38,63,12,4,6),
                      farmscore = 100,
                      measures = NULL,
                      sector = c('dairy', 'arable'),
                      output = 'scores'
  )
  
  # run tests on format and output values
  test_that("check ecoregeling", {
    expect_equal(
      object = test$fields$s_er_tot,
      expected = rep(0,9),
      tolerance = 0.01)
  })
  
  # run tests on format and output values
  test_that("check ecoregeling", {
    expect_equal(
      object = test$farm$s_er_medal,
      expected = 'none')
  })

  # get internal table with measures
  dt.measures <- as.data.table(BBWPC::bbwp_measures)
  dt.measures <- dt.measures[!is.na(eco_id)]
  
  # make measurement list for 2 of the 4 fields
  measures <- rbind(data.table(id = 1, dt.measures[grepl('B189|G50|G3|B137|B172|G84',bbwp_id)]),
                    data.table(id = 3, dt.measures[grepl('B135|G84|B118|G58|B146',bbwp_id)]))
  measures$bbwp_status <- 'given for ANLB'
  


# run example 2 with any measures taken
test <- ecoregeling(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
                    B_GWL_CLASS = c('GtIII', 'GtI', 'GtV'),
                    A_P_SG = c(0.4, 0.8, 1),
                    B_SLOPE_DEGREE = c(1.5,4,1.5),
                    B_LU_BBWP = c('gras_permanent','rooivrucht','gras_permanent'),
                    B_LU_BRP = c(265,2741,259),
                    B_LU_ARABLE_ER = c(T,T,T),
                    B_LU_PRODUCTIVE_ER = c(T,T,T),
                    B_LU_CULTIVATED_ER = c(T,T,T),
                    B_AER_CBS = c('LG05','LG14','LG02'),
                    M_DRAIN = c(TRUE, FALSE, TRUE),
                    D_SA_W = c(0, 0.5, 1),
                    B_AREA = c(100,80,2.5),
                    farmscore = 100,
                    measures = measures,
                    sector = c('dairy', 'arable'),
                    output = 'scores'
)

  # run tests on format and output values
  test_that("check ecoregeling", {
    expect_equal(
      object = test$fields$s_er_farm_tot,
      expected = c(88,71,2),
      tolerance = 0.01)
  })
  
  test_that("check ecoregeling", {
    expect_equal(
      object = as.character(unlist(test$farm)),
      expected = c(15,15,15,15,1,100,50,'gold',175,100),
      tolerance = 0.01)
  })

# run example 3 with any measures taken
  test <- ecoregeling(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
                      B_GWL_CLASS = c('GtIII', 'GtI', 'GtV'),
                      A_P_SG = c(0.4, 0.8, 1),
                      B_SLOPE_DEGREE = c(1.5,4,1.5),
                      B_LU_BBWP = c('gras_permanent','rooivrucht','gras_permanent'),
                      B_LU_BRP = c(265,2741,259),
                      B_LU_ARABLE_ER = c(T,T,T),
                      B_LU_PRODUCTIVE_ER = c(T,T,T),
                      B_LU_CULTIVATED_ER = c(T,T,T),
                      B_AER_CBS = c('LG05','LG14','LG02'),
                      M_DRAIN = c(TRUE, FALSE, TRUE),
                      D_SA_W = c(0, 0.5, 1),
                      B_AREA = c(100,80,2.5),
                      farmscore = 100,
                      measures = measures,
                      sector = c('dairy', 'arable'),
                      output = 'measures'
  )
  
  # run tests on format and output values
  test_that("check ecoregeling", {
    expect_equal(
      object = names(test$measures[[1]]),
      expected = c("top_er_tot","top_er_soil","top_er_water","top_er_climate","top_er_biodiversity", "top_er_landscape","top_er_reward"))
  })
  
  test_that("check ecoregeling", {
    expect_equal(
      object = test$measures[[1]]$top_er_tot,
      expected = c("B166" ,"B162", "B132", "B144", "B139"))
  })
  
  