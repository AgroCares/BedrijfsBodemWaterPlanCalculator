require(testthat)

  # # default input for testing
  B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei')
  B_GWL_CLASS = c('GtIII', 'GtI', 'GtV')
  B_AER_CBS = c('LG05','LG14','LG02')
  A_P_SG = c(0.4, 0.8, 1)
  B_SLOPE_DEGREE = c(1.5,4,1.5)
  B_AER_CBS = c('LG05','LG14','LG02')
  B_LU_BBWP = c(1,1,1)
  B_LU_ECO1 = c(F,F,F)
  B_LU_ECO2 = c(F,F,F)
  B_LU_ECO3 = c(F,F,F)
  B_LU_ECO4 = c(F,F,F)
  B_LU_ECO5 = c(T,T,T)
  B_LU_ECO6 = c(F,F,F)  
  B_LU_ECO7 = c(F,F,F)
  B_LU_ECO8 = c(T,T,T)
  B_LU_ECO9 = c(T,T,T)
  B_LU_ECO10 = c(T,T,T)
  M_DRAIN = c(TRUE, FALSE, TRUE)
  D_SA_W = c(0, 0.5, 1)
  B_AREA = c(100,80,2.5)
  measures = NULL
  farmscore = 100
  sector = c('dairy', 'arable')
  output = 'scores'
  medalscore = 'gold'

# run example 1 without any measures taken
test <- ecoregeling(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
                    B_GWL_CLASS = c('GtIII', 'GtI', 'GtV'),
                    A_P_SG = c(0.4, 0.8, 1),
                    B_SLOPE_DEGREE = c(1.5,4,1.5),
                    B_AER_CBS = c('LG05','LG14','LG02'),
                    B_LU_BBWP = c(1,4,1),
                    B_LU_ECO1 = c(F,F,F),
                    B_LU_ECO2 = c(F,F,F),
                    B_LU_ECO3 = c(F,F,F),
                    B_LU_ECO4 = c(F,F,F),
                    B_LU_ECO5 = c(T,T,T),
                    B_LU_ECO6 = c(F,F,F), 
                    B_LU_ECO7 = c(F,F,F),
                    B_LU_ECO8 = c(T,T,T),
                    B_LU_ECO9 = c(T,T,T),
                    B_LU_ECO10 = c(T,T,T),
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
      expected = c('farm','fields'))
  })
  
  test_that("check ecoregeling", {
    expect_equal(
      object = colnames(test$fields),
      expected = c("field_id","s_er_soil","s_er_water","s_er_climate","s_er_biodiversity",
                   "s_er_landscape","s_er_tot","s_er_reward","s_er_medal"))
  })
  
  test_that("check ecoregeling", {
    expect_equal(
      object = test$fields$s_er_tot,
      expected = c(9,9,9),
      tolerance = 0.01)
  })
  
  test_that("check ecoregeling", {
    expect_equal(
      object = as.character(unlist(test$farm)),
      expected = c(11,11,6,13,4,9,0,'none'),
      tolerance = 0.01)
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
                    B_LU_BBWP = c(1,4,1),
                    B_LU_ECO1 = c(F,F,F),
                    B_LU_ECO2 = c(F,F,F),
                    B_LU_ECO3 = c(F,F,F),
                    B_LU_ECO4 = c(F,F,F),
                    B_LU_ECO5 = c(T,T,T),
                    B_LU_ECO6 = c(F,F,F), 
                    B_LU_ECO7 = c(F,F,F),
                    B_LU_ECO8 = c(T,T,T),
                    B_LU_ECO9 = c(T,T,T),
                    B_LU_ECO10 = c(T,T,T),
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
      object = test$fields$s_er_tot,
      expected = c(100,100,100),
      tolerance = 0.01)
  })
  
  test_that("check ecoregeling", {
    expect_equal(
      object = as.character(unlist(test$farm)),
      expected = c(100,100,100,100,100,100,90,'bronze'),
      tolerance = 0.01)
  })

# run example 3 with any measures taken
  test <- ecoregeling(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
                      B_GWL_CLASS = c('GtIII', 'GtI', 'GtV'),
                      A_P_SG = c(0.4, 0.8, 1),
                      B_SLOPE_DEGREE = c(1.5,4,1.5),
                      B_LU_BBWP = c(1,4,1),
                      B_LU_ECO1 = c(F,F,F),
                      B_LU_ECO2 = c(F,F,F),
                      B_LU_ECO3 = c(F,F,F),
                      B_LU_ECO4 = c(F,F,F),
                      B_LU_ECO5 = c(T,T,T),
                      B_LU_ECO6 = c(F,F,F), 
                      B_LU_ECO7 = c(F,F,F),
                      B_LU_ECO8 = c(T,T,T),
                      B_LU_ECO9 = c(T,T,T),
                      B_LU_ECO10 = c(T,T,T),
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
      expected = c("B155" ,"B126", "B132", "B175", "B139"))
  })
  
  