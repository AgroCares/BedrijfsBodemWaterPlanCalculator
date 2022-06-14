require(testthat)

  # # default input for testing
  # B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei')
  # B_GWL_CLASS = c('GtIII', 'GtI', 'GtV')
  # B_AER_CBS = c('LG05','LG14','LG02')
  # A_P_SG = c(0.4, 0.8, 1)
  # B_SLOPE_DEGREE = c(1.5,4,1.5)
  # B_AER_CBS = c('LG05','LG14','LG02')
  # B_LU_BRP = c(265, 1932, 266)
  # B_LU_BBWP = c(1,4,1)
  # M_DRAIN = c(TRUE, FALSE, TRUE)
  # D_SA_W = c(0, 0.5, 1)
  # D_AREA = c(100,80,2.5)
  # measures = NULL
  # farmscore = 100
  # sector = c('dairy', 'arable')
  # output = 'scores'

# run example 1 without any measures taken
test <- ecoregeling(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
                    B_GWL_CLASS = c('GtIII', 'GtI', 'GtV'),
                    A_P_SG = c(0.4, 0.8, 1),
                    B_SLOPE_DEGREE = c(1.5,4,1.5),
                    B_AER_CBS = c('LG05','LG14','LG02'),
                    B_LU_BRP = c(265, 1932, 266),
                    B_LU_BBWP = c(1,4,1),
                    M_DRAIN = c(TRUE, FALSE, TRUE),
                    D_SA_W = c(0, 0.5, 1),
                    D_AREA = c(100,80,2.5),
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
      expected = c("field_id","s_er_soil","s_er_water","s_er_climate","s_er_biodiversity","s_er_landscape","s_er_tot","reward"))
  })
  
  test_that("check ecoregeling", {
    expect_equal(
      object = test$fields$s_er_tot,
      expected = c(6,6,6),
      tolerance = 0.01)
  })
  
  test_that("check ecoregeling", {
    expect_equal(
      object = as.character(unlist(test$farm)),
      expected = c(6,9,6,11,6,0,460,'silver'),
      tolerance = 0.01)
  })


# get internal table with measures
dt.measures <- as.data.table(BBWPC::bbwp_measures)
dt.measures <- dt.measures[!is.na(eco_id)]

# make measurement list for 2 of the 4 fields
measures <- rbind(data.table(id = 1, dt.measures[c(2,5,18,28,32,3,38,43,58)]),
                  data.table(id = 3, dt.measures[c(7,21,30,46,5)]))


# run example 2 with any measures taken
test <- ecoregeling(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
                    B_GWL_CLASS = c('GtIII', 'GtI', 'GtV'),
                    A_P_SG = c(0.4, 0.8, 1),
                    B_SLOPE_DEGREE = c(1.5,4,1.5),
                    B_LU_BRP = c(265, 1932, 266),
                    B_LU_BBWP = c(1,4,1),
                    B_AER_CBS = c('LG05','LG14','LG02'),
                    M_DRAIN = c(TRUE, FALSE, TRUE),
                    D_SA_W = c(0, 0.5, 1),
                    D_AREA = c(100,80,2.5),
                    farmscore = 100,
                    measures = measures,
                    sector = c('dairy', 'arable'),
                    output = 'scores'
)

  # run tests on format and output values
  test_that("check ecoregeling", {
    expect_equal(
      object = test$fields$s_er_tot,
      expected = c(36,6,40),
      tolerance = 0.01)
  })
  
  test_that("check ecoregeling", {
    expect_equal(
      object = as.character(unlist(test$farm)),
      expected = c(23,23,27,51,16,15,802,'silver'),
      tolerance = 0.01)
  })

# run example 3 with any measures taken
  test <- ecoregeling(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei'),
                      B_GWL_CLASS = c('GtIII', 'GtI', 'GtV'),
                      A_P_SG = c(0.4, 0.8, 1),
                      B_SLOPE_DEGREE = c(1.5,4,1.5),
                      B_LU_BRP = c(265, 1932, 266),
                      B_LU_BBWP = c(1,4,1),
                      B_AER_CBS = c('LG05','LG14','LG02'),
                      M_DRAIN = c(TRUE, FALSE, TRUE),
                      D_SA_W = c(0, 0.5, 1),
                      D_AREA = c(100,80,2.5),
                      farmscore = 100,
                      measures = measures,
                      sector = c('dairy', 'arable'),
                      output = 'measures'
  )
  
  # run tests on format and output values
  test_that("check ecoregeling", {
    expect_equal(
      object = names(test$measures[[1]]),
      expected = c("top_er_tot","top_er_soil","top_er_water","top_er_climate","top_er_biodiversity", "top_er_landscape"))
  })
  
  test_that("check ecoregeling", {
    expect_equal(
      object = test$measures[[1]]$top_er_tot,
      expected = c('B156','B133','G50'))
  })
  
  