
# default inputs for testing
    
  B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen')
  B_AREA = c(45,18,0.8,6)
  S_ER_SOIL = c(75,85,100,100)
  S_ER_WATER = c(75,85,100,100)
  S_ER_CLIMATE = c(75,85,100,100)
  S_ER_BIODIVERSITY = c(75,85,100,100)
  S_ER_LANDSCAPE = c(75,85,100,100)
  S_ER_TOT = c(75,85,100,100)
  S_ER_REWARD = c(125,45,0,25)

# run example without any measures taken  

  # run test example
  test <- er_medal(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                   B_AREA = c(45,18,0.8,6),
                   S_ER_SOIL = c(75,85,100,100),
                   S_ER_WATER = c(75,85,100,100),
                   S_ER_CLIMATE = c(75,85,100,100),
                   S_ER_BIODIVERSITY = c(75,85,100,100),
                   S_ER_LANDSCAPE = c(75,85,100,100),
                   S_ER_TOT = c(75,85,100,100),
                   S_ER_REWARD = c(125,45,0,25))
  
test_that("check er_medal", {
  expect_equal(
    names(test),
    expected = c('field','farm'),
    tolerance = 0.01)
})

test_that("check er_medal", {
  expect_equal(
    test$field$medal,
    expected = c('silver',rep('none',3)),
    tolerance = 0.01)
})

test_that("check er_medal", {
  expect_equal(
    test$farm,
    expected = c('bronze'),
    tolerance = 0.01)
})


