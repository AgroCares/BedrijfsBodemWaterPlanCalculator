
  # #default inputs for testing
  # S_ER_TOT = seq(0, 10, 2.5)
  # S_ER_SOIL = seq(0, 10, 2.5)
  # S_ER_WATER = seq(0, 10, 2.5)
  # S_ER_CLIMATE = seq(0, 10, 2.5)
  # S_ER_BIODIVERSITY = seq(0, 10, 2.5)
  # S_ER_LANDSCAPE = seq(0, 10, 2.5)
  # S_ER_REWARD = c(250,4650,213,15,680)
  # B_AREA = 1
  # B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen','veen')
  # sector = "dairy"
  # B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland','LG14')


# run example without any measures taken  

test_that("check er_farm_score", {
  expect_equal(
    er_farm_score(
      S_ER_TOT = seq(0, 10, 2.5),
      S_ER_SOIL = seq(0, 10, 2.5),
      S_ER_WATER = seq(0, 10, 2.5),
      S_ER_CLIMATE = seq(0, 10, 2.5),
      S_ER_BIODIVERSITY = seq(0, 10, 2.5),
      S_ER_LANDSCAPE = seq(0, 10, 2.5),
      S_ER_REWARD = c(250,4650,213,15,680),
      B_AREA = rep(1,5),
      B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen','veen'),
      sector = "dairy",
      B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland','LG14'),
      measures = NULL
    ),
    expected = data.table(
      S_ER_SOIL = 25,
      S_ER_WATER = 25,
      S_ER_CLIMATE = 25,
      S_ER_BIODIVERSITY = 25,
      S_ER_LANDSCAPE = 25,
      S_ER_TOT = 25,
      S_ER_REWARD = 5808
    ),
    tolerance = 0.01)
})


  # run example with measures taken 

  # #default inputs for testing
  # S_ER_TOT = seq(0, 100, 25)
  # S_ER_SOIL = seq(0, 10, 2.5)
  # S_ER_WATER = seq(0, 100, 25)
  # S_ER_CLIMATE = seq(0, 10, 2.5)
  # S_ER_BIODIVERSITY = seq(0, 10, 2.5)
  # S_ER_LANDSCAPE = seq(0, 10, 2.5)
  # S_ER_REWARD = c(250,4650,213,15,680)
  # B_AREA = c(15, 0.8, 45, 95, 12)
  # B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen','veen')
  # sector = "dairy"
  # B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland','LG14')

  # get internal table with measures
  dt.measures <- as.data.table(BBWPC::bbwp_measures)
  dt.measures <- dt.measures[!is.na(eco_id)]
  
  # make measurement list for 2 of the 4 fields
  measures <- rbind(data.table(id = 1, dt.measures[c(2,5,18,28,32,3,38,43,62)]),
                    data.table(id = 3, dt.measures[c(7,21,30,46,5)]))
  

test_that("check er_farm_score", {
  expect_equal(
    er_farm_score(
      S_ER_TOT = seq(0, 100, 25),
      S_ER_SOIL = seq(0, 10, 2.5),
      S_ER_WATER = seq(0, 100, 25),
      S_ER_CLIMATE = seq(0, 10, 2.5),
      S_ER_BIODIVERSITY = seq(0, 10, 2.5),
      S_ER_LANDSCAPE = seq(0, 10, 2.5),
      S_ER_REWARD = c(250,4650,213,15,680),
      B_AREA = c(15,0.8,45,95,12),
      B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen','veen'),
      sector = "dairy",
      B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland','LG14'),
      measures = measures
    ),
    expected = data.table(
      S_ER_SOIL = 25,
      S_ER_WATER = 253,
      S_ER_CLIMATE = 25,
      S_ER_BIODIVERSITY = 26,
      S_ER_LANDSCAPE = 25,
      S_ER_TOT = 34,
      S_ER_REWARD = 6796
    ),
    tolerance = 0.01)
})

