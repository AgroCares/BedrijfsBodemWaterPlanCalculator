
require(testthat)

  # default input for testing
  # B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen')
  # B_LU_BBWP = rep('gras_permanent',4)
  # B_LU_BRP = rep(265,4)
  # B_LU_ARABLE_ER = c(T,T,T,T)
  # B_LU_PRODUCTIVE_ER = c(T,T,T,T)
  # B_LU_CULTIVATED_ER = c(T,T,T,T)
  # B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland')
  # B_AREA = c(45000,18000,8000,60000)
  # measures = NULL
  # sector = 'dairy'

# run example 1 without any measures taken
test <- er_croprotation(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                        B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                        B_LU_BRP = c(265,2741,2741,259),
                        B_LU_ARABLE_ER = c(T,T,T,T),
                        B_LU_PRODUCTIVE_ER = c(T,T,T,T),
                        B_LU_CULTIVATED_ER = c(T,T,T,T),
                        B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland'),
                        B_AREA = c(450000,180000,8000,60000),
                        measures = NULL,
                        sector = 'dairy'
                        )

test_that("Test er_croprotation works without measures", {
  test <- er_croprotation(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                          B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                          B_LU_BRP = c(265,2741,2741,259),
                          B_LU_ARABLE_ER = c(T,T,T,T),
                          B_LU_PRODUCTIVE_ER = c(T,T,T,T),
                          B_LU_CULTIVATED_ER = c(T,T,T,T),
                          B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland'),
                          B_AREA = c(450000,180000,8000,60000),
                          measures = NULL,
                          sector = 'dairy'
  )
  
  expect_equal(
    object = dim(test),
    expected = c(1,8),
    tolerance = 0.01)
  
  expect_equal(
    object = colnames(test),
    expected = c('farmid','biodiversity','climate','landscape','soil','water','total','S_ER_REWARD'),
    tolerance = 0.01)
  
  expect_equal(
    object = as.numeric(test),
    expected = c(1,0,0,0,0,0,0,0),
    tolerance = 0.1,
    ignore_attr = TRUE)
})

test_that("Test er_croprotation works with field and farm level measures", {
  # get internal table with measures
  dt.measures <- as.data.table(BBWPC::bbwp_measures)
  dt.measures <- dt.measures[!is.na(eco_id)]
  
  # make measurement list for 2 of the 4 fields
  measures <- rbind(data.table(id = 1, dt.measures[grepl('B189|G50|G3|B137|B172|G84',bbwp_id)]),
                    data.table(id = 3, dt.measures[grepl('B135|G84|B118|G58|B146',bbwp_id)]))
  measures$bbwp_status <- 'given for ANLB'
  
  test <- er_croprotation(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                          B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                          B_LU_BRP = c(265,2741,2741,259),
                          B_LU_ARABLE_ER = c(T,T,T,T),
                          B_LU_PRODUCTIVE_ER = c(T,T,T,T),
                          B_LU_CULTIVATED_ER = c(T,T,T,T),
                          B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland'),
                          B_AREA = c(450000,180000,8000,60000),
                          measures = measures,
                          sector = 'dairy'
  )
  
  expect_equal(
    object = dim(test),
    expected = c(1,8),
    tolerance = 0.01)
  
  expect_equal(
    object = colnames(test),
    expected = c('farmid','biodiversity','climate','landscape','soil','water','total','S_ER_REWARD'),
    tolerance = 0.01)
  
  expect_equal(
    object = as.numeric(test),
    expected = c(1,1.61,7.4,2.7,0,12.5,24.3,580.23),
    tolerance = 0.1,
    ignore_attr = TRUE)
})



test_that("Test er_croprotation if only field measures are taken and insufficient crop diversification", {
  # add test when only measurements on field level are given
  dt.measures <- as.data.table(BBWPC::bbwp_measures)
  dt.measures <- dt.measures[!is.na(eco_id) & level=='field']
  
  # make measurement list for 2 of the 4 fields
  measures <- rbind(data.table(id = 1, dt.measures[grepl('B189|G50|G3|B137|B172|G84',bbwp_id)]),
                    data.table(id = 3, dt.measures[grepl('B135|G84|B118|G58|B146',bbwp_id)]))
  measures$bbwp_status <- 'given for ANLB'
  
  # run example 2 without any measures taken and insufficient crop diversificaiton
  test <- er_croprotation(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                          B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                          B_LU_BRP = c(265,2741,2741,259),
                          B_LU_ARABLE_ER = c(T,T,T,T),
                          B_LU_PRODUCTIVE_ER = c(T,T,T,T),
                          B_LU_CULTIVATED_ER = c(T,T,T,T),
                          B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland'),
                          B_AREA = c(450000,180000,8000,60000),
                          measures = measures,
                          sector = 'dairy'
  )
  
  expect_equal(
    object = as.numeric(test),
    expected = c(1,0,0,0,0,0,0,0),
    tolerance = 0.1,
    ignore_attr = TRUE)
})


test_that("Test er_croprotation with only field measures and sufficient crop diversification", {
  # add test when only measurements on field level are given
  dt.measures <- as.data.table(BBWPC::bbwp_measures)
  dt.measures <- dt.measures[!is.na(eco_id) & level=='field']
  
  # make measurement list for 2 of the 4 fields
  measures <- rbind(data.table(id = 1, dt.measures[grepl('B189|G50|G3|B137|B172|G84',bbwp_id)]),
                    data.table(id = 3, dt.measures[grepl('B135|G84|B118|G58|B146',bbwp_id)]))
  measures$bbwp_status <- 'given for ANLB'
  
  # run example 2 without any measures taken and insufficient crop diversificaiton
  test <- er_croprotation(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                          B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                          B_LU_BRP = c(265,2741,2785,259),
                          B_LU_ARABLE_ER = c(T,T,T,T),
                          B_LU_PRODUCTIVE_ER = c(T,T,T,T),
                          B_LU_CULTIVATED_ER = c(T,T,T,T),
                          B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland'),
                          B_AREA = c(450000,180000,8000,60000),
                          measures = measures,
                          sector = 'dairy'
  )
  
  expect_equal(
    object = as.numeric(test),
    expected = c(1,0.89,0,0.76,0.41,0.50,2.57,14.3),
    tolerance = 0.1,
    ignore_attr = TRUE)
})

test_that("Test er_croprotation with only farm measures", {
  # add test when only measurements on farm level are given
  dt.measures <- as.data.table(BBWPC::bbwp_measures)
  dt.measures <- dt.measures[!is.na(eco_id) & level=='farm']
  
  # make measurement list for 2 of the 4 fields
  measures <- rbind(data.table(id = 1, dt.measures[grepl('B190|B189|G50|G3|B137|B172|G84',bbwp_id)]),
                    data.table(id = 3, dt.measures[grepl('B190|B191|G84|B118|G58|B146',bbwp_id)]))
  measures$bbwp_status <- 'given for ANLB'
  
  # run example 2 without any measures taken and sufficient crop diversificaiton
  test <- er_croprotation(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                          B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                          B_LU_BRP = c(265,2741,2785,259),
                          B_LU_ARABLE_ER = c(T,T,T,T),
                          B_LU_PRODUCTIVE_ER = c(T,T,T,T),
                          B_LU_CULTIVATED_ER = c(T,T,T,T),
                          B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland'),
                          B_AREA = c(450000,180000,8000,60000),
                          measures = measures,
                          sector = 'dairy'
  )
  
  expect_equal(
    object = as.numeric(test),
    expected = c(1,2.5,2.02,0.76,0.41,4.98,10.67,580.2),
    tolerance = 0.1,
    ignore_attr = TRUE)
})

test_that("test er_croprotation with only farm measures except the better crop diversification measures", {
  # add test when only measurements on farm level are given
  dt.measures <- as.data.table(BBWPC::bbwp_measures)
  dt.measures <- dt.measures[!is.na(eco_id) & level=='farm']
  
  # make measurement list for 2 of the 4 fields
  measures <- rbind(data.table(id = 1, dt.measures[grepl('B189|G50|G3|B137|B172|G84',bbwp_id)]),
                    data.table(id = 3, dt.measures[grepl('B189|G84|B118|G58|B146',bbwp_id)]))
  measures$bbwp_status <- 'given for ANLB'
  
  # run example 2 without any measures taken and sufficient crop diversificaiton
  test <- er_croprotation(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                          B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                          B_LU_BRP = c(265,2741,2785,259),
                          B_LU_ARABLE_ER = c(T,T,T,T),
                          B_LU_PRODUCTIVE_ER = c(T,T,T,T),
                          B_LU_CULTIVATED_ER = c(T,T,T,T),
                          B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland'),
                          B_AREA = c(450000,180000,8000,60000),
                          measures = measures,
                          sector = 'dairy'
  )
  
  expect_equal(
    object = as.numeric(test),
    expected = c(1,2.5,2.02,0.76,0.41,4.98,10.67,580.2),
    tolerance = 0.1,
    ignore_attr = TRUE)
})



test_that("Test er_croprotation with only reduction of N fertiliser on one field", {
  # add test when only measurements on farm level are given
  dt.measures <- as.data.table(BBWPC::bbwp_measures)
  dt.measures <- dt.measures[!is.na(eco_id) & level=='farm']
  
  # make measurement list for 2 of the 4 fields
  measures <- rbind(data.table(id = 1, dt.measures[grepl('G50|G3|B137|B172|G84',bbwp_id)]),
                    data.table(id = 3, dt.measures[grepl('G84|B118|G58|B146',bbwp_id)]))
  measures$bbwp_status <- 'given for ANLB'
  
  # run example 2 without any measures taken and sufficient crop diversificaiton
  test <- er_croprotation(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                          B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                          B_LU_BRP = c(265,2741,2785,259),
                          B_LU_ARABLE_ER = c(T,T,T,T),
                          B_LU_PRODUCTIVE_ER = c(T,T,T,T),
                          B_LU_CULTIVATED_ER = c(T,T,T,T),
                          B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland'),
                          B_AREA = c(450000,180000,8000,60000),
                          measures = measures,
                          sector = 'dairy'
  )
  
  expect_equal(
    object = as.numeric(test),
    expected = c(1,2.5,2.02,0.76,0.41,4.98,10.67,580.2),
    tolerance = 0.1,
    ignore_attr = TRUE)
})
