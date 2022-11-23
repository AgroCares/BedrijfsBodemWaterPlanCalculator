# test er_pdf

# first an indirect test is done by repeating the test for crop_rotation but now with argument TRUE
# explicit tests for er_pdf need still to be made

  # run example for crop rotation
  test <- er_croprotation(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                          B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                          B_LU_BRP = c(265,2741,2741,259),
                          B_LU_ARABLE_ER = c(T,T,T,T),
                          B_LU_PRODUCTIVE_ER = c(T,T,T,T),
                          B_LU_CULTIVATED_ER = c(T,T,T,T),
                          B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland'),
                          B_AREA = c(450000,180000,8000,60000),
                          measures = NULL,
                          sector = 'dairy',
                          pdf = TRUE)
  
  test_that("check er_croprotation", {
    expect_equal(
      object = dim(test$pdf),
      expected = c(0,9),
      tolerance = 0.01)
  })
  
  test_that("check er_croprotation", {
    expect_equal(
      object = colnames(test$pdf),
      expected = c("level","summary","B_AREA_tot","climate","soil","water","landscape" ,"biodiversity" ,"total"),
      tolerance = 0.01)
  })
  
  # get internal table with measures
  dt.measures <- as.data.table(BBWPC::bbwp_measures)
  dt.measures <- dt.measures[!is.na(eco_id)]
  
  # make measurement list for 2 of the 4 fields
  measures <- rbind(data.table(id = 1, dt.measures[grepl('B189|G50|G3|B137|B172|G84',bbwp_id)]),
                    data.table(id = 3, dt.measures[grepl('B135|G84|B118|G58|B146',bbwp_id)]))
  measures$bbwp_status <- 'given for ANLB'
  
  # run example 2 without any measures taken
  test <- er_croprotation(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                          B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                          B_LU_BRP = c(265,2741,2741,259),
                          B_LU_ARABLE_ER = c(T,T,T,T),
                          B_LU_PRODUCTIVE_ER = c(T,T,T,T),
                          B_LU_CULTIVATED_ER = c(T,T,T,T),
                          B_AER_CBS = c('Bouwhoek en Hogeland','LG14','LG12','Westelijk Holland'),
                          B_AREA = c(450000,180000,8000,60000),
                          measures = measures,
                          sector = 'dairy',pdf=TRUE
  )
  
  
  test_that("check er_croprotation", {
    expect_equal(
      object = dim(test$pdf),
      expected = c(2,9),
      tolerance = 0.01)
  })
  
  test_that("check er_croprotation", {
    expect_equal(
      object = colnames(test$pdf),
      expected = c("level","summary","B_AREA_tot","climate","soil","water","landscape" ,"biodiversity" ,"total"),
      tolerance = 0.01)
  })
    
  test_that("check er_croprotation", {
    expect_equal(
      object = test$pdf$total,
      expected = c(0.2,0.4),
      tolerance = 0.01)
  })
  

# pdf <- er_pdf(croprotation = FALSE,
#               measurescores = FALSE,
#               dt.field.measures = dt2,
#               dt.farm.measures = dt4, 
#               B_AREA = B_AREA)