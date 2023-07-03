require(testthat)

  # # default input for testing
  # B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen')
  # B_LU_BRP = c(265,2005,256,259)
  # B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais')
  # B_GWL_CLASS = 'GtIII' 
  # A_P_SG = rep(25,4) 
  # B_SLOPE_DEGREE = rep(2.5,4)
  # M_DRAIN = rep(TRUE,4)
  # D_SA_W = rep(0.5,4)
  # D_OPI_NGW = c(0,0.1, 0.5, 1)
  # D_OPI_NSW = c(0,0.1, 0.5, 1)
  # D_OPI_PSW = c(0,0.1, 0.5, 1) 
  # D_OPI_NUE = c(0,0.1, 0.5, 1)  
  # D_OPI_WB = c(0, 0.1,0.5, 1) 
  # measures = NULL
  # sector = 'dairy'

# run example 1 without any measures taken
test <- bbwp_meas_rank(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                       B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                       B_GWL_CLASS = 'GtIII', 
                       A_P_SG = rep(25,4) ,
                       B_SLOPE_DEGREE = rep(2.5,4),
                       B_AER_CBS = c('LG05','LG14','LG03','LG02'),
                       M_DRAIN = rep(TRUE,4),
                       D_SA_W = rep(0.5,4),
                       S_BBWP_NGW  = c(0,0.1, 0.5, 1)*100, 
                       S_BBWP_NSW = c(0,0.1, 0.5, 1)*100, 
                       S_BBWP_PSW = c(0,0.1, 0.5, 1)*100, 
                       S_BBWP_NUE = c(0,0.1, 0.5, 1)*100,  
                       S_BBWP_WB = c(0, 0.1,0.5, 1)*100, 
                       measures = NULL,
                       sector = 'dairy'
                       )

test_that("check bbwp_meas_rank", {
  expect_equal(
    object = dim(test),
    expected = c(16,7),
    tolerance = 0.01)
})

test_that("check bbwp_meas_rank", {
  expect_equal(
    object = colnames(test),
    expected = c('id',"top_bbwp_tot","top_bbwp_ngw","top_bbwp_nsw","top_bbwp_psw","top_bbwp_wb","top_bbwp_nue"),
    tolerance = 0.01)
})

test_that("check bbwp_meas_rank", {
  expect_equal(
    object = test$top_bbwp_tot[c(1,2,7,12,15)],
    expected = c('G53',"G27",'G21','G27','G17'),
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
test <- bbwp_meas_rank(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                       B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                       B_GWL_CLASS = c('GtIII', 'GtVII','GtII','GtIV'),
                       A_P_SG = rep(25,4) ,
                       B_SLOPE_DEGREE = rep(2.5,4),
                       B_AER_CBS = c('LG05','LG14','LG03','LG02'),
                       M_DRAIN = rep(TRUE,4),
                       D_SA_W = rep(0.5,4),
                       S_BBWP_NGW = c(0.8,0.1, 0.5, 1)*100, 
                       S_BBWP_NSW = c(0.2,0.1, 0.5, 1)*100, 
                       S_BBWP_PSW = c(0.9,0.1, 0.5, 1)*100, 
                       S_BBWP_NUE = c(0.1,0.1, 0.5, 1)*100,  
                       S_BBWP_WB = c(0.22, 0.1,0.5, 1)*100, 
                       measures = measures,
                       sector = 'dairy'
)

test_that("check bbwp_meas_rank", {
  expect_equal(
    object = dim(test),
    expected = c(16,7),
    tolerance = 0.01)
})

test_that("check bbwp_meas_rank", {
  expect_equal(
    object = colnames(test),
    expected = c('id',"top_bbwp_tot","top_bbwp_ngw","top_bbwp_nsw","top_bbwp_psw","top_bbwp_wb","top_bbwp_nue"),
    tolerance = 0.01)
})

test_that("check bbwp_meas_rank", {
  expect_equal(
    object = test$top_bbwp_tot[c(1,2,5,9,16)],
    expected = c("G53" , "G8BWP4",  "G11aBWP3", "G54" , NA),
    tolerance = 0.01)
})

# run example without any measures taken and a high risk for N and P loss to surface water
test <- bbwp_meas_rank(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                       B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                       B_GWL_CLASS = 'GtIII', 
                       A_P_SG = rep(55,4) ,
                       B_SLOPE_DEGREE = rep(2.5,4),
                       B_AER_CBS = c('LG05','LG14','LG03','LG02'),
                       M_DRAIN = rep(TRUE,4),
                       D_SA_W = rep(.9,4),
                       S_BBWP_NGW  = c(99,99,99,99), 
                       S_BBWP_NSW = c(10,10,10,10),  
                       S_BBWP_PSW = c(10,10,10,10),  
                       S_BBWP_NUE = c(99,99,99,99),   
                       S_BBWP_WB = c(99,99,99,99), 
                       measures = NULL,
                       sector = 'dairy'
)

test_that("check bbwp_meas_rank", {
  expect_equal(
    object = test[id==1,top_bbwp_tot],
    expected = c("G19" , "G17",  "G16", "G80" , 'G68'),
    tolerance = 0.01)
})

# run example without any measures taken and a high risk for N leaching
test <- bbwp_meas_rank(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                       B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                       B_GWL_CLASS = 'GtIII', 
                       A_P_SG = rep(55,4) ,
                       B_SLOPE_DEGREE = rep(2.5,4),
                       B_AER_CBS = c('LG05','LG14','LG03','LG02'),
                       M_DRAIN = rep(TRUE,4),
                       D_SA_W = rep(.9,4),
                       S_BBWP_NGW  = c(10,10,10,10), 
                       S_BBWP_NSW = c(99,99,99,99),  
                       S_BBWP_PSW = c(99,99,99,99),  
                       S_BBWP_NUE = c(99,99,99,99),   
                       S_BBWP_WB = c(99,99,99,99), 
                       measures = NULL,
                       sector = 'dairy'
)

test_that("check bbwp_meas_rank", {
  expect_equal(
    object = test[id==1,top_bbwp_tot],
    expected = c('B131','G65','G27','G25','G12BWP10'),
    tolerance = 0.01)
})


# run example without any measures taken and a high risk for water retention and supply
test <- bbwp_meas_rank(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                       B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                       B_GWL_CLASS = 'GtIII', 
                       A_P_SG = rep(55,4) ,
                       B_SLOPE_DEGREE = rep(2.5,4),
                       B_AER_CBS = c('LG05','LG14','LG03','LG02'),
                       M_DRAIN = rep(TRUE,4),
                       D_SA_W = rep(.9,4),
                       S_BBWP_NGW  = c(99,99,99,99), 
                       S_BBWP_NSW = c(99,99,99,99),  
                       S_BBWP_PSW = c(99,99,99,99),  
                       S_BBWP_NUE = c(99,99,99,99),   
                       S_BBWP_WB = c(10,10,10,10), 
                       measures = NULL,
                       sector = 'dairy'
)

test_that("check bbwp_meas_rank", {
  expect_equal(
    object = test[id==1,top_bbwp_tot],
    expected = c("G8BWP4","G6BWP1","G12BWP10", "G53","B112"),
    tolerance = 0.01)
})



# run example without any measures taken and a high risk for low nutrient use efficiency
test <- bbwp_meas_rank(B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen'),
                       B_LU_BBWP = c('gras_permanent','rooivrucht','rooivrucht','mais'),
                       B_GWL_CLASS = 'GtIII', 
                       A_P_SG = rep(55,4) ,
                       B_SLOPE_DEGREE = rep(2.5,4),
                       B_AER_CBS = c('LG05','LG14','LG03','LG02'),
                       M_DRAIN = rep(TRUE,4),
                       D_SA_W = rep(.9,4),
                       S_BBWP_NGW  = c(99,99,99,99), 
                       S_BBWP_NSW = c(99,99,99,99),  
                       S_BBWP_PSW = c(99,99,99,99),  
                       S_BBWP_NUE = c(10,10,10,10),   
                       S_BBWP_WB = c(99,99,99,99), 
                       measures = NULL,
                       sector = 'dairy'
)

test_that("check bbwp_meas_rank", {
  expect_equal(
    object = test[id==1,top_bbwp_tot],
    expected = c("G36","G53","G84", "B131","G25"),
    tolerance = 0.01)
})



#merge(test[id==1],BBWPC::bbwp_measures[,.(bbwp_id,summary)],by.x='top_bbwp_tot',by.y='bbwp_id')
