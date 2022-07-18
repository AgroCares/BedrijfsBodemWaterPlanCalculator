require(testthat)

  # # default input for testing
  # B_SOILTYPE_AGR = c('dekzand', 'loess', 'rivierklei','veen')
  # B_LU_BRP = c(265,2005,256,259)
  # B_LU_BBWP = c(1,4,4,9)
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
                       B_LU_BBWP = c(1,4,4,9),
                       B_GWL_CLASS = 'GtIII', 
                       A_P_SG = rep(25,4) ,
                       B_SLOPE_DEGREE = rep(2.5,4),
                       B_AER_CBS = c('LG05','LG14','LG03','LG02'),
                       M_DRAIN = rep(TRUE,4),
                       D_SA_W = rep(0.5,4),
                       D_OPI_NGW = c(0,0.1, 0.5, 1), 
                       D_OPI_NSW = c(0,0.1, 0.5, 1), 
                       D_OPI_PSW = c(0,0.1, 0.5, 1), 
                       D_OPI_NUE = c(0,0.1, 0.5, 1),  
                       D_OPI_WB = c(0, 0.1,0.5, 1), 
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
    expected = c(NA,"G21","G66" ,"G87", "G20"),
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
                       B_LU_BBWP = c(1,4,4,9),
                       B_GWL_CLASS = c('GtIII', 'GtVII','GtII','GtIV'),
                       A_P_SG = rep(25,4) ,
                       B_SLOPE_DEGREE = rep(2.5,4),
                       B_AER_CBS = c('LG05','LG14','LG03','LG02'),
                       M_DRAIN = rep(TRUE,4),
                       D_SA_W = rep(0.5,4),
                       D_OPI_NGW = c(0.8,0.1, 0.5, 1), 
                       D_OPI_NSW = c(0.2,0.1, 0.5, 1), 
                       D_OPI_PSW = c(0.9,0.1, 0.5, 1), 
                       D_OPI_NUE = c(0.1,0.1, 0.5, 1),  
                       D_OPI_WB = c(0.22, 0.1,0.5, 1), 
                       measures = measures,
                       sector = 'dairy'
)

test_that("check bbwp_meas_rank", {
  expect_equal(
    object = dim(test),
    expected = c(20,7),
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
    expected = c("G66" , "G68",  "G20", "B118" , "G87"),
    tolerance = 0.01)
})


