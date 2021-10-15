# test functions for conversion funs of AgNuRe
test_that("estimation kgetal (ag_con_kg) works", {
  expect_equal(
    ag_con_kg(B_SOILTYPE_AGR = 'dekzand',
               A_SOM_LOI = 5, 
               A_CLAY_MI = 15, 
               A_PH_CC = 5.5, 
               A_CEC_CO = 125, 
               A_K_CC = 45, 
               A_K_CO_PO = 12),
    expected = 49,
    tolerance = 0.1)
})

test_that("estimation kgetal (ag_con_kg) works", { 
  expect_equal(
    ag_con_kg(B_SOILTYPE_AGR = c('dekzand','duinzand','loess','rivierklei','zeeklei','dalgrond'),
              A_SOM_LOI = rep(5,6), 
              A_CLAY_MI = seq(0,15,length.out = 6), 
              A_PH_CC = rep(4.5,6), 
              A_CEC_CO = rep(125,6), 
              A_K_CC = seq(20,60,length.out = 6), 
              A_K_CO_PO = seq(6,18,length.out = 6)),
    expected = c(23,64,64,113,62,74),
    tolerance = 0.1)
})

# test functions for conversion funs of AgNuRe
test_that("estimation pH-kCl2 (ag_con_phk) works", {
  expect_equal(
    ag_con_phk(A_PH_CC = seq(4,9,0.5)),
    expected = c(3.7,4.3,4.8,5.4,5.9,6.4,7,7.5,8,8.6,9.1),
    tolerance = 0.1)
})

test_that("ag_con_bd works", {
  expect_equal(
    ag_con_bd(
      A_SOM_LOI = 2,
      B_SOILTYPE_AGR = "zeeklei"
    ),
    expected = 1230,
    tolerance = 1
  )
  expect_equal(
    ag_con_bd(
      A_SOM_LOI = c(0.5, 3, 6, 8, 7, 5, 15, 25, 6),
      B_SOILTYPE_AGR = c("duinzand", "dekzand", "zeeklei", "rivierklei", "maasklei", "dalgrond", "moerige_klei", "veen", "loess")
    ),
    expected = c(1500, 1370, 1074, 1020, 1045, 1281, 912, 840, 124),
    tolerance = 1
  )
})


test_that("ag_con_limefactor works", {
  expect_equal(
    ag_con_limefactor(
      A_SOM_LOI = 2,
      B_SOILTYPE_AGR = "zeeklei",
      A_CLAY_MI = 15,
      D_BDS = 1200
    ),
    expected = 102,
    tolerance = 1
  )
  expect_equal(
    ag_con_limefactor(
      A_SOM_LOI =seq(1,15, length.out = 9),
      B_SOILTYPE_AGR =c("duinzand", "dekzand", "zeeklei", "rivierklei", "maasklei", "dalgrond", "moerige_klei", "veen", "loess"),
      A_CLAY_MI = seq(1,15, length.out = 9),
      D_BDS = rep(1200,9)
    ),
    expected = c(46,81,83,118,156,187,212,226,302),
    tolerance = 1
  )
})


test_that("ag_con_tpH_bld_s works", {
  expect_equal(
    ag_con_tpH_bld_s(
      B_LU_BRP = 2014,
      A_SOM_LOI = 2.5,
      B_SOILTYPE_AGR = "dekzand",
      D_CP_POTATO = 0.2, 
      D_CP_STARCH = 0.2, 
      D_CP_SUGARBEET = .01
    ),
    expected = 5.8,
    tolerance = 0.1
  )
  expect_equal(
    ag_con_tpH_bld_s(
      B_LU_BRP = rep(2014,3),
      A_SOM_LOI = rep(2.5,3),
      B_SOILTYPE_AGR = c("dekzand","veen","dalgrond"),
      D_CP_POTATO = c(0.1,0.2,0.3), 
      D_CP_STARCH = c(0,0.1,0.2), 
      D_CP_SUGARBEET =  rep(0.1,3)
    ),
    expected = c(5.4,5.4,5.8),
    tolerance = .1
  )
})

test_that("ag_con_tpH_bld_cl works", {
  expect_equal(
    ag_con_tpH_bld_cl(
      B_LU_BRP = 2014,
      A_SOM_LOI = 2.5,
      B_SOILTYPE_AGR = "rivierklei",
      A_CLAY_MI = 25
    ),
    expected = 6.5,
    tolerance = 0.1
  )
  expect_equal(
    ag_con_tpH_bld_cl(
      B_LU_BRP = rep(2014,7),
      A_SOM_LOI = rep(2.5,7),
      B_SOILTYPE_AGR = c("duinzand", "zeeklei", "rivierklei", "maasklei", "moerige_klei", "zeeklei", "loess"),
     A_CLAY_MI = rep(3,7)
    ),
    expected = c(6.3,6.3,6.1,6.1,6.3,6.3,6.4),
    tolerance = .1
  )
})

test_that("ag_con_tpH_sms works", {
  expect_equal(
    ag_con_tpH_sms(
      B_LU_BRP = 259,
      A_SOM_LOI = 2.5,
      B_SOILTYPE_AGR = "rivierklei",
      A_CLAY_MI = 25
    ),
    expected = 6.4,
    tolerance = 0.1
  )
  expect_equal(
    ag_con_tpH_sms(
      B_LU_BRP = rep(259,7),
      A_SOM_LOI = rep(2.5,7),
      B_SOILTYPE_AGR = c("duinzand", "zeeklei", "rivierklei", "maasklei", "moerige_klei", "zeeklei", "loess"),
      A_CLAY_MI = rep(3,7)
    ),
    expected = c(6.2,6.2,6,6,6.2,6.2,6.3),
    tolerance = .1
  )
})

test_that("ag_con_tpH_gld works", {
  expect_equal(
    ag_con_tpH_gld(
      B_LU_BRP = 265,
      B_SOILTYPE_AGR = "rivierklei"
    ),
    expected = 5,
    tolerance = 0.1
  )
  expect_equal(
    ag_con_tpH_gld(
      B_LU_BRP = rep(265,7),
      B_SOILTYPE_AGR = c("duinzand", "zeeklei", "rivierklei", "maasklei", "moerige_klei", "zeeklei", "loess")
    ),
    expected = rep(5,7),
    tolerance = .1
  )
})

test_that("ag_con_tpH_bld works", {
  expect_equal(
    ag_con_tpH_bld(
      B_LU_BRP = 2014,
      A_SOM_LOI = 2.5,
      B_SOILTYPE_AGR = "dekzand",
      D_CP_POTATO = 0.2, 
      D_CP_STARCH = 0.2, 
      D_CP_SUGARBEET = .01,
      A_CLAY_MI = 2.5
    ),
    expected = 5.8,
    tolerance = 0.1
  )
  expect_equal(
    ag_con_tpH_bld(
      B_LU_BRP = rep(2014,6),
      A_SOM_LOI = rep(2.5,6),
      B_SOILTYPE_AGR = c("dekzand","veen","dalgrond","rivierklei","zeeklei","duinzand"),
      D_CP_POTATO = c(0.1,0.2,0.6), 
      D_CP_STARCH = c(0,0.1,0.2,0.2,0.1,0), 
      D_CP_SUGARBEET =  rep(0.1,6),
      A_CLAY_MI = rep(5,6)
    ),
    expected = c(5.4,5.4,5.7,6.1,6.3,6.3),
    tolerance = .1
  )
})

test_that("ag_con_tpH_bol works", {
  expect_equal(
    ag_con_tpH_bol(
      B_LU_BRP = 176,
      A_SOM_LOI = 2.5,
      B_SOILTYPE_AGR = "rivierklei",
      A_CLAY_MI = 25
    ),
    expected = 6.4,
    tolerance = 0.1
  )
  expect_equal(
    ag_con_tpH_bol(
      B_LU_BRP = rep(176,7),
      A_SOM_LOI = rep(2.5,7),
      B_SOILTYPE_AGR = c("duinzand", "zeeklei", "rivierklei", "maasklei", "moerige_klei", "zeeklei", "loess"),
      A_CLAY_MI = rep(3,7)
    ),
    expected = c(6.6,6.6,6,6,6.6,6.6,6.3),
    tolerance = .1
  )
})

test_that("ag_con_tpH_bkg works", {
  expect_equal(
    ag_con_tpH_bkg(
      B_LU_BRP = 229,
      A_SOM_LOI = 2.5,
      B_SOILTYPE_AGR = "rivierklei"
    ),
    expected = 5.5,
    tolerance = 0.1
  )
  expect_equal(
    ag_con_tpH_bkg(
      B_LU_BRP = rep(229,7),
      A_SOM_LOI = rep(2.5,7),
      B_SOILTYPE_AGR = c("duinzand", "zeeklei", "rivierklei", "maasklei", "moerige_klei", "zeeklei", "loess")
    ),
    expected = rep(5.5,7),
    tolerance = .1
  )
})


test_that("ag_con_tpH works", {
  expect_equal(
    ag_con_tpH(
      B_LU_BRP = 2014,
      A_SOM_LOI = 2.5,
      B_SOILTYPE_AGR = "dekzand",
      D_CP_POTATO = 0.2, 
      D_CP_STARCH = 0.2, 
      D_CP_SUGARBEET = .01,
      A_CLAY_MI = 2.5
    ),
    expected = 5.8,
    tolerance = 0.1
  )
  expect_equal(
    ag_con_tpH(
      B_LU_BRP = c(2014,259,265,2014,259,265),
      A_SOM_LOI = rep(2.5,6),
      B_SOILTYPE_AGR = c("dekzand","veen","dalgrond","rivierklei","zeeklei","duinzand"),
      D_CP_POTATO = c(0.1,0.2,0.6), 
      D_CP_STARCH = c(0,0.1,0.2,0.2,0.1,0), 
      D_CP_SUGARBEET =  rep(0.1,6),
      A_CLAY_MI = rep(5,6)
    ),
    expected = c(5.4,5.3,5,6.1,6.2,5),
    tolerance = .1
  )
})

test_that("ag_con_cuhno3 works", {
  expect_equal(
    ag_con_cuhno3(
      A_SOM_LOI = 2.5,
      A_CLAY_MI = 2.5,
      A_K_CC = 35,
      A_CU_CC = 150
    ),
    expected = 7.5,
    tolerance = 0.1
  )
  expect_equal(
    ag_con_cuhno3(
      A_SOM_LOI = rep(2.5,10),
      A_CLAY_MI = c(rep(2.5,5),rep(15,5)),
      A_K_CC = seq(1,600,length.out = 10),
      A_CU_CC = seq(0.1,1000,length.out = 10)
    ),
    expected = c(3.94,6.48,6.54,6.57,6.6,8.99,9.3,9.56,9.8,10.01),
    tolerance = .1
  )
})

test_that("ag_con_mnaah works", {
  expect_equal(
    ag_con_mnaah(
     A_PH_KCL = 6.5,
     A_MN_CC = 20000
    ),
    expected = 34630,
    tolerance = 0.1
  )
  expect_equal(
    ag_con_mnaah(
      A_PH_KCL = seq(3,8,length.out = 10),
      A_MN_CC = seq(0.1, 10000,length.out = 10)
    ),
    expected = c(2.6,4.28,7.88,16.77,41.7,123.16,435.76,1869,9836,64123),
    tolerance = .1
  )
})

test_that("ag_con_mgnacl works", {
  expect_equal(
    ag_con_mgnacl(
      A_MG_CC = 400
    ),
    expected = 788,
    tolerance = 0.1
  )
  expect_equal(
    ag_con_mgnacl(
      A_MG_CC = seq(1, 1100,length.out = 10)
    ),
    expected = c(0,238,481,723,966,1208,1451,1694,1936,2179),
    tolerance = .1
  )
})

test_that("ag_con_bhw works", {
  expect_equal(
    ag_con_bhw(
      A_SOM_LOI = 2.5,
      B_SOILTYPE_AGR = "dekzand",
     A_PH_CC = 5.5,
     A_B_CC = 500
    ),
    expected = 0.86,
    tolerance = 0.01
  )
  expect_equal(
    ag_con_bhw(
      A_SOM_LOI = rep(2.5,6),
      B_SOILTYPE_AGR = c("dekzand","veen","dalgrond","rivierklei","zeeklei","duinzand"),
      A_PH_CC = seq(4,8,length.out = 6),
      A_B_CC = seq(0.01,1000,length.out = 6)
    ),
    expected = c(0,0.7,0.8,1.6,2.1,1.8),
    tolerance = .1
  )
})


test_that("ag_con_coaa works", {
  expect_equal(
    ag_con_coaa(
      A_CLAY_MI = 5,
      A_CO_CC = 500,
      A_MG_CC = 500,
      A_PH_CC = 5.8
    ),
    expected = 3.00,
    tolerance = 0.01
  )
  expect_equal(
    ag_con_coaa(
      A_CLAY_MI = rep(5,5),
      A_CO_CC = seq(1, 1000,length.out = 5),
      A_MG_CC = seq(1, 1000,length.out = 5),
      A_PH_CC = rep(5.8,5)
    ),
    expected = c(0,1.5,3,4.46,5.91),
    tolerance = .1
  )
})

