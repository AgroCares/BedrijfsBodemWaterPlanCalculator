test_that("check bbwp_field_indicators", {
  expect_equal(
    bbwp_field_indicators(
      D_NGW_SCR = seq(0, 1, by = 0.2),
      D_NGW_LEA = seq(0, 1, by = 0.2),
      D_NGW_NLV = seq(0, 1, by = 0.2),
      D_NSW_SCR = seq(0, 1, by = 0.2),
      D_NSW_GWT = seq(0, 1, by = 0.2),
      D_NSW_RO = seq(0, 1, by = 0.2),
      D_NSW_WS = seq(0, 1, by = 0.2),
      D_NSW_NLV = seq(0, 1, by = 0.2),
      D_NSW_SLOPE = seq(0, 1, by = 0.2),
      D_PSW_SCR = seq(0, 1, by = 0.2),
      D_PSW_GWT = seq(0, 1, by = 0.2),
      D_PSW_RO = seq(0, 1, by = 0.2),
      D_PSW_WS = seq(0, 1, by = 0.2),
      D_PSW_PCC = seq(0, 1, by = 0.2),
      D_PSW_PSG = seq(0, 1, by = 0.2),
      D_PSW_PRET = seq(0, 1, by = 0.2),
      D_PSW_SLOPE = seq(0,1,by = 0.2),
      D_NUE_WRI = seq(0, 1, by = 0.2),
      D_NUE_PBI = seq(0, 1, by = 0.2),
      D_NUE_WDRI = seq(0, 1, by = 0.2),
      D_NUE_NLV = seq(0, 1, by = 0.2),
      D_WUE_WWRI = seq(0, 1, by = 0.2),
      D_WUE_WDRI = seq(0, 1, by = 0.2),
      D_WUE_WHC = seq(0, 1, by = 0.2)
    ),
    expected = data.table(
      D_RISK_NGW = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      D_RISK_NSW = c(0.01,0.01,0.4,0.6,0.8,1),
      D_RISK_PSW = c(0.01,0.01,0.42,0.62,0.81,1),
      D_RISK_NUE = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      D_RISK_WB = c(0, 0.2, 0.4, 0.6, 0.8, 1)
    ),
    tolerance = 0.01)
})
