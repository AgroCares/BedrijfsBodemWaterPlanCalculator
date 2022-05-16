#' Calculate five risk or opportunity indicators for an agricultural field
#'
#' These include indicators: 
#' for limited efficiency of nitrogen and phosphorus; a high risk is also indicative of high potential to improve NUE.
#' for the potential to buffer and store water and efficiently use water for plant growth.
#' for the risk for nitrogen and phosphorus leaching and runoff to surface water.
#' for the risk for nitrate leaching to groundwater.
#'
#' @param D_NGW_SCR (numeric) The relative score of soil compaction risk for N loss to groundwater
#' @param D_NGW_LEA (numeric) The relative score of nitrate leaching risk for N loss to groundwater
#' @param D_NGW_NLV (numeric) The relative score of N mineralization for N loss to groundwater
#' @param D_NSW_SCR (numeric) The relative score of soil compaction risk for N loss to surface water
#' @param D_NSW_GWT (numeric) The relative score of soil wetness for N loss to surface water
#' @param D_NSW_RO (numeric) The relative score of runoff risks for N loss to surface water
#' @param D_NSW_WS (numeric) The relative score of wet surrounding for N loss to surface water
#' @param D_NSW_NLV (numeric) The relative score of N mineralization for N loss to surface water
#' @param D_PSW_SCR (numeric) The relative score of soil compaction risk for P loss to surface water
#' @param D_PSW_GWT (numeric) The relative score of soil wetness for P loss to surface water
#' @param D_PSW_RO (numeric) The relative score of runoff risks for P loss to surface water
#' @param D_PSW_WS (numeric) The relative score of wet surrounding for P loss to surface water
#' @param D_PSW_PCC (numeric) The relative score of P levels in soil solution for P loss to surface water
#' @param D_PSW_PSG (numeric) The relative score of P saturation degree for P loss to surface water
#' @param D_PSW_PRET (numeric) The relative score of P retention capacity for P loss to surface water
#' @param D_NUE_WRI (numeric) The relative score of soil water stress for improved eficiency of N and P inputs
#' @param D_NUE_PBI (numeric) The relative score of P availability for improved eficiency of N and P inputs
#' @param D_NUE_WDRI (numeric) The relative score of drought stress for improved eficiency of N and P inputs
#' @param D_NUE_NLV (numeric) The relative score of N mineralization for improved eficiency of N and P inputs
#' @param D_WUE_WWRI (numeric) The relative score of soil wetness stress for improved eficiency of water
#' @param D_WUE_WDRI (numeric) The relative score of drought stress for improved eficiency of water
#' @param D_WUE_WHC (numeric) The relative score of drought stress for improved eficiency of water
#' 
#' @import data.table
#' @import OBIC
#'
#' @export
# calculate the risk or opportunity indicators for a field
bbwp_field_indicators <- function(D_NGW_SCR,D_NGW_LEA,D_NGW_NLV,
                                  D_NSW_SCR,D_NSW_GWT,D_NSW_RO,D_NSW_WS,D_NSW_NLV,
                                  D_PSW_SCR,D_PSW_GWT,D_PSW_RO,D_PSW_WS,D_PSW_PCC,D_PSW_PSG,D_PSW_PRET,
                                  D_NUE_WRI,D_NUE_PBI,D_NUE_WDRI,D_NUE_NLV,
                                  D_WUE_WWRI,D_WUE_WDRI,D_WUE_WHC){
  
  # check length inputs
  arg.length <- max(
    length(D_NGW_SCR),length(D_NGW_LEA),length(D_NGW_NLV),
    length(D_NSW_SCR),length(D_NSW_GWT),length(D_NSW_RO),length(D_NSW_WS),length(D_NSW_NLV),
    length(D_PSW_SCR),length(D_PSW_GWT),length(D_PSW_RO),length(D_PSW_WS),length(D_PSW_PCC),length(D_PSW_PSG),length(D_PSW_PRET),
    length(D_NUE_WRI),length(D_NUE_PBI),length(D_NUE_WDRI),length(D_NUE_NLV),
    length(D_WUE_WWRI),length(D_WUE_WDRI),length(D_WUE_WHC)
  )
  
  # add checks on input
  
  
  # copy input in one data.table
  dt <- data.table(
    id = 1:arg.length,
    D_NGW_SCR = D_NGW_SCR,
    D_NGW_LEA = D_NGW_LEA,
    D_NGW_NLV = D_NGW_NLV,
    D_NSW_SCR = D_NSW_SCR,
    D_NSW_GWT = D_NSW_GWT,
    D_NSW_RO = D_NSW_RO,
    D_NSW_WS = D_NSW_WS,
    D_NSW_NLV = D_NSW_NLV,
    D_PSW_SCR = D_PSW_SCR,
    D_PSW_GWT = D_PSW_GWT,
    D_PSW_RO = D_PSW_RO,
    D_PSW_WS = D_PSW_WS,
    D_PSW_PCC = D_PSW_PCC,
    D_PSW_PSG = D_PSW_PSG,
    D_PSW_PRET = D_PSW_PRET,
    D_NUE_WRI = D_NUE_WRI,
    D_NUE_PBI = D_NUE_PBI,
    D_NUE_WDRI = D_NUE_WDRI,
    D_NUE_NLV = D_NUE_NLV,
    D_WUE_WWRI = D_WUE_WWRI,
    D_WUE_WDRI = D_WUE_WDRI,
    D_WUE_WHC = D_WUE_WHC,
    D_RISK_NGW = NA_real_,
    D_RISK_NSW = NA_real_,
    D_RISK_PSW = NA_real_,
    D_RISK_NUE = NA_real_,
    D_RISK_WB = NA_real_
  )
  
  
  # integrate all relative field risk indicators into one for indictor for N loss to groundwater
  dt[, D_RISK_NGW := (wf(D_NGW_SCR) * D_NGW_SCR + 3 * wf(D_NGW_LEA) * D_NGW_LEA + 2 * wf(D_NGW_NLV) * D_NGW_NLV) /
       (wf(D_NGW_SCR) + 3 * wf(D_NGW_LEA) + 2 * wf(D_NGW_NLV))]
  
  # integrate all relative field risk indicators into one for indictor for N loss to surface water
  dt[, D_RISK_NSW := (wf(D_NSW_SCR) * D_NSW_SCR + wf(D_NSW_GWT) * D_NSW_GWT + wf(D_NSW_RO) * D_NSW_RO + wf(D_NSW_WS) * D_NSW_WS + 3 * wf(D_NSW_NLV) * D_NSW_NLV ) /
       (wf(D_NSW_SCR) + wf(D_NSW_GWT) + wf(D_NSW_RO) + wf(D_NSW_WS) + 3 * wf(D_NSW_NLV))]
  
  # integrate all relative field risk indicators into one for indictor for P loss to surface water
  dt[, D_RISK_PSW := (2 * wf(D_PSW_SCR) * D_PSW_SCR + wf(D_PSW_GWT) * D_PSW_GWT + 2 * wf(D_PSW_RO) * D_PSW_RO + 2 * wf(D_PSW_WS) * D_PSW_WS + wf(D_PSW_PCC) * D_PSW_PCC + wf(D_PSW_PSG) * D_PSW_PSG + wf(D_PSW_PRET) * D_PSW_PRET ) /
       (2 * wf(D_PSW_SCR) + wf(D_PSW_GWT) + 2 * wf(D_PSW_RO) + 2 * wf(D_PSW_WS) + wf(D_PSW_PCC) + wf(D_PSW_PSG) + wf(D_PSW_PRET))]
  
  # integrate all relative field risk indicators into one for indictor for N and P efficiency of inputs
  dt[, D_RISK_NUE := (wf(D_NUE_WRI) * D_NUE_WRI + 2 * wf(D_NUE_PBI) * D_NUE_PBI + wf(D_NUE_NLV) * D_NUE_NLV + wf(D_NUE_WDRI) * D_NUE_WDRI) /
       (wf(D_NUE_WRI) + 2 * wf(D_NUE_PBI) + wf(D_NUE_NLV) + wf(D_NUE_WDRI))]
  
  # integrate all relative field risk indicators into one for indictor for water retention and efficiency
  dt[, D_RISK_WB := (wf(D_WUE_WWRI) * D_WUE_WWRI + wf(D_WUE_WDRI) * D_WUE_WDRI + 2 * wf(D_WUE_WHC) * D_WUE_WHC) / (wf(D_WUE_WWRI) + wf(D_WUE_WDRI) + 2 * wf(D_WUE_WHC))]
  
  # normalise these indicators ???
  
  setorder(dt, id)
  
  # extract output
  out <- dt[,mget(c('D_RISK_NGW','D_RISK_NSW','D_RISK_PSW','D_RISK_NUE','D_RISK_WB'))]
  
  # return output
  return(out)
  
  
}


#' Helper function to weight and correct the risk and scores
#' 
#' @param x The risk or score value to be weighted
#' 
#' @export
wf <- function(x, type = "indicators") {
  
  if (type == "indicators") {
    
    y <- 1 / (1 - x + 0.2)
    
  } else if (type == "score") {
    
    y <- 1 / (x * 0.01 + 0.2)
    
  }
  
  return(y)
}

