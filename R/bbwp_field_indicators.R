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
#' @param D_NSW_SLOPE (numeric) The relative score of runoff risks for N loss to surface water given slope
#' @param D_NSW_WS (numeric) The relative score of wet surrounding for N loss to surface water
#' @param D_NSW_NLV (numeric) The relative score of N mineralization for N loss to surface water
#' @param D_PSW_SCR (numeric) The relative score of soil compaction risk for P loss to surface water
#' @param D_PSW_GWT (numeric) The relative score of soil wetness for P loss to surface water
#' @param D_PSW_RO (numeric) The relative score of runoff risks for P loss to surface water
#' @param D_PSW_SLOPE (numeric) The relative score of runoff risks for P loss to surface water given slope
#' @param D_PSW_WS (numeric) The relative score of wet surrounding for P loss to surface water
#' @param D_PSW_PCC (numeric) The relative score of P levels in soil solution for P loss to surface water
#' @param D_PSW_PSG (numeric) The relative score of P saturation degree for P loss to surface water
#' @param D_PSW_PRET (numeric) The relative score of P retention capacity for P loss to surface water
#' @param D_NUE_WRI (numeric) The relative score of soil water stress for improved efficiency of N and P inputs
#' @param D_NUE_PBI (numeric) The relative score of P availability for improved efficiency of N and P inputs
#' @param D_NUE_WDRI (numeric) The relative score of drought stress for improved efficiency of N and P inputs
#' @param D_NUE_NLV (numeric) The relative score of N mineralization for improved efficiency of N and P inputs
#' @param D_WUE_WWRI (numeric) The relative score of soil wetness stress for improved efficiency of water
#' @param D_WUE_WDRI (numeric) The relative score of drought stress for improved efficiency of water
#' @param D_WUE_WHC (numeric) The relative score of drought stress for improved efficiency of water
#' @param penalty (boolean) the option to apply a penalty for high risk BBWP field indicators
#' @param D_GW_GWR (numeric) The evaluated score for groundwater recharge
#' 
#' @import data.table
#' @import OBIC
#' 
#' @details
#' bbwp_field_indicators is typically called after \link{bbwp_field_properties} which
#' calculates the inputs for bbwp_field_indicators
#' 
#' 
#' @returns A data.table with five columns: D_RISK_NGW, D_RISK_NSW, D_RISK_PSW,
#' D_RISK_NUE, and D_RISK_WB. The number of rows corresponds with the number of 
#' fields given as input i.e. the length of the vector inputs
#' 
#' @examples
#' # example with 6 fields
#' bbwp_field_indicators(
#' D_NGW_SCR = seq(0, 1, by = 0.2),
#' D_NGW_LEA = seq(0, 1, by = 0.2),
#' D_NGW_NLV = seq(0, 1, by = 0.2),
#' D_NSW_SCR = seq(0, 1, by = 0.2),
#' D_NSW_GWT = seq(0, 1, by = 0.2),
#' D_NSW_RO = seq(0, 1, by = 0.2),
#' D_NSW_WS = seq(0, 1, by = 0.2),
#' D_NSW_NLV = seq(0, 1, by = 0.2),
#' D_NSW_SLOPE = seq(0, 1, by = 0.2),
#' D_PSW_SCR = seq(0, 1, by = 0.2),
#' D_PSW_GWT = seq(0, 1, by = 0.2),
#' D_PSW_RO = seq(0, 1, by = 0.2),
#' D_PSW_WS = seq(0, 1, by = 0.2),
#' D_PSW_PCC = seq(0, 1, by = 0.2),
#' D_PSW_PSG = seq(0, 1, by = 0.2),
#' D_PSW_PRET = seq(0, 1, by = 0.2),
#' D_PSW_SLOPE = seq(0,1,by = 0.2),
#' D_NUE_WRI = seq(0, 1, by = 0.2),
#' D_NUE_PBI = seq(0, 1, by = 0.2),
#' D_NUE_WDRI = seq(0, 1, by = 0.2),
#' D_NUE_NLV = seq(0, 1, by = 0.2),
#' D_WUE_WWRI = seq(0, 1, by = 0.2),
#' D_WUE_WDRI = seq(0, 1, by = 0.2),
#' D_WUE_WHC = seq(0, 1, by = 0.2),
#' penalty = FALSE,
#' D_GW_GWR = seq(0, 1, by = 0.2)
#' )
#'
#' @export
# calculate the risk or opportunity indicators for a field
bbwp_field_indicators <- function(D_NGW_SCR,D_NGW_LEA,D_NGW_NLV,
                                  D_NSW_SCR,D_NSW_GWT,D_NSW_RO,D_NSW_SLOPE, D_NSW_WS,D_NSW_NLV,
                                  D_PSW_SCR,D_PSW_GWT,D_PSW_RO,D_PSW_SLOPE,D_PSW_WS,D_PSW_PCC,D_PSW_PSG,D_PSW_PRET,
                                  D_NUE_WRI,D_NUE_PBI,D_NUE_WDRI,D_NUE_NLV,
                                  D_WUE_WWRI,D_WUE_WDRI,D_WUE_WHC, penalty = TRUE,
                                  D_GW_GWR){
  
  # add visual bindings
  D_RISK_NGW = D_RISK_NSW = D_RISK_PSW = D_RISK_NUE = D_RISK_WB = id = NULL
  risk_cor = value = group = risk = mcf = WS = SLOPE = NULL
  
  # check length inputs
  arg.length <- max(
    length(D_NGW_SCR),length(D_NGW_LEA),length(D_NGW_NLV),
    length(D_NSW_SCR),length(D_NSW_GWT),length(D_NSW_RO),length(D_NSW_WS),length(D_NSW_NLV),length(D_NSW_SLOPE),
    length(D_PSW_SCR),length(D_PSW_GWT),length(D_PSW_RO),length(D_PSW_SLOPE),length(D_PSW_WS),length(D_PSW_PCC),length(D_PSW_PSG),length(D_PSW_PRET),
    length(D_NUE_WRI),length(D_NUE_PBI),length(D_NUE_WDRI),length(D_NUE_NLV),
    length(D_WUE_WWRI),length(D_WUE_WDRI),length(D_WUE_WHC), length(D_GW_GWR)
  )
  
  # copy input in one data.table
  dt <- data.table(id = 1:arg.length,
                   D_NGW_SCR = D_NGW_SCR,
                   D_NGW_LEA = D_NGW_LEA,
                   D_NGW_NLV = D_NGW_NLV,
                   D_NSW_SCR = D_NSW_SCR,
                   D_NSW_GWT = D_NSW_GWT,
                   D_NSW_RO = D_NSW_RO,
                   D_NSW_SLOPE = D_NSW_SLOPE,
                   D_NSW_WS = D_NSW_WS,
                   D_NSW_NLV = D_NSW_NLV,
                   D_PSW_SCR = D_PSW_SCR,
                   D_PSW_GWT = D_PSW_GWT,
                   D_PSW_RO = D_PSW_RO,
                   D_PSW_SLOPE = D_PSW_SLOPE,
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
                   D_GW_GWR = D_GW_GWR
                  )
  
  # melt the data.table to simplify corrections
  dt.melt <- data.table::melt(dt, id.vars = 'id',variable.name = 'risk')
  
  # add correction factor based on risk itself
  dt.melt[,risk_cor := wf(value,type = "indicators",penalty = penalty)]
  
  # add groups of risk indicators
  dt.melt[,group := gsub('_[A-Z]+$','',gsub('D_','',risk))]
  
  # add manual weighing factor for risks
  dt.melt[,mcf := 1]
  dt.melt[group=='NGW' & grepl('_LEA$',risk), mcf := 3]
  dt.melt[group=='NGW' & grepl('_NLV$',risk), mcf := 2]
  dt.melt[group=='NSW' & grepl('_NLV$',risk), mcf := 3]
  dt.melt[group=='PSW' & grepl('_SCR$|_RO$|_WS$',risk), mcf := 2]
  dt.melt[group=='NUE' & grepl('_PBI$',risk), mcf := 2]
  dt.melt[group=='WUE' & grepl('_WHC$',risk), mcf := 2]
  dt.melt[group=='GW', mcf := fifelse(grepl('_GWR$',risk), 1 , 0)] 
  
   
  # minimize risks when there are no ditches around the field (wet surrounding fraction < 0.2)
  
    # add criteria properties as column (to use as filter)
    dt.melt[,WS := value[risk=='D_NSW_WS'],by='id']
    dt.melt[,SLOPE := value[risk=='D_NSW_SLOPE'],by='id']
    
    # ensure that the final risk after aggregation gets the value 0.1 or 0.01
    dt.melt[WS <= 0.2 & SLOPE < 1 & group %in% c('NSW','PSW'), c('mcf','risk_cor','value') :=  list(1,1000,0.1)]
    dt.melt[WS <= 0.1 & SLOPE < 1 & group %in% c('NSW','PSW'), c('mcf','risk_cor','value') :=  list(1,1000,0.01)]
    dt.melt[,c('WS','SLOPE') := NULL]
  
  # calculate the mean aggregated risk indicators
  dt <- dt.melt[,list(risk = sum(risk_cor * value * mcf)/sum(risk_cor * mcf)),by=c('id','group')]
  dt <- dcast(dt,id~group,value.var='risk')
  
  # replace output names
  setnames(dt,
           old=c('NGW','NSW','NUE','PSW','WUE', 'GW'),
           new = c('D_RISK_NGW','D_RISK_NSW','D_RISK_NUE','D_RISK_PSW','D_RISK_WB', 'D_RISK_GWR'))
  
  # sort output based on id
  setorder(dt,id)
  
  # extract output
  out <- dt[,mget(c('D_RISK_NGW','D_RISK_NSW','D_RISK_PSW','D_RISK_NUE','D_RISK_WB', 'D_RISK_GWR'))]
  
  # return output
  return(out)
  
  
}
