#' Agronomic measures that can be applied
#' 
#' This table contains a series of agronomic measures, their applicability and effectiveness for multiple ecosystem service
#' 
#' @format A data.table with 61 rows and 22 columns:
#' \describe{
#'   \item{nr}{The van Gerven number of the measure, related to the study by Van Gerven et al. (2020)}
#'   \item{omschrijving}{Short description of the measure}
#'   \item{m_cat}{The category of the measure}
#'   \item{tp_mvh}{Applicability of the measure in the dairy sector}
#'   \item{tp_ab}{Applicability of the measure in the arable sector}
#'   \item{tp_vgg}{Applicability of the measure in the vegetable sector}
#'   \item{tp_ab}{Applicability of the measure in the bulb sector}
#'   \item{tp_ab}{Applicability of the measure in the tree and fruit tree sector}
#'   \item{tp_zand}{Applicability of the measure on sandy soils}
#'   \item{tp_klei}{Applicability of the measure on clay soils}
#'   \item{tp_veen}{Applicability of the measure on peat soils}
#'   \item{tp_metdrainage}{Applicability of the measure on fields with drainage}
#'   \item{tp_zonderdrainage}{Applicability of the measure on fields without drainage}
#'   \item{tp_nat}{Applicability of the measure on wet soils}
#'   \item{tp_droog}{Applicability of the measure on dry soils}
#'   \item{tp_nietontwaterd}{Applicability of the measure on fields without surrounding ditches}
#'   \item{pow}{Effectiveness of the measure for P loss to surface water}
#'   \item{now}{Effectiveness of the measure for N loss to surface water}
#'   \item{ngw}{Effectiveness of the measure for nitrate loss to groundwater}
#'   \item{e_np}{Effectiveness of the measure for improving nutrient use efficiency}
#'   \item{e_wb}{Effectiveness of the measure to improve water holding capacity of soil}
#'   \item{e_kosten}{index representing the costs for implementing the measure}
#' }
"bbwp_measures"

#' Local Surface Water database
#' 
#' This table contains the LSW database with desired change / reduction in N and P loss to surface water
#' The polygons (skipped in the ppr_tables_bbwp.R) are prepared by WEnR linking catchment regions to existing KRW water bodies.
#' 
#' @format A data.table with 550 rows and 5 columns:
#' \describe{
#'   \item{oow_id}{A unique id number}
#'   \item{oow_name}{Name of the catchment}
#'   \item{oow_nitrogen}{The desired reduction in agricultural N load to surface water}
#'   \item{oow_phosphate}{The desired reduction in agricultural P load to surface water}
#'   \item{oow_source}{the original source of the estimated N and P loeads}
#'}
"bbwp_oow"
