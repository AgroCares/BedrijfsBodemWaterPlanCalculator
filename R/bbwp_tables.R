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

#' The importance and scoring of environmental challenges for Ecoregelingen
#' 
#' This table contains the objective scoring and correction factors to estimate score for Ecoregeling
#' 
#' @format A data.table with 8 rows and 7 columns:
#' \describe{
#'   \item{soiltype}{bla bla}
#'   \item{type}{bla bla}
#'   \item{cf_soil}{bla bla}
#'   \item{cf_water}{bla bla}
#'   \item{cf_climate}{bla bla}
#'   \item{cf_biodiversity}{bla bla}
#'   \item{cf_landscape}{ bla bla}
#' }
"er_scoring"

#' The impact score of farm based measures
#' 
#' This table contains the scores of crop rotation related farm measures for Ecoregeling
#' 
#' @format A data.table with 40 rows and 5 columns:
#' \describe{
#'   \item{id}{bla bla}
#'   \item{indicator}{bla bla}
#'   \item{measure}{bla bla}
#'   \item{description}{bla bla}
#'   \item{er_score}{bla bla}
#' }
"er_farm_measure"

#' An overview of crop lists used in Ecoregelingen
#' 
#' This table contains the crop ids (b_lu_brp) for specific Ecoregelingen measures
#' 
#' @format A data.table with 75 rows and 4 columns:
#' \describe{
#'   \item{eco_id}{bla bla}
#'   \item{b_lu_brp}{bla bla}
#'   \item{b_lu_name}{bla bla}
#'   \item{er_description}{bla bla}
#' }
"er_crops"
