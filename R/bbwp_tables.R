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
#'   \item{soiltype}{The type of soil}
#'   \item{type}{The type of driving factor (either political urgency (urgency) or contribution to an environmental theme (aim))}
#'   \item{cf_soil}{Correction factor to estimate soil score for Ecoregeling}
#'   \item{cf_water}{Correction factor to estimate water score for Ecoregeling}
#'   \item{cf_climate}{Correction factor to estimate climate score for Ecoregeling}
#'   \item{cf_biodiversity}{Correction factor to estimate biodiversity score for Ecoregeling}
#'   \item{cf_landscape}{Correction factor to estimate landscape score for Ecoregeling}
#' }
"er_scoring"

#' The impact score of farm based measures
#' 
#' This table contains the scores of crop rotation related farm measures for Ecoregeling
#' 
#' @format A data.table with 40 rows and 5 columns:
#' \describe{
#'   \item{id}{A unique ID number}
#'   \item{indicator}{The environmental theme to which the measure applies}
#'   \item{eco_id}{The unique ID corresponding with a specific Ecoregelingen measure}
#'   \item{description}{Short decription of the measure} 
#'   \item{er_score}{The score of the measure on the environmental theme}
#' }
"er_farm_measure"

#' An overview of crop lists used in Ecoregelingen
#' 
#' This table contains the crop ids (b_lu_brp) for specific Ecoregelingen measures
#' 
#' @format A data.table with 75 rows and 4 columns:
#' \describe{
#'   \item{eco_id}{The unique ID corresponding with a specific Ecoregelingen measure}
#'   \item{b_lu_brp}{The crop type (conform BRP coding, preferable the most frequent crop on the field)}
#'   \item{b_lu_name}{The crop name of the most frequent crop on the field}
#'   \item{er_description}{The description of the crop related measure}
#' }
"er_crops"

#' A national dataset describing statistics of soil properties per Local Surface Water
#' 
#' This table contains mean and standard deviation of soil properties per Local Surface Water Polygon in the Netherlands
#' 
#' @format A data.table with 546 rows and 32 columns:
#' \describe{
#'   \item{c1}{bla bla}
#'   \item{c2}{bla bla}
#'   \item{c3}{bla bla}
#'   \item{c4}{bla bla}
#' }
"lsw"

#' An overview of correction factors per economic agricultural area
#' 
#' This table contains the correction factor for the financial reward differentiated per Agricultural Economic Region
#' 
#' @format A data.table with 14 rows and 3 columns:
#' \describe{
#'   \item{statcode}{bla bla}
#'   \item{statname}{bla bla}
#'   \item{er_cf}{bla bla}
#' }
"er_aer_reward"