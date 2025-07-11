#' Agronomic measures that can be applied
#' 
#' This table contains a series of agronomic measures, their applicability and effectiveness for multiple ecosystem services
#' 
#' \describe{
#'   \item{bbwp_id}{The unique BBWP measure id based on the van Gerven number of the measure, related to the study by Van Gerven et al. (2020)}
#'   \item{nr_mok}{A mock up code for bbwp measure id}
#'   \item{boot22_id}{The measure id refering to the measure code on the Dutch "BOOT-lijst 2022"}
#'   \item{eco_id}{The unique Ecoregeling measure id}
#'   \item{summary}{A short explanation of the measure}
#'   \item{description}{An elaborate description of the measure}
#'   \item{url}{The URL refers to a web resource containing a fact sheet of the specific measure}
#'   \item{level}{The spatial scale at which the measure is valid: "field" or "farm"}
#'   \item{category}{The measure category, choices: "teeltmaatregel", "bodemverbetering", "bedrijf", "slootkantbeheer", "gewasbescherming", "precisiebemesting", "niet_productief", "watervasthouden", or "erf"}
#'   \item{dairy}{Applicability of the measure in the dairy sector}
#'   \item{arable}{Applicability of the measure in the arable sector}
#'   \item{vegetables_outdoor}{Applicability of the measure in the (outdoor) vegetables sector}
#'   \item{bulbs}{Applicability of the measure in the bulb sector}
#'   \item{tree_nursery}{Applicability of the measure on tree nurseries}
#'   \item{sand}{Applicability of the measure on sandy soils}
#'   \item{loess}{Applicability of the measure on loess soils}
#'   \item{clay}{Applicability of the measure on clay soils}
#'   \item{peat}{Applicability of the measure on peat soils}
#'   \item{psw_psg_low}{Additional impact on the effectiveness of the measure for P losses to surface water when the P saturation index is low} 
#'   \item{psw_psg_medium}{Additional impact on the effectiveness of the measure for P losses to surface water when the P saturation index is medium}
#'   \item{psw_psg_high}{Additional impact on the effectiveness of the measure for P losses to surface water when the P saturation index is high}
#'   \item{nsw_drains}{Additional impact on the effectiveness of the measure for N losses to surface water on fields with drainage}
#'   \item{nsw_nodrains}{Additional impact on the effectiveness of the measure for N losses to surface water on fields without drainage}
#'   \item{nsw_gwl_high}{Additional impact on the effectiveness of the measure for N losses to surface water under high groundwater level conditions}
#'   \item{nsw_gwl_low}{Additional impact on the effectiveness of the measure for N losses to surface water under low groundwater level conditions}
#'   \item{psw_noslope}{Additional impact on the effectiveness of the measure for P losses to surface water when there is no slope}
#'   \item{psw_bulbs}{Additional impact on the effectiveness of the measure for P losses to surface water when applied on a bulb field}
#'   \item{ngw_grassland}{Additional impact on the effectiveness of the measure for N losses to groundwater on grasslands}
#'   \item{p_ow}{Effectiveness of the measure on total P load in surface water}
#'   \item{p_ow2}{Effectiveness of the measure on P concentration in ditch}
#'   \item{n_ow}{Effectiveness of the measure on total N load in surface water}
#'   \item{n_ow2}{Effectiveness of the measure on N concentration in ditch}
#'   \item{effect_ngw}{Effectiveness of the measure for nitrate losses to groundwater}
#'   \item{effect_psw}{Effectiveness of the measure for P losses to surface water}
#'   \item{effect_nsw}{Effectiveness of the measure for N losses to surface water}
#'   \item{effect_nue}{Effectiveness of the measure for improving nutrient use efficiency}
#'   \item{effect_gw}{Effectiveness of the measure to improve groundwater recharge}
#'   \item{effect_costs}{Index representing the costs for implementing the measure}
#'   \item{b_lu_arable_er}{Boolean indicating whether the crop falls within the ER category "arable"}
#'   \item{b_lu_productive_er}{Boolean indicating whether the crop falls within the ER category "productive"}
#'   \item{b_lu_cultivated_er}{Boolean indicating whether the crop falls within the ER category "cultivated"}
#'   \item{er_climate}{The measure impact score for eco theme climate}
#'   \item{er_soil}{The measure impact score for eco theme soil}
#'   \item{er_water}{The measure impact score for eco theme water}
#'   \item{er_landscape}{The measure impact score for eco theme landscape}
#'   \item{er_biodiversity}{The measure impact score for eco theme biodiversity}
#'   \item{er_euro_ha}{The estimated costs of the measure per hectare (euro/ha)}
#'   \item{er_euro_farm}{The estimated costs of measures per farm (euro/farm)}
#'   \item{bbwp_conflict}{Code indicating measure conflict: measures with same bbwp_conflict code are conflicting}
#'   \item{acc_anlb}{Indicator specifying how score and reward need to adjust when measure is already applied with Dutch ANLB, choices: "none", "score only", "reward only", "both score and reward"}
#'   \item{acc_glmc}{Indicator specifying how score and reward need to adjust when measure is already applied with Dutch GLMC, choices: "none", "score only", "reward only", "both score and reward"}
#'   \item{regio_factor}{Boolean specifying whether regional correction on measure costs based on agricultural economic region is required}
#'   \item{bodemkwaliteit}{Boolean specifying whether measure greatly contributes to eco theme soil}
#'   \item{waterkwaliteit}{Boolean specifying whether measure greatly contributes to eco theme water}
#'   \item{klimaat}{Boolean specifying whether measure greatly contributes to eco theme climate}
#'   \item{biodiversiteit}{Boolean specifying whether measure greatly contributes to eco theme biodiversity}
#'   \item{landschap}{Boolean specifying whether measure greatly contributes to eco theme landscape}
#'   \item{nc1}{Applicability of the measure on crop category "gras_permanent"}
#'   \item{nc2}{Applicability of the measure on crop category "gras_tijdelijk"}
#'   \item{nc3}{Applicability of the measure on crop category "rustgewas"}
#'   \item{nc4}{Applicability of the measure on crop category "rooivrucht"}
#'   \item{nc5}{Applicability of the measure on crop category "groenten"}
#'   \item{nc6}{Applicability of the measure on crop category "bollensierteelt"}
#'   \item{nc7}{Applicability of the measure on crop category "boomfruitteelt"}
#'   \item{nc8}{Applicability of the measure on crop category "natuur"}
#'   \item{nc9}{Applicability of the measure on crop category "mais"}
#'   \item{nc10}{Applicability of the measure on crop category "randensloot"}
#'   \item{nc11}{Applicability of the measure on crop category "vanggewas"}
#'   \item{nc12}{Applicability of the measure on crop category "eiwitgewas"}
#'   \item{categories}{Eco and/or BBWP categories to which the measure greatly contributes}
#'   \item{hoge_gronden}{weighing factor of effect_wb for the landscape category hoge gronden}
#'   \item{flanken}{weighing factor of effect_wb for the landscape category flanken}
#'   \item{beekdalen}{weighing factor of effect_wb for the landscape category beekdalen}
#'   \item{lokale_laagtes}{weighing factor of effect_wb for the landscape category lokale laagtes}
#'   \item{polders}{weighing factor of effect_wb for the landscape category polders}
#'   \item{effect_wb}{Effectiveness of the measure to improve water holding capacity of soil}
#' }
"bbwp_measures"

#' The importance and scoring of environmental challenges for Ecoregelingen
#' 
#' This table contains the objective scoring and correction factors to estimate score for Ecoregeling
#' 
#' \describe{
#'   \item{soiltype}{Agricultural soil type}
#'   \item{type}{The type of weighting factor: urgency (distribution of scores depending on political urgency) or aim (distribution of scores depending on aim for environmental challenges)}
#'   \item{cf_soil}{Weighting factor for soil}
#'   \item{cf_water}{Weighting factor for water}
#'   \item{cf_climate}{Weighting factor for climate}
#'   \item{cf_biodiversity}{Weighting factor for biodiversity}
#'   \item{cf_landscape}{Weighting factor for landscape}
#' }
"er_scoring"

#' The impact score of farm based measures
#' 
#' This table contains the scores of crop rotation related farm measures for Ecoregeling
#' 
#' \describe{
#'   \item{id}{Unique id for each impact score related to crop rotation measures per environmental theme}
#'   \item{indicator}{The environmental theme, choices: "soil", "water", "biodiversity", "climate" or "landscape"}
#'   \item{eco_id}{Ecoregeling measure id}
#'   \item{description}{Short description of the Ecoregeling measure}
#'   \item{er_score}{The impact score of the Ecoregeling measure}
#' }
"er_farm_measure"

#' The applicability of Ecoregeling Measures per crop type
#' 
#' This table contains the crops for which the Ecoregeling method counts scores and rewards
#' 
#' \describe{
#'   \item{eco_id}{The Ecoregeling measure id}
#'   \item{B_lU_BRP}{The Dutch crop id from the BRP}
#'   \item{eco_app}{A helper variable telling that the measure is applicable for that crop}
#' }
"er_measures"

#' An overview of crop lists used in Ecoregelingen
#' 
#' This table contains the crop ids (B_LU_BRP) with applicability on ER categories arable, productive and cultivated
#' 
#' \describe{
#'   \item{B_LU_BRP}{The Dutch crop code from the BRP}
#'   \item{B_LU_NAME}{The Dutch crop name}
#'   \item{B_LU_BBWP}{The Dutch BBWP category used for allocation of measures to BBWP crop categories}
#'   \item{B_LU_ARABLE_ER}{(boolean) indication whether the crop falls within the ER category "arable"}
#'   \item{B_LU_PRODUCTIVE_ER}{(boolean) indication whether the crop falls within the ER category "productive"}
#'   \item{B_LU_CULTIVATED_ER}{(boolean) indication whether the crop falls within the ER category "cultivated"}
#' }
"er_crops"

#' An overview of correction factors per economic agricultural area
#' 
#' This table contains the correction factor for the financial reward differentiated per Agricultural Economic Region
#' 
#' \describe{
#'   \item{statcode}{The unique code of the Agricultural Economic Region}
#'   \item{statname}{The name of the Agricultural Economic Region}
#'   \item{er_cf}{The value of the correction factor for the financial reward differentiated per Agricutural Economic Region}
#' }
"er_aer_reward"


#' A table with the BBWP parameters
#' 
#' This table contains the BBWP variables and their possible values according to pandex to standardize checks
#' 
#' \describe{
#'  \item{code}{The parameter code}
#'  \item{parameter}{Brief description of the parameter}
#'  \item{unit}{The unit of the parameter if applicable}
#'  \item{product}{Data classifier, A = Soil measurements, B = Environmental characteristics,
#'   D = Soil or feed characteristics derived from soil/feed measurements,
#'   M = Soil management measures, S = Scores, RM = Recommendations (measures/gifts),
#'   I = Indicators, F = Feed measurements, P = soil amendment product}
#'  \item{element}{Indicates the chemical element or parameter name}
#'  \item{method1}{Method used to determine value}
#'  \item{method2}{Additional details on method}
#'  \item{data_type}{Type of data the parameter pertains to: numeric, integer, char, bool, geom, enum}
#'  \item{value_min}{Lowest possible value the parameter may have if numeric or integer}
#'  \item{value_max}{Highest possible value the parameter may have if numeric or integer}
#'  \item{explanation}{Some additional explanation}
#'  \item{enum}{boolean whether parameter values are drawn from a limited set}
#'  \item{options}{Allowed values for a parameter of type enum seperated by "||"}
#'  \item{choices}{Vectorized version of options}
#'  }
"bbwp_parms"
