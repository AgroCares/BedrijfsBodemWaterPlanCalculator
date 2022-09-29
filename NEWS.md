# BBWPC v.0.10.5
## Changed
* `S_ER_FARM_TOT` in `er_opi` is now the sum of the five eco theme scores
* `dt.farm.score` in `er_opi` is now calculated as the weighted mean of the five eco theme scores (and does not include total score and reward in calculation of weighted mean anymore)
* changed name of `TOTAL` into `FARM_TOT` in input dt for `er_medal` in order to get the merge with aims right

## Fixed
* `s_er_water` in `er_main` and `er_opi` are no longer showing score for soil but for water now
* correction on calculation of `s_er_costs_silver` in `er_farm_aim`

# BBWPC v0.10.4
## Added
* the function `ecoregeling` in er_main gives an additional output object besides `farm` and `field`, named `farm_tresholds`
* farm thresholds are given for 7 aims of Ecoregeling, differentiated for level `bronze`, `silver` or `gold`

## Changed
* thresholds for costs changed from absolute values to percentages in `out` of `ecoregeling`
* field scores in output of `er_opi` have maximum levels similar to maximum levels of farm scores now

# BBWPC v0.10.3

## Changed
* Set maxima for the five eco scores to 15 and for `s_er_farm_tot` to 50.
* Set maximum for `s_er_costs` to 175 and convert to percentage between 0 and 100
* Set aim for landscape to 1 and set aim for water to 1 in case all fields have `B_SOILTYPE_AGR` "veen".
* Update `test-er`

# BBWPC v0.10.2
## Fixed
* correction on calculation of `S_ER_FARM_SCORE`: now averaged over the five eco themes
* the eco scores or eco aims are set to 0.001 when these are 0, to prevent outputs of NA's.

## Added
* argument `thresholds` to `er_farm_aim` for medals 
* `s_er_tot` to the field output
* aim for total farm score and costs as threshold for medals

## Changed
* maximum costs is set to 250
* changed order of output from `ecoregeling` in `er_main`
* update tests

# BBWPC v0.10.1
## Fixed
* reward is now corresponding with the medal and a fixed value for bronze, silver and gold
* minor error in test data `test-er`

## Added
* visible binding `s_er_reward` added to `er_main`

## Changed
* the field score is set equal to the farm score for Ecoregeling method
* input arguments `B_CT_??` are removed from `er_crop_rotation`,`er_field_score`
* input argument `E_ER_TOT` is removed from `er_farm_score`
* adjust targets for landscape in the final scoring in `er_medal` and `er_opi`; there is no minimum needed.
* the indicator score is set to the contribution of single fields to the farm score. The function `er_field_score` givers therefore the actual score per hectare, and not the relative score given target.
* measures table is updated (some levels (field vs farm) have been altered)

# BBWPC v0.10.0
## Fixed
* test csv files removed from package directory and dev directory
* threshold of scores for medals are set to zero for indicators climate and water (only on peat soils)
* the absolute total score is used for medal check instead of the integrative total score 
* no "terras" measures are recommended for fields where B_SLOPE_DEGREE is less than 2\% (fix in `bbwp_meas_rank`)

## Added
* function `er_opi` to estimate the contribution of single fields to the farm score
* exception for measure G20 is added to `er_meas_rank`, `er_meas_score` and `er_crop_rotation`.

## Changed
* the field score is set equal to the farm score for Ecoregeling method
* input arguments `B_CT_??` are removed from `er_crop_rotation`,`er_field_score`
* input argument `E_ER_TOT` is removed from `er_farm_score`
* adjust targets for landscape in the final scoring in `er_medal` and `er_opi`; there is no minimum needed.
* the indicator score is set to the contribution of single fields to the farm score. The function `er_field_score` givers therefore the actual score per hectare, and not the relative score given target.
* measures table is updated (some levels (field vs farm) have been altered)


# BBWPC v0.9.1
## Added
* Added table with BBWPC variable from pandex to standardise parameter checks

# BBWPC v0.9.0
## Changed
* delete `B_LU_ECO_x` as input of `er_main`, `er_meas_rank`, `er_meas_scores`, and `er_field_score`.
* inputs `B_LU_ECO8`, `B_LU_ECO9`, `B_LU_ECO10` renamed to `B_LU_ARABLE_ER`, `B_LU_PRODUCTIVE_ER`,and `B_LU_CULTIVATED_ER`
* reward is now fixed to the medal earned
* scores and applicability of several measures is updated according to comments of Boerennatuur in `bbwp_measures`

## Fixed
* selection criteria for Ecoregeling scoring is updated given applicability criteria from `bbwp_measures`
* correction farm weighted mean of ER scores in `er_crop_rotation`

## Added
* new package table with data.table of measures and which crops belong to these measures: `er_measures`
* regional weighing factor for scoring, in `er_measures` as well as `er_crop_rotation`
* added financial reward correction based on the agricultural economic region in the Netherlands, in `er_crop_rotation` and `er_measure_scores`

# BBWPC v0.8.2
## Changed
* Use csv as source for bbwp_measures and er_crops instead of binaries, issue #41
* Cleaned up /dev folder by removing unused files

## Fixed
* Fix encoding of `description` in `bbwp_measures`

# BBWPC v0.8.1
## Fixed
* Set NA to `ntb` for `description` in `bbwp_measures`

# BBWPC v0.8.0
## Changed
* B_LU_BBWP has been reclassified as a string with 12 crop catogories (linked to pandex)

## Fixed
* use of B_LU_BBWP in `bbwp_meas_score` and `bbwp_meas_rank` and related measure categories updated

# BBWPC v0.7.0
## Changed
* add `er_medal` to estimate the medal given score per field and farm

## Fixed
* error in renaming indicator after melt in `er_meas_score` and `er_crop_rotation`
* error in `er_crop_rotation` when multiple soils from same soiltype are given

# BBWPC v0.6.0
## Added
* table `bbwp_measures` is updated with scores, rewards and applicability of ER measures, issue #11
* accumulation requirements are added to `er_meas_rank` and `er_meas_score`, issue #13
* farm measure scoring is added to `er_croprotation`, issue #12
* measures input table requires a `bbwp_status` to account for accumulation effects
* `medalscore` added as argument to`er_farm_aim`, overruling input of argument `farmscore`

## Changed
* `B_LU_BRP` is removed as input for all `bbwp_x` functions, issue #35
* set default BRP codes per BBWP category for waterstress indicators in `bbwp_field_properties`
* `B_LU_BBWP`categories have been updated from 1*9 to 1*12

# BBWPC v0.5.8
## Fixed
* Farm level measures are no longer advised at field level in `bbwp_meas_rank` and `er_meas_rank`

# BBWPC v0.5.7
## Added
* lower the task (opgave) for wet and peat soils for nitrate leaching, issue #25

## Fixed
* remove incorrect B_GWL_CLASS unit check in `bbwp_field_properties`, issue #32

# BBWPC v0.5.6
## Fixed
* In the table `bbwp_measures` NA values are set to 0

# BBWPC v0.5.5
## Fixed
* Fixed error in description

# BBWPC v0.5.4
## Fixed
* Fixed error with check on B_GWL_CLASS, removed from bbwp_meas_rank

# BBWPC v0.5.3
## Changed
* Update check on B_AREA, set to m2

# BBWPC v0.5.2
## Changed
* Change variable names: `d_area` to `b_area`, `reward` to `s_er_reward`, `er_medal` to `s_er_medal`, `lat` and `lon` to `a_lat` and `a_lon`

# BBWPC v0.5.1
## Changed
* internal table `bbwp_measures` with column bbwp_conflict
* avoid additive scoring for conflicting measures in `bbwp_meas_score` and `bbp_meas_rank`
* avoid additive scoring for conflicting measures in `er_meas_score`
* add `er_medal` as farm output for `ecoregeling` 
* tests are updated

# BBWPC v0.5.0
## Added
* internal table `er_aer_reward`, prepared in dev
* check and automatic update `B_GWL_CLASS` via function `bbwp_format_aer`, included in all relevant bbwp and er functions, issue #18
* add `B_AER_CBS` as argument to all bbwp and er functions
* output `reward` added to `er_meas_score`, `er_field_scores` and `er_farm_score`, issue #17
* function `ecoregeling` adds `reward` as output in objects farm and field, issue #17
* argument `B_LU_BBWP` is converted from string to integerish by removing `cat_` in wrapper funs `ecoregeling` and `bbwp`

# BBWPC v0.4.2
## Added
* add LSW data as geopackage to dev
* add `bbwp_check_lsw` to check, update the LSW data, with option for spatial intersection when geopackage is available
* add internal package table `lsw` with the properties per local surface water (LSW)

## Changed
* argument `D_WP` changed into `D_SA_W` for all BBWP functions

# BBWPC v0.4.1
## Added
* B_SLOPE and B_SLOPE_DEGREE as input in wrapper function `bbwp`

## Changed
* change output of `er_meas_rank` from `top.x` to `top_er_x` with x being the targets
* change output of `bbwp_meas_rank` from `top.x` to `top_bbwp_x` with x being the targets
* replace `x_meas_rank` to inside the if*function in wrapper funs to speed up the code
* change output scores of `bbwp_field_score` and input for `bbwp_farm_score` from `D_OPI_x` to `S_BBWP_x`
* change output scores of `er_field_score` and input for `er_farm_score` from `D_OPI_x` to `S_ER_x`
* change B_SLOPE to B_SLOPE_DEGREE in all functions

# BBWPC v0.4.0
## Added
* add wrapper function `bbwp` to run BBWP for a series of fields
* add wrapper function `ecoregeling` to run Ecoregeling for a series of fields
* add associated test functions for `bbwp` and `ecoregeling`

# BBWPC v0.3.2
## Added
* add argument `B_SLOPE` to `bbwp_field_properties`,`bbwp_field_indicators`, issue #4
* add argument `B_LU_BBWP` to `bbwp_meas_score`
* add `id` to output `bbwp_meas_score`
* add hierarchy in runoff index for N and P, issue #2

## Changed
* update test functions for `bbwp_field_properties`,`bbwp_field_indicators`,
* selection top*5 measures in `bbwp_meas_rank` in data.table format
* remove duplicated NA outputs in `bbwp_meas_rank` when D_OPI is zero
* add pmax on D_OPI in `bbwp_field_scores`
* include `bbwp_check_meas` in all bbwp_functions to load, check and update the measures input data.table

# BBWPC v0.3.1
## Added
* er_farm_score, estimate farm averaged score for Ecoregeling method
* test function for `er_farm_score`

## Changed
* add check for D_AREA length to bbwp_farm_score
* update test function for `bbwp_farm_score`

# BBWPC v0.3.0
## Added
* er_farm_aim, estimate the Ecoregelingen (ER) score on farm level
* er_meas_rank, rank the list of measures given impact on ER targets
* er_meas_score, evaluate the impact of measures on ER targets given field properties
* er_field_scores, evaluate current status and impact of measures on field level
* er_croprotation, assess ER score following crop rotation plan without additional measures
* bbwp_check_meas in `bbwp_helpers` to check, update and adapt the list of measures for BBWP and ER
* table `bbwp_measures` showing all details (applicability, effectiviness) from measures for BBWP and ER
* table `er_scoring`, showing the importance and scoring of environmental challenges for ER
* table `er_farm_measure`, showing the impact of measures that are only valid on farm level
* table `er_crops`, showing specific crop lists used for the ER
* test functions for `er_farm_aim`, `er_meas_rank`, `er_meas_score`, `er_field_scores`, `er_croprotation` and `bbwp_check_meas`

## Updated
* ppr_tables_bbwp in `dev` along with associated csv files for tables `bbwp_measures`, `er_scoring`, `er_farm_measure` and `er_crops`

# BBWPC v0.2.0 2021*11*01
## Added
* Adds unit tests
* Adds GitHub Actions for R CMD CHECK and coverage
* Prepare code for package
* Adds README and changelog
