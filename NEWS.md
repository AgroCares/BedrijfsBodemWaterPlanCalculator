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
