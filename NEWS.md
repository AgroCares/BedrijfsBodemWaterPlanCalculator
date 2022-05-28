# Changelog BBWPC

## 0.3.1
### Added
- er_farm_score, estimate farm averaged score for Ecoregeling method
- test function for `er_farm_score`

### Updated
- add check for D_AREA length to bbwp_farm_score
- update test function for `bbwp_farm_score`

## 0.3.0
### Added
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

### Updated
* ppr_tables_bbwp in `dev` along with associated csv files for tables `bbwp_measures`, `er_scoring`, `er_farm_measure` and `er_crops`

## 0.2.0 2021-11-01
### Added
* Adds unit tests
* Adds GitHub Actions for R CMD CHECK and coverage
* Prepare code for package
* Adds README and changelog
