# Replicate tables

# Load data
peru_lr <- read_dta(file.path(data, "Peru_LR.dta"))

# Subset data for RD regression
peru_lr_rd <- peru_lr %>%
  filter(population_1972ths < 75 & agzones_1to7and11 == 1)

# Table 1
datasummary(
  (`Distance to ag. zone core (km)` = core_dist_kmio) + # nolint
  (`Land reform (prop. of district area)` = landredist_pc) + # nolint
  (`Total attacks` = totalevents) + # nolint
  (`Total deaths` = deaths) + # nolint
  (`Population (ths.)` = population_1972ths) + # nolint
  (`Road density` = d_roads_1973) + # nolint
  (`State personnel` = lemployees1961) + # nolint
  (`Elevation (ths. of meters)` = elev_1k) + # nolint
  (`Slope (degrees)` = slope) + # nolint
  (`Cultivable land (% area)` = cult_land) + # nolint
  (`Land area (hds. sq. km)` = superficie_km2_100) + # nolint
  (`Inside mita zone` = inside_mita) + # nolint
  (`Previous social movements` = movements) + # nolint
  (`Private land area (ths. ha.)` = prop_ha_ths) ~ # nolint
  Mean  * Arguments(fmt = "%.2f") +
  SD  * Arguments(fmt = "%.2f") +
  Min  * Arguments(fmt = "%.2f") +
  Max  * Arguments(fmt = "%.2f") +
  1,
  fmt = 0,
  data = peru_lr)

# Table 2

# Create vector of variables
vars_table2 <- c("population_1972ths", "d_roads_1973",
  "lemployees1961", "elev_1k", "slope", "cult_land",
  "superficie_km2_100", "inside_mita", "movements", "prop_ha_ths")

for (var in vars_table2) {
  summary(rdrobust(
    peru_lr_rd[[var]],
    peru_lr_rd$core_dist_kmio,
    p = 1,
    vce = "nn",
    cluster = peru_lr_rd$depcode,
    bwselect = "msetwo"))
}

# Table 4: RD Effect of Land Reform on Conflict Events

vars_table4 <- c("totalevents", "deaths",
  "norenewalvacanciapost", "guerreventsdummy_over3")

# List of covariates for row 1 of table 4
covs_1 <- c("xcoord", "ycoord", "xycoord", "x2coord", "y2coord",
  "core2_near_x", "core2_near_y", "core2_fid_id2", "core2_fid_id3",
  "core2_fid_id4", "core2_fid_id5", "core2_fid_id6", "core2_fid_id7",
  "core2_fid_id15", "core2_fid_id16")

summary(rdrobust(
  peru_lr_rd$totalevents,
  peru_lr_rd$core_dist_kmio,
  p = 1,
  vce = "nn",
  cluster = peru_lr_rd$depcode,
  bwselect = "msetwo"),
  all = TRUE,
  fuzzy = peru_lr_rd$landredist_pc,
  covs = covs_1)

# List of covariates for rows 2-5 of table 4 (and most tables)
covs_main <- c("lemployees", "population_1972ths", "d_roads_1973",
  "elev_1k", "slope", "cult_land", "inside_mita", "superficie_km2_100",
  "movements", "prop_ha_ths", "xcoord", "ycoord", "xycoord", "x2coord",
  "y2coord", "core2_near_x", "core2_near_y", "core2_fid_id2", "core2_fid_id3",
  "core2_fid_id4", "core2_fid_id5", "core2_fid_id6", "core2_fid_id7",
  "core2_fid_id15", "core2_fid_id16")

for (var in vars_table4) {
  summary(rdrobust(
    peru_lr_rd[[var]],
    peru_lr_rd$core_dist_kmio,
    p = 1,
    vce = "nn",
    cluster = peru_lr_rd$depcode,
    bwselect = "msetwo"),
    all = TRUE,
    fuzzy = peru_lr_rd$landredist_pc,
    covs = covs_main)
}

# Table 5: Placebo Tests

# a) Testing various placebo cutoffs

cutoff <- c(-15, -5, -2, 0, 2, 5, 15)

for (i in cutoff) {
  summary(rdrobust(
    peru_lr_rd$totalevents,
    peru_lr_rd$core_dist_kmio,
    p = 1,
    vce = "nn",
    cluster = peru_lr_rd$depcode,
    bwselect = "msetwo"),
    all = TRUE,
    fuzzy = peru_lr_rd$landredist_pc,
    covs = covs_main,
    c = cutoff[i])
}

# b) Testing uncultivable land

summary(rdrobust(
  peru_lr_rd$totalevents,
  peru_lr_rd$core_dist_kmio,
  p = 1,
  vce = "nn",
  cluster = peru_lr_rd$depcode,
  bwselect = "msetwo"),
  all = TRUE,
  fuzzy = peru_lr_rd$landredist_pc,
  covs = covs_main)

# c) Testing Ag Zone periphery/periphery boundary

peru_lr_agzone <- peru_lr %>%
  filter(agrozone == 2)

covs_2 <- c("lemployees", "population_1972ths", "d_roads_1973", "elev_1k",
"slope", "cult_land", "superficie_km2_100", "prop_ha_ths", "xcoord", "ycoord",
"xycoord", "x2coord", "y2coord", "plac_distseg1_agrozone2_near_x",
"plac_distseg1_agrozone2_near_y")

summary(rdrobust(
  peru_lr_agzone$totalevents,
  peru_lr_agzone$plac_dist_agzone2seg1_kmio,
  p = 1,
  vce = "nn",
  # cluster = peru_lr_agzone$depcode,
  bwselect = "msetwo"),
  all = TRUE,
  fuzzy = peru_lr_agzone$landredist_pc,
  covs = covs_2)

# Table 6: Mechanisms


# Panel A: Counterinsurgency and Government Strategy


# Panel B: Civilian Organization


# Panel C: Ideological Competition


# Panel D: Opportunity Costs
