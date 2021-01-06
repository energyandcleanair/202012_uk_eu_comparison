source('config.R')
source('utils.R')
source('data.R')


# Get measurements (incl. deweathered) ------------------------------------

m_uk <- data.meas_city_uk(use_cache=T)
m_eu <- data.meas_city_eu(use_cache=T)
m <- rbind(m_uk, m_eu)


# Stations per city -------------------------------------------------------

s.uk <- data.stations_per_cities_uk(export=T)
s.eu <- data.stations_per_cities_eu(export=T)


# Reduction tables ---------------------------------------------------------

t.red.all <- tables.reductions(m, "all", export=T)
t.red.background <- tables.reductions(m, "background", export=T)

m.impact <- utils.lockdown_impact(m)
tables.reductions_deweathered(m.impact, "all", export=T)
tables.reductions_deweathered(m.impact, "background", export=T)


# Plots: Averaged levels ---------------------------------------------------

plot.bar.average(m,"all")
plot.bar.average(m,"background")

plots.bar.reduction(m, "all")
plots.bar.reduction(m, "background")

plots.bar.reduction.deweathered(m.impact, "all")
plots.bar.reduction.deweathered(m.impact, "background")


# Violations -----------------------------------------------------
violations.all <- utils.violations("all")
plots.bar.violations(violations.all, location_type="all", org="WHO")
plots.bar.violations(violations.all, location_type="all", org="UK")

violations.background <- utils.violations("background")
plots.bar.violations(violations.background, location_type="background", org="WHO")
plots.bar.violations(violations.background, location_type="background", org="UK")


# Time series charts ------------------------------------------------------

plot.ts(m %>% filter(source=="defra"), 30,
                  "anomaly_gbm_lag1_city_background_mad_pbl",
                  polls="no2", filename="ts.uk.anomaly.background.png")

plot.ts(m %>% filter(source=="defra"), 30,
                  "anomaly_gbm_lag1_city_mad_pbl",
                  polls="no2", filename="ts.uk.anomaly.png")



# Observed difference in 2020 -------------------------------------------------

# Lockdown impact in 2020 -------------------------------------------------




# Health ------------------------------------------------------------------
#
# health.impact <- utils.health_impact(m)
# health.impact.simplified <- rcrea::health.simplify(health.impact)
