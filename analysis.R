source('config.R')
source('utils.R')
source('data.R')
source('plots.R')
source('tables.R')

# Get measurements (incl. deweathered) ------------------------------------
m_uk <- data.meas_city_uk(source="aurn", city=city_uk_aurn, use_cache=T)
m_eu <- data.meas_city_eu(use_cache=T)

# Quality checks
# Only keep stations with enough measurements
m_uk %<>% utils.quality_check(min_count=330)
m_eu %<>% utils.quality_check(min_count=330)

cities_uk_ok <- unique(m_uk$location_name)

m <- rbind(m_uk, m_eu)

# Stations per city -------------------------------------------------------

s.uk <- data.stations_per_cities_uk(source="aurn", city=unique(m_uk$location_name), export=T)
s.eu <- data.stations_per_cities_eu(export=T)


# Reduction tables ---------------------------------------------------------

t.red.all <- tables.reductions(m, "all", export=T)
t.red.background <- tables.reductions(m, "background", export=T)
t.red.background %>% group_by(source) %>% summarise(relative=mean(relative), count=n())


m.impact <- utils.lockdown_impact(m)
tables.reductions_deweathered(m.impact, "all", export=T)
t.red.dew.background <- tables.reductions_deweathered(m.impact, "background", export=T)
t.red.dew.background %>% group_by(source) %>% summarise(value=mean(value, na.rm=T), count=n())


# Average tables ---------------------------------------------------------

tables.average(m, "all", export=T)
tables.average(m, "background", export=T)

# Plots: Averaged levels ---------------------------------------------------

plot.bar.average(m,"all")
plot.bar.average(m,"background")

plots.bar.reduction(m, "all")
plots.bar.reduction(m, "background")

plots.bar.reduction.deweathered(m.impact, "all")
plots.bar.reduction.deweathered(m.impact, "background")


# Violations -----------------------------------------------------
violations.all <- data.violations(level="city", city=cities_uk_ok,
                                  location_type="all", use_cache=F)
plots.bar.violations(violations.all, location_type="all", org="WHO")
plots.bar.violations(violations.all, location_type="all", org="UK")

violations.background <-  data.violations(level="city", city=cities_uk_ok,
                                           location_type="background", use_cache=F)
plots.bar.violations(violations.background, location_type="background", org="WHO")
plots.bar.violations(violations.background, location_type="background", org="UK")


# Time series charts ------------------------------------------------------

plot.ts(m %>% filter(source=="defra"), 30,
                  "anomaly_gbm_lag1_city_background_mad_pbl",
                  polls="no2", filename="ts.uk.anomaly.background.png")

plot.ts(m %>% filter(source=="defra"), 30,
                  "anomaly_gbm_lag1_city_mad_pbl",
                  polls="no2", filename="ts.uk.anomaly.png")





# Health ------------------------------------------------------------------
#
# health.impact <- utils.health_impact(m)
# health.impact.simplified <- rcrea::health.simplify(health.impact)
