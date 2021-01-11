utils.lockdown_impact <- function(m){

  m.impact <- m %>% filter(process_id %in% c(
    "anomaly_gbm_lag1_city_mad_pbl",
    "anomaly_gbm_lag1_city_background_mad_pbl",
    "counterfactual_gbm_lag1_city_mad_pbl",
    "counterfactual_gbm_lag1_city_background_mad_pbl"),
    lubridate::year(date)==2020) %>%
    group_by(location_name, process_id, poll, source, country) %>%
    summarise(value=mean(value, na.rm=T)) %>%
    tidyr::spread("process_id","value") %>%
    mutate(
      impact_all=anomaly_gbm_lag1_city_mad_pbl/counterfactual_gbm_lag1_city_mad_pbl,
      impact_background=anomaly_gbm_lag1_city_background_mad_pbl/counterfactual_gbm_lag1_city_background_mad_pbl
    )

  m.impact %>% select(location_name, poll, source, country, impact_all, impact_background) %>%
    tidyr::pivot_longer(c(impact_all, impact_background), names_to="type", names_prefix="impact_")
}

utils.quality_check <- function(m, min_count=330){

  m.count <- m %>%
    filter(!is.na(value),
           process_id %in% c("city_background_day_mad","city_day_mad")) %>%
    group_by(location_name, process_id, year=lubridate::year(date),
             poll) %>%
    summarise(count=n(), max=max(value), min=min(value)) %>%
    arrange(count) %>%
    filter(year %in% seq(2017,2020)) %>%
    ungroup()

  m.count$ok <- (m.count$count > min_count)s

  cities_nonok <- unique(m.count[!m.count$ok,]$location_name)
  cities_ok <- unique(m.count[m.count$ok,]$location_name)
  print(paste(length(cities_nonok), "non valid cities: ", paste(cities_nonok, collapse=", ")))
  print(paste(length(cities_ok), "valid cities: ", paste(cities_ok, collapse=", ")))

  return(m %>%
           inner_join(m.count %>% filter(ok) %>% distinct(location_name, process_id)))

}

utils.health_impact <- function(m){

  m_scenarios <-
    rcrea::health.build.scenarios(m,
                                  process_anomaly="anomaly_gbm_lag1_city_mad_pbl",
                                  process_observation="city_day_mad")

  m_scenarios <- rcrea::utils.add_city_pop(m_scenarios)

  m_impact <- rcrea::health.impact(m_scenarios, date_from="2020-01-01", date_to="2020-12-31")
}

utils.violations.stations_to_cities <- function(m.violations.stations){
  # In this approach (not used in the report)
  # we collect violations per station and aggregate at the city level
  # A day with an exceedance at a single station is a day with an exceedance at the city level
  m.violations.city <-
    m.violations.stations %>%
    filter(exceedance) %>% # Shouldn't be necessary but just in case rcrea changes in the future
    group_by(city_name, frequency, date, standard_id, poll, threshold, organization, exceedance_allowed_per_year) %>%
    summarise(exceedance=max(exceedance)) %>%
    ungroup() %>%
    filter(exceedance==1) %>%
    group_by(year=lubridate::year(date), city_name, standard_id, poll, threshold, organization, frequency, exceedance_allowed_per_year) %>%
    arrange(date) %>%
    mutate(exceedance_this_year=row_number()) %>%
    mutate(violation=(exceedance_this_year>exceedance_allowed_per_year)) %>%
    filter(violation)

  return(m.violations.city)
}
