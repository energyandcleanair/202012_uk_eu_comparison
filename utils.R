utils.health_impact <- function(m){

  m_scenarios <-
    rcrea::health.build.scenarios(m,
                                  process_anomaly="anomaly_gbm_lag1_city_mad_pbl",
                                  process_observation="city_day_mad")

  m_scenarios <- rcrea::utils.add_city_pop(m_scenarios)

  m_impact <- rcrea::health.impact(m_scenarios, date_from="2020-01-01", date_to="2020-12-31")
}


