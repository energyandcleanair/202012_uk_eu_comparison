data.meas_city <- function(source, city, file, use_cache){

  process_id <- c(
    # Observed
    "city_day_mad",
    "city_background_day_mad",
    # Deweathered
    "anomaly_gbm_lag1_city_mad_pbl",
    "anomaly_gbm_lag1_city_background_mad_pbl",
    "counterfactual_gbm_lag1_city_mad_pbl",
    "counterfactual_gbm_lag1_city_background_mad_pbl"
  )

  if(!use_cache | !file.exists(file)){
    l <- rcrea::locations("city", source=source)
    m <- rcrea::measurements(city=city,
                                 source=source,
                                 poll="no2",
                                 process_id=process_id,
                                 date_from="2019-01-01",
                                 date_to="2020-12-31") %>%
      left_join(l %>% select(location_id=id))

    saveRDS(m, file)
    return(m)
  }else{
    readRDS(file)
  }
}


data.meas_city_uk <- function(source, city, use_cache=T){
  # Source can be either 'defra' or 'aurn'
  # Theoretically the same data but wasn't obtained
  # through the same means.
  # 'aurn' seems to have significantly fewer data gaps
  file <- file.path(dir_results_data,
                    paste("m",source,"city.uk.RDS",sep="."))
  data.meas_city(source=source, city=city,
                 file=file, use_cache=use_cache)
}


data.meas_city_eu <- function(use_cache=T){

  file <- file.path(dir_results_data,"m.eea.city.eu.RDS")
  data.meas_city(source="eea", city=city_eu,
                 file=file, use_cache=use_cache)
}


data.stations_per_cities <- function(source, city){

  rcrea::stations(source=source,
                       with_metadata = T,
                       keep_with_measurements_only = T,
                       city=city) %>%
    select(city_name, type, station_id=id) %>%
    arrange(city_name, type)
}


data.stations_per_cities_uk <- function(source="aurn", city=city_uk, export=T){
  s <- data.stations_per_cities(source, city)
  if(export){
    write.csv(s, file.path(dir_results_data, "stations.uk.csv"), row.names = F)
  }
  s
}


data.stations_per_cities_eu <- function(export=T){
  s <- data.stations_per_cities("eea", city_eu)
  if(export){
    write.csv(s, file.path(dir_results_data, "stations.eu.csv"), row.names = F)
  }
  s
}


data.violations <- function(level="city", city=city_uk_aurn, location_type="all", use_cache=T){

  file <- file.path(dir_results_data,
                    paste("violations",level,location_type,"csv", sep="."))

  if(!file.exists(file) | !use_cache){
    if(location_type=="all"){
      location_type <- NULL #"all" corresponds to NULL in rcrea package
    }

    v <- rcrea::violations(source="aurn",
                      level=level,
                      city=city,
                      date_from="2019-01-01",
                      date_to="2020-12-31",
                      location_type=location_type,
                      orgs=c("UK","WHO"))
    write.csv(v, file, row.names = F)
    return(v)
  }else(
    read.csv(file)
  )
}


data.consumption <- function(){
    d <- read.csv("data/uk_energy_2020.csv")
}



