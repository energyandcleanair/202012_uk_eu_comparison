tables.reductions <- function(m, location_type, export=T){

  get_reduction <- function(m, process_id){
   m %>%
      filter(process_id==!!process_id,
             lubridate::year(date) %in% c(2019,2020)) %>%
      group_by(country, poll, unit, location_name, source,year=lubridate::year(date)) %>%
      summarise(value=mean(value, na.rm=T)) %>%
      group_by(country, poll, unit, location_name, source) %>% # Not necessary but safer
      arrange(year) %>%
      mutate(relative=(value-lag(value))/lag(value)) %>%
      filter(year==2020) %>%
      ungroup() %>%
      select(-c(value)) %>%
      arrange(relative) %>%
      mutate(relative_pct=scales::percent(relative,accuracy = 1)) %>%
      select(country, source, poll, unit, year, location_name, relative_pct, relative)
  }

  process_id <- switch(location_type,
                       "background"="city_background_day_mad",
                       "all"="city_day_mad")

  m.reduction <- get_reduction(m, process_id)

  if(export){
    write.csv(m.reduction %>% filter(country=="GB"),
              file.path(dir_results_data, paste0("reductions.observed.uk.",location_type,".csv")), row.names = F)
    write.csv(m.reduction %>% filter(country!="GB"),
              file.path(dir_results_data, paste0("reductions.observed.eu.",location_type,".csv")), row.names = F)
  }

  return(m.reduction)
}

tables.average <- function(m, location_type, export=T){

  process_id <- switch(location_type,
                       "background"="city_background_day_mad",
                       "all"="city_day_mad")

  m.average <- m %>%
    filter(process_id==!!process_id,
           lubridate::year(date) %in% c(2020)) %>%
    group_by(country, poll, unit, location_name, source, year=lubridate::year(date)) %>%
    summarise(value=mean(value, na.rm=T)) %>%
    arrange(desc(value))


  if(export){
    write.csv(m.average %>% filter(country=="GB"),
              file.path(dir_results_data, paste0("average.observed.uk.",location_type,".csv")), row.names = F)
    write.csv(m.average %>% filter(country!="GB"),
              file.path(dir_results_data, paste0("average.observed.eu.",location_type,".csv")), row.names = F)
  }

  return(m.average)
}


tables.reductions_deweathered <- function(m.impact, location_type, export=T){

  m.reduction <- m.impact %>%
    filter(type==location_type) %>%
    arrange(value) %>%
    mutate(value_pct=scales::percent(value,accuracy = 1))

  if(export){
    write.csv(m.reduction %>% filter(country=="GB"),
              file.path(dir_results_data, paste0("reductions.deweathered.uk.",location_type,".csv")), row.names = F)
    write.csv(m.reduction %>% filter(country!="GB"),
              file.path(dir_results_data, paste0("reductions.deweathered.eu.",location_type,".csv")), row.names = F)
  }

  return(m.reduction)
}

