source('./config.R')


# Run deweathering (on server) --------------------------------------------

creadeweather::deweather(city=city_uk, source="defra", poll=c(rcrea::NO2,rcrea::PM25,rcrea::PM10,rcrea::O3))
creadeweather::deweather(city=city_eu, source="eea", poll=c(rcrea::NO2,rcrea::PM25,rcrea::PM10,rcrea::O3))

# Get measurements (incl. deweathered) ------------------------------------

m_uk <- rcrea::measurements(city=city_uk, source="defra", poll="no2", deweathered = NULL)
m_eu <- rcrea::measurements(city=city_eu, source="eea", poll="no2", deweathered = NULL)

l_uk <- rcrea::locations("city", source="defra", with_geometry = T)
l_eu <- rcrea::locations("city", source="eea", with_geometry = T)

m <- rbind(
  m_uk %>% left_join(l_uk %>% select(location_id=id, geometry)),
  m_eu %>% left_join(l_eu %>% select(location_id=id, geometry))
)



# Health ------------------------------------------------------------------
