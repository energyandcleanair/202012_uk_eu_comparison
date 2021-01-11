require(rcrea)
require(dplyr)
require(ggplot2)
require(rdefra)
require(zoo)
require(revgeo)
require(openxlsx)
require(RColorBrewer)
require(sf)
require(ggrepel)
require(highcharter)
require(magrittr)

# Directories -------------------------------------------------------------


dir_results <- "results"
dir.create(dir_results, showWarnings = F)

dir_results_plots <- file.path(dir_results, "plots")
dir.create(dir_results_plots, showWarnings = F, recursive = T)

dir_results_data <- file.path(dir_results, "data")
dir.create(dir_results_data, showWarnings = F, recursive = T)



# Cities ------------------------------------------------------------------

country_uk <- "GB"
city_uk <- c(
  "Aberdeen","Aldershot","Barnsley","Basildon","Belfast","Birkenhead","Birmingham","Blackburn","Blackpool","Bournemouth","Bradford","Brighton","Bristol","Burnley","Cambridge","Cardiff","Chatham","Coventry","Crawley","Derby","Doncaster","Dundee","Edinburgh","Exeter","Glasgow","Gloucester","Huddersfield","Hull","Ipswich","Leeds","Leicester","Liverpool","London","Luton","Manchester","Mansfield","Middlesbrough","Milton Keynes","Newcastle","Newport","Northampton","Norwich","Nottingham","Oxford","Peterborough","Plymouth","Portsmouth","Preston","Reading","Sheffield","Slough","Southampton","Southend","Stoke","Sunderland","Swansea","Swindon","Telford","Wakefield","Warrington","Wigan","Worthing","York"
)

city_uk_aurn <- c(
  "Aberdeen","Aldershot","Barnsley","Basildon","Belfast","Birkenhead","Birmingham","Blackburn with Darwen","Blackpool","Bournemouth","Bradford","Brighton and Hove","Bristol","Burnley","Cambridge","Cardiff","Chatham","Coventry","Crawley","Derby","Doncaster","Dundee","Edinburgh","Exeter","Glasgow","Gloucester","Huddersfield","Kingston upon Hull","Ipswich","Leeds","Leicester","Liverpool","London","Luton","Manchester","Mansfield","Middlesbrough","Milton Keynes","Newcastle upon Tyne","Newport","Northampton","Norwich","Nottingham","Oxford","Peterborough","Plymouth","Portsmouth","Preston","Reading","Sheffield","Slough","Southampton","Southend-on-Sea","Stoke on Trent","Sunderland","Swansea","Swindon","Telford and Wrekin","Wakefield","Warrington","Wigan","Worthing","York"
)

# city_uk <- rcrea::cities(source="aurn") %>% distinct(name) %>% pull()



country_eu <- NULL
city_eu <- c(
  "Amsterdam","Athens","Barcelona","Berlin","Bratislava","Brussels","Bucharest","Budapest",
  "Copenhagen","Dublin","Hamburg","Helsinki","Lisbon","Ljubljana","Luxemburg",
  "Madrid","Milan","Munich","Nicosia","Paris","Prague","Riga","Rome","Sofia","Stockholm",
  "Tallinn","The Hague","Valletta","Vienna","Vilnius","Warsaw","Zagreb","Oslo"
)


# Chart parameters --------------------------------------------------------

chart_width <- 8
chart_height <- 9
