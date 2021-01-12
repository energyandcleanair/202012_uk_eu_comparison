plots.bar.reduction <- function(m, location_type=NULL,
                                width=chart_width, height=chart_height){

  get_reduction <- function(m, process_id){
    m %>%
      filter(process_id==!!process_id,
             lubridate::year(date) %in% c(2019,2020)) %>%
      group_by(poll, unit, location_name, country, source,year=lubridate::year(date)) %>%
      summarise(value=mean(value, na.rm=T)) %>%
      group_by(poll, unit, location_name, source, country) %>% # Not necessary but safer
      arrange(year) %>%
      mutate(relative=(value-lag(value))/lag(value)) %>%
      filter(year==2020) %>%
      ungroup()
  }

  process_id <- switch(location_type,
                       "background"="city_background_day_mad",
                       "city_day_mad")

  m.plot <- get_reduction(m, process_id) %>%
    mutate(region=recode(country, "GB"="Great Britain", .default="Other"))

  m.plot$location_name <- reorder(m.plot$location_name, -m.plot$relative)

  plt <- ggplot(m.plot %>% filter(!is.na(relative))) +
    geom_bar(aes(relative, location_name, fill=region), stat="identity", position="dodge")

  caption_suffix=recode(location_type,
                        "all"="",
                        "background"="Background stations only.")

  plt <- plt +
    scale_x_continuous(labels=scales::percent) +
    theme_crea(legend.position="bottom") +
    rcrea::CREAtheme.scale_fill_crea_d(name=NULL) +
    labs(y=NULL, x=NULL,
         # title="NO2 levels in 2020"
         # subtitle="Observed NO2 levels in 2020 vs 2019",
         caption=paste("Source: CREA analysis based on European Environment Agency and DEFRA.",
                       caption_suffix)) +
    guides(fill = guide_legend(nrow = 1))


  ggsave(plot=plt,
         filename = file.path(dir_results_plots,
                              paste(c("bar","no2","reduction",
                                      location_type,"png"),collapse=".")),
         width=width,
         height=height)


}

plots.bar.reduction.deweathered <- function(m.impact, location_type=NULL,
                                            width=chart_width, height=chart_height){

  m.plot <- m.impact %>%
    mutate(type.city=paste(type,location_name,sep="-"),
           region=recode(country, "GB"="Great Britain", .default="Other"))

  m.plot$type.city <- reorder(m.plot$type.city, -m.plot$value)

  if(!is.null(location_type)){
    m.plot <- m.plot %>% filter(type==location_type)
  }

  plt <- ggplot(m.plot %>% filter(!is.na(value))) +
    geom_bar(aes(value, type.city, fill=region), stat="identity", position="dodge")

  if(!is.null(location_type)){
    caption_suffix=recode(location_type,
                          "all"="",
                          "background"="Background stations only.")
  }else{
    plt <- plt + facet_wrap(~type, scales="free_y")
    caption_suffix=""
  }

  plt <- plt +
    scale_y_discrete(labels=function(x){gsub("all-|background-","",x)}) +
    scale_x_continuous(labels=scales::percent) +
    theme_crea(legend.position="bottom") +
    rcrea::CREAtheme.scale_fill_crea_d(name=NULL) +
    labs(y=NULL, x=NULL,
         # title="NO2 levels in 2020"
         # subtitle="Difference between observed and predicted NO2 levels in 2020",
         caption=paste("Source: CREA analysis based on European Environment Agency and DEFRA.",
                       caption_suffix)) +
    guides(fill = guide_legend(nrow = 1))


  ggsave(plot=plt,
         filename=file.path(dir_results_plots,
                            paste(c("bar","no2","reduction","deweathered",
                                    location_type,"png"),collapse=".")),
         width=width,
         height=height)
}


plots.bar.violations <- function(violations, location_type="background", org="WHO",
                                 width=chart_width, height=chart_height){

  violations %<>% ungroup()
  cols <- c(no2=NA,so2=NA,pm25=NA,pm10=NA,o3=NA)

  v.plot <- violations %>%
    group_by(location_name, poll, organization,
             year=lubridate::year(date),
             date=lubridate::date(date)) %>%
    summarise(violation=sum(violation)>0) %>%
    ungroup() %>%
    filter(violation) %>%
    tidyr::spread("poll","violation") %>%
    tibble::add_column(!!!cols[!names(cols) %in% names(.)]) %>%
    rowwise() %>%
    mutate_at(c("no2", "o3","pm10","pm25","so2"), tidyr::replace_na, replace=0) %>%
    mutate(poll=ifelse(no2+o3+pm10+pm25+so2==1,
                       c("NO2", "Ozone","PM10","PM2.5","SO2")[as.logical(c(no2,o3,pm10,pm25,so2))],
                       "Multiple")) %>%
    group_by(location_name, organization, year, poll) %>%
    summarise(n_violations=n())

  v.plot %>% tidyr::spread("year","n_violations")
  v.plot.2020 <- v.plot %>% filter(year==2020, organization==!!org)
  v.plot.2019 <- v.plot %>% filter(year==2019, organization==!!org)

  v.plot.2020.total <- v.plot.2020 %>%
    group_by(location_name) %>%
    summarise(n_violations=sum(n_violations))

  lvs <- levels(reorder(v.plot.2020.total$location_name, v.plot.2020.total$n_violations))

  v.plot.2019.total <- v.plot.2019 %>%
    group_by(location_name) %>%
    summarise(n_violations=sum(n_violations)) %>%
    filter(location_name %in% lvs)


  v.plot.2019.label <- v.plot.2019.total %>% filter(location_name=="Manchester")

  plt <- ggplot() +
    geom_point(data=v.plot.2019.total %>%
                 filter(!is.na(location_name)),
               aes(y=location_name, x=n_violations)) +
    geom_bar(data=v.plot.2020,
             aes(y=location_name, x=n_violations, fill=poll),
             stat="identity") +
    ggrepel::geom_text_repel(data=v.plot.2019.label,
                             aes(n_violations, location_name),
                             box.padding = 1.5,
                             nudge_x=2,
                             nudge_y=-1,
                             label="2019 value") +
    labs(x="Number of days above WHO guidelines",
         y=NULL) +
    rcrea::theme_crea() +
    scale_y_discrete(breaks=lvs, labels=lvs) +
    scale_fill_brewer(palette="Spectral") +
    theme(panel.grid.major.y = element_line("grey95"),
          legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 1, reverse = T, title = "Responsible pollutant(s)"))

  f <- file.path(dir_results_plots,
                 paste0(c("bar","uk","violations",tolower(org),location_type,"png"),
                        collapse="."))

  ggsave(f,plt,
         width=width,
         height=height)

  return(plt)

}


plot.bar.average <- function(m, location_type="background", filename=NULL,
                             width=chart_width, height=chart_height){

  process_id <- ifelse(location_type=="background",
                       "city_background_day_mad",
                       "city_day_mad")

  m.plot <- m %>%
    filter(process_id==!!process_id,
           poll=="no2",
           lubridate::year(date) %in% c(2019,2020)) %>%
    mutate(region=recode(country, "GB"="Great Britain", .default="Other")) %>%
    group_by(location_name, year=lubridate::year(date), poll, unit, region) %>%
    summarise(value=mean(value, na.rm=T)) %>%
    filter(!is.na(value))

  m.plot$location_name %<>% factor(levels=m.plot %>% filter(year==2020) %>% arrange(value) %>% pull(location_name))

  m.plot.2020 <- m.plot %>% filter(year==2020)
  m.plot.2019 <- m.plot %>% filter(year==2019)
  m.plot.2019.label <- m.plot.2019 %>% filter(location_name=="Nottingham")

  plt <- ggplot() +
    geom_bar(data=m.plot.2020,
             aes(value, location_name, fill=region),
             stat="identity", position="dodge") +
    geom_point(data=m.plot.2019,
               aes(value, location_name), col="grey40")

  if(!is.null(location_type)){
    caption_suffix=recode(location_type,
                          "all"="",
                          "background"="Background stations only.")
  }else{
    plt <- plt + facet_wrap(~type, scales="free_y")
    caption_suffix=""
  }


  (plt <- plt +
      scale_y_discrete(labels=function(x){gsub("all-|background-","",x)}) +
      scale_x_continuous(expand=expansion(mult=c(0,0.1))) +
      theme_crea() +
      theme(panel.grid.major.y = element_line(colour="grey95"),
            panel.grid.major.x = element_line(colour="grey95")) +
      geom_vline(xintercept = 20, col="darkorange", linetype="dashed") +
      geom_vline(xintercept = 40, col="darkorange", linetype="dashed") +
      ggrepel::geom_text_repel(data=m.plot.2019.label,
                               aes(value, location_name),
                               box.padding = 1.5,
                               label="2019 value") +

      geom_text(data=tibble(x=c(20.5), y=c(6),
                            label=c("WHO Guideline\n[20µg/m3]")),
                aes(x,y,label=label),
                col="darkorange",
                hjust=0) +
      geom_text(data=tibble(x=c(39.5), y=c(15),
                            label=c("EU Limit\n[40µg/m3]")),
                aes(x,y,label=label),
                col="darkorange",
                hjust=1) +
      rcrea::CREAtheme.scale_fill_crea_d(name=NULL) +
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(nrow = 1)) +
      labs(y=NULL, x=NULL,
           # title="NO2 levels in 2020"
           # subtitle="Average NO2 levels in 2020 and 2019",
           caption=paste("Source: CREA analysis based on European Environment Agency and DEFRA.",
                         caption_suffix)))

  if(is.null(filename)){
    filename <- paste0("bar.no2.average",
                       ifelse(!is.null(location_type),
                              paste0(".", location_type),
                              ""),
                       ".png")
  }

  ggsave(plot=plt,
         filename=file.path(dir_results_plots,
                            filename),
         width=width,
         height=height)
}

plot.ts <- function(m, running_days, process_id, polls, filename=NULL,
                    folder=dir_results_plots, color_by="value",
                    width=chart_width, height=chart_height, nrow=NULL, ...){


  for(poll in polls){
    plt <- rcrea::plot_recents(meas_raw=m %>% select(-c(geometry)),
                               running_days=running_days,
                               process_id=process_id,
                               source="eea",
                               subfile_by="poll",
                               color_by=color_by,
                               subplot_by="location_name",
                               poll=poll,
                               ...
    ) + facet_wrap(~location_name, nrow=nrow)

    # Adding lockdown stages

    lockdown_stages <- rcrea::utils.lockdown_stages(unique(m$country))

    l <- m %>% select(-c(geometry)) %>%
      distinct(location_name, country) %>%
      left_join(lockdown_stages)

    # We want to add lockdown stages behind curves
    `-.gg` <- function(plot, layer) {
      if (missing(layer)) {
        stop("Cannot use `-.gg()` with a single argument. Did you accidentally put - on a new line?")
      }
      if (!is.ggplot(plot)) {
        stop('Need a plot on the left side')
      }
      plot$layers = c(layer, plot$layers)
      plot
    }

    if(color_by=="year"){
      l <- l %>% mutate(
        date_from='year<-'(date_from,0),
        date_to='year<-'(date_to,0))
      min_date <- as.POSIXct("0000-01-01", tz="UTC")
      max_date <- as.POSIXct("0001-01-01", tz="UTC")
    }else{
      min_date <- as.POSIXct("2020-01-01", tz="UTC")
      max_date <- as.POSIXct("2021-01-01", tz="UTC")
    }

    plt <- plt - geom_rect(data=l %>% filter(indicator=="lockdown", !is.na(date_from)),
                           aes(xmin=date_from, xmax=date_to, ymin=-Inf, ymax=+Inf,
                               fill=tools::toTitleCase(indicator)),
                           inherit.aes = F,
                           alpha=0.3) +
      scale_x_datetime(
        limits=c(min_date, max_date),
        breaks = seq(min_date,
                     max_date,
                     by="3 month"),
        minor_breaks = seq(min_date,
                           max_date,
                           by="1 month"),
        date_labels = "%b") +
      scale_fill_manual(name=NULL, values=c("orange","red")) +
      theme(panel.grid.minor = element_line("grey95"),
            panel.grid.major = element_line("grey60"),
            legend.position = "bottom")

    print(plt)

    if(!is.null(filename)){
      ggsave(filename=file.path(folder,gsub("\\{poll\\}",poll,filename)),
             plot=plt,
             width=width,
             height=height)
    }
  }
}



plot.combined_ts_hc <- function(m, running_days, process_id, filename="combined_ts.html", folder=dir_results_plots){

  m.plot <- m %>%
    filter(process_id==!!process_id) %>%
    rcrea::utils.running_average(running_days) %>%
    mutate(date=lubridate::date(date)) %>%
    select(location_name, date, value)
  (plt <- hchart(m.plot, "line", hcaes(x = date, y = value, group = location_name)))


  htmltools::browsable(plt) # print
  htmltools::save_html(plt, file.path(normalizePath(folder), filename)) # save
}


plots.map_lockdown_impact <- function(m.impact, location_type=NULL){

  m.plot <- sf::st_as_sf(m.impact) %>%
    filter(is.null(location_type) | type==location_type) %>%
    bind_cols(st_coordinates(.) %>% data.frame) %>%
    filter(country=="GB")

  plt <- ggplot(m.plot) +
    geom_sf(aes(col=value)) +
    scale_fill_continuous(labels=scales::percent) +
    geom_text_repel(aes(X,Y,label = location_name),
                    alpha = 0.75)

}


plot.measurements_w_transport <- function(m, t, running_days, process_id, polls,
                                          filename=NULL, folder=dir_results_plots, color_by="value",
                                          width=chart_width, height=chart_height, ...){

  for(poll in polls){

    poll_str <- rcrea::poll_str(poll)

    m.running <- m %>%
      sel(-c(geometry)) %>%
      filter(poll==!!poll,
             process_id==!!process_id) %>%
      rcrea::utils.running_average(running_days) %>%
      mutate(type="pollution",
             source=poll_str,
             date=lubridate::date(date))

    t.running <- t %>% rename(value=traffic) %>%
      mutate(type="traffic") %>%
      rcrea::utils.running_average(running_days)

    ggplot(m.running) +
      geom_line(data=m.running, aes(date, value), col="darkred") +
      geom_line(data=t.running, aes(date, value, col=source), linetype="dotted") +
      facet_wrap(~location_name) +
      scale_y_continuous(labels = scales::percent) +
      scale_color_manual(values=c("darkorange","darkred","darkred"),
                         name=NULL) +
      scale_linetype_manual(values=c("solid","dotted"), name=NULL) +
      theme_crea() +
      labs(title=paste0(poll_str," pollutant levels vs. traffic indexes"),
           y=NULL, x=NULL,
           caption="Source: CREA based on European Environment Agency, TomTom, and Apple Mobility.")




    # # Adding lockdown stages
    # lockdown_stages <- rcrea::utils.lockdown_stages(unique(m$country))

    l <- m %>% select(-c(geometry)) %>%
      distinct(location_name, country) %>%
      left_join(lockdown_stages)

    # We want to add lockdown stages behind curves
    `-.gg` <- function(plot, layer) {
      if (missing(layer)) {
        stop("Cannot use `-.gg()` with a single argument. Did you accidentally put - on a new line?")
      }
      if (!is.ggplot(plot)) {
        stop('Need a plot on the left side')
      }
      plot$layers = c(layer, plot$layers)
      plot
    }

    if(color_by=="year"){
      l <- l %>% mutate(
        date_from='year<-'(date_from,0),
        date_to='year<-'(date_to,0))
      min_date <- as.POSIXct("0000-01-01", tz="UTC")
      max_date <- as.POSIXct("0001-01-01", tz="UTC")
    }else{
      min_date <- as.POSIXct("2020-01-01", tz="UTC")
      max_date <- as.POSIXct("2021-01-01", tz="UTC")
    }

    plt <- plt - geom_rect(data=l %>% filter(indicator=="lockdown", !is.na(date_from)),
                           aes(xmin=date_from, xmax=date_to, ymin=-Inf, ymax=+Inf,
                               fill=tools::toTitleCase(indicator)),
                           inherit.aes = F,
                           alpha=0.3) +
      scale_x_datetime(
        limits=c(min_date, max_date),
        breaks = seq(min_date,
                     max_date,
                     by="3 month"),
        minor_breaks = seq(min_date,
                           max_date,
                           by="1 month"),
        date_labels = "%b") +
      scale_fill_manual(name=NULL, values=c("orange","red")) +
      theme(panel.grid.minor = element_line("grey95"),
            panel.grid.major = element_line("grey60"),
            legend.position = "bottom")

    print(plt)

    if(!is.null(filename)){
      ggsave(filename=file.path(folder,gsub("\\{poll\\}",poll,filename)),
             plot=plt,
             width=width,
             height=height,
             scale=1.5)
    }
  }
}


plot.combined_ts_hc <- function(m, running_days, process_id, filename="combined_ts.html", folder=dir_results_plots,
                                width=chart_width, height=chart_height){

  m.plot <- m %>%
    filter(process_id==!!process_id) %>%
    rcrea::utils.running_average(running_days) %>%
    mutate(date=lubridate::date(date)) %>%
    select(location_name, date, value)
  (plt <- hchart(m.plot, "line", hcaes(x = date, y = value, group = location_name)))


  htmltools::browsable(plt) # print
  htmltools::save_html(plt, file.path(normalizePath(folder), filename)) # save
}



plots.consumption <- function(d.consumption, width=6, height=3, export=T){

  d.plot <- d.consumption %>%
    filter(quarter %in% seq(1,3),
           year %in% c(2019,2020)) %>%
    group_by(year, sector, indicator, legend, unit) %>%
    summarise(value=sum(value)) %>%
    group_by(sector, indicator, legend, unit) %>%
    arrange(year) %>%
    mutate(change=value/lag(value)-1,
           change_str=scales::percent(change, 1)) %>%
    filter(year==2020,
           legend %in% c("Oil use (transportation)",
                         "Coal consumption",
                         "Gas consumption"))

  plt <- ggplot(d.plot, aes(y=legend, x=change)) +
    geom_bar(stat="identity", aes(fill=legend), show.legend = F) +
    geom_text(aes(label=change_str), nudge_x = -0.02) +
    theme_crea() +
    scale_x_continuous(labels=scales::percent) +
    theme(panel.grid.major = element_blank()) +
    rcrea::CREAtheme.scale_fill_crea_d() +
    labs(y=NULL, x = "Y-o-y variation",
         caption="Source: BEIS")

  if(export){
    ggsave(file.path(dir_results_plots,
                     "uk.consumption.png"),
           plot=plt,
           width=width, height=height)
  }
}


plots.bar.reduction.transport <- function(d, source="apple",
                                width=chart_width, height=6){


  d.plot <- d %>%
    mutate(region=ifelse(country=="GB","Great Britain", "Other")) %>%
    filter(source==!!source) %>%
    filter(!is.na(change))

  d.plot$city <- reorder(d.plot$city, -d.plot$change)

  caption <- recode(source,
                    "apple"="Source: Apple Mobility Trends. 2020 average of daily requests for directions compared to 13 January 2020",
                    "tomtom"="Source: TomTom Traffic Congestion Index 2020 vs 2019")

  (plt <- ggplot(d.plot) +
    geom_bar(aes(change, city, fill=region), stat="identity", position="dodge") +
    scale_x_continuous(labels=scales::percent) +
    theme_crea(legend.position="bottom") +
    rcrea::CREAtheme.scale_fill_crea_d(name=NULL) +
    labs(y=NULL, x=NULL,
         # title="NO2 levels in 2020"
         # subtitle="Observed NO2 levels in 2020 vs 2019",
         caption=caption) +
    guides(fill = guide_legend(nrow = 1)))


  ggsave(plot=plt,
         filename = file.path(dir_results_plots,
                              paste0("bar.transportation.",source,".png")),
         width=width,
         height=height)


}
