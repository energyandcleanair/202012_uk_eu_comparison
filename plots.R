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

plots.health_impact_bar <- function(his, indicator="deaths"){

  legend <- list(
    "deaths"="Avoided deaths"
  )

  his.long <- his %>%
    tidyr::pivot_longer(cols=-location_id, names_to="indicator") %>%
    filter(indicator %in% !!indicator)

  ggplot(his.long) +
    geom_bar(aes(x=value,y=location_id,group=indicator,fill=indicator), stat="identity") +
    labs(x=legend[[indicator]],
         y=NULL,
         title="Avoided health impact")

}
