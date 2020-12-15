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
