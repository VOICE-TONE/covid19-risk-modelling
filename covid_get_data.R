start <- "2020-04-04"
end <- today()

### Getting Dallas Fort Worth Data

### Facebook
fb_cli <- suppressMessages(
          covidcast_signal(data_source = "fb-survey", signal = "smoothed_wcli",
                                start_day = start, end_day = end,
                                geo_type = "county",
                                geo_values = dfw_fips)
          )

#saveRDS(fb_cli, "fb_cli.rds")

saveRDS(fb_cli, "fb_cli0.rds")



fb_cmnty_cli <- suppressMessages(
          covidcast_signal(data_source = "fb-survey", signal = "smoothed_hh_cmnty_cli",
                           start_day = start, end_day = end,
                           geo_type = "county",
                           geo_values = dfw_fips)
          )

#saveRDS(fb_cmnty_cli, "fb_cmnty_cli.rds")

saveRDS(fb_cmnty_cli, "fb_cmnty_cli0.rds")


### Safegraph

####Fulltime
sg_mob_ft <- suppressMessages(
              covidcast_signal(data_source = "safegraph", signal = "full_time_work_prop",
                                start_day = start, end_day = end,
                                geo_type = "county",
                                geo_values = dfw_fips)
              )

saveRDS(sg_mob_ft, "sg_mob_ft.rds")


#### Partime
sg_mob_pt <- suppressMessages(
              covidcast_signal(data_source = "safegraph", signal = "part_time_work_prop",
                                start_day = start, end_day = end,
                                geo_type = "county",
                                geo_values = dfw_fips)
              )

saveRDS(sg_mob_pt, "sg_mob_pt.rds")