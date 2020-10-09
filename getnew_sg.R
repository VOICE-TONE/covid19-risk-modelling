library(lubridate)
library(covidcast)

start <- "2020-04-04"
end <- today()


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