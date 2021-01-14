
#########################################################################################
### Required Libraries.R and Params.R to be loaded first to get the value of state      #
#########################################################################################
### Uncomment the below line if ran within loading params.R
#source("params.R")


### Safegraph

####Fulltime
sg_mob_ft <- suppressMessages(
              covidcast_signal(data_source = "safegraph", signal = "full_time_work_prop",
                                start_day = start, end_day = end,
                                geo_type = "county",
                                geo_values = dfw_fips)
              )

#saveRDS(sg_mob_ft, "sg_mob_ft.rds")

saveRDS(sg_mob_ft, paste0(path, "_sg_mob_ft.rds"))

#### Partime
sg_mob_pt <- suppressMessages(
              covidcast_signal(data_source = "safegraph", signal = "part_time_work_prop",
                                start_day = start, end_day = end,
                                geo_type = "county",
                                geo_values = dfw_fips)
              )

#saveRDS(sg_mob_pt, "sg_mob_pt.rds")

saveRDS(sg_mob_pt, paste0(path, "_sg_mob_pt.rds"))
