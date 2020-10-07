start <- "2020-04-04"
end <- today()


### Getting Dallas Fort Worth Data

### Facebook
##################################################
#                                                #
##### Fb covid like symptoms #####################
fb_cli <- suppressMessages(
  covidcast_signal(data_source = "fb-survey", signal = "smoothed_wcli",
                   start_day = start, end_day = end,
                   geo_type = "county",
                   geo_values = dfw_fips)
)

saveRDS(fb_cli, "fb_cli.rds")


##################################################
#                                                #
##### Community covid like infection #############
fb_cmnty_cli <- suppressMessages(
  covidcast_signal(data_source = "fb-survey", signal = "smoothed_hh_cmnty_cli",
                   start_day = start, end_day = end,
                   geo_type = "county",
                   geo_values = dfw_fips)
)

saveRDS(fb_cmnty_cli, "fb_cmnty_cli.rds")

##################################################
#                                                #
##### Fb social connectedness index ##############

fb_sci <- read.table("../../covid19/social-connectedness-index/county_county_aug2020.tsv", sep = "\t", header = T)

fb_sci_dfw <- fb_sci %>% filter(user_loc %in% dfw_fips & fr_loc %in% dfw_fips)

saveRDS(fb_sci_dfw, "fb_sci_dfw.rds")


### Data Exploration

head(fb_sci_dfw)


#fb_sci_dfw %>% filter(user_loc==48085) %>% mutate(soc_cnx = filter(fr_loc==user_loc))

## Filtering values of reference columns or diagonal elements
fb_sci_self <- fb_sci_dfw %>% filter(user_loc==fr_loc)

fb_sci_self$tot <- fb_sci_dfw %>% 

## Setting the reference of each data point
fb_sci_dfw$ref <- lapply(fb_sci_dfw$user_loc, function(x) fb_sci_self[fb_sci_self$user_loc==x, "scaled_sci"]) %>% unlist()


## Calcularing the total connectedness (Total friends count per county)

tmp_df <- fb_sci_dfw %>% dplyr::group_by(user_loc) %>% summarise(Total=sum(scaled_sci), .groups="drop")


fb_sci_dfw <- left_join(fb_sci_dfw,tmp_df, by="user_loc")

## Relative connectedness (counties relatively to their reference)
fb_sci_dfw$rel_cnx <- fb_sci_dfw$scaled_sci/fb_sci_dfw$ref

## Connectedness proportion based on total county connectedness

fb_sci_dfw$rate_cnx <- fb_sci_dfw$scaled_sci/fb_sci_dfw$Total


fb_sci_dfw$GEOID <- as.character(fb_sci_dfw$fr_loc)




##################################################
#                                                #
############FB Movement range ####################

fb_mov <- read.table("../../covid19/fb-movement-range-data/movement-range-2020-10-01.txt", sep = "\t", header = T)
