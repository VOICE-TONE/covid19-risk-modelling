#########################################################################################
### Required Libraries.R and Params.R to be loaded first to get the value of state      #
#########################################################################################
### Uncomment the below line if ran within loading params.R
#source("params.R")

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

#saveRDS(fb_cli, "data/fb_cli.rds")

saveRDS(fb_cli, paste0(path, "_fb_cli.rds"))


##################################################
#                                                #
##### Community covid like infection #############
fb_cmnty_cli <- suppressMessages(
  covidcast_signal(data_source = "fb-survey", signal = "smoothed_hh_cmnty_cli",
                   start_day = start, end_day = end,
                   geo_type = "county",
                   geo_values = dfw_fips)
)

#saveRDS(fb_cmnty_cli, "data/fb_cmnty_cli.rds")

saveRDS(fb_cmnty_cli, paste0(path,"_fb_cmnty_cli.rds"))


##################################################
#                                                #
##### Fb social connectedness index ##############

## path_sci is defined in the Params.R file

#### uncomment all the below to work with social connectedness


# fb_sci <- read.table(path_sci, sep = "\t", header = T)

# fb_sci_dfw <- fb_sci %>% filter(user_loc %in% dfw_fips & fr_loc %in% dfw_fips)

# saveRDS(fb_sci_dfw, paste0(path,"_fb_sci_dfw.rds"))


#### Uncommenting ends here



##################################################
### Data Exploration - Not part of model building#

# head(fb_sci_dfw)


#fb_sci_dfw %>% filter(user_loc==48085) %>% mutate(soc_cnx = filter(fr_loc==user_loc))

## Filtering values of reference columns or diagonal elements
#fb_sci_self <- fb_sci_dfw %>% filter(user_loc==fr_loc)

## Setting the reference of each data point
#fb_sci_dfw$ref <- lapply(fb_sci_dfw$user_loc, function(x) fb_sci_self[fb_sci_self$user_loc==x, "scaled_sci"]) %>% unlist()


## Calcularing the total connectedness (Total friends count per county)

#tmp_df <- fb_sci_dfw %>% dplyr::group_by(user_loc) %>% summarise(Total=sum(scaled_sci), .groups="drop")


#fb_sci_dfw <- left_join(fb_sci_dfw,tmp_df, by="user_loc")

## Relative connectedness (counties relatively to their reference)
#fb_sci_dfw$rel_cnx <- fb_sci_dfw$scaled_sci/fb_sci_dfw$ref

## Connectedness proportion based on total county connectedness

#fb_sci_dfw$rate_cnx <- fb_sci_dfw$scaled_sci/fb_sci_dfw$Total


#fb_sci_dfw$GEOID <- as.character(fb_sci_dfw$fr_loc)




##################################################
#             TO BE COMPLETED                    #
############FB Movement range ####################

# fb_mov <- read.table("../../covid19/fb-movement-range-data/movement-range-2020-10-01.txt", sep = "\t", header = T)
