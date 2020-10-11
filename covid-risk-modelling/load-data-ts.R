
########## Values to be derived from the main application
##### Combining all datasets 

start <- ymd("2020-06-24")

end <- today()-3

county= "Tarrant County"


dfw_fips <- c(48085,48113, 48121, 48139, 48221, 48231, 48251, 48257, 48367, 48397, 48425, 48439, 48497)

dfw_counties <- tibble(county=fips_to_name(dfw_fips))


## Loading data

## Facebook CLI
fb_cli <- readRDS("data/fb_cli.rds")

## Facebook Community CLI
fb_cmnty_cli <- readRDS("data/fb_cmnty_cli.rds")

## Facebook Social Connected Ness
fb_sci_dfw <- readRDS("data/fb_sci_dfw.rds")

## Safegraph data loading
sg_mob_ft <- readRDS("data/sg_mob_ft.rds")

## Safegraph parttime tracker
sg_mob_pt <- readRDS("data/sg_mob_pt.rds")

## Apple mobility data
apple_mob_dfw <- readRDS("data/apple_mob_dfw.rds")
