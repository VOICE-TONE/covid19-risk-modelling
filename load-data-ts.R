
#### Requires parameter file params.R to be loaded 

#########################################################################################
### Required Libraries.R and Params.R to be loaded first to get the value of state      #
#########################################################################################
### Uncomment the below line if ran within loading params.R
#source("params.R")

# Uncomment this to revert
#dfw_fips <- c(48085,48113, 48121, 48139, 48221, 48231, 48251, 48257, 48367, 48397, 48425, 48439, 48497)

# comment this to revert


## Loading data

## Facebook CLI
fb_cli <- readRDS(paste0(path, "_fb_cli.rds"))

## Facebook Community CLI
fb_cmnty_cli <- readRDS(paste0(path,"_fb_cmnty_cli.rds"))

## Facebook Social Connected Ness
fb_sci_dfw <- readRDS(paste0(path,"_fb_sci_dfw.rds"))

## Safegraph data loading
sg_mob_ft <- readRDS(paste0(path, "_sg_mob_ft.rds"))

## Safegraph parttime tracker
sg_mob_pt <- readRDS(paste0(path,"_sg_mob_pt.rds"))

## Apple mobility data
apple_mob_dfw <- readRDS(paste0(path,"_apple_mob_dfw.rds"))
