#########################################################################################
### Required Libraries.R and Params.R to be loaded first to get the value of state      #
#########################################################################################
### Uncomment the below line if ran within loading params.R
#source("params.R")

### Getting Apple data

apple_mob <- read.csv(path_apl, header = TRUE, sep = ",")

apple_mob_dfw <- apple_mob %>% dplyr::filter(region %in% dfw_counties$county & sub.region=="Texas")

apple_mob_dfw <- gather(apple_mob_dfw, "date","value",6:length(names(apple_mob_dfw)))

apple_mob_dfw <- apple_mob_dfw[23:nrow(apple_mob_dfw),]

apple_mob_dfw$date <- gsub("X","",apple_mob_dfw$date)

apple_mob_dfw$date <- ymd(apple_mob_dfw$date)

apple_mob_dfw$value <- as.numeric(apple_mob_dfw$value)

### Saving Dallas Fort Worth data
#saveRDS(apple_mob_dfw, "data/apple_mob_dfw.rds")

saveRDS(apple_mob_dfw, paste0(path,"_apple_mob_dfw.rds"))


# Data Exploration
#apple_mob_dfw %>% filter(region == "Denton County") %>% plot_ly(x=~transportation_type, y=~value, type = "box")


#apple_mob_dfw %>% filter(region == "Denton County") %>% 
#  plot_ly(x=~date, color = ~transportation_type) %>%
#  add_trace(y=~value, mode="lines")



#apple_mob_dfw %>% 
#  group_by(region) %>%
#  plot_ly(x=~transportation_type, y=~value, type = "box")


#apple_mob_dfw %>%
#  plot_ly(x=~date, color = ~transportation_type) %>%
#  add_trace(y=~value, mode="lines")
