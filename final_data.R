
###### Load tf_fb.R file here for Facebook ######


###### Load tf_sg.R file here for Safegraph ######


###### Load tf_apl.R file here for Apple #######



##### Loading Tarrant County data for the purpose of model building

load_county <- function(){
  
  ### Positivity rates Tarrant County
  
  pos_tarrant <- read.csv("data/tarrant_county_data.csv", header = T)
  
  pos_tarrant$Day <- dmy(pos_tarrant$Day)
  
  pos_tarrant <- pos_tarrant %>% 
    arrange(Day) %>%
    dplyr::filter(dplyr::between(Day, as.Date(start)-6, as.Date(end))) %>% 
    dplyr::select(date=Day,pos=Percent_positivity) %>%
    dplyr::arrange(date, county)
  
  ### Calculating 7days moving average
  pos_tarrant$pos_ma <- rollmean(pos_tarrant$pos, 7, align = "right", na.pad = TRUE)
  
  pos_tarrant <- na.omit(pos_tarrant)
  
  pos_tarrant$pos_ma <- pos_tarrant$pos_ma
  
  return(pos_tarrant)
  
}


### Loading County positivity rate
pos_tarrant <- load_county()

### final dataset

comb_final <- function(df=pos_tarrant){
  
  df1 <- left_join(pos_tarrant[,-2], fb_df_tarrant[,-1])
  df2 <- left_join(df1,sg_df_tarrant[,-1])
  df_final <- left_join(df2, apple_mob_tarrant[,-c(2,13)])
  
  df_final$comp_indice <- df_final$transit/(df_final$transit+df_final$driving)
  
  df_final <- na.omit(df_final)
  
  
  df_final <- na.omit(df_final[,c("date","pos_ma","cli","cmnt_cli","fulltime","parttime","comp_indice")])
  
  return(df_final)
}

### building Final dataset
df_final <- comb_final(df=pos_tarrant)






#### Dealing with NA data
# The below output shows that wise county, Hunt County and Rockwall County have less than 50 data points for the fb_cmnt_cli and zero for fb_cli.
#Which is not enough 

#fb_stats <- fb_df %>% group_by(fips) %>% summarise(count=n(), start=min(date), end=max(date), .groups="drop")
#fb_stats$county <- fips_to_name(fb_stats$fips)
#fb_stats <- fb_stats %>% arrange(desc(count))


#ggplot(fb_stats, aes(x=reorder(county, -count), y=count, fill=count>100))+
#  geom_bar(stat = "identity")+
#  labs(title = "Data vailability by county", x="County", y="Count")+
#  theme_bw()+theme(axis.text.x = element_text(angle = 90))


## Time space coverage (incmoplete)

#library(reshape)
#library(lattice)

#fb_df2 <- melt(fb_df, id.vars = c("date", "fips"), measure.vars = c("cli", "cmnt_cli"))

#xyplot(value~date | variable, group=fips, fb_df2, t="l")

#plot(fb_df$cli ~ fb_df$fips)

#fb_df2 %>% filter(variable=="cli") %>% 
#  group_by(fips) %>% 
#  plot_ly(x=~sort(date), group=~fips, color = ~fips) %>% 
#  add_trace(y=~value,mode="lines")


### Regression controlling for time and County effect (Fixed Effect Model)

#fe_lm <- lm()


### Timeseries Analysis

### Metro Level
## fb Community CLI Mean and Median Values
#fb_test_med <- fb_cmnty_cli %>% group_by(time_value) %>% summarise(Avg = mean(value),Med = median(value), .groups="drop")

#fb_test_ts <- ts(fb_test_med[,2:3])

#plot(fb_test_ts)

#pacf(fb_test_ts)


### County Level
## 


