
###### Load tf_fb.R file here for Facebook ######


###### Load tf_sg.R file here for Safegraph ######


###### Load tf_apl.R file here for Apple #######



##### Loading Tarrant County data for the purpose of model building



##### If the function load_county() is not found, load the functions.R file


### Loading County positivity rate
pos_tarrant <- load_county()


### building Final dataset
df_final <- comb_final(df=pos_tarrant)



#### Preparing time series data for analysis
k <- nrow(df_final)-14


##############################################################
####  Making the dataset stationary by first differencing ####

df_final_ts <- ts(df_final[1:k,-1])
df_final_ts_sta <- diff(df_final_ts,1)


##############################################################
############## Creating the test timeseries ##################
df_test <- na.omit(df_final[(k+1):nrow(df_final),])

df_test_ts <- ts(df_test[,-c(1:2)])

### Stationarizing the test dataset
df_test_ts_sta <- diff(df_test_ts,1)





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


