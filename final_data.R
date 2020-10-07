##### Combining all datasets 

start <- ymd("2020-06-24")

end <- today()-3

county= "Tarrant County"

### Facebook combined
fb_cli_df <- fb_cli %>% 
  arrange(geo_value, time_value) %>% 
  filter(dplyr::between(time_value, as.Date(start), as.Date(end))) %>% 
  mutate(fips=geo_value, date=time_value, cli=value) %>%
  dplyr::select(fips, date, cli)


fb_cmnt_cli_df <- fb_cmnty_cli %>% 
  arrange(geo_value, time_value) %>% 
  filter(dplyr::between(time_value, as.Date(start), as.Date(end))) %>% 
  mutate(fips=geo_value, date=time_value, cmnt_cli=value) %>%
  dplyr::select(fips, date, cmnt_cli)


fb_df <- full_join(fb_cli_df, fb_cmnt_cli_df, by=c("fips","date"))

# fb Tarrant

fb_df_tarrant <- fb_df %>% filter(fips == name_to_fips(county))

### Safegraph combined

sg_ft_df <- sg_mob_ft %>% 
  arrange(geo_value, time_value) %>% 
  filter(dplyr::between(time_value, as.Date(start), as.Date(end))) %>% 
  mutate(fips=geo_value, date=time_value, fulltime=value) %>%
  dplyr::select(fips, date, fulltime)


sg_pt_df <- sg_mob_pt %>% 
  arrange(geo_value, time_value) %>% 
  filter(dplyr::between(time_value, as.Date(start), as.Date(end))) %>% 
  mutate(fips=geo_value, date=time_value, parttime=value) %>%
  dplyr::select(fips, date, parttime)


sg_df <- full_join(sg_ft_df, sg_pt_df, by=c("fips","date"))

#Tarrant County

sg_df_tarrant <- sg_df %>% filter(fips == as.numeric(name_to_fips("Tarrant County")))

# 7 Days moving average Safegraphe data
sg_df_tarrant$fulltime <- rollmean(sg_df_tarrant$fulltime, 7, align = "right", na.pad = TRUE)
sg_df_tarrant$parttime <- rollmean(sg_df_tarrant$parttime, 7, align = "right", na.pad = TRUE)


### Apple mobility combined

apple_mob_tarrant <- apple_mob_tarrant1 %>% 
  dplyr::arrange(date) %>%
  dplyr::filter(dplyr::between(date, as.Date(start), as.Date(end)))


### Positivity rates Tarrant County

pos_tarrant <- read.csv("data/tarrant_county_data.csv", header = T)

pos_tarrant$Day <- dmy(pos_tarrant$Day)

pos_tarrant <- pos_tarrant %>% 
  arrange(Day) %>%
  dplyr::filter(dplyr::between(Day, as.Date(start)-6, as.Date(end))) %>% 
  dplyr::select(date=Day,pos=Percent_positivity) %>%
  dplyr::arrange(date, county, )

### Calculating 7days moving average
pos_tarrant$pos_ma <- rollmean(pos_tarrant$pos, 7, align = "right", na.pad = TRUE)

pos_tarrant <- na.omit(pos_tarrant)

pos_tarrant$pos_ma <- pos_tarrant$pos_ma/100

### final dataset

df1 <- left_join(pos_tarrant[,-2], fb_df_tarrant[,-1])
df2 <- left_join(df1,sg_df_tarrant[,-1])
df_final <- left_join(df2, apple_mob_tarrant[,-c(2,13)])

df_final$comp_indice <- df_final$transit/(df_final$transit+df_final$driving)

df_final <- na.omit(df_final)


df_final <- na.omit(df_final[,c("date","pos_ma","cli","cmnt_cli","fulltime","parttime","comp_indice")])




#### Test Dataset








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


