### Data Transformation main file

#########################################################################################
### Required Libraries.R and Params.R to be loaded first to get the value of state      #
#########################################################################################
### Uncomment the below line if ran within loading params.R
#source("params.R")



## Running Safegraph dataframe combination function
sg_df <- sg_combined(df1=sg_mob_ft, df2=sg_mob_pt, start=start, end=end)



########## This part for the main Application interface ########
#Selected County

sg_df_tarrant <- sg_df %>% filter(fips == as.numeric(name_to_fips(county, state = state)))

# 7 Days moving average Safegraphe data
sg_df_tarrant$fulltime <- rollmean(sg_df_tarrant$fulltime, 7, align = "right", na.pad = TRUE)
sg_df_tarrant$parttime <- rollmean(sg_df_tarrant$parttime, 7, align = "right", na.pad = TRUE)

