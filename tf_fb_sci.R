### Facebook connectedness data loading and transformation

#fb_sci_dfw <- trans_fb_sci()

#########################################################################################
### Required Libraries.R and Params.R to be loaded first to get the value of state      #
#########################################################################################
### Uncomment the below line if ran within loading params.R
#source("params.R")


##### Facebook
# Running the function
fb_df <- fb_combined(df1=fb_cli, df2=fb_cmnty_cli, start=start, end=end)


############ All lines below go to the main App #####

## For selected county in county variable. e.g: Tarrant County

fb_df_tarrant <- fb_df %>% filter(fips == as.numeric(name_to_fips(county, state = state)))
