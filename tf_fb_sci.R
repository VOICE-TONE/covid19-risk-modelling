### Data Transformation main file

### Facebook Data Transformation
# Fb social Connectedness

trans_fb_sci <- function(){
  
  ## Filtering values of reference columns or diagonal elements
  fb_sci_self <- fb_sci_dfw %>% filter(user_loc==fr_loc)
  
  ## Setting the reference of each data point
  fb_sci_dfw$ref <- lapply(fb_sci_dfw$user_loc, function(x) fb_sci_self[fb_sci_self$user_loc==x, "scaled_sci"]) %>% unlist()
  
  ## Calcularing the total connectedness (Total friends count per county)
  tmp_df <- fb_sci_dfw %>% dplyr::group_by(user_loc) %>% summarise(Total=sum(scaled_sci), .groups="drop")
  
  
  fb_sci_dfw <- left_join(fb_sci_dfw,tmp_df, by="user_loc")
  
  rm(tmp_df)
  
  ## Relative connectedness (counties relatively to their reference)
  fb_sci_dfw$rel_cnx <- fb_sci_dfw$scaled_sci/fb_sci_dfw$ref
  
  ## Connectedness proportion based on total county connectedness
  
  fb_sci_dfw$rate_cnx <- fb_sci_dfw$scaled_sci/fb_sci_dfw$Total
  
  
  fb_sci_dfw$GEOID <- as.character(fb_sci_dfw$fr_loc)
  
  return(fb_sci_dfw)
  
}


fb_sci_dfw <- trans_fb_sci()




##### Facebook

fb_combined <- function(df1=fb_cli, df2=fb_cmnty_cli, start=start, end=end){
  
  ### Facebook combined
  fb_cli_df <- df1 %>% 
    arrange(geo_value, time_value) %>% 
    filter(dplyr::between(time_value, as.Date(start), as.Date(end))) %>% 
    mutate(fips=geo_value, date=time_value, cli=value) %>%
    dplyr::select(fips, date, cli)
  
  
  fb_cmnt_cli_df <- df2 %>% 
    arrange(geo_value, time_value) %>% 
    filter(dplyr::between(time_value, as.Date(start), as.Date(end))) %>% 
    mutate(fips=geo_value, date=time_value, cmnt_cli=value) %>%
    dplyr::select(fips, date, cmnt_cli)
  
  
  fb_df <- full_join(fb_cli_df, fb_cmnt_cli_df, by=c("fips","date"))
  
  return(fb_df)
  
}


# Running the function
fb_df <- fb_combined(df1=fb_cli, df2=fb_cmnty_cli, start=start, end=end)







############ All lines below go to the main App #####

## For selected county in county variable. e.g: Tarrant County

fb_df_tarrant <- fb_df %>% filter(fips == name_to_fips(county))