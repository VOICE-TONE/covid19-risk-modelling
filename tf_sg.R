### Data Transformation main file

### Safegraph combined

sg_combined <- function(df1=sg_mob_ft, df2=sg_mob_pt, start=start, end=end){

  ## This function combines safegraph fulltime and parttime data
  sg_ft_df <- df1 %>% 
    dplyr::arrange(geo_value, time_value) %>% 
    dplyr::filter(dplyr::between(time_value, as.Date(start), as.Date(end))) %>% 
    dplyr::mutate(fips=geo_value, date=time_value, fulltime=value) %>%
    dplyr::select(fips, date, fulltime)
  
  
  sg_pt_df <- df2 %>% 
    arrange(geo_value, time_value) %>% 
    filter(dplyr::between(time_value, as.Date(start), as.Date(end))) %>% 
    mutate(fips=geo_value, date=time_value, parttime=value) %>%
    dplyr::select(fips, date, parttime)
  
  
  sg_df <- full_join(sg_ft_df, sg_pt_df, by=c("fips","date"))
  
  return(sg_df)
  
}

## Running Safegraph dataframe combination function
sg_df <- sg_combined(df1=sg_mob_ft, df2=sg_mob_pt, start=start, end=end)



########## This part for the main Application interface ########
#Selected County

sg_df_tarrant <- sg_df %>% filter(fips == as.numeric(name_to_fips("Tarrant County")))

# 7 Days moving average Safegraphe data
sg_df_tarrant$fulltime <- rollmean(sg_df_tarrant$fulltime, 7, align = "right", na.pad = TRUE)
sg_df_tarrant$parttime <- rollmean(sg_df_tarrant$parttime, 7, align = "right", na.pad = TRUE)

