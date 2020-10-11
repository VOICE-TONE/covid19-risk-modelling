### Data Transformation main file

### Facebook Data Transformation
# Fb social Connectedness

## Filtering values of reference columns or diagonal elements
fb_sci_self <- fb_sci_dfw %>% filter(user_loc==fr_loc)

fb_sci_self$tot <- fb_sci_dfw %>% 
  
  ## Setting the reference of each data point
  fb_sci_dfw$ref <- lapply(fb_sci_dfw$user_loc, function(x) fb_sci_self[fb_sci_self$user_loc==x, "scaled_sci"]) %>% unlist()


## Calcularing the total connectedness (Total friends count per county)

tmp_df <- fb_sci_dfw %>% dplyr::group_by(user_loc) %>% summarise(Total=sum(scaled_sci), .groups="drop")


fb_sci_dfw <- left_join(fb_sci_dfw,tmp_df, by="user_loc")

## Relative connectedness (counties relatively to their reference)
fb_sci_dfw$rel_cnx <- fb_sci_dfw$scaled_sci/fb_sci_dfw$ref

## Connectedness proportion based on total county connectedness

fb_sci_dfw$rate_cnx <- fb_sci_dfw$scaled_sci/fb_sci_dfw$Total


fb_sci_dfw$GEOID <- as.character(fb_sci_dfw$fr_loc)