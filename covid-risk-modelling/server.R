#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)

##### Loadong all VECM models used for forecasting

load("models/final_model.rda")
load("data/tr_cnt_county.rda")
load("data/tarrant_zip_est.rda")
load("data/df_final.rda")
load("data/df_test_ts.rda")

fwisd_lat_lon <- read.csv("data/fwisd_schools_lat_lon.csv")
fwisd_lat_lon$zip <- gsub("TX ", "", fwisd_lat_lon$zipcode)


##### Setting the number of days to predict to 14
ndays <- 14


# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {

    
    ### Plotting the forecast 14 days
    output$forecasting <- renderPlotly({
        

        VAR_forecast <- predict(final_model, n.ahead = ndays)
        
        ####################################################################
        ############           Plotting the forecast        ################
        ####################################################################
        
        ### Creating the vector of days ahead corresponding to the forecast
        days_ahead <- last(df_final$date)+days(0:ndays)[-1]
        fcst_ahead <- VAR_forecast$fcst$pos_ma[,"fcst"]
        
        ### only the data part of the forecast ndays ahead
        df_fcst_only <- tibble(date=days_ahead, 
                               value=VAR_forecast$fcst$pos_ma[,"fcst"], 
                               lower=VAR_forecast$fcst$pos_ma[,"lower"],
                               upper=VAR_forecast$fcst$pos_ma[,"upper"])
        
        ### Binding the dates of the actual to the days ahead and the Actual values to the prediction ahead
        df_fcst <- tibble(date=c(df_final$date, days_ahead), value=c(df_final$pos_ma,VAR_forecast$fcst$pos_ma[,"fcst"]))
        
        
        
        plot_ly() %>%
            add_lines(data = tail(df_fcst,60), x=~date, y=~value, name="Positivity rate") %>%
            add_ribbons(data = df_fcst_only, 
                        x=~date,
                        ymin = ~lower,
                        ymax = ~upper,
                        color=I("orange"),
                        name = "95% confidence") %>%
            layout(title=paste(input$county, "risk prediction."), yaxis=list(title=paste("Positivity rate.\n", "(7days moving average)")))
    })
    

    ### Plotting the forecast 14 days
    output$forecasting2 <- renderPlotly({
        
        ### Forecasting ahead without data based on the model above with the lowest MAPE
        VAR_forecast <- predict(final_model, n.ahead=ndays)
        
        
        ####################################################################
        ############           Plotting the forecast        ################
        ####################################################################
        
        ### Creating the vector of days ahead corresponding to the forecast
        days_ahead <- last(df_final$date)+days(0:ndays)[-1]
        fcst_ahead <- VAR_forecast$fcst$pos_ma[,"fcst"]
        
        ### only the data part of the forecast ndays ahead
        df_fcst_only <- tibble(date=days_ahead, 
                               value=VAR_forecast$fcst$pos_ma[,"fcst"], 
                               lower=VAR_forecast$fcst$pos_ma[,"lower"],
                               upper=VAR_forecast$fcst$pos_ma[,"upper"])
        
        ### Binding the dates of the actual to the days ahead and the Actual values to the prediction ahead
        df_fcst <- tibble(date=c(df_final$date, days_ahead), value=c(df_final$pos_ma,VAR_forecast$fcst$pos_ma[,"fcst"]))
        
        
        
        plot_ly() %>%
            add_lines(data = tail(df_fcst,ndays), x=~date, y=~value, name="Positivity rate", line= list(color="Red",dash="dash")) %>%
            add_ribbons(data = tail(df_fcst_only,ndays), 
                        x=~date,
                        ymin = ~lower,
                        ymax = ~upper,
                        color=I("orange"),
                        opacity=0.3,
                        name = "95% confidence") %>%
            layout(title=paste(input$county, "covid risk prediction (",ndays, "days ahead)."), yaxis=list(title=paste("Positivity rate.\n", "(7days moving average)")))
    })
    
    
    
    
    
    
    ### Plotting the predicted vs Actual 14 days
    output$prediction <- renderPlotly({
        
       
        ##### Plotting the models
        
        pred_var1 <- predict(final_model, new_data=df_test_ts, n.ahead=ndays)
        
        pred_7d <- tibble(Actual=tail(df_final,ndays)$pos_ma, 
                          Fitted=pred_var1$fcst$pos_ma[1:ndays,"fcst"], 
                          date=tail(df_final$date, ndays), 
                          lower=pred_var1$fcst$pos_ma[1:ndays,"lower"],
                          upper=pred_var1$fcst$pos_ma[1:ndays,"upper"])
        
        
        plt_R <- plot_ly(data = pred_7d, x=~date) %>%
            add_trace(y=~Actual, name="Actual", type="scatter" ,mode="lines+markers") %>%
            add_trace(y=~Fitted, name="Fitted", type="scatter",mode="lines+markers") %>%
            add_ribbons(data = pred_7d,
                        x=~date,
                        ymin = ~lower,
                        ymax = ~upper,
                        color=I("orange"),
                        name = "95% confidence",
                        opacity=0.2)
        
        plt_R %>%
            layout(title="Prediction on test data", yaxis=list(title=paste("Positivity rate.\n", "(7days moving average)")))
        
    })
    
    ###############################################################
    ######################## plotting maps ########################
    ###############################################################
    
    
    #### Getting the risk of the selected day for all counties
    
    tr_risk <- reactive({
      
      ### Extracting the risk of a selected date
      risk0 <- filter(tarrant_zip_est,date==input$date)[,-c(1:2)] %>% t()
      
      ## Normalizing the risk to make it visible in prediction
      
      tt <- tibble(GEOID10=gsub("Z", "", row.names(risk0)), risk_new=risk0)
      
      tt %>% mutate(norm1 = round((risk_new-min(risk_new)/(max(risk_new)-min(risk_new))),1))
      
    })
    
    
    
    output$map <- renderLeaflet({
        
       ### Extracting the risk of a selected date

        ## Simulating the risk per zipcode
        tr_cnt_county <- left_join(tr_cnt_county,tr_risk(), by="GEOID10")
        
        
        ### Plotting map
        
        ### Preparing the colors Reds in 10 different levels for plotting
        qpal <- colorQuantile("Reds", tr_cnt_county$norm1, n = 5) ### For coloring the tiles
        #qpal0 <- colorQuantile("Reds", tr_cnt_county$risk, n = 5) ### For the legend
        
        # create a leaflet object
        qq <- leaflet() %>%  
          addTiles() %>%  
          addProviderTiles(providers$Esri.WorldImagery, 
                           group = "Esri World Imagery") %>%
          addPolylines(data = tr_cnt_county, 
                       color = "#000000", weight = 5, 
                       smoothFactor = 0.5,opacity = 1.0)
        
        
          qq <- qq %>% addPolygons(data = tr_cnt_county, stroke = FALSE, smoothFactor = 0.2, 
                                   fillOpacity = 1.0, color =~qpal(norm1), 
                                   highlight = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                                   label = ~paste0(GEOID10, " (Risk:", risk_new,")"), group = "hover") %>% 
            addLayersControl(baseGroups = c("OSM", "Esri World Imagery"), 
                             overlayGroups = c("SESYNC"),
                             options = layersControlOptions(collapsed = FALSE)) %>%
            addLegend(pal = qpal, values = tr_cnt_county$norm1, title = "Covid Risk")
        
        ### Calculating the risk at selected date for selected school
        
        
        df <- full_join(fwisd_lat_lon, tr_risk(), by=c("zip"="GEOID10")) 

        df_school <- df %>% dplyr::filter(title %in% input$school) 

        qq %>% 
            addMarkers(data = df_school, 
                       #popup = ~htmlEscape(paste0("Name: ",title ,"\n","Risk:" ,round(risk_new,2), "\n" ,"zipcode:", zip)), 
                       popup = ~htmlEscape(paste0(title,":\n",zip)), 
                       lat=df_school$lat,
                       lng =df_school$long,
                       options = markerOptions(draggable = TRUE)) %>%
            addPopups(data = df_school, 
                      popup = ~paste0(title, ":\n", zipcode),
                      options = popupOptions(maxWidth = 200, minWidth = 30, maxHeight = NULL, autoPan = TRUE)) %>%
                     #popup = ~paste0("<b>Name:", title, "\n","Risk:", round(risk_new,2), "\n","zipcode:", zipcode)) %>%
            setView(lng=df_school$long[1], lat=df_school$lat[1], zoom = 11)
        
        #################################################
        ########### Simpler map using plot_ly ########### 
        ###### Color quantile to be improved ############
       # plot_ly(data = tr_cnt_county) %>% 
      #    add_sf(color=~risk, split=~GEOID10, span=I(1), 
      #           text=~paste(GEOID10, ": risk(", risk, ")"), 
      #           hoverinfo="text", hoveron="fills") %>% 
      #    layout(showlegend=FALSE) %>% 
      #    colorbar(title="Covid Risk")
        
    })
    
    
    ### Using the filtered dataset for display of the data period selected
    output$df <- renderDataTable({

      ##### Predict for the number of days selected

      ll <- tibble(zipcode=tr_risk()$GEOID10, risk=round(tr_risk()$risk_new,2)) %>% dplyr::arrange(desc(risk)) %>% head(10)
      
      

    })
    
    
})