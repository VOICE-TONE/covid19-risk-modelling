## Covid Risk Detection app for Counties and Zip Codes in Texas

source("load-data.R")

############ Loading required datasets ############


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DFW Covid early risk detection"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("date",
                        "Selected date:",
                        min = min(tarrant_data_ma$date),
                        max = max(tarrant_data_ma$date),
                        value = max(tarrant_data_ma$date),
                        ),
            
            selectInput(inputId = "county", 
                        label = "Select a County", 
                        choices = unique(dfw_counties$county),
                        selected = unique(dfw_counties$county)[12])
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            tabsetPanel(
                
                tabPanel("Plots",
                         plotlyOutput("trend"),
                         
                         plotlyOutput("countiesmap")
                         ),
                
                tabPanel("Tables",
                
                        textOutput("county"),
            
                        dataTableOutput("df")
                        
                        )
            )
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
    

    output$df <- renderDataTable({
        
        df <- get_county_test(county_name = input$county) %>% 
            filter(county==input$county & between(date, min(date), max(date)))
        
        
    })

    

    ### First output

    output$trend <- renderPlotly({
        
        if(input$county == "Tarrant County"){
           
            df <- tarrant_pred %>% filter(between(date, min(date), input$date))
            
            plot_ly(data = df, x=~date, colors = "Set2") %>%
                add_trace(y=~Actual, name="Actual", mode="lines+markers") %>%
                add_trace(y=~Fitted, name="Predicted", mode="lines+markers") %>%
                layout(title="Tarrant County Risk Prediction")
            
        } else{
            
            # PLotting timeseries of county using plotly
            
            df_cnt <- get_county_test(county_name = input$county) %>% 
                filter(county==input$county & between(date, min(date), input$date))
            
            df <- pred_county(df=df_cnt)
            
            # Plotting the interactive timeline
            plot_ly(data = df, x=~date, colors = "Set2") %>%
                add_trace(y=~Fitted, name="Predicted", mode="lines+markers") %>%
                layout(title=paste(input$county," Risk Prediction"))
            
        } ## End Else
        
    }) ## End RenderPlotly
    
    
    #### The Map of counties
    
    output$countiesmap <- renderPlotly({
        

        ### Selecting risk on given date
        
        cnt_risk <- all_counties_pred %>% filter(date == input$date)
        
        cnt_risk <- cnt_risk[,-1] %>% t()
        
        cnt_risk <- tibble(fips=rownames(cnt_risk), Risk=cnt_risk[,1])
        
        cnt_risk$county <- fips_to_name(cnt_risk$fips)
        
        
        p <- plot_usmap("counties",color = "red", values="Risk", alpha=0.25,include = c(dfw_fips), data = cnt_risk, labels = TRUE) +
            scale_fill_continuous(name = "Covid risk rate per county", label = scales::comma, low="white", high="red") + 
            theme(legend.position = "right", title = element_text(size = 12, colour = "black", face="bold"))+
            labs(title = paste0("DFW Counties covid risk prediction on ", input$date),
                 caption = "These are the states in the Pacific Timezone.")
        
        ggplotly(p)
    })
    
    #### Next output
    
    output$county <- renderText({
        
        paste("Data from :",input$county)
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
