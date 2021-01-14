#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("libraries.R")
source("params.R")

#source("covid_get_apple.R")
#source("covid_get_fb.R")
#source("covid_get_sg.R")

source("functions.R")
source("load-data-ts.R")
source("tf_apl.R")
source("tf_fb_sci.R")
source("tf_sg.R")
source("final_data.R")



#begins <- first(df_final$date)

#ends <- today()+14


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Dallas Fort Worth COVID19 Risk Modelling"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
        #    sliderInput("date_range",
        #                "Selected a period:",
        #                min = first(df_final$date),
        #                max = today()+14,
        #                value = c(today(), today()+14)
        #    ),
            
            selectInput(inputId = "county", 
                        label = "Select a County", 
                        choices = unique(dfw_counties$county)[1],
                        selected = unique(dfw_counties$county)[1])
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            tabsetPanel(
                
                tabPanel("Plots",
                         br(),
                         br(),
                         
                         plotlyOutput("forecasting"),
                         
                         br(),
                         br(),
                         br(),
                         
                         plotlyOutput("forecasting2")
                         
                ),
                
                tabPanel("Tables",
                         
                         dataTableOutput("df")
                         
                )
            )
            
        )
    )
))
