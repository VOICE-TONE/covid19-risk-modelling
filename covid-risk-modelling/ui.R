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

#source("functions.R")
#source("load-data-ts.R")
#source("tf_apl.R")
#source("tf_fb_sci.R")
#source("tf_sg.R")
#source("final_data.R")
load("data/tr_cnt_county.rda")
load("data/tarrant_zip_est.rda")
fwisd_lat_lon <- read.csv("data/fwisd_schools_lat_lon.csv")
fwisd_lat_lon$zip <- gsub("TX ", "", fwisd_lat_lon$zipcode)

### Selecting a central zipcode for zomming 
sel <- fwisd_lat_lon %>% filter(zip %in% "76111") %>% dplyr::select(title)

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
                        selected = unique(dfw_counties$county)[1]),
            
            selectInput(inputId = "school", 
                        label = "Select a school", 
                        choices = sort(fwisd_lat_lon$title),
                        selected = sel$title[1],
                        multiple = TRUE),
            
            sliderInput(inputId = "date", 
                        label = "Date:", 
                        value = today(),
                        step = 1,
                        min = min(tarrant_zip_est$date), 
                        max = max(tarrant_zip_est$date))
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            tabsetPanel(
                
                tabPanel("Maps",
                         leafletOutput("map",
                                       height = 1000)
                ),
                
                
                tabPanel("Top 10 Zipcodes",
                         
                         dataTableOutput("df")
                         
                ),
                
                tabPanel("Plots",
                         br(),
                         
                         plotlyOutput("forecasting"),
                         
                         br(),
                         
                )
                
            )
            
        )
    )
))
