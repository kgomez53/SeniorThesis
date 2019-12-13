#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)

vars <- c("Average Length Of Stay Of Foreign Guests (in Days)" = "ALOS", 
          "Average Price per Unit of Housing Construction (in Rupiah)" = "aphc", 
          "Average Daily Consumption of Calorie per Capita" = "adcc",
          "Gross Enrollment Ratio for Elementary Schools (SD) (in students per school)" = "ger_sd",
          "Gross Enrollment Ratio for Middle Schools (SMP) (in students per school)" = "ger_smp", 
          "Gross Enrollment Ratio for High Schools (SMA) (in students per school)"="ger_sma", 
          "Harvested Area of Soybeans (in Hectares)"="has", 
          "Illiteracy Rate_Under15"="ir_under15",
          "Illiteracy Rate_15to44"="ir_15to44",
          "Illiteracy Rate_Over45"="ir_over45", 
          "Democracy Index"="di",
          "Net Enrollment Ratio for Elementary Schools (SD) (as % of relevant population)"="ner_sd",
          "Net Enrollment Ratio for Middle Schools (SMP) (as % of relevant population)"="ner_smp",
          "Net Enrollment Ratio for High Schools (SMA) (as % of relevant population)"="ner_sma",
          "Number of Civil Servants per Province"="cs",
          "Percent of Female Representatives of The Regional Representative Council"="r_dpd_pp",
          "Number of Skilled Workers in Construction"="swc",
          "Number of Villages"="villages",
          "Percentage of Households With Female Household Head Who Worked"="pfhw",
          "Percentage of Married Women Aged 15-49 Using Contraception Method"="mwuc",
          "Percentage of Poor People in A Province That Live in Rural Areas" ="ppp",
          "Percentage of Population That Is Female"="gender",
          "Poverty Severity Index"="psi",
          "Production of Paddy By Province (in tons)" = "paddy_prod", 
          "Production of Maize By Province (in tons)" = "maize_prod",
          "Room Occupancy Rate in Hotels"="ror",
          "School Participation Rate For People Aged 13 to 15"="spr_1315",
          "School Participation Rate For People Aged 16 to 18"="spr_1618",
          "School Participation Rate For People Aged 19 to 24"="spr_1924",
          "Wetland Area By Province (in hectares)"="wap")
years <- c("2014" = "2014","2009" = "2009")
IndoData_2009 <- read.csv("Indo_2009.csv")
IndoData_2014 <- read.csv("Indo_2014.csv")
Indonesia_2009 <- read.csv("IndoShow_2009.csv")
Indonesia_2014 <- read.csv("IndoShow_2014.csv")


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    dashboardPage(skin = "green",
        dashboardHeader(title = "Data Visualization"
                        ),
        
        dashboardSidebar(
            sidebarMenu(
                menuItem("Map", tabName = "Map", icon = icon("dashboard")),
                menuItem("Data Explorer", tabName = "Data", icon = icon("bar-chart-o")),
                menuItem("Notes", tabName = "Notes", icon = icon("list-alt")))
        ),
        
        dashboardBody(
            
            tabItems(
                #The Map Tab
                tabItem(tabName = "Map",
                    titlePanel("Map of Indonesia With Provincial Data Visualization"),
                    
                    leafletOutput("indo_base_map",height = 1000),
                    
                    #IF WE WANT TABLES 
                    plotOutput("coolplot"),
                    
                    #Sidebar with buttons
                    sidebarPanel(id = "controls", class = "panel panel-default", fixed = TRUE,draggable = TRUE, top = 60, left = "auto", right = 20,
                    bottom = "auto", width = 320, height = "auto", 
                    selectInput("size", "Select a variable to display by province.", vars), 
                    radioButtons("year", "Year",
                                 choices = years,
                                 selected = "2014")
                    
                    )
                    
                    #End of Map tab
                    ),

               # The Data Tab
               # Want Reactive Data Frame (User can Choose Which Variables They Want)
               # Only do One Year By one year
                tabItem(tabName = "Data", 
                        titlePanel("Data Explorer for Indonesian Provincial Data"),
                        sidebarLayout(
                            sidebarPanel(width = 4,
                                conditionalPanel(
                                    'input.dataset === "Indonesia_2014"', 
                                    checkboxGroupInput("varshow", "Columns to show:",
                                                       names(Indonesia_2014[,2:37]), selected = names(Indonesia_2014))
                                ),
                                conditionalPanel(
                                    'input.dataset === "Indonesia_2009"',
                                    checkboxGroupInput("varshow2", "Columns to show:",
                                                       names(Indonesia_2009[,2:38]), selected = names(Indonesia_2009))
                                )
                            ),
                            mainPanel(
                                tabsetPanel(
                                    id = 'dataset',
                                    tabPanel("Indonesia_2014", DT::dataTableOutput("indo14"), style = 'overflow-x: scroll'),
                                    tabPanel("Indonesia_2009", DT::dataTableOutput("indo09"), style = 'overflow-x: scroll')
                                )
                            )
                        )
                        
                        ),

                #TheNotesTab
                tabItem(tabName = "Notes", h2("Variable References"), h5("Average Length Of Stay Of Foreign Guests (in Days)= ALOS"),h5( 
                                                                       "Average Price per Unit of Housing Construction (in Rupiah) = aphc "),h5(" 
                                                                       Average Daily Consumption of Calorie per Capita = adcc "),h5("
                                                                       Gross Enrollment Ratio for Elementary Schools (SD) (in students per school) = ger_sd "),h5("
                                                                        Gross Enrollment Ratio for Middle Schools (SMP) (in students per school) = ger_smp "),h5(" 
                                                                        Gross Enrollment Ratio for High Schools (SMA) (in students per school) = ger_sma  "),h5(" 
                                                                        Harvested Area of Soybeans (in Hectares) = has  "),h5(" 
                                                                        Illiteracy Rate_Under15 = ir_under15  "),h5("
                                                                        Illiteracy Rate_15to44 = ir_15to44  "),h5("
                                                                        Illiteracy Rate_Over45 = ir_over45  "),h5(" 
                                                                        Democracy Index = di  "),h5("
                                                                        Net Enrollment Ratio for Elementary Schools (SD) (as % of relevant population) = ner_sd  "),h5("
                                                                        Net Enrollment Ratio for Middle Schools (SMP) (as % of relevant population) = ner_smp  "),h5("
                                                                        Net Enrollment Ratio for High Schools (SMA) (as % of relevant population) = ner_sma  "),h5("
                                                                        Number of Civil Servants per Province = cs  "),h5("
                                                                        Percent of Female Representatives of The Regional Representative Council = r_dpd_pp  "),h5("
                                                                        Number of Skilled Workers in Construction = swc  "),h5("
                                                                        Number of Villages = villages  "),h5("
                                                                        Percentage of Households With Female Household Head Who Worked = pfhw  "),h5("
                                                                        Percentage of Married Women Aged 15-49 Using Contraception Method = mwuc  "),h5("
                                                                        Percentage of Poor People in A Province That Live in Rural Areas  = ppp  "),h5("
                                                                        Percentage of Population That Is Female = gender  "),h5("
                                                                        Poverty Severity Index = psi  "),h5("
                                                                        Production of Paddy By Province (in tons)  =  paddy_prod  "),h5(" 
                                                                        Production of Maize By Province (in tons)  =  maize_prod  "),h5("
                                                                        Room Occupancy Rate in Hotels = ror  "),h5("
                                                                        School Participation Rate For People Aged 13 to 15 = spr_1315  "),h5("
                                                                        School Participation Rate For People Aged 16 to 18 = spr_1618  "),h5("
                                                                        School Participation Rate For People Aged 19 to 24 = spr_1924  "),h5("
                                                                        Wetland Area By Province (in hectares) = wap")))
        ))))
       






#WORKSPACE BELOW
     
    # Application title
        # titlePanel("Map"),
        # leafletOutput("indo_base_map",height = 1000)
        # ))))
#         ,
# 
#         vars <- c(
#             "Indo Data 1" = "indo1",
#             "Indo 2" = "indo2",
#             "Indo 3" = "indo 3",
#             "Indo 4" = "indo 4",
#             "Indo 5" = "indo5"
#         ),
# 
#         years <- c(
#             "2014" = "2014",
#             "2009" = "2009"
#         ),
# 
#         #Dropdown box
#         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
#                       draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto", width = 320, height = "auto", selectInput("color", "Color", vars),
#                       selectInput("size", "Size", vars, selected = "adultpop"), selectInput("year", "Year", years)),
# 
# 
#         # Sidebar with a slider input for number of bins
#         sidebarLayout(
#             sidebarPanel(
#                 sliderInput("bins",
#                             "Number of bins:",
#                             min = 1,
#                             max = 50,
#                             value = 30)
#             ),
# 
#             # Show a plot of the generated distribution
#             mainPanel(
#                 plotOutput("distPlot")
#             )
#     )
# ))))
