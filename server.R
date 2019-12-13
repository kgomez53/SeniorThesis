#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#TO DO:

#Regression Tab???
#Notes Tab???



library(shiny)
library(leaflet)
library(raster)
library(shinydashboard)
library(RColorBrewer)
library(ggplot2)
library(DT)

#Variables and Data Sets
IndoData_2009 <- read.csv("Indo_2009.csv")
IndoData_2014 <- read.csv("Indo_2014.csv")
Indonesia_2009 <- read.csv("IndoShow_2009.csv")
Indonesia_2014 <- read.csv("IndoShow_2014.csv")

vars <- c("ALOS" = "ALOS", "APHC" = "aphc", 
          "ADCC" = "adcc","GER_SD" = "ger_sd","GER_SMP" = "ger_smp", 
          "GER_SMA"="ger_sma", "HAS"="has", "IR_Under15"="ir_under15",
          "IR_15to44"="ir_15to44","IR_Over45"="ir_over45", "DI"="di",
          "NER_SD"="ner_sd","NER_SMP"="ner_smp","NER_SMA"="ner_sma",
          "CS"="cs","R_DPD_PP"="r_dpd_pp","SWC"="swc","Villages"="villages",
          "PFHW"="pfhw","MWUC"="mwuc","PPP" ="ppp","Gender"="gender","PSI"="psi",
          "Paddy_Prod" = "paddy_prod", "Maize_Prod" = "maize_prod","ROR"="ror",
          "SPR_1315"="spr_1315","SPR_1618"="spr_1618","SPR_1924"="spr_1924","WAP"="wap")
         
years <- c("2014" = "2014","2009" = "2009")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    #get coordinates for markers
    coordinates <- read.csv("Coord.csv")
    
    #Create Color Palettes for each variable in 2014 (OR MAKE IT BY WHO THEY VOTEd FOR)
    color_pal_14 <- colorFactor(palette = c("red","purple"), domain = IndoData_2014$Winner, na.color = "#808080")
    
    #Create Color Palettes for each variable in 2009
    color_pal_09 <- colorFactor(palette = c("red","orange","navy"), domain = IndoData_2009$Winner, na.color = "#808080")
    
    #MAP WITH MARKERS
    output$indo_base_map <- renderLeaflet({
        
        #2014
        if(input$year == "2014"){
            data_holder <- IndoData_2014
            
            #ALOS
            if (input$size == "ALOS"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(ALOS^(1.25)),
                                     label = paste(as.character(data_holder$province),":", data_holder$ALOS)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map

            }
            #APHC
            else if (input$size == "aphc"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(APHC^(.4)),
                                     label = paste(as.character(data_holder$province),":",data_holder$APHC)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map

            }
            #ADCC
            else if (input$size == "adcc"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(ADCC^(.3)),
                                     label = paste(as.character(data_holder$province),":",data_holder$ADCC)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #GER_SD
            else if (input$size == "ger_sd"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(GER_SD^(.5)*1),
                                     label = paste(as.character(data_holder$province),":",data_holder$GER_SD)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #GER_SMP
            else if (input$size == "ger_smp"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(GER_SMP^(.5)*1),
                                     label = paste(as.character(data_holder$province),":",data_holder$GER_SMP)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #GER_SMA
            else if (input$size == "ger_sma"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(GER_SMA^(.5)*1),
                                     label = paste(as.character(data_holder$province),":",data_holder$GER_SMA)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
            
            }
            #HAS
            else if (input$size == "has"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(HAS^(.2)),
                                     label = paste(as.character(data_holder$province),":",data_holder$HAS)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #IR_Under15
            else if (input$size == "ir_under15"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(IR_UNDER15^(.5)*4),
                                     label = paste(as.character(data_holder$province),":",data_holder$IR_UNDER15)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #IR_15to44
            else if (input$size == "ir_15to44"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(IR_15TO44^(.5)*4),
                                     label = paste(as.character(data_holder$province),":",data_holder$IR_15TO44)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #IR_Over45
            else if (input$size == "ir_over45"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(IR_OVER45^(.5)*2.5),
                                     label = paste(as.character(data_holder$province),":",data_holder$IR_OVER45)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #DI
            else if (input$size == "di"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(DI/6.5),
                                     label = paste(as.character(data_holder$province),":",data_holder$DI)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #NER_SD
            else if (input$size == "ner_sd"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(NER_SD/8),
                                     label = paste(as.character(data_holder$province),":",data_holder$NER_SD)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #NER_SMP
            else if (input$size == "ner_smp"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(NER_SMP/8),
                                     label = paste(as.character(data_holder$province),":",data_holder$NER_SMP)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
            
            }
            #NER_SMA
            else if (input$size == "ner_sma"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(NER_SMA/8),
                                     label = paste(as.character(data_holder$province),":",data_holder$NER_SMA)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #CS
            else if (input$size == "cs"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(CS/50000),
                                     label = paste(as.character(data_holder$province),":",data_holder$CS)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #R_DPD_PP
            else if (input$size == "r_dpd_pp"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(R_DPD_PP*15),
                                     label = paste(as.character(data_holder$province),":",data_holder$R_DPD_PP)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #SWC
            else if (input$size == "swc"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(SWC^(.25)),
                                     label = paste(as.character(data_holder$province),":",data_holder$SWC)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #Villages
            else if (input$size == "villages"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(VILLAGES^(.30)),
                                     label = paste(as.character(data_holder$province),":",data_holder$VILLAGES)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #PFHW
            else if (input$size == "pfhw"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(PFHW/7),
                                     label = paste(as.character(data_holder$province),":",data_holder$PFHW)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #MWUC
            else if (input$size == "mwuc"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(MWUC/7),
                                     label = paste(as.character(data_holder$province),":",data_holder$MWUC)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #PPP
            else if (input$size == "ppp"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(PPP*10),
                                     label = paste(as.character(data_holder$province),":",data_holder$PPP)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #Gender
            else if (input$size == "gender"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(GENDER/8),
                                     label = paste(as.character(data_holder$province),":",data_holder$GENDER)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #PSI
            else if (input$size == "psi"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(PSI*15),
                                     label = paste(as.character(data_holder$province),":",data_holder$PSI)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #Paddy_Prod
            else if (input$size == "paddy_prod"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(PADDY_PROD^(.1)*2),
                                     label = paste(as.character(data_holder$province),":",data_holder$PADDY_PROD)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #Maize_Prod
            else if (input$size == "maize_prod"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(MAIZE_PROD/7),
                                     label = paste(as.character(data_holder$province),":",data_holder$MAIZE_PROD)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #ROR
            else if (input$size == "ror"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(ROR/5),
                                     label = paste(as.character(data_holder$province),":",data_holder$ROR)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #SPR_1315
            else if (input$size == "spr_1315"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(SPR_1315/10),
                                     label = paste(as.character(data_holder$province),":",data_holder$SPR_1315)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #SPR_1618
            else if (input$size == "spr_1618"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(SPR_1618/7),
                                     label = paste(as.character(data_holder$province),":",data_holder$SPR_1618)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #SPR_1924
            else if (input$size == "spr_1924"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(SPR_1924/4),
                                     label = paste(as.character(data_holder$province),":",data_holder$SPR_1924)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #WAP
            else if (input$size == "wap"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_14(Winner),
                                     radius= ~(WAP^(.2)/1.5),
                                     label = paste(as.character(data_holder$province),":",data_holder$WAP)) %>%
                    addLegend("bottomright", pal = color_pal_14, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            
            #Everything Else - can delete
            # }else{
            #     map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
            #         addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
            #         addCircleMarkers(data_holder$longitude,data_holder$latitude,
            #                          color = ~color_pal_14(Winner),
            #                          label = paste(as.character(data_holder$province),":",data_holder$ADCC)) %>%
            #         addLegend("bottomright", pal = color_pal_14, values = ~Winner,
            #                   title = "Election Results",
            #                   opacity = .9) %>%
            #         setView(120,.7, 4.4) %>%
            #         setMaxBounds(100, -1, 140, -5)
            #     map
            }

        }else if (input$year == 2009){
            data_holder <- IndoData_2009
            #ALOS
            if (input$size == "ALOS"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(ALOS^(1.25)),
                                     label = paste(as.character(data_holder$province),":", data_holder$ALOS)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #APHC
            else if (input$size == "aphc"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(APHC^(.4)),
                                     label = paste(as.character(data_holder$province),":",data_holder$APHC)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #ADCC
            else if (input$size == "adcc"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(ADCC^(.3)),
                                     label = paste(as.character(data_holder$province),":",data_holder$ADCC)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #GER_SD
            else if (input$size == "ger_sd"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(GER_SD^(.5)*1),
                                     label = paste(as.character(data_holder$province),":",data_holder$GER_SD)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #GER_SMP
            else if (input$size == "ger_smp"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(GER_SMP^(.5)*1),
                                     label = paste(as.character(data_holder$province),":",data_holder$GER_SMP)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #GER_SMA
            else if (input$size == "ger_sma"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(GER_SMA^(.5)*1),
                                     label = paste(as.character(data_holder$province),":",data_holder$GER_SMA)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #HAS
            else if (input$size == "has"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(HAS^(.2)),
                                     label = paste(as.character(data_holder$province),":",data_holder$HAS)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #IR_Under15
            else if (input$size == "ir_under15"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(IR_UNDER15^(.5)*4),
                                     label = paste(as.character(data_holder$province),":",data_holder$IR_UNDER15)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #IR_15to44
            else if (input$size == "ir_15to44"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(IR_15TO44^(.5)*4),
                                     label = paste(as.character(data_holder$province),":",data_holder$IR_15TO44)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #IR_Over45
            else if (input$size == "ir_over45"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(IR_OVER45^(.5)*2.5),
                                     label = paste(as.character(data_holder$province),":",data_holder$IR_OVER45)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #DI
            else if (input$size == "di"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(DI/6.5),
                                     label = paste(as.character(data_holder$province),":",data_holder$DI)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #NER_SD
            else if (input$size == "ner_sd"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(NER_SD/8),
                                     label = paste(as.character(data_holder$province),":",data_holder$NER_SD)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #NER_SMP
            else if (input$size == "ner_smp"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(NER_SMP/8),
                                     label = paste(as.character(data_holder$province),":",data_holder$NER_SMP)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #NER_SMA
            else if (input$size == "ner_sma"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(NER_SMA/8),
                                     label = paste(as.character(data_holder$province),":",data_holder$NER_SMA)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #CS
            else if (input$size == "cs"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(CS/50000),
                                     label = paste(as.character(data_holder$province),":",data_holder$CS)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
            
            }
            #R_DPD_PP
            else if (input$size == "r_dpd_pp"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(R_DPD_PP*15),
                                     label = paste(as.character(data_holder$province),":",data_holder$R_DPD_PP)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #SWC
            else if (input$size == "swc"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(SWC^(.25)),
                                     label = paste(as.character(data_holder$province),":",data_holder$SWC)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #Villages
            else if (input$size == "villages"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(VILLAGES^(.30)),
                                     label = paste(as.character(data_holder$province),":",data_holder$VILLAGES)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #PFHW
            else if (input$size == "pfhw"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(PFHW/7),
                                     label = paste(as.character(data_holder$province),":",data_holder$PFHW)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #MWUC
            else if (input$size == "mwuc"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(MWUC/7),
                                     label = paste(as.character(data_holder$province),":",data_holder$MWUC)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #PPP
            else if (input$size == "ppp"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(PPP*10),
                                     label = paste(as.character(data_holder$province),":",data_holder$PPP)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #Gender
            else if (input$size == "gender"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(GENDER/8),
                                     label = paste(as.character(data_holder$province),":",data_holder$GENDER)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #PSI
            else if (input$size == "psi"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(PSI*15),
                                     label = paste(as.character(data_holder$province),":",data_holder$PSI)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #Paddy_Prod
            else if (input$size == "paddy_prod"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(PADDY_PROD^(.1)*2),
                                     label = paste(as.character(data_holder$province),":",data_holder$PADDY_PROD)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #Maize_Prod
            else if (input$size == "maize_prod"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(MAIZE_PROD/7),
                                     label = paste(as.character(data_holder$province),":",data_holder$MAIZE_PROD)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #ROR
            else if (input$size == "ror"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(ROR/5),
                                     label = paste(as.character(data_holder$province),":",data_holder$ROR)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #SPR_1315
            else if (input$size == "spr_1315"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(SPR_1315/10),
                                     label = paste(as.character(data_holder$province),":",data_holder$SPR_1315)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #SPR_1618
            else if (input$size == "spr_1618"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(SPR_1618/7),
                                     label = paste(as.character(data_holder$province),":",data_holder$SPR_1618)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #SPR_1924
            else if (input$size == "spr_1924"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(SPR_1924/4),
                                     label = paste(as.character(data_holder$province),":",data_holder$SPR_1924)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
                
            }
            #WAP
            else if (input$size == "wap"){
                map <- leaflet(data = data_holder, options = leafletOptions(minZoom = 4.4)) %>%
                    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                    addCircleMarkers(data_holder$longitude,data_holder$latitude,
                                     color = ~color_pal_09(Winner),
                                     radius= ~(WAP^(.2)/1.5),
                                     label = paste(as.character(data_holder$province),":",data_holder$WAP)) %>%
                    addLegend("bottomright", pal = color_pal_09, values = ~Winner,
                              title = "Election Results",
                              opacity = .9) %>%
                    setView(120,.7, 4.4) %>%
                    setMaxBounds(100, -1, 140, -5)
                map
            }
        } 
    })
    
    
    #Plot on MAp Tab
    output$coolplot <- renderPlot({
        if(input$year[1] == "2014"){
            barplot(IndoData_2014[,toupper(input$size)],names.arg=IndoData_2014$province,
                    ylab="Units As Described In Selection",
                    col=color_pal_14(IndoData_2014$Winner),
                    main=paste("Selected Variable","(",input$size,")","by Province"),
                    border="black",las=2, cex.names = .6)
        }
        else if (input$year == "2009"){
            barplot(IndoData_2009[,toupper(input$size)],names.arg=IndoData_2009$province,
                    ylab="Units As Described In Selection",
                    col=color_pal_09(IndoData_2009$Winner),
                    main=paste("Selected Variable","(",input$size,")","by Province"),
                    border="black",las=2, cex.names = .6)
        }
    })
    
    
    #Data Explorer Tab
    output$indo14 <- DT::renderDataTable({
        DT::datatable(Indonesia_2014[,input$varshow, drop = FALSE])
    })
    
    output$indo09 <- DT::renderDataTable({
        DT::datatable(Indonesia_2009[,input$varshow2, drop = FALSE])
    })
    
    

})
