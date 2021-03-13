# Valo Mara CS 424 Project 2 Spring '21

library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)

# ----- DATA CLEANUP -----
dataClean <- function(csvFilename){
  # read in data
  plnt <- read.csv(csvFilename)
  plnt[, 3:15] <- lapply(plnt[,3:15], as.numeric)
  
  # join other columns together
  plnt$OTHER <- rowSums(plnt[,14:15])
  plnt$OTHER1 <- NULL
  plnt$OTHER2 <- NULL
  
  # grab the max production of each plant and assign it as its primary energy source 
  # factorize the energy column for easier categorization
  plnt$ENERGY <- colnames(plnt[,5:14])[max.col(abs(plnt[,5:14]), ties.method = "first")]
  plnt$ENERGY <- factor(plnt$ENERGY)
  
  # join total generation across all energy sources
  plnt$TOTAL_GEN <- rowSums(plnt[,5:14])
  
  # calculate percentage of each energy source at a plant
  plnt$PERCENT_COAL <- round(plnt$COAL/plnt$TOTAL_GEN, digits = 3)
  plnt$PERCENT_OIL <- round(plnt$OIL/plnt$TOTAL_GEN, digits = 3)
  plnt$PERCENT_GAS <- round(plnt$GAS/plnt$TOTAL_GEN, digits = 3)
  plnt$PERCENT_NUCLEAR <- round(plnt$NUCLEAR/plnt$TOTAL_GEN, digits = 3)
  plnt$PERCENT_HYDRO <- round(plnt$HYDRO/plnt$TOTAL_GEN, digits = 3)
  plnt$PERCENT_BIO <- round(plnt$BIO/plnt$TOTAL_GEN, digits = 3)
  plnt$PERCENT_WIND <- round(plnt$WIND/plnt$TOTAL_GEN, digits = 3)
  plnt$PERCENT_SOLAR <- round(plnt$SOLAR/plnt$TOTAL_GEN, digits = 3)
  plnt$PERCENT_GEO <- round(plnt$GEO/plnt$TOTAL_GEN, digits = 3)
  plnt$PERCENT_OTHER <- round(plnt$OTHER/plnt$TOTAL_GEN, digits = 3)
  
  # adding all renewable columns
  plnt$TOTAL_RENEWABLE <- rowSums(plnt[,9:13])
  
  # adding all non-renewable columns
  plnt$TOTAL_NON_RENEWABLE <- rowSums(plnt[,c(5:8, 14)])
  
  # calculating percentage of renewable/non-renewable energy generated
  plnt$PERCENT_RENEWABLE <- round(plnt$TOTAL_RENEWABLE/plnt$TOTAL_GEN, digits = 3)
  plnt$PERCENT_NON_RENEWABLE <- round(plnt$TOTAL_NON_RENEWABLE/plnt$TOTAL_GEN, digits = 3)
  
  return(plnt)
}

plnt18 <- dataClean("plnt.csv")
plnt10 <- dataClean("plnt2010.csv")
plnt00 <- dataClean("plnt2000.csv")
# ----- LISTS FOR CLARIFICATION -----

# these lists are used whenever a checkbox needs it or a inputselection needs it
renewable <- c("HYDRO", "BIO", "WIND", "SOLAR", "GEO")
non_renewable <- c("COAL", "OIL", "GAS", "NUCLEAR", "OTHER")
selections <- c("COAL", "OIL", "GAS", "NUCLEAR", "HYDRO", "BIO", "WIND", "SOLAR", "GEO", "OTHER",
                "RENEWABLE", "NON-RENEWABLE", "ALL")
states <- c(1:50)
names(states) <- state.name
centers <- state.center
# ----- UI -----
ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 2"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   sidebarMenu(
                     menuItem("Illinois Map", tabName = "ilmap", icon = icon("bolt")),
                     menuItem("Map Compare", tabName = "comp", icon = icon("fas fa-balance-scale")),
                     menuItem("US View", tabName = "usview", icon = icon("far fa-map")),
                     menuItem("About", tabName = "about", icon = icon("fas fa-info"))
                   )),
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    tabItems(
      tabItem(tabName = "ilmap",
              fluidRow(
                column(2, checkboxGroupInput("energy_types", "Select Items to Filter",
                                             selections, selected = "ALL")
                ),
                column(10, box(title = "Power Plants Located in Illinois", solidHeader = TRUE, 
                          width = 12, leafletOutput("ilmap", height = "80vh"))
                )
      )), # ilmap end
      tabItem(tabName = "comp",
              fluidRow(
                column(1, checkboxGroupInput("box1", "Select Items to Filter",
                                             selections, selected = "ALL",
                                             width = NULL),
                       selectInput("states1", "Select State", states, selected = 13)
                ),
                column(5, tabBox(title = "Visualization 1", selected = "2000",
                                 width = NULL,
                                 tabPanel("2000", leafletOutput("vis1tab2000", height = "65vh")),
                                 tabPanel("2010", leafletOutput("vis1tab2010", height = "65vh")),
                                 tabPanel("2018", leafletOutput("vis1tab2018", height = "65vh")))
                ),
                column(5, tabBox(title = "Visualization 2", selected = "2018",
                                 width = NULL,
                                 tabPanel("2000", leafletOutput("vis2tab2000", height = "65vh")),
                                 tabPanel("2010", leafletOutput("vis2tab2010", height = "65vh")),
                                 tabPanel("2018", leafletOutput("vis2tab2018", height = "65vh")))
                ),
                column(1, checkboxGroupInput("box2", "Select Items to Filter",
                                             selections, selected = "ALL"),
                       selectInput("states2", "Select State", states, selected = 13)
                )
              ),
              fluidRow(
                column(12, align = "center",
                       checkboxGroupInput("boxBoth", "Dual Control",
                                          selections, inline = TRUE)
                       )
              )
      ), # comp end
      tabItem(tabName = "usview",
              fluidRow(
                column(1, checkboxGroupInput("box3", "Select Items to Filter",
                                   selections,
                                   width = NULL)
                ),
                column(11, tabBox(title = "US Map View", id = "tabset3",
                                  width = NULL,
                                  tabPanel("2000", leafletOutput("usmap2000", height = "65vh")),
                                  tabPanel("2010", leafletOutput("usmap2010", height = "65vh")),
                                  tabPanel("2018", leafletOutput("usmap2018", height = "65vh"))),
                )
              ),
              fluidRow(
                column(12, align = "center",
                       sliderInput("range", "Power Generation Range:",
                                    min = 0, max = 32000000,
                                    value = c(0, 32000000),
                                    width = "60vw",
                                    post = "MWh")
                )
      )), # usview end
      tabItem(tabName = "about",
              h2("About this project"),
              p("This project was made by Valo Mara for CS 424 Spring '21"),
              p(),
              tagList("All data available from ", a("the EPA website here", 
                                                    href = "https://www.epa.gov/egrid/download-data")),
              p("Data used is the eGRID2018v2 Data File (XLSX) from the PLNT18 tab"),
              p("Other data is from eGRID historical files (1996-2016), specifically for the years 2000 and 2010"),
              p(),
              tagList("All code available on ", a("github", href = "https://github.com/vmara2/cs424p2"))
              
      ) # about end
    ) # tabItems end 
  ) #dashboardBody end
)
# ----- SERVER -----
server <- function(input, output, session) {
  
  # observer for bottom checkbox on the compate page
  # changes the output of the two checkboxes
  # so that they match the bottom checkbox
  observe({
    x <- input$boxBoth
    
    # whenever x is updated, then other two will update
    updateCheckboxGroupInput(session, "box1",
                             selected = x
    )
    updateCheckboxGroupInput(session, "box2",
                             selected = x
    )
  })
  # the following reactives just extract the subset needed for a leaftlet map
  
  # ---- Illinois Page Reactive ----
  # process the input from checkbox on Illinois map
  sourceReactive <- reactive({
    if("ALL" %in% input$energy_types){plnt18}
    else if("RENEWABLE" %in% input$energy_types){plnt18[plnt18$ENERGY %in% renewable, ]}
    else if("NON-RENEWABLE" %in% input$energy_types){plnt18[plnt18$ENERGY %in% non_renewable, ]}
    else{plnt18[plnt18$ENERGY %in% input$energy_types, ]}
  })
  
  # ---- Comparison Page Reactives ----
  left2000Reactive <- reactive({
    if("ALL" %in% input$box1){plnt00}
    else if("RENEWABLE" %in% input$box1){plnt00[plnt00$ENERGY %in% renewable, ]}
    else if("NON-RENEWABLE" %in% input$box1){plnt00[plnt00$ENERGY %in% non_renewable, ]}
    else{plnt00[plnt00$ENERGY %in% input$box1, ]}
  })
  
  left2010Reactive <- reactive({
    if("ALL" %in% input$box1){plnt10}
    else if("RENEWABLE" %in% input$box1){plnt10[plnt10$ENERGY %in% renewable, ]}
    else if("NON-RENEWABLE" %in% input$box1){plnt10[plnt10$ENERGY %in% non_renewable, ]}
    else{plnt10[plnt10$ENERGY %in% input$box1, ]}
  })
  
  left2018Reactive <- reactive({
    if("ALL" %in% input$box1){plnt18}
    else if("RENEWABLE" %in% input$box1){plnt18[plnt18$ENERGY %in% renewable, ]}
    else if("NON-RENEWABLE" %in% input$box1){plnt18[plnt18$ENERGY %in% non_renewable, ]}
    else{plnt18[plnt18$ENERGY %in% input$box1, ]}
  })
  
  right2000Reactive <- reactive(
    if("ALL" %in% input$box2){plnt00}
    else if("RENEWABLE" %in% input$box2){plnt00[plnt00$ENERGY %in% renewable, ]}
    else if("NON-RENEWABLE" %in% input$box2){plnt00[plnt00$ENERGY %in% non_renewable, ]}
    else{plnt00[plnt00$ENERGY %in% input$box2, ]}
  )
  
  right2010Reactive <- reactive(
    if("ALL" %in% input$box2){plnt10}
    else if("RENEWABLE" %in% input$box2){plnt10[plnt10$ENERGY %in% renewable, ]}
    else if("NON-RENEWABLE" %in% input$box2){plnt10[plnt10$ENERGY %in% non_renewable, ]}
    else{plnt10[plnt10$ENERGY %in% input$box2, ]}
  )
  
  right2018Reactive <- reactive(
    if("ALL" %in% input$box2){plnt18}
    else if("RENEWABLE" %in% input$box2){plnt18[plnt18$ENERGY %in% renewable, ]}
    else if("NON-RENEWABLE" %in% input$box2){plnt18[plnt18$ENERGY %in% non_renewable, ]}
    else{plnt18[plnt18$ENERGY %in% input$box2, ]}
  )
  
  # ---- US Map Reactives
  usview1Reactive <- reactive({
    plnt00 <- plnt00[plnt00$TOTAL_GEN >= input$range[1] & plnt00$TOTAL_GEN <= input$range[2],]
    
    if("ALL" %in% input$box3){plnt00}
    else if("RENEWABLE" %in% input$box3){plnt00[plnt00$ENERGY %in% renewable, ]}
    else if("NON-RENEWABLE" %in% input$box3){plnt00[plnt00$ENERGY %in% non_renewable, ]}
    else{plnt00[plnt00$ENERGY %in% input$box3, ]}
  })
  
  usview2Reactive <- reactive({
    plnt10 <- plnt10[plnt10$TOTAL_GEN >= input$range[1] & plnt10$TOTAL_GEN <= input$range[2],]
    
    if("ALL" %in% input$box3){plnt10}
    else if("RENEWABLE" %in% input$box3){plnt10[plnt10$ENERGY %in% renewable, ]}
    else if("NON-RENEWABLE" %in% input$box3){plnt10[plnt10$ENERGY %in% non_renewable, ]}
    else{plnt10[plnt10$ENERGY %in% input$box3, ]}
  })
  
  usview3Reactive <- reactive({
    plnt18 <- plnt18[plnt18$TOTAL_GEN >= input$range[1] & plnt18$TOTAL_GEN <= input$range[2],]
    
    if("ALL" %in% input$box3){plnt18}
    else if("RENEWABLE" %in% input$box3){plnt18[plnt18$ENERGY %in% renewable, ]}
    else if("NON-RENEWABLE" %in% input$box3){plnt18[plnt18$ENERGY %in% non_renewable, ]}
    else{plnt18[plnt18$ENERGY %in% input$box3, ]}
  })
  
  # in the outputs, there are conditionals rendering two maps.
  # this is done to prevent errors when a checkbox group is empty 
  # or a subset returns a dataframe with no rows. 
  
  
  # ---- Illinois Output ----
  output$ilmap <- renderLeaflet({
    
    # grabbing all power plants in illinois
    ilReactive <- sourceReactive()
    ilplants <- subset(ilReactive, ilReactive$STATE == "IL")
    
    # creating color pallette
    pal <- colorFactor(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"),
                       ilplants$ENERGY)
    
    # handles case where no selections are made and rows are empty
    if(is.null(input$energy_types) | nrow(ilplants) == 0) {
      leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
        setView(-89, 40, zoom = 7) %>%
        #creates a JS button on the leaflet map
        addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                                 onClick = JS("function(btn, map){ map.setView([40,-89], 7);}")))
    }
    else {
      # creating map and adding markers on it
    leaflet(ilplants) %>% addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(lat = ~LAT, lng = ~LON, label = ~NAME, 
                     color = ~pal(ilplants$ENERGY), stroke = FALSE, fillOpacity = 1, radius = 4) %>% 
    addLegend("bottomright", pal = pal, values = ~ENERGY, 
                  title = "Power Plant Energy Type", opacity = 1) %>%
    setView(-89, 40, zoom = 7) %>%
    addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                             onClick = JS("function(btn, map){ map.setView([40,-89], 7);}")))
    }
  })
  # ---- Compare Outputs ----
  output$vis1tab2000 <- renderLeaflet({
    left2000 <- left2000Reactive()

    pal <- colorFactor(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"),
                       left2000$ENERGY)
    
    if(is.null(input$box1) | nrow(left2000) == 0){
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopo") %>%
        addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") %>%
        setView(centers$x[as.numeric(input$states2)], centers$y[as.numeric(input$states2)], zoom = 6) %>%
        addLayersControl(baseGroups = c("CartoDB Positron", "Esri WorldTopo", "CartoDB Dark Matter"), 
                         position = "topright") %>%
        addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                                 onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
    else {
      leaflet(left2000) %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopo") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") %>%
      addCircleMarkers(lat = ~LAT, lng = ~LON, label = ~NAME,
                       color = ~pal(left2000$ENERGY), stroke = FALSE, fillOpacity = 1, 
                       radius = ~scales::rescale(TOTAL_GEN, c(4,10)),
                       popup = ~paste(NAME, "<br>",
                                     "\n% Renewable: ", scales::percent(PERCENT_RENEWABLE), "<br>",
                                     "\n% Non-Renewable: ", scales::percent(PERCENT_NON_RENEWABLE), "<br>",
                                     "Generation Capacity: ", TOTAL_GEN, " MWh", "<br>")) %>%
      addLegend("bottomright", pal = pal, values = ~ENERGY,
                title = "Power Plant Energy Type", opacity = 1) %>%
      # taking the input as as a number below because the coordinates are ordered by state number
      setView(centers$x[as.numeric(input$states1)], centers$y[as.numeric(input$states1)], zoom = 6) %>%
      addLayersControl(baseGroups = c("CartoDB Positron", "Esri WorldTopo", "CartoDB Dark Matter"), 
                       position = "topright") %>%
      addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                               onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
  })
  
  output$vis1tab2010 <- renderLeaflet({
    left2010 <- left2010Reactive()
    
    pal <- colorFactor(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"),
                       left2010$ENERGY)
    
    if(is.null(input$box1) | nrow(left2010) == 0){
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopo") %>%
        addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") %>%
        setView(centers$x[as.numeric(input$states2)], centers$y[as.numeric(input$states2)], zoom = 6) %>%
        addLayersControl(baseGroups = c("CartoDB Positron", "Esri WorldTopo", "CartoDB Dark Matter"), 
                         position = "topright") %>%
        addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                                 onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
    else {
      leaflet(left2010) %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopo") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") %>%
      addCircleMarkers(lat = ~LAT, lng = ~LON, label = ~NAME,
                       color = ~pal(left2010$ENERGY), stroke = FALSE, fillOpacity = 1, 
                       radius = ~scales::rescale(TOTAL_GEN, c(4,10)),
                       popup = ~paste(NAME, "<br>",
                                      "\n% Renewable: ", scales::percent(PERCENT_RENEWABLE), "<br>",
                                      "\n% Non-Renewable: ", scales::percent(PERCENT_NON_RENEWABLE), "<br>",
                                      "Generation Capacity: ", TOTAL_GEN, " MWh", "<br>")) %>%
      addLegend("bottomright", pal = pal, values = ~ENERGY,
                title = "Power Plant Energy Type", opacity = 1) %>%
      setView(centers$x[as.numeric(input$states1)], centers$y[as.numeric(input$states1)], zoom = 6) %>%
      addLayersControl(baseGroups = c("CartoDB Positron", "Esri WorldTopo", "CartoDB Dark Matter"), 
                       position = "topright") %>%
      addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                               onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
  })
  
  output$vis1tab2018 <- renderLeaflet({
    left2018 <- left2018Reactive()
    
    pal <- colorFactor(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"),
                       left2018$ENERGY)
    
    if(is.null(input$box1) | nrow(left2018) == 0){
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopo") %>%
        addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") %>%
        setView(centers$x[as.numeric(input$states2)], centers$y[as.numeric(input$states2)], zoom = 6) %>%
        addLayersControl(baseGroups = c("CartoDB Positron", "Esri WorldTopo", "CartoDB Dark Matter"), 
                         position = "topright") %>%
        addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                                 onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
    else{
      leaflet(left2018) %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopo") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") %>%
      addCircleMarkers(lat = ~LAT, lng = ~LON, label = ~NAME,
                       color = ~pal(left2018$ENERGY), stroke = FALSE, fillOpacity = 1, 
                       radius = ~scales::rescale(TOTAL_GEN, c(4,10)),
                       popup = ~paste(NAME, "<br>",
                                      "\n% Renewable: ", scales::percent(PERCENT_RENEWABLE), "<br>",
                                      "\n% Non-Renewable: ", scales::percent(PERCENT_NON_RENEWABLE), "<br>",
                                      "Generation Capacity: ", TOTAL_GEN, " MWh", "<br>")) %>%
      addLegend("bottomright", pal = pal, values = ~ENERGY,
                title = "Power Plant Energy Type", opacity = 1) %>%
      setView(centers$x[as.numeric(input$states1)], centers$y[as.numeric(input$states1)], zoom = 6) %>%
      addLayersControl(baseGroups = c("CartoDB Positron", "Esri WorldTopo", "CartoDB Dark Matter"), 
                       position = "topright") %>%
      addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                               onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
  })
  
  output$vis2tab2000 <- renderLeaflet({
    right2000 <- right2000Reactive()
    
    pal <- colorFactor(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"),
                       right2000$ENERGY)
    
    if(is.null(input$box2) | nrow(right2000) == 0){
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopo") %>%
        addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") %>%
        setView(centers$x[as.numeric(input$states2)], centers$y[as.numeric(input$states2)], zoom = 6) %>%
        addLayersControl(baseGroups = c("CartoDB Positron", "Esri WorldTopo", "CartoDB Dark Matter"), 
                         position = "topright") %>%
        addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                                 onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
    else {
      leaflet(right2000) %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopo") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") %>%
      addCircleMarkers(lat = ~LAT, lng = ~LON, label = ~NAME,
                       color = ~pal(right2000$ENERGY), stroke = FALSE, fillOpacity = 1, 
                       radius = ~scales::rescale(TOTAL_GEN, c(4,10)),
                       popup = ~paste(NAME, "<br>",
                                      "\n% Renewable: ", scales::percent(PERCENT_RENEWABLE), "<br>",
                                      "\n% Non-Renewable: ", scales::percent(PERCENT_NON_RENEWABLE), "<br>",
                                      "Generation Capacity: ", TOTAL_GEN, " MWh", "<br>")) %>%
      addLegend("bottomright", pal = pal, values = ~ENERGY,
                title = "Power Plant Energy Type", opacity = 1) %>%
      setView(centers$x[as.numeric(input$states2)], centers$y[as.numeric(input$states2)], zoom = 6) %>%
      addLayersControl(baseGroups = c("CartoDB Positron", "Esri WorldTopo", "CartoDB Dark Matter"), 
                       position = "topright") %>%
      addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                               onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
  })
  
  output$vis2tab2010 <- renderLeaflet({
    right2010 <- right2010Reactive()
    
    pal <- colorFactor(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"),
                       right2010$ENERGY)
    
    if(is.null(input$box2) | nrow(right2010) == 0){
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopo") %>%
        addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") %>%
        setView(centers$x[as.numeric(input$states2)], centers$y[as.numeric(input$states2)], zoom = 6) %>%
        addLayersControl(baseGroups = c("CartoDB Positron", "Esri WorldTopo", "CartoDB Dark Matter"), 
                         position = "topright") %>%
        addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                                 onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
    else {
      leaflet(right2010) %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopo") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") %>%
      addCircleMarkers(lat = ~LAT, lng = ~LON, label = ~NAME,
                       color = ~pal(right2010$ENERGY), stroke = FALSE, fillOpacity = 1, 
                       radius = ~scales::rescale(TOTAL_GEN, c(4,10)),
                       popup = ~paste(NAME, "<br>",
                                      "\n% Renewable: ", scales::percent(PERCENT_RENEWABLE), "<br>",
                                      "\n% Non-Renewable: ", scales::percent(PERCENT_NON_RENEWABLE), "<br>",
                                      "Generation Capacity: ", TOTAL_GEN, " MWh", "<br>")) %>%
      addLegend("bottomright", pal = pal, values = ~ENERGY,
                title = "Power Plant Energy Type", opacity = 1) %>%
      setView(centers$x[as.numeric(input$states2)], centers$y[as.numeric(input$states2)], zoom = 6) %>%
      addLayersControl(baseGroups = c("CartoDB Positron", "Esri WorldTopo", "CartoDB Dark Matter"), 
                       position = "topright") %>%
      addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                               onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
  })
  
  output$vis2tab2018 <- renderLeaflet({
    right2018 <- right2018Reactive()
    
    pal <- colorFactor(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"),
                       right2018$ENERGY)
    
    if(is.null(input$box2) | nrow(right2018) == 0){
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopo") %>%
        addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") %>%
        setView(centers$x[as.numeric(input$states2)], centers$y[as.numeric(input$states2)], zoom = 6) %>%
        addLayersControl(baseGroups = c("CartoDB Positron", "Esri WorldTopo", "CartoDB Dark Matter"), 
                         position = "topright") %>%
        addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                                 onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
    else{
      leaflet(right2018) %>% 
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopo") %>%
        addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") %>%
        addCircleMarkers(lat = ~LAT, lng = ~LON, label = ~NAME,
                         color = ~pal(right2018$ENERGY), stroke = FALSE, fillOpacity = 1, 
                         radius = ~scales::rescale(TOTAL_GEN, c(4,10)), 
                         popup = ~paste(NAME, "<br>",
                                        "\n% Renewable: ", scales::percent(PERCENT_RENEWABLE), "<br>",
                                        "\n% Non-Renewable: ", scales::percent(PERCENT_NON_RENEWABLE), "<br>",
                                        "Generation Capacity: ", TOTAL_GEN, " MWh", "<br>")) %>%
        addLegend("bottomright", pal = pal, values = ~ENERGY,
                  title = "Power Plant Energy Type", opacity = 1) %>%
        setView(centers$x[as.numeric(input$states2)], centers$y[as.numeric(input$states2)], zoom = 6) %>%
        addLayersControl(baseGroups = c("CartoDB Positron", "Esri WorldTopo", "CartoDB Dark Matter"), 
                         position = "topright") %>%
        addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                                 onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
  })
  # ---- US Map Outputs ----
  output$usmap2000 <- renderLeaflet({
    us2000 <- usview1Reactive()

    pal <- colorFactor(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"),
                       us2000$ENERGY)
    
    if(is.null(input$box3) | nrow(us2000) == 0) {
        leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
        setView(-95.665, 37.6, zoom = 4) %>%
        addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                                 onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
    else{
        leaflet(us2000) %>% addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(lat = ~LAT, lng = ~LON, label = ~NAME,
                         color = ~pal(us2000$ENERGY), stroke = FALSE, fillOpacity = 1, radius = 4) %>%
        addLegend("bottomright", pal = pal, values = ~ENERGY,
                  title = "Power Plant Energy Type", opacity = 1) %>%
        setView(-95.665, 37.6, zoom = 4) %>%
        addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                                 onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
  })
  
  output$usmap2010 <- renderLeaflet({
    us2010 <- usview2Reactive()
    
    pal <- colorFactor(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"),
                       us2010$ENERGY)
    
    if(is.null(input$box3) | nrow(us2010) == 0) {
      leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
        setView(-95.665, 37.6, zoom = 4) %>%
        addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                                 onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
    else {
        leaflet(us2010) %>% addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(lat = ~LAT, lng = ~LON, label = ~NAME,
                         color = ~pal(us2010$ENERGY), stroke = FALSE, fillOpacity = 1, radius = 4) %>%
        addLegend("bottomright", pal = pal, values = ~ENERGY,
                  title = "Power Plant Energy Type", opacity = 1) %>%
        setView(-95.665, 37.6, zoom = 4) %>%
        addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                                 onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
  })
  
  output$usmap2018 <- renderLeaflet({
    us2018 <- usview3Reactive()
    
    pal <- colorFactor(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"),
                       us2018$ENERGY)
    
    if(is.null(input$box3) | nrow(us2018) == 0) {
      leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
        setView(-95.665, 37.6, zoom = 4) %>%
        addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                                 onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
    else {
      leaflet(us2018) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(lat = ~LAT, lng = ~LON, label = ~NAME,
                       color = ~pal(us2018$ENERGY), stroke = FALSE, fillOpacity = 1, radius = 4) %>%
      addLegend("bottomright", pal = pal, values = ~ENERGY,
                title = "Power Plant Energy Type", opacity = 1) %>%
      setView(-95.665, 37.6, zoom = 4) %>%
      addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                               onClick = JS("function(btn, map){ map.setView([37.6,-95.665], 4);}")))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
