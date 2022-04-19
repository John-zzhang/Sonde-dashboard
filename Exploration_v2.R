
library(magrittr)
library(htmltools)
library(shinydashboard)


# set default sample site (Id=85006) and calcualte the min and max year
yearStart <- join_camrose_pelcomb %>% dplyr::filter(ID==85006) %$% min(lubridate::year(`Date (MM/DD/YYYY)`))
yearEnd <- join_camrose_pelcomb %>% dplyr::filter(ID==85006) %$% max(lubridate::year(`Date (MM/DD/YYYY)`))

# UI component
explorationUI <- function(id){
     ns <- NS(id)
    
     fluidPage(
         shinyWidgets::useShinydashboard(),
       
         fluidRow(
                 column(7, 
                          wellPanel(
                                  style = "border-color: blue; height: 400px;",
                                     # p(tags$b("1. Select a sample point on the map"), 
                                     #      style = "font-size:16px; color:#4997d0"),
                                     leafletOutput(ns("sampleMap"),width = "100%",height = 365)
                                    )
                       ), 
                 
                 column(5,
                        dataTableOutput(ns("table"))
                       ) 
                ),

         fluidRow(
              
                column(7,
                         p(tags$b("1. Choose a start and an end year"), style = "font-size:16px;color:#4997d0;"),
                         
                         sliderInput(ns("yearRange"),
                                     # label="2. Choose a start and end year:",
                                     label = NULL,
                                     min = yearStart,
                                     max = yearEnd,
                                     value=c(yearStart,yearEnd),
                                     width = '800px',
                                     animate = TRUE,
                                     sep = "" )
                      )
                 ),
        
         fluidRow(
            
               column(6,
                      p(tags$b("2. Choose sample site(s) from the list"), style = "font-size:16px;color:#4997d0;"),
                      wellPanel(
                        style = "border-color: blue",
                        selectizeInput(ns("sampleSelection"),label = "", 
                                       choice = NULL,
                                       # choices =metadata_Sonde$Name,
                                       multiple = TRUE,
                                       selected = "Camrose_knock Brook at Cutty B",
                                       options = list(placeholder = 'select sample site(s)')
                                       )
                                )
                      ),
       
               column(5,
                       p(tags$b("2. Choose an element from the list"), style = "font-size:16px;color:#4997d0;"),
                       
                       wellPanel(
                            style = "border-color: blue",
                            # selectizeInput(ns("sampleSelection"),label = "1. Please select a sample point", choices =metadata_Sonde$df_Name,selected = "camrose_knock"),
                       uiOutput(ns("paraSelection"))
                             )
                    )
               ),
         
         hr(),
         plotlyOutput(ns("plot")),
         hr(),
         
         fluidRow(
           
           column(6,
                  p(tags$b("3. Choose sample site(s) from the list"), style = "font-size:16px;color:#4997d0;"),
                  wellPanel(
                    style = "border-color: blue",
                    selectizeInput(ns("sampleSelection2"),label = "", 
                                   choice = NULL,
                                   # choices =metadata_Sonde$Name,
                                   # multiple = TRUE,
                                   selected = "Camrose_knock Brook at Cutty B",
                                   options = list(placeholder = 'select sample site(s)')
                    )
                  )
           ),
           
           column(5,
                  p(tags$b("3. Choose an element from the list"), style = "font-size:16px;color:#4997d0;"),
                  
                  wellPanel(
                    style = "border-color: blue",
                    # selectizeInput(ns("sampleSelection"),label = "1. Please select a sample point", choices =metadata_Sonde$df_Name,selected = "camrose_knock"),
                    uiOutput(ns("paraSelection2"))
                  )
           )
         ),
         # textOutput(ns("textSampleSite")),
         # textOutput(ns("testSelection")),
         # textOutput(ns("textSample")),
         hr(),
         plotlyOutput(ns("plot2")),
         hr()
     )
  
        }

# Exploration Server
explorationServer <- function (input,output,session){
  
 
  
  # Render leaflet
  
  output$sampleMap <- renderLeaflet({
                          leaflet(data=metadata_Sonde) %>%
                          setView(lng = -4.98,lat = 51.82, zoom = 13) %>%
                        
                          setMaxBounds(-5.42, 51.25, -2.08, 53.39) %>% 
                          addTiles() %>%
                          addProviderTiles("Esri.WorldTopoMap", group = "USGS USImagery") %>%
                          addProviderTiles("Esri.WorldImagery",group = "Esri WorldImagery") %>%
                          addProviderTiles("Esri.WorldShadedRelief",group = "Esri WorldShadedRelief") %>%
                          addProviderTiles(providers$Esri.WorldGrayCanvas,group =  "Esri WorldGrayCanvas") %>%
                     
                          addMarkers(~Longitude, ~Latitude,
                                     popup = ~as.character(paste(ID,Name)),
                                     layerId =~ID,
                                     # icon = pin_darkblue,
                                     label = ~as.character(paste(ID,Name)),
                                     labelOptions = labelOptions(textsize = "10px"),
                                    group = "Markers") %>% 
    
                         addPolygons(data =  simplifiedWalesBoundaries,
                                              color = "black",
                                              fillOpacity = 0,
                                              weight  = 1,
                                              popup = ~name,
                                              group = "County Boundary") %>%
                        addPolylines(data = simplifiedRiver,
                                               color = "blue",
                                               weight  = 1,
                                               popup =~RIV_NAME,
                                               group = "River"
                                                ) %>% 
                        addMiniMap(width = 150,
                                   height = 150,
                                   toggleDisplay = TRUE,
                                   autoToggleDisplay = TRUE,
                                   minimized = TRUE) %>% 

                        addLayersControl(
                               baseGroups = c("OpenStreetMap", "USGS USImagery","Esri WorldImagery",
                                              "Esri WorldShadedRelief","Esri WorldGrayCanvas"),
                               overlayGroups = c("Markers","County Boundary", "River")
                                          ) %>% 
      
                        hideGroup("County Boundary") %>%
                  
                        leafem::addMouseCoordinates() %>%
                  
                        inlmisc::AddHomeButton() %>%
                        inlmisc::AddSearchButton(group = "Markers", zoom = 15,
                                                    textPlaceholder = "Search sample sites...")
                     
                })
  
             
  # create a reactive value to store the clicked station
    selectedSample <- reactiveValues(ID = 85006)
    
  # update the SelectizeInput
    updateSelectizeInput(session, "sampleSelection", choices = metadata_Sonde$Name, server = TRUE)
    
    # update the SelectizeInput2
    updateSelectizeInput(session, "sampleSelection2", choices = metadata_Sonde$Name, server = TRUE)
    
   # store the click station id
      observeEvent(input$sampleMap_marker_click, {
         selectedSample$ID <-  input$sampleMap_marker_click$id
      
       # update slider input min, max and range value for year
         yearStart <- join_camrose_pelcomb %>%
         dplyr::filter(ID==selectedSample$ID) %$% min(lubridate::year(`Date (MM/DD/YYYY)`))
      
         yearEnd <- join_camrose_pelcomb %>%
         dplyr::filter(ID==selectedSample$ID) %$% max(lubridate::year(`Date (MM/DD/YYYY)`))
      
         yearValueRange <- c(yearStart,yearEnd)
      
         updateSliderInput(session, "yearRange",
                        min = yearStart,
                        max = yearEnd,
                        value = yearValueRange)
        })  
    
        filteredData <- reactive({
                        filterd <- join_camrose_pelcomb %>%
                          dplyr::filter(ID==selectedSample$ID)
                         })
        
        filteredDataDropBox <- reactive({
       
                    filterd2 <- join_camrose_pelcomb %>%
                    dplyr::filter(Name %in% input$sampleSelection)
        })
        
        filteredDataDropBox2 <- reactive({
          
          filterd3 <- join_camrose_pelcomb %>%
            dplyr::filter(Name %in% input$sampleSelection2)
        })
     
        output$paraSelection <- renderUI({
            # varSelectInput(session$ns("variable"), label = "2. Please select a variable", data = get(input$sampleSelection),selected = "pH")
          varSelectInput(session$ns("variable"),  
                           label = "",
                           # data = join_camrose_pelcomb %>% dplyr::filter(ID==selectedSample$ID),
                           data = filteredDataDropBox(),
                           selected = "pH")
            
                  })
        
        output$paraSelection2 <- renderUI({
          # varSelectInput(session$ns("variable"), label = "2. Please select a variable", data = get(input$sampleSelection),selected = "pH")
          varSelectInput(session$ns("variable2"),  
                         label = "",
                         # data = join_camrose_pelcomb %>% dplyr::filter(ID==selectedSample$ID),
                         data = filteredDataDropBox2(),
                         multiple = TRUE,
                         selected = "pH")
                         # options = list(placeholder = 'select variable(s)')
                     
        })
       
          # dataInput <- reactive({
          #   test <- as.character(input$variable)})
          
          # output$textSample <- renderText({
          #   paste0("The selected sample variable is ", input$variable)
          #          # as.character(input$variable))
          # }
          # )
          # 
          # output$textSampleSite <- renderText({
          #   paste0("The selected sample site is ", input$sampleSelection)
          # })
          # 
          # output$texttestSelection <- renderText({
          #   paste0("The selected sample site is ", input$variable2)
          # })
          
  # Render table    
          output$table <- renderDataTable({
            # selSampleID <- selectedSample$ID
            metadata_Sonde %>%
              #
              dplyr::select(.,c(1:7)) %>%
              dplyr::mutate(flag=ifelse(ID==selectedSample$ID,"TRUE","FALSE")) %>%
              dplyr::arrange(desc(flag)) %>%
              # dplyr::select(.,c(1:4)) %>%
              # dplyr::mutate(ranking = rank(desc(`catchment-area`))) %>%

              datatable(extension = "Scroller",
                        options = list(deferRender=TRUE,
                                       scrollY=300,
                                       scrollX = TRUE,
                                       scroller=TRUE,
                                       autoWidth = FALSE,
                                       columnDefs = list(list(width = '100px', targets = c(0,2)))

                        )) %>%
              formatStyle('ID', target = 'row', backgroundColor = styleEqual(selectedSample$ID, 'lightskyblue'))
          })

          # update the SelectizeInput
          
          updateSelectizeInput(session, inputId = "sampleSelection", choices = metadata_Sonde$Name, server = FALSE)
          
          
 # Render plotly depending of the selected parameter

        output$plot <- renderPlotly({
          
          # if(is.null(input$sampleSelection)){return()}

          # if(is.null(input$sampleSelection)){return()}
          
           req(input$sampleSelection)

           # df <- get(input$sampleSelection)
           # df <- join_camrose_pelcomb %>% 
           #              dplyr::filter(ID == selectedSample$ID)
            # df <- filteredDataDropBox() %>%
            #   dplyr::filter(lubridate::year(`Date (MM/DD/YYYY)`)>=input$yearRange[1] & lubridate::year(`Date (MM/DD/YYYY)`)<=input$yearRange[2]) %>%
            #   dplyr::filter(Name==input$sampleSelection[[1]])
            # 
            # n <- length(input$sampleSelection)
            # 
            # p <- plot_ly()
            
            # for (i in 1:n)
            # {
            sampleData <- filteredDataDropBox() %>% 
              # join_camrose_pelcomb %>%
              dplyr::filter(lubridate::year(`Date (MM/DD/YYYY)`)>=input$yearRange[1] & lubridate::year(`Date (MM/DD/YYYY)`)<=input$yearRange[2]) %>%
              group_by(Name) 
            
            plot_ly(data = sampleData, x = ~lubridate::ymd_hms(Time),
                    y = ~ sampleData[[as.name(input$variable)]], type = "scatter", mode = "lines",color = ~Name ) %>% 
            
            # 
               layout(
            title= list(text = paste0("Time series plot of ",
                                            input$variable),
                                     # "at sample site ", input$sampleSelection$Name,
            #                                 # metadata_Sonde$name[metadata_Sonde$ID==selectedSample$ID],
            #                                 "at site of ",
            #                                 metadata_Sonde$Name[metadata_Sonde$Name == input$sampleSelection],
            #                                 "ID =",unique(df$ID)),
                               font = list(size = 18,
                                           face = "bold")
                               ),
            # 
                  xaxis = list(title = "Date-Time",
                                font = list(
                                            family = "Courier New, monospace",
                                            size = 15,
                                            color = "RebeccaPurple")
                                ),

                  yaxis = list(title = as.character(input$variable),
                                font = list(
                                            family = "Courier New, monospace",
                                            size = 15,
                                            color = "RebeccaPurple")
                                ),
                
               
                  plot_bgcolor='#e5ecf6', 
                  paper_bgcolor = "rgba(0, 0, 0, 0)",
                  fig_bgcolor   = "rgba(0, 0, 0, 0)"
                 )
        })
         
        
        output$plot2 <- renderPlotly({
          
          req(input$sampleSelection2)
          
          n <- length(input$variable2)
         
          plot_list = vector("list", n)
         
          sampleData2 <- filteredDataDropBox2() %>%
                       dplyr::filter(lubridate::year(`Date (MM/DD/YYYY)`)>=input$yearRange[1] & lubridate::year(`Date (MM/DD/YYYY)`)<=input$yearRange[2])

          for (i in 1:n)
          {
        
            plot_list[[i]] <- plotly_build(plot_ly(data = sampleData2, x = ~lubridate::ymd_hms(Time),
                 y = ~ sampleData2[[as.name(input$variable2[[i]])]], name = input$variable2[[i]], type = "scatter", mode = "lines",evaluate = TRUE) %>% 
             
                layout(
                
                   xaxis = list(title = "Date-Time",
                              font = list(
                                family = "Courier New, monospace",
                                size = 15,
                                color = "RebeccaPurple")
                 ),
                 
                   yaxis = list(title = as.character(input$variable2[[i]]),
                              font = list(
                                family = "Courier New, monospace",
                                size = 11,
                                color = "RebeccaPurple")
                 ),
                 
              
                 plot_bgcolor='#e5ecf6',
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 fig_bgcolor   = "rgba(0, 0, 0, 0)"
            ) 
            
            )     
 
          }
          
       
         subplot(plot_list,nrows= (as.integer(n/3)+1),shareX = TRUE,titleY = TRUE,margin = 0.05) %>% 
           layout(title = list(text = paste0("Sample site _ ", input$sampleSelection2),
                          font = list(
                            # family = "Courier New, monospace",
                                       size = 18,
                                       face = "bold",
                                       color = "RebeccaPurple")))
         
       
        })
    }

shinyApp(explorationUI,explorationServer)

