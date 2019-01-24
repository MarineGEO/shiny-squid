#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(marinegeoParseR)
library(DT)
library(leaflet)
library(ggplot2)
library(EML)
library(tidyverse)
library(uuid)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("ShinySquid"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        # Input: Select a file ----
       fileInput("file1", "Choose Squidpop File",
                 multiple = FALSE,
                 accept = c(".xlsx")),
       # Copy the chunk below to make a group of checkboxes
       # checkboxGroupInput("checkGroup", label = h5("Deployment Time"), 
       #                    choices = list("One hour" = 1, "Full Day" = 24),
       #                    selected = c(1, 24)),
       
       # Button
       # downloadButton("downloadData", "Download flatten Dataset"),
       # downloadButton("downloadEML", "Download EML"),
       downloadButton("downloadZip", label = "Data Package")
                   
                   
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
        tabPanel("Table", DT::dataTableOutput('contents')),
        tabPanel("Map",
                 leafletOutput("map", height="600px")),
        tabPanel("Figure", plotOutput("plot"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   tabs <- reactive({
     req(input$file1)
     readSquid(input$file1$datapath)
   })

   output$contents <- DT::renderDataTable({
     df <- flattenSquid(tabs()) 
   })
   
   flat <- reactive({
     req(tabs)
     df <- flattenSquid(tabs())
     df <- df %>% rowwise() %>%  mutate(id=UUIDgenerate()) 
   })
   
   getColor <- function(habitat){
     if(habitat=="Fore Reef"){
       "#F8766D"
     } else if (habitat=="Mangrove"){
       "#B79F00"
     } else if (habitat=="Patch Reef"){
       "#00BA38" 
     } else if (habitat=="Sand"){
       "#619CFF" 
     } else if (habitat=="Seagrass"){
       "#F564E3"
     } else {
       "black"
     }
   }
   
   
   output$map<-renderLeaflet({
     
     flat() %>% 
       rowwise() %>% 
       dplyr::mutate(circle_color = getColor(habitat)) %>% 
       df2sf() %>% 
       leaflet::leaflet() %>% 
       addProviderTiles(providers$Esri.WorldImagery, options = providerTileOptions(opacity = 0.55)) %>% # esri imagery
       addProviderTiles(providers$OpenMapSurfer.Roads, options = providerTileOptions(opacity = 0.35)) %>% #open map surfer
       addCircleMarkers(fillColor=~circle_color, radius=10, stroke = TRUE, fillOpacity = 1, color="black", weight=.5, layerId = flat()$id)
     })
   
   output$plot <- renderPlot({
      flat() %>% 
       mutate(percentEat=numberGone/numberRecovered) %>% 
       ggplot(aes(factor(elapsedTime), percentEat*100, group=location, color=habitat))+
       geom_point(stat="identity", position = position_dodge(0.2), alpha=0.9, shape=21, size=4, stroke=1.1)+
       ylab("% Eaten")+
       xlab("Elapsed time (hours)")+
       geom_line(linetype="dashed", alpha=0.25, color="black")+
       theme_bw()+
       theme(panel.grid.major = element_blank(),
             plot.title = element_text(hjust = 0.5),
             panel.grid.minor = element_blank(),
             legend.position="none", # position of legend or none
             legend.direction="horizontal", # orientation of legend
             legend.title= element_blank(), # no title for legend
             legend.key.size = unit(1.0, "cm"),
             axis.text.x=element_text(size=10),
             axis.text.y=element_text(size=10)
       )+
       facet_grid(~habitat)+
       theme(strip.background = element_blank(),
             strip.placement = "outside", 
             strip.text.x = element_text(size = 10))
     
     
   })
   
   # # Downloadable csv of selected dataset ----
   # output$downloadData <- downloadHandler(
   #   
   #   filename = function() {
   #     paste("test", ".csv", sep = "")
   #   },
   #   content = function(file) {
   #     df <- flattenSquid(tabs()) 
   #     write.csv(df, file, row.names = FALSE)
   #   }
   # )
   # 
   # output$downloadEML <- downloadHandler(
   #   
   #   filename = function() {
   #     paste("test", ".xml", sep = "")
   #   },
   #   content = function(file) {
   #     
   #     marinegeoParseR::packageSquid(input$file1$datapath, "test.csv", "eml.xml")
   #     #write.csv(df, file, row.names = FALSE)
   #   }
   # )
   
   
   output$downloadZip <- downloadHandler(
     filename = function() {
       zipname = gsub(pattern="\\.xlsx", "",input$file1$name)
       print(paste("Saving", zipname, "as a zip file"))
       paste(zipname, "zip", sep=".")
     },
     content = function(fname) {
       #fs <- c()
       tmpdir <- tempdir()
       setwd(tempdir())
       # for (i in c(1,2,3,4,5)) {
       #   path <- paste0("sample_", i, ".csv")
       #   fs <- c(fs, path)
       #   write(i*2, path)
       # }
       
       # write csv to tempdir
       df <- flattenSquid(tabs()) 
       csv_path <- gsub(pattern="\\.xlsx", ".csv",input$file1$name)
       write.csv(df, csv_path, row.names = FALSE)
       
       # write EMLto temp dir
       eml_path <- gsub(pattern="\\.xlsx", ".xml",input$file1$name)
       packageSquid(input$file1$datapath, csv_path, eml_path)
       
       # files to zip
       fs <- c(csv_path, eml_path)
       zip(zipfile=fname, files=fs)
     },
     contentType = "application/zip"
   )
   
   
   observeEvent(input$map_marker_click, {
     click <-  input$map_marker_click
     print(click)
    
     loc <- flat() %>% filter(id==click$id) 
     locat <- loc$location
     
     output$popupgraph <- renderPlot({
       flat() %>%  filter(location==locat) %>% 
         df2sf() %>% 
         ggplot(aes(factor(elapsedTime), numberGone/numberRecovered*100, fill=factor(elapsedTime)))+
         geom_bar(stat = "identity")+
         xlab("Elapsed Time (hours)")+
         ylab("% Eaten")+
         facet_wrap(~location)+
         theme_bw()+
         theme(panel.grid.major = element_blank(),
               plot.title = element_text(hjust = 0.5),
               panel.grid.minor = element_blank(),
               legend.position="none", # position of legend or none
               legend.direction="horizontal", # orientation of legend
               legend.title= element_blank(), # no title for legend
               legend.key.size = unit(1.0, "cm"),
               axis.text.x=element_text(size=10),
               axis.text.y=element_text(size=10)
         )+
         facet_grid(~location)+
         theme(strip.background = element_blank(),
               strip.placement = "outside", 
               strip.text.x = element_text(size = 10))
     })
     
   })
   
   
   
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

