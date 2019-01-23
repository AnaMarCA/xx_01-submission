
library(shiny)
library(xts)
library(leaflet)
library(dplyr)
library(ggplot2)

Nor_cities_post <- read.csv("postnummer-2.txt", sep= "\t", fileEncoding="UTF-8")
alarm_data_pj10 <- read.csv("alarm_data_pj10.csv")
alarm_data_pj10$date <- as.Date(alarm_data_pj10$date)

cities_post <- Nor_cities_post %>% 
  select(KOMMUNE, LAT,LON) %>% 
  group_by(KOMMUNE) %>% 
  mutate(Municipality=KOMMUNE,
         nrow= seq(1:n()) ,
         dist_mun = n_distinct(Municipality)) %>% 
  ungroup %>% 
  select(-KOMMUNE) %>%  filter(nrow==1) %>%  select(-dist_mun,-nrow)

map_data <- alarm_data_pj10 %>% 
  left_join(cities_post) %>% 
  select(date,potential_outbreak,Municipality,LAT,LON)%>% 
  na.omit() %>%
  ungroup() %>% 
  # filter(date > "2010-10-11") %>% 
  filter(date > "2010-10-11")

df_for_shin <- alarm_data_pj10 %>% 
  left_join(cities_post) %>% 
  select(date,potential_outbreak,Municipality,LAT,LON)%>% 
  na.omit() %>%
  ungroup() %>% 
  # filter(date > "2010-10-11") %>% 
  filter(date > "2010-01-04") %>% 
  group_by(date) %>% 
  summarise(weeks_outbreaks= sum(potential_outbreak)) 

jpeg(filename = "shiny_plot.jpeg")  
df_for_shin %>% 
  ggplot() +
  geom_rect(aes(xmin=as.Date("2010-10-11"), xmax=as.Date("2010-12-27"), ymin=0, ymax= Inf), alpha = 0.19, fill = "salmon") +
  geom_line(aes(x=date,y=weeks_outbreaks)) + xlab("Date") + ylab("Number of cases of disease X ")

dev.off()


## shiny ####


names(map_data)[4]<-paste("lat") 
names(map_data)[5]<-paste("lon") 
names(map_data)[1]<-paste("Date") 
map_data$potential_outbreak <- as.factor(map_data$potential_outbreak )

#date <-seq(as.Date("2010-10-18"), as.Date("2010-12-27"), by="day") 
date <- map_data %>% 
  na.omit() %>%
  ungroup() %>% 
  filter(Date > "2010-10-11") %>% filter(Municipality=="Alta") %>%  select(Date) %>%  pull()



ui <- fluidPage(
  titlePanel( 
    fluidRow( "Disease X outbreak investigation")),
  sidebarPanel(  p("Play video or set the time of observation."),
                 sliderInput("time", "date",min(date), 
                             max(date),
                             value = min(date),
                             step=7,
                             animate=T)),
  mainPanel(h3("Locations of potential outbreak events for disease X in Norway, for the period of 2010-10-18 to 2010-12-27." ),
            h3("The municipalities marked in green displayed abnormally high counts of reported disease events. The interactive map displays this change in the locations."),
            br()),
  mainPanel(img(src="shiny_plot.jpeg", height = 250, width = 450),
            img(src="Chart1.png", height = 50, width = 250)),
  
  leafletOutput("mymap", height=1100 )
)


server <- function(input, output, session) {
  
  getColor <- function(map_data) {
    sapply(map_data$potential_outbreak, function(potential_outbreak) {
      if(potential_outbreak == 1) {
        "red"
      } else if(potential_outbreak == 0) {
        "green"
      } else {
        "orange"
      } })
  }
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(map_data)
  )
  
  points <- reactive({
    map_data %>% 
      filter(Date== input$time) %>% 
      filter(potential_outbreak== 1)
  })
  
  # points2 <- reactive({
  #   map_data %>% 
  #     filter(Date== input$time) %>% 
  #     filter(potential_outbreak == 0)
  # })
  center <- reactive({
    
    if(is.null(input$map01_center)){
      return(c(179.462, -20.64275))
    }else{
      return(input$map01_center)
    }
    
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(17.888237,64.5783089,zoom = 5) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE) 
      ) %>% 
      
      addAwesomeMarkers(data = points(), ~lon, ~lat, icon=icons, label=~as.character(points()$Municipality)) # %>% 
    #  addAwesomeMarkers(data = points2(), ~lon, ~lat, icon=icons, label=~as.character(points2()$Municipality), group = "No alarm")# %>% 
    # Layers control
    #  addLayersControl(
    #    overlayGroups = c("No alarm","Outbreak"),
    #     options = layersControlOptions(collapsed = FALSE)
    #  )
  })
}


shinyApp(ui, server)



