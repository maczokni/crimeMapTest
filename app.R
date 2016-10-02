library(shiny)
library(leaflet)
library(rgdal)

ui <- fluidPage(
  titlePanel("This is an example map"),
  
  sidebarLayout(
    sidebarPanel( "Crime data from police.uk Boundary data from London datastore (data.london.gov.uk)"),
    mainPanel("Here are some crimes in London", leafletOutput("map", height = 800)) 
  )
)

server <- function(input, output) {
  #read in boundary file
  londonBoroughs <- readOGR(dsn = "/Volumes/AwesomeBackup/untitled folder/dirs/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI", "London_Borough_Excluding_MHW")
  #read in csv of crimes
  londonCrimes <- read.csv('2016-01-metropolitan-street.csv')
  #remove any rows with no coords
  londonCrimes_SP <- subset(londonCrimes, Latitude != "" | Longitude != "")
  #turn into spatial obj
  coords <- cbind(longitude = londonCrimes_SP$Longitude, latitude = londonCrimes_SP$Latitude)
  londonCrimes_SP <- SpatialPointsDataFrame(coords, data = londonCrimes_SP, proj4string = CRS("+proj=longlat +datum=WGS84"))
  #turn boundary file to wsg84
  boroughs.wgs84 <- spTransform(londonBoroughs, CRS("+proj=longlat +datum=WGS84"))
  
  #count points in polygon
  crimePerBorough <- over(londonCrimes_SP, boroughs.wgs84)
  
  #turn count table into dataframe
  counts <- as.data.frame(table(crimePerBorough$NAME)) #make a df from our count table
  names(counts)[names(counts) == 'Var1'] <- 'NAME'     #give those variables some meaningful names
  names(counts)[names(counts) == 'Freq'] <- 'crimes'
  
  #join to boundary polygon
  boroughs.wgs84@data = data.frame(boroughs.wgs84@data, counts[match(boroughs.wgs84@data[,'NAME'], counts[,'NAME']),])
  
  #now make leaflet map
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = boroughs.wgs84$burglaries
  )
  
  output$map <- renderLeaflet({
  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons( data = boroughs.wgs84,
                 stroke = TRUE, weight= 1, fillOpacity = 0.8, 
                 color = ~pal(crimes)#, 
                 # popup = boroughs_popup
                 )
  })
  
  }


shinyApp(ui = ui, server = server)
