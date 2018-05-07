library(shiny)
library(dplyr)
library(ggplot2)
library(geosphere)
library(ggmap)


shifts = read.csv("shifts.csv")

cgdistance = shifts %>%
  group_by(EmployeeKey) %>%
  summarise(avgdist = mean(Distance))

shifts = shifts %>%
  left_join(x=shifts, y=cgdistance, by= "EmployeeKey")

lengthofstay = shifts %>%
  group_by(CustomerKey) %>%
  summarise(startdate = min(DateKeyService),
            enddate = max(DateKeyService))
  

shifts = shifts %>% 
  mutate(Distance = distHaversine(p1 = cbind(shifts$employeelon,shifts$employeelat),                                  p2=cbind(shifts$customerlon,shifts$customerlat))*0.000621371) %>%
  filter(Distance < 100) %>%
  

torrance = qmap("Torrance", zoom=11, maptype="hybrid")
culvercity = qmap("Culver City, CA", zoom=11, maptype="hybrid")
dallas = qmap("Dallas, TX", zoom=10, maptype="hybrid")
scottsdale = qmap("Scottsdale, AZ", zoom=10, maptype="hybrid")
encino = qmap("encino, CA", zoom=11, maptype="hybrid")
irvine = qmap("Irvine, CA", zoom=11, maptype="hybrid")
fullerton = qmap("Fullerton, CA", zoom=11, maptype="hybrid")
sandiego = qmap("San Diego, CA", zoom=11, maptype="hybrid")
carlsbad = qmap("Carlsbad, CA", zoom=11, maptype="hybrid")
walnutcreek = qmap("Walnut Creek, CA", zoom=11, maptype="hybrid")
santaclara = qmap("Santa Clara, CA", zoom=11, maptype="hybrid")
pasadena = qmap("Pasadena, CA", zoom=11, maptype="hybrid")


torrancedata = shifts %>%
  filter(LocationName=="Torrance")
torrance + geom_point(data=torrancedata, aes(x=customerlon, y=customerlat),
                      color="purple")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

