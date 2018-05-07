library(shiny)
library(dplyr)
library(ggplot2)
library(geosphere)
library(ggmap)
library(lubridate)
library(futile.logger)
library(utils)
library(gridExtra)

    retry <- function(expr, isError=function(x) "try-error" %in% class(x), maxErrors=5, sleep=0) {
      attempts = 0
      retval = try(eval(expr))
      while (isError(retval)) {
        attempts = attempts + 1
        if (attempts >= maxErrors) {
          msg = sprintf("retry: too many retries [[%s]]", capture.output(str(retval)))
          flog.fatal(msg)
          stop(msg)
        } else {
          msg = sprintf("retry: error in attempt %i/%i [[%s]]", attempts, maxErrors, 
                        capture.output(str(retval)))
          flog.error(msg)
          warning(msg)
        }
        if (sleep > 0) Sys.sleep(sleep)
        retval = try(eval(expr))
      }
      return(retval)
    }

shifts = read.csv("shifts.csv")

# calculating distance between client and caregiver

locations = shifts %>%
  group_by(LocationName)%>%
  summarise(count = n())

torrance = retry(qmap("Torrance", zoom=10, maptype="hybrid"),maxErrors=10,sleep=1)
culvercity = retry(qmap("Culver City, CA", zoom=11, maptype="hybrid"),maxErrors = 10,sleep=1)
dallas = retry(qmap("Dallas, TX", zoom=10, maptype="hybrid"),maxErrors = 10,sleep=1)
scottsdale = retry(qmap("Scottsdale, AZ", zoom=10, maptype="hybrid"),maxErrors = 10,sleep=1)
encino = retry(qmap("encino, CA", zoom=11, maptype="hybrid"),maxErrors = 10,sleep=1)
irvine = retry(qmap("Irvine, CA", zoom=11, maptype="hybrid"),maxErrors = 10,sleep=1)
fullerton = retry(qmap("Fullerton, CA", zoom=11, maptype="hybrid"),maxErrors = 10,sleep=1)
sandiego = retry(qmap("San Diego, CA", zoom=11, maptype="hybrid"),maxErrors = 10,sleep=1)
carlsbad = retry(qmap("Carlsbad, CA", zoom=11, maptype="hybrid"),maxErrors = 10,sleep=1)
walnutcreek = retry(qmap("Walnut Creek, CA", zoom=11, maptype="hybrid"),maxErrors = 10,sleep=1)
santaclara = retry(qmap("Santa Clara, CA", zoom=11, maptype="hybrid"),maxErrors = 10,sleep=1)
pasadena = retry(qmap("Pasadena, CA", zoom=11, maptype="hybrid"),maxErrors = 10,sleep=1)



# Define UI for application that draws a histogram
ui = fluidPage(
  titlePanel(title= "24Hr HomeCare Statistical Visualizations",
             windowTitle="24HrHomeCareStats"),
  sidebarLayout(
    sidebarPanel(
      helpText("24Hr HomeCare has 12 branch locations, and our metrics are
               displayed on the branch level"),
      selectInput(inputId = "branch",
                  choices=locations$LocationName,
                  label = "Choose a Branch Location:")
    ),
        # Show a plot of the generated distribution
      mainPanel(helpText("The graphs below depict the location of Caregivers and Customers. The Caregiver
                         graph also shows the average distance the caregiver travels to the client's home,
                         while the Customer graph shows the average monthly revenue that the customer
                         generates"),
        fluidRow(
        splitLayout(cellWidths=c("50%", "50%"),plotOutput(outputId = "cgmap", height=600,width=600),
                plotOutput(outputId = "clientmap", height=600,width=600))))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
shifts = reactive({
  shift=read.csv("shifts.csv")
})
  
  # calculating distance between client and caregiver

  output$cgmap=renderPlot({
    
   branchmap = switch(input$branch,
                       "Torrance"=torrance,
                       "Encino"=encino,
                       "Santa Clara"=santaclara,
                       "Carlsbad"=carlsbad,
                       "Culver City"=culvercity,
                       "Dallas"=dallas,
                       "Fullerton"=fullerton,
                       "Irvine"=irvine,
                       "San Diego"=sandiego,
                       "Scottsdale"=scottsdale,
                       "Pasadena"=pasadena,
                       "Walnut Creek"=walnutcreek)
   cgtitle = switch(input$branch,
                          "Torrance"="Torrance Caregiver Locations",
                          "Encino"="Encino Caregiver Locations",
                          "Santa Clara"="Santa Clara Caregiver Locations",
                          "Carlsbad"="Carlsbad Caregiver Locations",
                          "Culver City"="Culver City Caregiver Locations",
                          "Dallas"="Dallas Caregiver Locations",
                          "Fullerton"="Fullerton Caregiver Locations",
                          "Irvine"="Irvine Caregiver Locations",
                          "San Diego"="San Diego Caregiver Locations",
                          "Scottsdale"="Scottsdale Caregiver Locations",
                          "Pasadena"="Pasadena Caregiver Locations",
                          "Walnut Creek"="Walnut Creek Caregiver Locations")
   
    
    branchmap + geom_point(data=shifts()[shifts()$LocationName==input$branch,], 
                           aes(x=employeelon, y=employeelat,color
                           = monthlyrev),
                           size=4)+
      scale_color_gradient(low = "white", high="violetred4", name="Avg. Distance")+
      theme(legend.position = c(.11,.875))+
      ggtitle(cgtitle)+
      theme(plot.title = element_text(size = 20,family = "calibri"))
    
  })
  
  output$clientmap=renderPlot({
    
    branchmap = switch(input$branch,
                       "Torrance"=torrance,
                       "Encino"=encino,
                       "Santa Clara"=santaclara,
                       "Carlsbad"=carlsbad,
                       "Culver City"=culvercity,
                       "Dallas"=dallas,
                       "Fullerton"=fullerton,
                       "Irvine"=irvine,
                       "San Diego"=sandiego,
                       "Scottsdale"=scottsdale,
                       "Pasadena"=pasadena,
                       "Walnut Creek"=walnutcreek)
    
    customertitle = switch(input$branch,
                       "Torrance"="Torrance Customer Locations",
                       "Encino"="Encino Customer Locations",
                       "Santa Clara"="Santa Clara Customer Locations",
                       "Carlsbad"="Carlsbad Customer Locations",
                       "Culver City"="Culver City Customer Locations",
                       "Dallas"="Dallas Customer Locations",
                       "Fullerton"="Fullerton Customer Locations",
                       "Irvine"="Irvine Customer Locations",
                       "San Diego"="San Diego Customer Locations",
                       "Scottsdale"="Scottsdale Customer Locations",
                       "Pasadena"="Pasadena Customer Locations",
                       "Walnut Creek"="Walnut Creek Customer Locations")
    
    branchmap + geom_point(data=shifts()[shifts()$LocationName==input$branch,], 
                           aes(x=customerlon, y=customerlat,color = monthlyrev),
                           size=4)+
      scale_color_gradient(low = "white", high="chartreuse4", name="Avg. Monthly Rev")+
      theme(legend.position = c(.11,.875))+
      ggtitle(customertitle)+
      theme(plot.title = element_text(size = 20, family = "calibri"))
      })
    }

# Run the application 
shinyApp(ui = ui, server = server)

