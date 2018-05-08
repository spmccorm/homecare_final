library(shiny)
library(dplyr)
library(ggplot2)
library(geosphere)
library(ggmap)
library(lubridate)
library(futile.logger)
library(utils)

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

torrance = retry(qmap("Torrance", zoom=10, maptype="hybrid"),maxErrors=20,sleep=1)
culvercity = retry(qmap("Culver City, CA", zoom=11, maptype="hybrid"),maxErrors = 20,sleep=1)
dallas = retry(qmap("Dallas, TX", zoom=10, maptype="hybrid"),maxErrors = 20,sleep=1)
scottsdale = retry(qmap("Scottsdale, AZ", zoom=10, maptype="hybrid"),maxErrors = 20,sleep=1)
encino = retry(qmap("encino, CA", zoom=11, maptype="hybrid"),maxErrors = 20,sleep=1)
irvine = retry(qmap("Irvine, CA", zoom=11, maptype="hybrid"),maxErrors = 20,sleep=1)
fullerton = retry(qmap("Fullerton, CA", zoom=11, maptype="hybrid"),maxErrors = 20,sleep=1)
sandiego = retry(qmap("San Diego, CA", zoom=11, maptype="hybrid"),maxErrors = 20,sleep=1)
carlsbad = retry(qmap("Carlsbad, CA", zoom=11, maptype="hybrid"),maxErrors = 20,sleep=1)
walnutcreek = retry(qmap("Walnut Creek, CA", zoom=11, maptype="hybrid"),maxErrors = 20,sleep=1)
santaclara = retry(qmap("Santa Clara, CA", zoom=11, maptype="hybrid"),maxErrors = 20,sleep=1)
pasadena = retry(qmap("Pasadena, CA", zoom=11, maptype="hybrid"),maxErrors = 20,sleep=1)

locationslist = shifts %>%
  group_by(LocationName) %>%
  summarise(count = n())
locationslist$LocationName = as.character(locationslist$LocationName)

ui = fluidPage(
  titlePanel(title= "24Hr HomeCare Statistical Visualizations",
             windowTitle="24HrHomeCareStats"),
  sidebarLayout(
    sidebarPanel(
      helpText("24Hr HomeCare has 12 branch locations, and our metrics are
               displayed on the branch level"),
      selectInput(inputId = "branch",
                  choices=locationslist[,1],selected = "Torrance",
                  label = "Choose a Branch Location:"),width=2
    ),
        
      mainPanel(helpText("The graphs below depict the location of Caregivers and Customers. The Caregiver
                         graph also shows the average distance the caregiver travels to the client's home,
                         while the Customer graph shows the average monthly revenue that the customer
                         generates"),
        fluidRow(
        splitLayout(cellWidths=c("50%", "50%"),plotOutput(outputId = "cgmap", height=640,width=640),
                plotOutput(outputId = "clientmap", height=640,width=640))),
        fluidRow(
          splitLayout(cellWidths=c("50%", "50%"),plotOutput(outputId = "distances"),
                      plotOutput(outputId = "monthlyrevs"))),
        plotOutput(outputId = "comparison")
   ))
)

server <- function(input, output) {
  
  shifts = reactive({
    shifts=read.csv("shifts.csv")
  })
  



  output$cgmap=renderPlot({
    
   branchmap = switch(input$branch,
                       "Torrance"=torrance,
                       "Encino"=encino,
                       "SantaClara"=santaclara,
                       "Carlsbad"=carlsbad,
                       "CulverCity"=culvercity,
                       "Dallas"=dallas,
                       "Fullerton"=fullerton,
                       "Irvine"=irvine,
                       "SanDiego"=sandiego,
                       "Scottsdale"=scottsdale,
                       "Pasadena"=pasadena,
                       "WalnutCreek"=walnutcreek)
   cgtitle = switch(input$branch,
                          "Torrance"="Torrance Caregiver Locations",
                          "Encino"="Encino Caregiver Locations",
                          "SantaClara"="Santa Clara Caregiver Locations",
                          "Carlsbad"="Carlsbad Caregiver Locations",
                          "CulverCity"="Culver City Caregiver Locations",
                          "Dallas"="Dallas Caregiver Locations",
                          "Fullerton"="Fullerton Caregiver Locations",
                          "Irvine"="Irvine Caregiver Locations",
                          "SanDiego"="San Diego Caregiver Locations",
                          "Scottsdale"="Scottsdale Caregiver Locations",
                          "Pasadena"="Pasadena Caregiver Locations",
                          "WalnutCreek"="Walnut Creek Caregiver Locations")
   
  

(branchmap + geom_point(data=filter(shifts(), LocationName==input$branch), 
                           aes(x=employeelon, y=employeelat,color
                           = avgdist),
                           size=3)+
      scale_color_gradient(low = "white", high="violetred4", name="Avg. Distance")+
      theme(legend.position = c(.11,.875))+
      ggtitle(cgtitle)+
      theme(plot.title = element_text(size = 20,family = "calibri")) )
    
    
  })
  
  output$clientmap=renderPlot({
    
    branchmap = switch(input$branch,
                       "Torrance"=torrance,
                       "Encino"=encino,
                       "SantaClara"=santaclara,
                       "Carlsbad"=carlsbad,
                       "CulverCity"=culvercity,
                       "Dallas"=dallas,
                       "Fullerton"=fullerton,
                       "Irvine"=irvine,
                       "SanDiego"=sandiego,
                       "Scottsdale"=scottsdale,
                       "Pasadena"=pasadena,
                       "WalnutCreek"=walnutcreek)
    
    customertitle = switch(input$branch,
                       "Torrance"="Torrance Customer Locations",
                       "Encino"="Encino Customer Locations",
                       "SantaClara"="Santa Clara Customer Locations",
                       "Carlsbad"="Carlsbad Customer Locations",
                       "CulverCity"="Culver City Customer Locations",
                       "Dallas"="Dallas Customer Locations",
                       "Fullerton"="Fullerton Customer Locations",
                       "Irvine"="Irvine Customer Locations",
                       "SanDiego"="San Diego Customer Locations",
                       "Scottsdale"="Scottsdale Customer Locations",
                       "Pasadena"="Pasadena Customer Locations",
                       "WalnutCreek"="Walnut Creek Customer Locations")
    
    (branchmap + geom_point(data=filter(shifts(), LocationName==input$branch), 
                           aes(x=customerlon, y=customerlat,color = avgmonthlyrev),
                           size=3)+
      scale_color_gradient(low = "white", high="chartreuse4", name="Avg. Monthly Rev")+
      theme(legend.position = c(.11,.875))+
      ggtitle(customertitle)+
      theme(plot.title = element_text(size = 20, family = "Calibri")))
    
      })
  
    
  output$distances = (renderPlot({
    ggplot(data=filter(shifts(), LocationName==input$branch), aes(x=avgdist))+
      geom_histogram(fill="violetred4")+
      ggtitle("Histogram of Average Distance Traveled by Caregivers")+
      xlab("Average Distance")+
      ylab("Count")+
      theme(plot.title = element_text(size = 20, family = "Calibri"))
    }))
  output$monthlyrevs = (renderPlot({
    ggplot(data=filter(shifts(), LocationName==input$branch), aes(x=avgmonthlyrev))+
      geom_histogram(fill="chartreuse4")+
      ggtitle("Histogram of Average Customer Monthly Revenue")+
      xlab("Average Revenue")+
      ylab("Count")+
      theme(plot.title = element_text(size = 20, family = "Calibri"))
  }))

output$comparison = renderPlot({
  ggplot(data=shifts(), aes(x=LocationName, y=mean(avgdist), z=mean(cgLTV)))+
    geom_bar(position = "dodge")
  
})
  
    }

shinyApp(ui = ui, server = server)


