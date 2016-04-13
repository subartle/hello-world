#What is Shiny:
#a web application framework for R. Outcome is an interactive webpage.
#You only need R in order to create a Shiny app, you dont need wb application development skills
#Shiny Structure
#UI.R: user interface script, controls the layout and appearance of your app. ALl of the UI elements, such as slider bar and page layout

library("shiny")

#DATA STEPS
data <- read.csv("https://raw.githubusercontent.com/subartle/hello-world/master/data.csv")
countrieslist <- as.character(data[ , "Origin"])
countries <- unique(unlist(countrieslist))

#Define the overall UI
#fluid page divides the page into columns and rows
my.ui <- shinyUI(navbarPage("International student", 
                            tabPanel("Pie Chart",
                                     fluidPage(   
                                       titlePanel("Student type"),
                                       sidebarLayout(     
                                         sidebarPanel(
                                           selectInput("type", "Type:",
                                                       choices=colnames(data[,3:7])),
                                           hr(), 
                                           sliderInput("year", "Year",
                                                       min = 2005, max = 2012, value = 1),
                                           hr(),
                                           helpText("Data from Institute of International Education")
                                         ),
                                         mainPanel(
                                           plotOutput("studentPlot") 
                                         )     
                                       )
                                     )    
                            ),
                            tabPanel("Line Chart",
                            fluidPage(
                              titlePanel("Student origin"),
                              sidebarLayout(
                                sidebarPanel(
                                  selectInput("linecountry","Country",
                                              choices = countries),
                                  helpText("Changes of number based country")
                                ),
                                mainPanel(
                                  plotOutput("originLinechart")
                                )
                              )
                            )
)
)
)

my.server <- shinyServer(function(input, output){
  piedata <- reactive({
    a <- subset(data, Year %in% input$year)
    a <- droplevels(a)
    return(a)
  })
  
  linedata <- reactive({
    a <- subset(data, Origin %in% input$linecountry)
    a <- droplevels(a)
    return(a)
  })
  
  output$studentPlot <- renderPlot({
    studentdata <- piedata()
    pie(studentdata[1:10, input$type],
        labels = studentdata[1:10, 2],
        main = "Student Origin Composition",
        col = rainbow(10))
  })
  
  output$originLinechart <- renderPlot({
    linechartdata <- linedata()
    #Render a line plot
    plot(linechartdata$Year,
         linechartdata$Total,
         type = "b",
         main = "Student Number Trend",
         xlab = "Year",
         ylab = "Number of Student",
         col = "red",
         xlim=c(2005,2012))
  })
})

shinyApp( ui=my.ui, server=my.server )
