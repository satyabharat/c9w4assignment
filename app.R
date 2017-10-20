#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)


# Loading Data into from csv file

inputd <- read.csv("prjInput.csv", header=TRUE)
colnames(inputd)[1]<- "vertical"
colnames(inputd)[6]<-"loc"
colnames(inputd)[7]<-"effloc"

ui <- fluidPage(
   
   # Application Complexities based on industry segment analyzed
   titlePanel("Industry wise compexities"),
   
   # Sidebar with a slider input to select industry, country, year. 
   sidebarLayout(
      sidebarPanel(
        selectInput("vert", "Select Industry:",
                    choices = inputd$vertical),
        selectInput("cntry", "Select Country:",
                    choices = inputd$Country),
        submitButton("Submit")
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("myplot")
      )
   )
)

# Define server logic required to plot

server <- function(input, output) {

   output$myplot <- renderPlot({
     
     fltr1 <- subset(inputd, (inputd$vertical == input$vert & inputd$Country == input$cntry))
     
     fltr1$loc = as.numeric(fltr1$loc)
     
     fyloc <- aggregate(loc~FY+Quarter, data=fltr1, FUN=sum)
   
     
    plot(fyloc,fltr1$vertical)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

