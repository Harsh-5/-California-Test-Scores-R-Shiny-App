library(shiny)
library(ggplot2)
library(dplyr)

setwd("/Users/drewyounkin/Downloads")
caschools <- read.csv("CASchools.csv")

ui <- fluidPage(
    titlePanel("California Test Scores"),
    
    sidebarLayout(
        sidebarPanel(
            # Input Selection
            selectInput("ctyInput", "County",
                        choices = c(unique(caschools$county)),selected = "Los Angeles"),
            # Slider
            sliderInput("icmInput", "Income", round(min(caschools$income),2), round(max(caschools$income),2),
                        c(round(min(caschools$income),2)+1, round(max(caschools$income),2)-1)),
            
           radioButtons("gradeInput", "Size of School",c("KK-06", "KK-08"),selected = "KK-08")
            
            
        ),
        mainPanel(
            # Plot widget
            plotOutput("caschoolplot"),
            
            #leave two lines
            br(), br(),
            
            # Table widget
            tableOutput("results")
        )
    )
)

server <- function(input, output) {
    # Get the subset from the original data frame
    filtered <- reactive({
        caschools %>%
            filter(income >= input$icmInput[1],
                   income <= input$icmInput[2],
                   county == input$ctyInput,
                   grades == input$gradeInput
            )
       
    })
    
 
    
    
    output$caschoolplot <- renderPlot({
        
        ggplot(filtered(), aes(x=teachers,y=math)) +
            geom_point(aes(size= expenditure,color = expenditure))
    })
    
    output$results <- renderTable({
        
        filtered()
    })
}
shinyApp(ui = ui, server = server)