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
library(reshape)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Workout 02: Investing"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      flowLayout(
         sliderInput("initial", "Initial Amount", 
                     min = 1,
                     max = 100000,
                     value = 1000), 
         sliderInput("rate","Return Rate(in %)", 
                     min = 1,
                     max = 20,
                     value = 5),
         sliderInput("years", "Years", 
                     min = 1,
                     max = 50,
                     value = 10),
         sliderInput("contrib", "Annual Contribution", 
                     min = 0,
                     max = 50000,
                     value = 2000),
         sliderInput("growth", "Growth Rate(in %):", 
                     min = 1,
                     max = 20,
                     value = 2),
         selectInput("facet", "Facet?", c("Yes", "No"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("timeline"),
         tableOutput("table")
      )
   )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
   output$timeline <- renderPlot({
     modalities <- data.frame(
       years = 0: input$years,
       no_contrib = rep(input$initial, input$years + 1),
       fixed_contrib = rep(input$initial, input$years + 1),
       growing_contrib = rep(input$initial, input$years + 1)
     )

     for(y in 1:input$years){
       fv <- input$initial * (1 + input$rate)** y
       fva <- input$contrib * ((1 + input$rate)**y - 1) / input$rate
      fvga <- input$contrib * ((1 + input$rate)**y - (1 + input$growth)**y) / (input$rate - input$growth)
       modalities$no_contrib[y + 1] <- fv
       modalities$fixed_contrib[y + 1] <- fv + fva
       modalities$growing_contrib[y + 1] <- fv + fvga
     }
      ggplot(modalities, aes(years)) + geom_line(aes(y = no_contrib, color = 'no_contrib'))+
        geom_line(aes(y = fixed_contrib, color = "fixed_contrib")) + geom_line(aes(y = growing_contrib, color = "growing_contrib")) +  
        ggtitle("Three modes of investing")
   })
   
   output$table <- renderTable(
     {
       modalities <- data.frame(
         years = 0: input$years,
         no_contrib = rep(input$initial, input$years + 1),
         fixed_contrib = rep(input$initial, input$years + 1),
         growing_contrib = rep(input$initial, input$years + 1)
       )
       
       for(y in 1:input$years){
         fv <- input$initial * (1 + input$rate)** y
         fva <- input$contrib * ((1 + input$rate)**y - 1) / input$rate
         fvga <- input$contrib * ((1 + input$rate)**y - (1 + input$growth)**y) / (input$rate - input$growth)
         modalities$no_contrib[y + 1] <- fv
         modalities$fixed_contrib[y + 1] <- fv + fva
         modalities$growing_contrib[y + 1] <- fv + fvga
       }
       
       return(modalities)
       
     }
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

