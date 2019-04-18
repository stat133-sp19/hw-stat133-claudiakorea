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
         selectInput("facet", "Facet?", c("No", "Yes"))
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
     if (input$facet == "No"){
       modalities <- data.frame(
         years = 0: input$years,
         no_contrib = rep(input$initial, input$years + 1),
         fixed_contrib = rep(input$initial, input$years + 1),
         growing_contrib = rep(input$initial, input$years + 1)
       )
  
       for(y in 1:input$years){
         rate <- input$rate/100
         growth <- input$growth/100
         fv <- input$initial * (1 + rate)** y
         fva <- input$contrib * ((1 + rate)**y - 1) / rate
        fvga <- input$contrib * ((1 + rate)**y - (1 + growth)**y) / (rate - growth)
         modalities$no_contrib[y + 1] <- fv
         modalities$fixed_contrib[y + 1] <- fv + fva
         modalities$growing_contrib[y + 1] <- fv + fvga
       }
       
        ggplot(modalities, aes(years)) + geom_line(aes(y = no_contrib, color = 'no_contrib'))+
          geom_line(aes(y = fixed_contrib, color = "fixed_contrib")) + geom_line(aes(y = growing_contrib, color = "growing_contrib")) +  
          geom_point(aes(y = fixed_contrib, color = "fixed_contrib")) + geom_point(aes(y = growing_contrib, color = "growing_contrib")) +  
          geom_point(aes(y = no_contrib, color = 'no_contrib'))+
          ggtitle("Three modes of investing")
     }
     else{
       investments <- c(
         'regular savings',
         'high-yield savings',
         'index fund')
       rates <- list(
         regular = 0.0001,
         high_yield = 0.0225,
         index_fund = 0.065
         
       )
       
       scenarios <- list(1:3)
       rate <- input$rate/100
       growth <- input$growth/100
       for (r in 1:length(rates)){
         balances <- data.frame(
           year = 0:input$years,
           product = investments[r],
           no_contrib = rep(input$initial, input$years + 1),
           fixed_contrib = rep(input$initial, input$years + 1),
           growing_contrib = rep(input$initial, input$years + 1)
         )
         for (y in 1:input$years){
           
           fv <- input$initial * (1 + rates[[r]])** y
           fva <- input$contrib * ((1 + rates[[r]])**y - 1) / rates[[r]]
           fvga <- input$contrib * ((1 + rates[[r]])**y - (1 + growth)**y) / (rates[[r]] - growth)
           balances$no_contrib[y + 1] <- fv
           balances$fixed_contrib[y + 1] <- fv + fva
           balances$growing_contrib[y + 1] <- fv + fvga 
         }
         scenarios[[r]] <- balances
       }
      dat <- do.call("rbind", scenarios)
      
      ggplot(dat, aes(x = year)) 
      
     }
   })
   
   output$table <- renderTable(
     {
       if (input$facet == "No"){
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
       else{
         investments <- c(
           'regular savings',
           'high-yield savings',
           'index fund')
         rates <- list(
           regular = 0.0001,
           high_yield = 0.0225,
           index_fund = 0.065
           
         )
         
         scenarios <- list(1:3)
         rate <- input$rate/100
         growth <- input$growth/100
         for (r in 1:length(rates)){
           balances <- data.frame(
             year = 0:input$years,
             product = investments[r],
             no_contrib = rep(input$initial, input$years + 1),
             fixed_contrib = rep(input$initial, input$years + 1),
             growing_contrib = rep(input$initial, input$years + 1)
           )
           for (y in 1:input$years){
             
             fv <- input$initial * (1 + rates[[r]])** y
             fva <- input$contrib * ((1 + rates[[r]])**y - 1) / rates[[r]]
             fvga <- input$contrib * ((1 + rates[[r]])**y - (1 + growth)**y) / (rates[[r]] - growth)
             balances$no_contrib[y + 1] <- fv
             balances$fixed_contrib[y + 1] <- fv + fva
             balances$growing_contrib[y + 1] <- fv + fvga 
           }
           scenarios[[r]] <- balances
         }
         dat <- do.call("rbind", scenarios)
         return(dat)
       }
       
     }
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

