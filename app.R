#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Lokta Volterra Equations"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("alpha",
                     "Value of alpha:",
                     min = 0.1,
                     max = 0.9,
                     value = 0.2)
      ,
         sliderInput("beta",
                  "Value of beta:",
                  min = 0.001,
                  max = 0.009,
                  value = 0.002)
   ,
   sliderInput("gamma",
               "Value of gamma:",
               min = 0.01,
               max = 0.90,
               value = 0.04)
   ,
      sliderInput("delta",
            "Value of delta:",
            min = 0.0001,
            max = 0.0009,
            value = 0.0004)
),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot1"),
         plotOutput("distPlot2")
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  library("deSolve")
   myOutput <- reactive(
      
    
    parameters <- c(alpha =  inputPanel("alpha"),
                     beta = inputPanel("beta"),
                     gamma=inputPanel("gamma"),
                     delta=inputPanel("delta")),
                                      
  )
  pH=runif(1,0,1)
  pP=runif(1,0,1)
  
  state <- c(H = 100+pH,
             P = 40+pP
  )
  
  
  Lorenz<-function(t, state, parameters) {
    with(as.list(c(state, parameters)),{
      # rate of change
      dH <- input$alpha*H-input$beta*H*P
      dP <- -input$gamma*P + input$delta * H * P 
      #    dZ <- b*X*Y + X*Z - Z     
      # return the rate of change
      list(c(dH, dP))
    })   # end with(as.list ...
  }
  
  
  times <- seq(0, 200, by = 1)
  
  out <- ode(y = state, times = times, func = Lorenz, parms = parameters)
  
   output$distPlot1 <- renderPlot({

     plot(out[,"H"],xlab = "time", ylab = "H",col="red")
     lines(out[,"P"],xlab = "time", ylab = "P",col="blue")
     mtext(outer = TRUE, side = 3, "Lokta Voltera test", cex = 1.5)
     
     
           # generate bins based on input$bins from ui.R
     # x    <- faithful[, 2] 
     # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$distPlot2 <- renderPlot({
   
     plot(x=out[,"H"],y=out[,"P"],xlab = "H", ylab = "P",col="yellow")
     
       
      })
   
   
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

