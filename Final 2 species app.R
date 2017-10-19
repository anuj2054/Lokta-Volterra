#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(deSolve)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Guruacharya Timilsina app for Lokta Volterra Equations"),
  verbatimTextOutput("value"),
  
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
                  value = 0.0008)
      ,
      sliderInput("initialpreypopulation",
                  "Value of initial prey population:",
                  min = 1,
                  max = 1000,
                  value = 200, 
                  animate = TRUE)
      ,
      sliderInput("initialpredatorpopulation",
                  "Value of initial predator population:",
                  min = 1,
                  max = 1000,
                  value = 10, 
                  animate = TRUE)
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
  myOutput <- reactive({
    
    parameters <- c(alpha = input$alpha,
                    beta = input$beta ,
                    gamma=input$gamma,
                    delta=input$delta
    )
    pH=runif(1,0,1)
    pP=runif(1,0,1)
    
    state <- c(H = input$initialpreypopulation+pH,
               P = input$initialpredatorpopulation+pP
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
    Hout<-out[,"H"]
    Pout<-out[,"P"]
    return(data.frame(Hout,Pout))
    
  })
  
  
  output$distPlot1 <- renderPlot({
    
    plot(myOutput()$Hout,xlab = "time", ylab = "Red for H, Blue for P",col="red")
    lines(myOutput()$Pout,xlab = "time", ylab = "P",col="blue")
    mtext(outer = TRUE, side = 3, "Lokta Voltera test", cex = 1.5)
    
    
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2] 
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$distPlot2 <- renderPlot({
    
    plot(x=myOutput()$Hout,y=myOutput()$Pout,xlab = "H", ylab = "P",col="green")
    output$value <- renderText({ "The classic predator prey model of interacting population is due to Alfred Lotka and Vito Voltera. It was first formulated in 1920s. It serves as a valid starting point for most satisfactory models for interacting populations. This application helps us see the basic relationship between two species - predator and prey. The term 'predation' should be understood in wide sense of interaction of type +- which covers predation in the strict sense as well as host-parasitoid relationships and interaction of herbivores and the plants they eat. Here, P is predator and H is prey. -alpha- is net relative rate of growth for preys. Preys are caught by predators at a rate -beta- that is proportional to H(t) and P(t).-Gamma- for predators represents mortality.-delta- times the product of population of predators and prey at that time gives rate of predator increase resulting from it.  (Source - Mathematical Modeling in the Life Sciences by Doucet and Sloep)" })
    
    
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
