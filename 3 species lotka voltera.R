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
library(scatterplot3d)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Guruacharya Timilsina app for Three Species Lokta Volterra Equations"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("a",
                  "Value of a:",
                  min = 0.1,
                  max = 0.9,
                  value = 0.2)
      ,
      sliderInput("b",
                  "Value of b:",
                  min = 0.001,
                  max = 0.009,
                  value = 0.002)
      ,
      sliderInput("c",
                  "Value of c:",
                  min = 0.01,
                  max = 0.90,
                  value = 0.04)
      ,
      sliderInput("d",
                  "Value of d:",
                  min = 0.0001,
                  max = 0.0009,
                  value = 0.0004)
      ,
      sliderInput("e",
                  "Value of e:",
                  min = 0.0001,
                  max = 0.0009,
                  value = 0.0004)
      
      ,
      
      sliderInput("f",
                  "Value of f:",
                  min = 0.0001,
                  max = 0.0009,
                  value = 0.0004)
      ,
      sliderInput("g",
                  "Value of g:",
                  min = 0.0001,
                  max = 0.0009,
                  value = 0.0004)
      
            ,
      
      sliderInput("initialpreypopulation",
                  "Value of initial prey population:",
                  min = 1,
                  max = 1000,
                  value = 0.0004)
      ,
      sliderInput("initialpredatorpopulation",
                  "Value of initial predator population:",
                  min = 1,
                  max = 1000,
                  value = 0.0004)
      ,
      sliderInput("initialtertiarypredatorpopulation",
                  "Value of initial tertiary predator population:",
                  min = 1,
                  max = 1000,
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
  myOutput <- reactive({
    
    parameters <- c(a = input$a,
                    b = input$b ,
                    c = input$c,
                    d = input$d,
                    e = input$e,
                    f = input$f,
                    g = input$g
    )
    pH=runif(1,0,1)
    pP=runif(1,0,1)
    pT=runif(1,0,1)
    
    state <- c(H = input$initialpreypopulation+pH,
               P = input$initialpredatorpopulation+pP,
               T = input$initialtertiarypredatorpopulation+pT
    )
    
    
    Lorenz<-function(t, state, parameters) {
      with(as.list(c(state, parameters)),{
        # rate of change
        dH <- input$a*H-input$b*H*P
        dP <- -input$c*P + input$d * H * P-input$e*P*T 
        dT <- -input$f*T+input$g*P*T
        #    dZ <- b*X*Y + X*Z - Z     
        # return the rate of change
        list(c(dH, dP, dT))
      })   # end with(as.list ...
    }
    
    
    times <- seq(0, 200, by = 1)
    
    out <- ode(y = state, times = times, func = Lorenz, parms = parameters)
    Hout<-out[,"H"]
    Pout<-out[,"P"]
    Tout<-out[,"T"]
    return(data.frame(Hout,Pout, Tout))
    
  })
  
  
  output$distPlot2 <- renderPlot({
    
    plot(myOutput()$Hout,xlab = "time", ylab = "H",col="red")
    lines(myOutput()$Pout,xlab = "time", ylab = "P",col="blue")
    lines(myOutput()$Tout,xlab = "time", ylab = "T",col="green")
    mtext(outer = TRUE, side = 3, "Lokta Voltera test", cex = 1.5)
    
    
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2] 
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$distPlot1 <- renderPlot({
    
    
    
    scatterplot3d(x = myOutput()$Hout, y = myOutput()$Pout, z = myOutput()$Tout, main="3D plot", xlab="Values of H", ylab="Values of P", zlab="values of T", axis = TRUE, tick.marks = TRUE, label.tick.marks = TRUE, grid = TRUE, box = FALSE, highlight.3d = TRUE)
    
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
