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
  titlePanel("Guruacharya Timilsina app for Three Species Lotka Voltera Equations"),
  verbatimTextOutput("value"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("a",
                  "Value of a:",
                  min = 0.1,
                  max = 0.9,
                  value = 0.2, 
                  animate = TRUE)
      
      ,
      sliderInput("b",
                  "Value of b:",
                  min = 0.001,
                  max = 0.009,
                  value = 0.002, 
                  animate = TRUE)
      ,
      sliderInput("c",
                  "Value of c:",
                  min = 0.01,
                  max = 0.90,
                  value = 0.04,
                  animate = TRUE)
      ,
      sliderInput("d",
                  "Value of d:",
                  min = 0.0001,
                  max = 0.0009,
                  value = 0.0004, 
                  animate = TRUE)
      ,
      sliderInput("e",
                  "Value of e:",
                  min = 0.0001,
                  max = 0.0009,
                  value = 0.0004, 
                  animate = TRUE)
      
      ,
      
      sliderInput("f",
                  "Value of f:",
                  min = 0.00001,
                  max = 0.0001,
                  value = 0.0004,
                  animate = TRUE)
      ,
      sliderInput("g",
                  "Value of g:",
                  min = 0.00009,
                  max = 0.0009,
                  value = 0.00008, 
                  animate = TRUE)
      
      ,
      
      sliderInput("initialfirstpopulation",
                  "Value of initial first species population:",
                  min = 1,
                  max = 10000,
                  value = 5000, 
                  animate = TRUE)
      ,
      sliderInput("initialsecondpopulation",
                  "Value of initial second species population:",
                  min = 1,
                  max = 10000,
                  value = 800, 
                  animate = TRUE)
      ,
      sliderInput("initialthirdpopulation",
                  "Value of initial third species population:",
                  min = 1,
                  max = 10000,
                  value = 350, 
                  animate = TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3 ("Plain 3D Plot"),
      plotOutput("distPlot1"),
     h3("Modifiable 3D Plot"),
       plotlyOutput("plot"),
     h3("3 species against time"),
      plotOutput("distPlot2"),
     img(src='http://images.slideplayer.com/28/9383075/slides/slide_2.jpg', align = "left", length = 500, width = 500)
      
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
    
    state <- c(H = input$initialfirstpopulation+pH,
               P = input$initialsecondpopulation+pP,
               T = input$initialthirdpopulation+pT
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
  
  output$plot <- renderPlotly(
    plot_ly( x = myOutput()$Hout, y = myOutput()$Pout, z = myOutput()$Tout, colors = NULL, linetypes = NULL, width = NULL, height = NULL ) %>%
      add_markers() %>% 
      layout(scene = list(xaxis = list(title = 'First Species'),
                          yaxis = list(title = 'Second Species'),
                          zaxis = list(title = 'Third Species'))
  ))
  output$distPlot2 <- renderPlot({
    
    plot(myOutput()$Hout,xlab = "time", ylab = "red for x, blue for y and green for z",col="red")
    lines(myOutput()$Pout,xlab = "time", ylab = "y",col="blue")
    lines(myOutput()$Tout,xlab = "time", ylab = "z",col="green")
    mtext(outer = TRUE, side = 3, "Lokta Voltera test", cex = 1.5)
    
    
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2] 
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$distPlot1 <- renderPlot({
    
    
    
    scatterplot3d(x = myOutput()$Hout, y = myOutput()$Pout, z = myOutput()$Tout, main="3D plot", xlab="Values of x", ylab="Values of y", zlab="values of z", axis = TRUE, tick.marks = TRUE, label.tick.marks = TRUE, grid = TRUE, box = TRUE, highlight.3d = TRUE)
    output$value <- renderText({ "The classic predator prey model of interacting population is due to Alfred Lotka and Vito Voltera. It was first formulated in 1920s. It serves as a valid starting point for most satisfactory models for interacting populations. This application helps us see the basic relationship between three species interaction. It is modified form of two species Lotka Voltera equation. a,b,c,d,e,f and g are constants of three variable non linear equations. a represents the natural growth rate of the prey in the absence of predators, b represents the effect of predation on the prey, c represents the natural death rate of the predator in the absence of prey, d represents the efficiency and propagation rate of the predator in the presence of
      prey. e represents the effect of predation on species y by species z, f represents the natural death rate of the predator z in the absence of prey and . g represents the efficiency and propagation rate of the predator z in the presence of
      prey.   (Source - Mathematical Modeling in the Life Sciences by Doucet and Sloep)" })
    
    })
  
  
  
  
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
