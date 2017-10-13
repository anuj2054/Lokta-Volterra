library("deSolve")
parameters <- c(alpha = 0.2,
                beta = 0.002 ,
                gamma=0.9,
                delta=0.0004
)

pH=runif(1,0,1)
pP=runif(1,0,1)

#px=0
#py=0
#pz=0


state <- c(H = 100+pH,
           P = 40+pP
           )


Lorenz<-function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    # rate of change
    dH <- alpha*H-beta*H*P
    dP <- -gamma*P + delta * H * P 
#    dZ <- b*X*Y + X*Z - Z     
    # return the rate of change
    list(c(dH, dP))
  })   # end with(as.list ...
}


times <- seq(0, 200, by = 1)

out <- ode(y = state, times = times, func = Lorenz, parms = parameters)
#head(out)

#layout(matrix(c(1,2,3),nrow=3))
#par(oma = c(0, 0, 3, 0))
#plot(out, xlab = "time", ylab = "-")
plot(out[,"H"],xlab = "time", ylab = "H",col="red")
lines(out[,"P"],xlab = "time", ylab = "P",col="blue")
mtext(outer = TRUE, side = 3, "Lokta Voltera test", cex = 1.5)

plot(x=out[,"H"],y=out[,"P"],xlab = "H", ylab = "P",col="yellow")


mean(out[,"H"])
mean(out[,"P"])

