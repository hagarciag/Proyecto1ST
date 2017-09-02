# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



#https://shiny.rstudio.com/articles/function.html
#https://github.com/jdvelasq/series-de-tiempo/blob/master/01-R-probabilidad.ipynb
#http://janzilinsky.com/r-shiny-app-chart-tutorial-subsamples/
#http://www.di.fc.ul.pt/~jpn/r/distributions/fitting.html

binner <- function(var) {
  require(shiny)
  shinyApp(
    ui = fluidPage(
      sidebarLayout(
        sidebarPanel(sliderInput("n", "Granularidad", 5, 100, 10)),
        mainPanel(plotOutput("hist"))
      )
    ),
    server = function(input, output) {
      output$hist <- renderPlot({


        hist(var, prob=TRUE, breaks = input$n,
             col = "skyblue", border = "white")
        #title(main="Numbers over the years")
        #title(xlab="Year")
        #title(ylab="Number of people")
        d <- density(var)
        lines(d, col = 'red', lwd = 2)
      })
    }
  )
}


n<-1000
x<-rnorm(n,0,1)
hist(x,freq=TRUE)
binner(x)




#media
#desviacion estandar
#la función de densidad


install.packages("rriskDistributions")
library(rriskDistributions)
res1<-fit.cont(data2fit=rnorm(374,40,1))
res1
