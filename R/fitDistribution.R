
# https://stats.stackexchange.com/questions/76994/how-do-i-check-if-my-data-fits-an-exponential-distribution
binner <- function(var) {
  require(shiny)
  library(MASS)
  library(fitdistrplus)
  set.seed(101)
  shinyApp(

    # Define UI for random distribution app ----
    ui <- fluidPage(

      # App title ----
      titlePanel("Tabsets"),

      # Sidebar layout with input and output definitions ----
      sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(

          # Input: Select the random distribution type ----
          radioButtons("dist", "Distribution type:",
                       c("Normal" = "norm",
                         "Uniform" = "unif",
                         "Log-normal" = "lnorm",
                         "Exponential" = "exp")),

          # br() element to introduce extra vertical spacing ----
          br(),

          # Input: Slider for the number of observations to generate ----
          sliderInput("n",
                      "Number of observations:",
                      value = 500,
                      min = 1,
                      max = 1000)

        ),

        # Main panel for displaying outputs ----
        mainPanel(

          # Output: Tabset w/ plot, summary, and table ----
          tabsetPanel(type = "tabs",
                      tabPanel("Graficas", plotOutput("plot")),
                      tabPanel("Estadisticos", verbatimTextOutput("summary"))
          )

        )
      )
    ),

    # Define server logic for random distribution app ----
    server <- function(input, output) {

      # Reactive expression to generate the requested distribution ----
      # This is called whenever the inputs change. The output functions
      # defined below then use the value computed from this expression
      d <- reactive({
        dist <- switch(input$dist,
                       norm = rnorm,
                       unif = runif,
                       lnorm = rlnorm,
                       exp = rexp,
                       rnorm)

        dist(input$n)
      })

      # Generate a plot of the data ----
      # Also uses the inputs to build the plot label. Note that the
      # dependencies on the inputs and the data reactive expression are
      # both tracked, and all expressions are called in the sequence
      # implied by the dependency graph.
      output$plot <- renderPlot({
        dist <- input$dist
        n <- input$n


        hist(var, prob=TRUE,
             main = paste("r", dist, "(", n, ")", sep = ""),
             col = "#75AADB", border = "white", ylab = "Probabilidad")




        den <- density(var)
        lines(den, col = 'red', lwd = 2)

        AjustmentTypes<-c("Aproximación empirica","Distribución teorica")
        if(dist=='norm'){
          fit <- fitdistr(var, densfun="normal")
          #hist(d(), pch=20, breaks=25, prob=TRUE, main="")
          curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col="green", lwd=2, add=T)
          #print("Normal")

          AjustmentTypes<-c("Aproximación empirica","Distribución teorica Normal")
        }

        if(dist=='lnorm'){
          #fit <- fitdistr(d(), "lnorm")
          fit <- fitdist(var, "lnorm")
          curve(dlnorm(x, fit$estimate[1], fit$estimate[2]), col="green", lwd=2, add=T)
          #AjustmentTypes<-c("Aproximación empirica","Distribución teorica LNormal")
          AjustmentTypes<-c("Aproximación empirica","Distribución teorica LNorm")
        }

        if(dist=='exp'){
          #fit <- fitdistr(d(), "lnorm")
          fit <- fitdistr(var, "exponential")
          #curve(dlnorm(x, fit$estimate[1], fit$estimate[2]), col="green", lwd=2, add=T)
          curve(dexp(x, rate = fit$estimate), col = "green", lwd=2, add = TRUE)
          #AjustmentTypes<-c("Aproximación empirica","Distribución teorica LNormal")
          AjustmentTypes<-c("Aproximación empirica","Distribución teorica Exp")
          print(ks.test(var, "pexp", fit$estimate)) # p-value > 0.05 -> distribution not refused
        }

        #legend("topright", col=c('red', 'green'), legend=nombres, bty="o")
        legend("topright", legend=AjustmentTypes, fill=c('red', 'green'), col=c('red', 'green'),
               bty="n")


      })

      # Generate a summary of the data ----
      output$summary <- renderTable({

        data.frame(

          as.character(summary(var)))



      })

    }

    # Create Shiny app ----
    #shinyApp(ui, server)

  )
}


n<-1000
x<-rnorm(n,0,1)
y<-rlnorm(n,100,5)
z<-rexp(n,1.85)
w<-runif(n)
hist(x,freq=TRUE)
#Normal
binner(x)
#Lognormal
binner(y)
#Uniforme
binner(z)
#Exponencial
binner(w)
