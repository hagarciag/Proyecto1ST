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
                      tabPanel("Plot", plotOutput("plot")),
                      tabPanel("Summary", verbatimTextOutput("summary")),
                      tabPanel("Table", tableOutput("table"))
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


        hist(d(), prob=TRUE,
             main = paste("r", dist, "(", n, ")", sep = ""),
             col = "#75AADB", border = "white", ylab = "Probabilidad")




        den <- density(d())
        lines(den, col = 'red', lwd = 2)

        AjustmentTypes<-c("Aproximación empirica","Distribución teorica")
        if(dist=='norm'){
          fit <- fitdistr(d(), densfun="normal")
          #hist(d(), pch=20, breaks=25, prob=TRUE, main="")
          curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col="green", lwd=2, add=T)
          #print("Normal")

          AjustmentTypes<-c("Aproximación empirica","Distribución teorica Normal")
        }

        if(dist=='lnorm'){
          #fit <- fitdistr(d(), "lnorm")
          fit <- fitdist(d(), "lnorm")
          curve(dlnorm(x, fit$estimate[1], fit$estimate[2]), col="green", lwd=2, add=T)
          #AjustmentTypes<-c("Aproximación empirica","Distribución teorica LNormal")
        }

        #legend("topright", col=c('red', 'green'), legend=nombres, bty="o")
        legend("topright", legend=AjustmentTypes, fill=c('red', 'green'), col=c('red', 'green'),
               bty="n")


      })

      # Generate a summary of the data ----
      output$summary <- renderPrint({
        summary(d())
      })

      # Generate an HTML table view of the data ----
      output$table <- renderTable({
        d()
      })

    }

    # Create Shiny app ----
    #shinyApp(ui, server)

  )
}


n<-1000
x<-rnorm(n,0,1)
hist(x,freq=TRUE)
binner(x)
