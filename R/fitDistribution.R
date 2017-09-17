
# https://stats.stackexchange.com/questions/76994/how-do-i-check-if-my-data-fits-an-exponential-distribution
binner <- function(var) {
  require(shiny)
  library(MASS)
  library(fitdistrplus)
  library(shinythemes)
  set.seed(101)
  shinyApp(

    # Define UI for random distribution app ----
    ui <- fluidPage(

      theme = shinytheme("darkly"),

      # App title ----
      titlePanel("Temporal Series"),

      # Sidebar layout with input and output definitions ----
      sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(
          fileInput('file1', 'Import data',
                    accept=c('text/csv',
                             'text/comma-separated-values,text/plain',
                             '.csv')),

          checkboxInput('header', 'Header', TRUE),

          # Input: Select the random distribution type ----
          # checkboxGroupInput
          checkboxGroupInput("dist", "Distribution type:",
                             c("Normal" = "norm",
                               "Uniform" = "unif",
                               "Log-normal" = "lnorm",
                               "Exponential" = "exp",
                               "Chi-Squared" = "chisq",
                               "Logistic" = "logis",
                               "Cauchy" = "cauchy"), selected = "norm"),

          # br() element to introduce extra vertical spacing ----
          br()

          # # Input: Slider for the number of observations to generate ----
          # sliderInput("n",
          #  "Number of observations:",
          #  value = 500,
          #  min = 1,
          #   max = 1000)

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
                       chisq = rchisq,
                       logis = rlogis,
                       cauchy = rcauchy)

        dist(input$n)

        datos <- input$file1

        if (is.null(datos))
          datos <- var




        ##file1 <- input$file
        ##if(is.null(file1)){return()}
        ##read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)

      })

      # Generate a plot of the data ----
      # Also uses the inputs to build the plot label. Note that the
      # dependencies on the inputs and the data reactive expression are
      # both tracked, and all expressions are called in the sequence
      # implied by the dependency graph.
      output$plot <- renderPlot({
        datos <- input$file1
        if (is.null(datos))
          muestra <- var
        else{
          if(input$header)
          {
            muestra = scan(datos$datapath,skip=1)
          }
          else{
            muestra = scan(datos$datapath)
          }
        }

        dist <- input$dist
        # n <- input$n


        hist(muestra, prob=TRUE,main = paste("Hernancho"),
             col = "#75AADB", border = "white", ylab = "Probabilidad")

        den <- density(muestra)
        lines(den, col = 'red', lwd = 2)

        AjustmentTypes<-c("Aproximación empirica")
        VectorColores <- c("red")


        for (i in 1:length(input$dist)){
          if(dist[i]=='norm'){
            fit <- fitdistr(muestra, densfun="normal")
            #hist(d(), pch=20, breaks=25, prob=TRUE, main="")
            curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col="blue", lwd=2, add=T)
            #print("Normal")

            AjustmentTypes<-c(AjustmentTypes,"Distribución teorica Normal")
            VectorColores <- c(VectorColores,"blue")
          }

          if(dist[i]=='lnorm'){
            #fit <- fitdistr(d(), "lnorm")
            fit <- fitdist(muestra, "lnorm")
            curve(dlnorm(x, fit$estimate[1], fit$estimate[2]), col="orange", lwd=2, add=T)
            AjustmentTypes<-c(AjustmentTypes,"Distribución teorica LNormal")
            VectorColores <- c(VectorColores,"orange")
          }

          if(dist[i]=='unif'){
            x <- fitdist(muestra, "unif", method="mle")
            a <- x$estimate[1]
            b <- x$estimate[2]
            curve(dunif(x,min = a,max = b), col="green", lwd=2, add=T)
            AjustmentTypes<-c(AjustmentTypes,"Distribución teorica Uniforme")
            VectorColores <- c(VectorColores,"green")
          }

          if(dist[i]=='chisq'){
            chi_df <- fitdistr(muestra,"chi-squared",start=list(df=2),method="BFGS") ## Fitting
            chi_k <- chi_df[[1]][1] ## Degrees of freedom
            curve(dchisq(x,df=chi_k), col="purple", lwd=2, add=T)
            AjustmentTypes<-c(AjustmentTypes,"Distribución teorica Chi-cuadrado")
            VectorColores <- c(VectorColores,"purple")
          }

          if(dist[i]=='logis'){
            fit <- fitdistr(muestra, densfun="logistic")
            curve(dlogis(x, fit$estimate[1], fit$estimate[2]), col="gray", lwd=2, add=T)
            AjustmentTypes<-c(AjustmentTypes,"Distribución teorica Logistica")
            VectorColores <- c(VectorColores,"gray")
          }

          if(dist[i]=='cauchy'){
            fit <- fitdistr(muestra, densfun="cauchy")
            curve(dcauchy(x, fit$estimate[1], fit$estimate[2]), col="pink", lwd=2, add=T)
            AjustmentTypes<-c(AjustmentTypes,"Distribución teorica Cauchy")
            VectorColores <- c(VectorColores,"pink")
          }

          if(dist[i]=='exp'){
            fit <- fitdistr(muestra, "exponential")
            curve(dexp(x, rate = fit$estimate), col = "gold", lwd=2, add = TRUE)
            AjustmentTypes<-c(AjustmentTypes,"Distribución teorica Exponencial")
            VectorColores <- c(VectorColores,"gold")
          }
        }



        #legend("topright", col=c('red', 'green'), legend=nombres, bty="o")
        legend("topright", legend=AjustmentTypes, fill=VectorColores, col=VectorColores,
               bty="n")

      })

      # Generate a summary of the data ----
      output$summary <- renderPrint({
        datos <- input$file1
        if (is.null(datos))
          return(NULL)
        if(input$header)
        {
          muestra = scan(datos$datapath,skip=1)
        }
        else{
          muestra = scan(datos$datapath)
        }
        summary(muestra)
      })

      # Generate an HTML table view of the data ----
      ## output$table <- renderTable({
      ##   d()
      ## })

      output$table <- renderTable({
        datos <- input$file1
        if (is.null(datos))
          return(NULL)
        if(input$header)
        {
          muestra = scan(datos$datapath,skip=1)
        }
        else{
          muestra = scan(datos$datapath)
        }
        muestra
      })

    }

    # Create Shiny app ----
    #shinyApp(ui, server)

  )
}


#n<-1000
#x<-rnorm(n,0,1)
#y<-rlnorm(n,100,5)
#z<-rexp(n,1.85)
#w<-runif(n)
#hist(x,freq=TRUE)
#Normal
#binner(x)
#Lognormal
#binner(y)
#Exponencial
#binner(z)
#Uniforme
#binner(w)
