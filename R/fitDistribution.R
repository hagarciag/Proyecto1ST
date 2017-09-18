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
# https://stats.stackexchange.com/questions/76994/how-do-i-check-if-my-data-fits-an-exponential-distribution


# https://stats.stackexchange.com/questions/76994/how-do-i-check-if-my-data-fits-an-exponential-distribution
binner <- function(var) {
  require(shiny)
  library(MASS)
  library(fitdistrplus)
  library(shinythemes)
  library(DT)
  set.seed(101)
  shinyApp(

    # Define UI for random distribution app ----
    ui <- fluidPage(

      #theme = shinytheme("darkly"),

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
                      tabPanel("Plot", plotOutput("plot"),br(),DT::dataTableOutput('estadisticos'),br(),verbatimTextOutput("ensayo")),
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

        hist(muestra, prob=TRUE,main = paste("Ajuste de funciones"),
             col = "#75AADB", border = "white", ylab = "Probabilidad")

        den <- density(muestra)
        media_muestral<-mean(muestra)
        lines(den, col = 'red', lwd = 2)

        AjustmentTypes<-c("Aproximación empirica")
        VectorColores <- c("red")
        Media <- c(media_muestral)
        Distribucion <- c("Empirica")
        List_Fits <- list()
        AIC<- c(NA)

        for (i in 1:length(input$dist)){
          if(dist[i]=='norm'){
            fit_normal <- fitdistr(muestra, densfun="normal")
            curve(dnorm(x, fit_normal$estimate[1], fit_normal$estimate[2]), col="blue", lwd=2, add=T)

            AjustmentTypes<-c(AjustmentTypes,"Distribución teorica Normal")
            VectorColores <- c(VectorColores,"blue")
            Media <- c(Media,fit_normal$estimate[1])
            Distribucion <- c(Distribucion, "Normal")
            List_Fits[["fit_normal"]] <- fit_normal
            AIC <- c(AIC,AIC(fit_normal))
            #print(fit_normal$estimate)
            #print(fit_normal$sd)
            #print(fit_normal$vcov)
            #print(AIC(fit_normal))
          }

          if(dist[i]=='lnorm'){
            #fit_lnormal <- fitdistr(d(), "lnorm")
            fit_lnormal <- fitdist(muestra, "lnorm")
            curve(dlnorm(x, fit_lnormal$estimate[1], fit_lnormal$estimate[2]), col="orange", lwd=2, add=T)
            AjustmentTypes<-c(AjustmentTypes,"Distribución teorica LNormal")
            VectorColores <- c(VectorColores,"orange")
            Media <- c(Media,fit_lnormal$estimate[1])
            Distribucion <- c(Distribucion, "LogNormal")
            List_Fits[["fit_lnormal"]] <- fit_lnormal
            AIC <- c(AIC,fit_lnormal$aic)
            #print(fit_lnormal$aic)
            #print(fit_lnormal$bic)
          }

          if(dist[i]=='unif'){
            fit_unif <- fitdist(muestra, "unif", method="mle")
            a <- fit_unif$estimate[1]
            b <- fit_unif$estimate[2]
            curve(dunif(x,min = a,max = b), col="green", lwd=2, add=T)
            AjustmentTypes<-c(AjustmentTypes,"Distribución teorica Uniforme")
            VectorColores <- c(VectorColores,"green")
            Media <- c(Media,0.5*(a+b))
            Distribucion <- c(Distribucion, "Uniforme")
            List_Fits[["fit_unif"]] <- fit_unif
            AIC <- c(AIC,fit_unif$aic)
            print(fit_unif$aic)
          }

          if(dist[i]=='chisq'){
            fit_chisq <- fitdistr(muestra,"chi-squared",start=list(df=2),method="BFGS") ## Fitting
            chi_k <- fit_chisq[[1]][1] ## Degrees of freedom
            curve(dchisq(x,df=chi_k), col="purple", lwd=2, add=T)
            AjustmentTypes<-c(AjustmentTypes,"Distribución teorica Chi-cuadrado")
            VectorColores <- c(VectorColores,"purple")
            Media <- c(Media,chi_k)
            Distribucion <- c(Distribucion, "Chi-Cuadrado")
            List_Fits[["fit_chisq"]] <- fit_chisq
            AIC <- c(AIC,AIC(fit_chisq))
          }

          if(dist[i]=='logis'){
            fit_logist <- fitdistr(muestra, densfun="logistic")
            curve(dlogis(x, fit_logist$estimate[1], fit_logist$estimate[2]), col="gray", lwd=2, add=T)
            AjustmentTypes<-c(AjustmentTypes,"Distribución teorica Logistica")
            VectorColores <- c(VectorColores,"gray")
            Media <- c(Media,fit_logist$estimate[1])
            Distribucion <- c(Distribucion, "Logistica")
            List_Fits[["fit_logist"]] <- fit_logist
            AIC <- c(AIC,AIC(fit_logist))
          }

          if(dist[i]=='cauchy'){
            fit_cauchy <- fitdistr(muestra, densfun="cauchy")
            curve(dcauchy(x, fit_cauchy$estimate[1], fit_cauchy$estimate[2]), col="pink", lwd=2, add=T)
            AjustmentTypes<-c(AjustmentTypes,"Distribución teorica Cauchy")
            VectorColores <- c(VectorColores,"pink")
            Media <- c(Media,NA)
            Distribucion <- c(Distribucion, "Cauchy")
            List_Fits[["fit_cauchy"]] <- fit_cauchy
            AIC <- c(AIC,AIC(fit_cauchy))
          }

          if(dist[i]=='exp'){
            fit_exp <- fitdistr(muestra, "exponential")
            curve(dexp(x, rate = fit_exp$estimate), col = "gold", lwd=2, add = TRUE)
            #ks<-ks.test(muestra, "pexp", fit_exp$estimate)
            #print(ks$p.value)
            #print(ks$statistic)
            #print(ks$p.value)

            AjustmentTypes<-c(AjustmentTypes,"Distribución teorica Exponencial")
            VectorColores <- c(VectorColores,"gold")
            Media <- c(Media,1/fit_exp$estimate)
            Distribucion <- c(Distribucion, "Exponencial")
            List_Fits[["fit_exp"]] <- fit_exp
            AIC <- c(AIC,AIC(fit_exp))
          }
        }



        #legend("topright", col=c('red', 'green'), legend=nombres, bty="o")
        legend("topright", legend=AjustmentTypes, fill=VectorColores, col=VectorColores,
               bty="n")

        #gofstat(List_Fits, fitnames = Distribucion)

        estadisticos <- data.frame(Media, AIC)
        row.names(estadisticos) <- Distribucion
        output$estadisticos <- DT::renderDataTable(
          DT::datatable(estadisticos, options = list(searching = FALSE, paging = FALSE, bLengthChange = FALSE))
        )




      })

      # Generate a summary of the data ----
      output$summary <- renderPrint({
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
        summary(muestra)
      })

      # Generate an HTML table view of the data ----
      ## output$table <- renderTable({
      ##   d()
      ## })

      output$table <- renderTable({
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
        muestra
      })


        #Generacion Tabla
        output$ensayo <- renderPrint({
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
          #summary(muestra)
          summary(muestra)

        })


    }

    # Create Shiny app ----
    #shinyApp(ui, server)

  )
}


#n<-1000
#x<-rnorm(n,84,20)
#y<-rlnorm(n,4,1)
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
