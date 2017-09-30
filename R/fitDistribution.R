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
#https://stats.stackexchange.com/questions/76994/how-do-i-check-if-my-data-fits-an-exponential-distribution
#https://gist.github.com/aagarw30/c593799bc7d8557dc863411bb552e4f4

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

      theme = shinytheme("cerulean"),

      # App title ----
      titlePanel("Time Series"),

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



          conditionalPanel(
            condition = "input.tabselected!=1",
            radioButtons('t_suavizado', 'Métodos',
                         c('Regresion Lineal Simple'=5,
                           'Regresion Cuadratica'=6,
                           'Regresion Cubica'=7,
                           ##'Regresion Multiple con Estacionalidad'=8,
                           'Suavizado Exponencial Simple'=1,
                           'Suavizado Exponencial Doble'=2,
                           'Suavizado Exponencial Triple Aditiva'=3,
                           'Suavizado Exponencial Triple Multiplicativas'=4
                         ),
                         '5'),
            tags$hr(),
            radioButtons('frecuencia_usuario', 'Frecuencia',
                         c('Mensual'=12,
                           'Trimestral'=4,'Cuatrimestral'=3,'Semestral'=2),
                         '12'),
            tags$hr(),
            sliderInput("n", "Periodos:",
                        min=0, max=12, value=12)

          ),





          tags$hr(),

          conditionalPanel(
            condition = "input.tabselected==1",
            checkboxGroupInput("dist", "Tipo de Distribucion:",
                               c("Normal" = "norm",
                                 "Uniform" = "unif",
                                 "Log-normal" = "lnorm",
                                 "Exponential" = "exp",
                                 "Chi-Squared" = "chisq",
                                 "Logistic" = "logis",
                                 "Cauchy" = "cauchy"), selected = "norm")
          ),

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
                      tabPanel("Distribuciones", value=1, plotOutput("plot"),br(),DT::dataTableOutput('estadisticos'),br(),verbatimTextOutput("ensayo")),
                      tabPanel("Grafico", value=2,plotOutput("plot1"),plotOutput("plot2")),
                      tabPanel("Análisis series de tiempo", value=3, verbatimTextOutput("resumen"),br(),verbatimTextOutput("resumen3")),
#                      tabPanel("Análisis series de tiempo", value=3, verbatimTextOutput("resumen"),br(),verbatimTextOutput("resumen2"),br(),"AIC",verbatimTextOutput("resumen3")),
                      tabPanel("Table", value=4, tableOutput("table")),
                      id = "tabselected"
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

      #####
      fecha<- function(freq,pron,total){
        n<-total-pron
        anio<-0
        periodo<-0
        i<-0
        while (n>0) {
          i<-i+1;
          for (j in seq(1:freq)) {
            if(n<=0)
            {
              break();
            }
            anio<-i;
            periodo<-j;
            n<-n-1;
          }
        }
        return(c(anio,periodo))
      }
      serieTiempo <- function(datos,frecuencia){
        frecuencia<-as.numeric(input$frecuencia_usuario)
        ts.data<-ts(datos,start=1,frequency=frecuencia)
        return(ts.data)
      }
      aprendizaje <- function(datos,frecuencia,periodo){
        totalDatos<-length(datos)
        frecuencia<-as.numeric(input$frecuencia_usuario)
        pronostico<- periodo
        posicion<-fecha(frecuencia,pronostico,totalDatos)

        ts.data<-ts(datos,start=1,frequency=frecuencia)
        x.fit<-window(ts.data,start=1, end=posicion)

        return(x.fit)
      }
      pronostico <- function(datos,frecuencia,periodo){
        totalDatos<-length(datos)
        frecuencia<-as.numeric(input$frecuencia_usuario)
        pronostico<- periodo
        ts.data<-ts(datos,start=1,frequency=frecuencia)
        posicion<-fecha(frecuencia,pronostico-1,totalDatos)
        x.for<-window(ts.data,start=posicion)

        return(x.for)
      }


      inFile <- reactive({
        datos <- input$file1

        if (is.null(datos))
          return(var)

        if (is.null(datos))
          return(NULL)
        if(input$header)
        {
          aux = scan(datos$datapath,skip=1)
        }
        else{
          aux = scan(datos$datapath)
        }


      })


      output$histograma <- renderPlot({

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
        ##hist(muestra)
        hist(muestra, prob=TRUE, main = paste("Ajuste de Funciones"),
             col = "#75AADB", border = "white", ylab = "Probabilidad", xlab = "Serie")
      })

      output$plot1 <- renderPlot({


        mydata=inFile()
        if (is.null(mydata))
          return(NULL)
        tipoR <- as.numeric(input$t_suavizado)
        f <- as.numeric(input$frecuencia_usuario)
        p <- as.numeric(input$n)
        ts.data<-serieTiempo(mydata,f)
        x.fit<-aprendizaje(mydata,f,p)
        x.for<-pronostico(mydata,f,p)
        if(tipoR==1)
        {
          msimple<-HoltWinters(x=x.fit,alpha = NULL, beta = FALSE, gamma = FALSE)
          psimple<-predict(msimple,n.ahead=p)
          plot.ts(ts.data,lwd=3,col='black',type='o',main='Suavizado Simple (funcion predict)')
          lines(msimple$fitted[,1],col='orange',lwd=3)
          lines(psimple[,1],col='blue',lwd=3)
          legend("topleft",c('Original','Suavizado simple','Pronostico simple'), lwd=c(3,3,3),col = c('black','orange','blue'))

        }

        else if(tipoR==2)
        {


          mdouble<-HoltWinters(x=x.fit,alpha = NULL, beta = NULL, gamma = FALSE)
          pdouble<-predict(mdouble,n.ahead=p)
          plot.ts(ts.data,lwd=3,col='black',type='o',main='Suavizado Doble (funcion predict)')
          lines(mdouble$fitted[,1],col='orange',lwd=3)
          lines(pdouble[,1],col='blue',lwd=3)
          legend("topleft",c('Original','Suavizado doble','Pronostico doble'), lwd=c(3,3,3),col = c('black','orange','blue'))

        }
        else if(tipoR==3)
        {
          mtripleAdd<-HoltWinters(x=x.fit,seasonal = "additive")
          ptripleAdd<-predict(mtripleAdd,n.ahead=p)
          plot.ts(ts.data,lwd=3,col='black',type='o',main='Suavizado Triple Componente Aditiva (funcion predict)')
          lines(mtripleAdd$fitted[,1],col='orange',lwd=3)
          lines(ptripleAdd[,1],col='blue',lwd=3)
          legend("topleft",c('Original','Suavizado triple aditiva','Pronostico triple aditiva'), lwd=c(3,3,3),col = c('black','orange','blue'))

        }
        else if (tipoR==4)
        {

          mtripleMult<-HoltWinters(x=x.fit,seasonal = "multiplicative")
          ptripleMult<-predict(mtripleMult,n.ahead=p)
          plot.ts(ts.data,lwd=3,col='black',type='o',main='Suavizado Triple Componente Multiplicativa (funcion predict)')
          lines(mtripleMult$fitted[,1],col='orange',lwd=3)
          lines(ptripleMult[,1],col='blue',lwd=3)
          legend("topleft",c('Original','Suavizado triple multiplicativa','Pronostico triple multiplicativa'), lwd=c(3,3,3),col = c('black','orange','blue'))

        }
        else if (tipoR==5)
        {
          t<- seq(1:length(ts.data))                          # Variable independiente t: Tiempo
          rlineal <- lm(ts.data~t)
          m.fit = ts(rlineal$fitted.values, freq=f, start=c(1,1))  # Valores ajustados del model
          plot.ts(ts.data,lwd=3,col='black',type='o',main='Regresion Lineal Simple')
          lines(m.fit,col='orange',lwd=3)
          legend("topleft",c('Original','Regresión Lineal Simple'), lwd=c(3,3,3),col = c('black','orange','blue'))

        }
        else if (tipoR==6)
        {
          t<- seq(1:length(ts.data))                          # Variable independiente t: Tiempo
          tt<- t*t
          rcuadratica <- lm(ts.data~t+tt)
          m.fit = ts(rcuadratica$fitted.values, freq=f, start=c(1,1))  # Valores ajustados del model
          plot.ts(ts.data,lwd=3,col='black',type='o',main='Regresion Cuadratica')
          lines(m.fit,col='orange',lwd=3)
          legend("topleft",c('Original','Regresión Cuadratica'), lwd=c(3,3,3),col = c('black','orange','blue'))

        }
        else if (tipoR==7)
        {
          t<- seq(1:length(ts.data))                          # Variable independiente t: Tiempo
          tt<- t*t
          ttt<-t*t*t
          rcubica <- lm(ts.data~t+tt+ttt)
          m.fit = ts(rcubica$fitted.values, freq=f, start=c(1,1))  # Valores ajustados del model
          plot.ts(ts.data,lwd=3,col='black',type='o',main='Regresion Cubica')
          lines(m.fit,col='orange',lwd=3)
          legend("topleft",c('Original','Regresión Cubica'), lwd=c(3,3,3),col = c('black','orange','blue'))

        }
        else if (tipoR==8)
        {
          T=length(ts.data)           # Variable independiente t: Tiempo
          t=seq(1:(T-p))
          It=seasonaldummy(x.fit)
          restacional = lm(x.fit ~ t + It)
          m.fit = ts(restacional$fitted.values, freq=f, start=c(1,1))  # Valores ajustados del modelo
          plot.ts(ts.data,lwd=3,col='black',type='o',main='Regresion Multiple con Estacionalidad')
          lines(m.fit,col='orange',lwd=3)


        }

      })

      output$plot2 <- renderPlot({


        mydata=inFile()
        if (is.null(mydata))
          return(NULL)
        tipoR <- as.numeric(input$t_suavizado)
        f <- as.numeric(input$frecuencia_usuario)
        p <- as.numeric(input$n)
        ts.data<-serieTiempo(mydata,f)
        x.fit<-aprendizaje(mydata,f,p)
        x.for<-pronostico(mydata,f,p)
        if(tipoR==1)
        {
          msimple<-HoltWinters(x=x.fit,alpha = NULL, beta = FALSE, gamma = FALSE)
          psimple<-forecast:::forecast.HoltWinters(msimple,h=p)
          forecast:::plot.forecast(psimple,lwd=3,col='black',type='o')

        }

        else if(tipoR==2)
        {


          mdouble<-HoltWinters(x=x.fit,alpha = NULL, beta = NULL, gamma = FALSE)
          pdouble<-forecast:::forecast.HoltWinters(mdouble,h=p)
          forecast:::plot.forecast(pdouble,lwd=3,col='black',type='o')

        }
        else if(tipoR==3)
        {
          mtripleAdd<-HoltWinters(x=x.fit,seasonal = "additive")
          ptripleAdd<-forecast:::forecast.HoltWinters(mtripleAdd,h=p)
          forecast:::plot.forecast(ptripleAdd,lwd=3,col='black',type='o')
        }
        else if (tipoR==4)
        {

          mtripleMult<-HoltWinters(x=x.fit,seasonal = "multiplicative")
          ptripleMult<-predict(mtripleMult,n.ahead=p)
          ptripleMult<-forecast:::forecast.HoltWinters(mtripleMult,h=p)
          forecast:::plot.forecast(ptripleMult,lwd=3,col='black',type='o')

        }
        else if (tipoR==5)
        {
          t<- seq(1:length(ts.data))                          # Variable independiente t: Tiempo
          rlineal <- lm(ts.data~t)
          par(mfrow=c(2,2))
          options(repr.plot.width=10, repr.plot.height=6)
          residual<-rlineal$residuals
          plot(t,residual,
               type='h',
               ylab='',main="Residuales Modelo Lineal",
               col="#75AADB",lwd=3)

          abline(h=0,lty=2)        # Linea para la media

          plot(density(residual),        # Gráfica de densidad
               xlab='x',
               main= 'Densidad Residuales Modelo Lineal',
               col="#75AADB",lwd=3)

          qqnorm(residual)               # Gráfica qqnorm para probar normalidad
          qqline(residual,col='#75AADB',lwd=3)         # Linea

          acf(residual, ci.type="ma",60) # Prueba ACF
        }
        else if (tipoR==6)
        {
          t<- seq(1:length(ts.data))  # Variable independiente t: Tiempo
          tt<-t*t
          rcuadratica <- lm(ts.data~t+tt)
          par(mfrow=c(2,2))
          options(repr.plot.width=10, repr.plot.height=6)
          residual<-rcuadratica$residuals
          plot(t,residual,
               type='h',
               ylab='',main="Residuales Modelo Cuadrático",
               col="purple",lwd=3)

          abline(h=0,lty=2)        # Linea para la media

          plot(density(residual),        # Gráfica de densidad
               xlab='x',
               main= 'Densidad Residuales Modelo Cuadrático',
               col="purple",lwd=3)

          qqnorm(residual)               # Gráfica qqnorm para probar normalidad
          qqline(residual,col='purple',lwd=3)         # Linea

          acf(residual, ci.type="ma",60) # Prueba ACF
        }
        else if (tipoR==7)
        {
          t<- seq(1:length(ts.data))  # Variable independiente t: Tiempo
          tt<-t*t
          ttt<-t*t*t
          rcubica <- lm(ts.data~t+tt+ttt)
          par(mfrow=c(2,2))
          options(repr.plot.width=10, repr.plot.height=6)
          residual<-rcubica$residuals
          plot(t,residual,
               type='h',
               ylab='',main="Residuales Modelo Cubico",
               col="#75AADB",lwd=3)

          abline(h=0,lty=2)        # Linea para la media

          plot(density(residual),        # Gráfica de densidad
               xlab='x',
               main= 'Densidad Residuales Modelo Cubico',
               col="#75AADB",lwd=3)

          qqnorm(residual)               # Gráfica qqnorm para probar normalidad
          qqline(residual,col='#75AADB',lwd=3)         # Linea

          acf(residual, ci.type="ma",60) # Prueba ACF
        }
        else if (tipoR==8)
        {

          T = length(x.fit)
          t=seq(1:T)
          It=seasonaldummy(x.fit)
          restacional = lm(x.fit ~ t + It)
          Itf = seasonaldummyf(x.fit,p)
          tf = seq(T+1,T+p,1)
          ## Predicción
          pestacional = predict(restacional,data.frame(t = tf,It=I(Itf)))
          posicion<-fecha(f,p-1,length(mydata))
          pestacional<-ts(pestacional,freq=f,start=posicion)
          options(repr.plot.width=10, repr.plot.height=6)

          plot.ts( ts.data,
                   type = 'o',
                   lwd=3,main='Prediccion Regresion Multiple con Estacionalidad')

          lines(pestacional,col='blue',lwd=3)
        }
      })



      output$resumen <- renderPrint({

        mydata=inFile()
        if (is.null(mydata))
          return(NULL)
        tipoR <- as.numeric(input$t_suavizado)
        f <- as.numeric(input$frecuencia_usuario)
        p <- as.numeric(input$n)
        ts.data<-serieTiempo(mydata,f)
        x.fit<-aprendizaje(mydata,f,p)
        x.for<-pronostico(mydata,f,p)
        if(tipoR==1)
        {
          msimple<-HoltWinters(x=x.fit,alpha = NULL, beta = FALSE, gamma = FALSE)
          msimple

        }

        else if(tipoR==2)
        {

          mdouble<-HoltWinters(x=x.fit,alpha = NULL, beta = NULL, gamma = FALSE)
          mdouble
        }
        else if(tipoR==3)
        {
          mtripleAdd<-HoltWinters(x=x.fit,seasonal = "additive")
          mtripleAdd
        }
        else if(tipoR==4)
        {

          mtripleMult<-HoltWinters(x=x.fit,seasonal = "multiplicative")
          mtripleMult

        }
        else if (tipoR==5)
        {
          t<- seq(1:length(ts.data))                          # Variable independiente t: Tiempo
          rlineal <- lm(ts.data~t)
          summary(rlineal)
        }
        else if (tipoR==6)
        {
          t<- seq(1:length(ts.data))                          # Variable independiente t: Tiempo
          tt<- t*t
          rcuadratica <- lm(ts.data~t+tt)
          summary(rcuadratica)
          #output$resumen2 <- renderPrint({AIC(rcuadratica)})
        }
        else if (tipoR==7)
        {
          t<- seq(1:length(ts.data))                          # Variable independiente t: Tiempo
          tt<- t*t
          ttt<-t*t*t
          rcubica <- lm(ts.data~t+tt+ttt)
          summary(rcubica)
        }
        else if (tipoR==8)
        {
          T=length(ts.data)           # Variable independiente t: Tiempo
          t=seq(1:(T-p))
          It=seasonaldummy(x.fit)
          restacional = lm(x.fit ~ t + It)
          summary(restacional)
        }

      })

      output$error <- renderPrint({

        mydata=inFile()
        if (is.null(mydata))
          return(NULL)
        tipoR <- as.numeric(input$t_suavizado)
        f <- as.numeric(input$frecuencia_usuario)
        p <- as.numeric(input$n)
        ts.data<-serieTiempo(mydata,f)
        x.fit<-aprendizaje(mydata,f,p)
        x.for<-pronostico(mydata,f,p)
        if(tipoR==1)
        {
          msimple<-HoltWinters(x=x.fit,alpha = NULL, beta = FALSE, gamma = FALSE)
          paste("Error:",msimple$SSE)

        }

        else if(tipoR==2)
        {

          mdouble<-HoltWinters(x=x.fit,alpha = NULL, beta = NULL, gamma = FALSE)
          paste("Error:",mdouble$SSE)
        }
        else if(tipoR==3)
        {
          mtripleAdd<-HoltWinters(x=x.fit,seasonal = "additive")
          paste("Error:",mtripleAdd$SSE)
        }
        else if(tipoR==4)
        {

          mtripleMult<-HoltWinters(x=x.fit,seasonal = "multiplicative")
          paste("Error:",mtripleMult$SSE)

        }
        else
        {
          NULL
        }

      })
      ####


      # Generate a plot of the data ----
      # Also uses the inputs to build the plot label. Note that the
      # dependencies on the inputs and the data reactive expression are
      # both tracked, and all expressions are called in the sequence
      # implied by the dependency graph.
      output$plot <- renderPlot({
        datos <- input$file1
        muestra <-NULL

        if (is.null(datos))
          datos <- var
        else{
          if(input$header)
          {
            muestra = scan(datos$datapath,skip=1)
          }
          else{
            muestra = scan(datos$datapath)
          }
        }

        if (is.null(datos) && is.null(muestra))
          return(NULL)
        else if(is.null(muestra)){
          muestra <- var
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
        Kolmogorov_Smirnov<-c(NA)

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

            ks<-ks.test(muestra, "pnorm", fit_normal$estimate[1], fit_normal$estimate[2])
            print(ks$p.value)
            Kolmogorov_Smirnov<-c(Kolmogorov_Smirnov,ks$p.value)
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
            ks<-ks.test(muestra, "plnorm", fit_lnormal$estimate[1], fit_lnormal$estimate[2])
            print(ks$p.value)
            Kolmogorov_Smirnov<-c(Kolmogorov_Smirnov,ks$p.value)
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
            #print(fit_unif$aic)

            ks<-ks.test(muestra, "punif", fit_unif$estimate[1], fit_unif$estimate[2])
            print(ks$p.value)
            Kolmogorov_Smirnov<-c(Kolmogorov_Smirnov,ks$p.value)
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

            ks<-ks.test(muestra, "pchisq", chi_k)
            print(ks$p.value)
            Kolmogorov_Smirnov<-c(Kolmogorov_Smirnov,ks$p.value)
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

            ks<-ks.test(muestra, "plogis", fit_logist$estimate[1], fit_logist$estimate[2])
            print(ks$p.value)
            Kolmogorov_Smirnov<-c(Kolmogorov_Smirnov,ks$p.value)
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

            ks<-ks.test(muestra, "pcauchy", fit_cauchy$estimate[1], fit_cauchy$estimate[2])
            print(ks$p.value)
            Kolmogorov_Smirnov<-c(Kolmogorov_Smirnov,ks$p.value)
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

            ks<-ks.test(muestra, "pexp", fit_exp$estimate)
            print(ks$p.value)
            Kolmogorov_Smirnov<-c(Kolmogorov_Smirnov,ks$p.value)
          }
        }



        #legend("topright", col=c('red', 'green'), legend=nombres, bty="o")
        legend("topright", legend=AjustmentTypes, fill=VectorColores, col=VectorColores,
               bty="n")

        #gofstat(List_Fits, fitnames = Distribucion)

        estadisticos <- data.frame(Media, AIC, Kolmogorov_Smirnov)
        row.names(estadisticos) <- Distribucion
        output$estadisticos <- DT::renderDataTable(
          DT::datatable(estadisticos, options = list(searching = FALSE, paging = FALSE, bLengthChange = FALSE))
        )




      })

      # Generate a summary of the data ----
      #       output$summary <- renderPrint({
      #         datos <- input$file1
      #         if (is.null(datos))
      #           muestra <- var
      # 		else{
      # 			if(input$header)
      # 			{
      # 			muestra = scan(datos$datapath,skip=1)
      # 			}
      # 			else{
      # 			muestra = scan(datos$datapath)
      # 			}
      #         }
      #         summary(muestra)
      #       })

      #########Resumen y error############
      output$resumen2 <- renderPrint({

        mydata=inFile()
        if (is.null(mydata))
          return(NULL)
        tipoR <- as.numeric(input$t_suavizado)
        f <- as.numeric(input$frecuencia_usuario)
        ##f <- 12
        p <- as.numeric(input$n)
        ##p <- 12
        ts.data<-serieTiempo(mydata,f)
        x.fit<-aprendizaje(mydata,f,p)
        x.for<-pronostico(mydata,f,p)
        if(tipoR==1)
        {
          msimple<-HoltWinters(x=x.fit,alpha = NULL, beta = FALSE, gamma = FALSE)
          msimple

        }

        else if(tipoR==2)
        {

          mdouble<-HoltWinters(x=x.fit,alpha = NULL, beta = NULL, gamma = FALSE)
          mdouble
        }
        else if(tipoR==3)
        {
          mtripleAdd<-HoltWinters(x=x.fit,seasonal = "additive")
          mtripleAdd
        }
        else if(tipoR==4)
        {

          mtripleMult<-HoltWinters(x=x.fit,seasonal = "multiplicative")
          mtripleMult

        }
        else if (tipoR==5)
        {
          t<- seq(1:length(ts.data))                          # Variable independiente t: Tiempo
          rlineal <- lm(ts.data~t)
          summary(rlineal)
        }
        else if (tipoR==6)
        {
          t<- seq(1:length(ts.data))                          # Variable independiente t: Tiempo
          tt<- t*t
          rcuadratica <- lm(ts.data~t+tt)
          summary(rcuadratica)
          #output$resumen2 <- renderPrint({AIC(rcuadratica)})
        }
        else if (tipoR==7)
        {
          t<- seq(1:length(ts.data))                          # Variable independiente t: Tiempo
          tt<- t*t
          ttt<-t*t*t
          rcubica <- lm(ts.data~t+tt+ttt)
          summary(rcubica)
        }
        else if (tipoR==8)
        {
          T=length(ts.data)           # Variable independiente t: Tiempo
          t=seq(1:(T-p))
          It=seasonaldummy(x.fit)
          restacional = lm(x.fit ~ t + It)
          summary(restacional)
        }

      })

      output$resumen3 <- renderPrint({

        mydata=inFile()
        if (is.null(mydata))
          return(NULL)
        tipoR <- as.numeric(input$t_suavizado)
        f <- as.numeric(input$frecuencia_usuario)
        ##f <- 12
        p <- as.numeric(input$n)
        ##p <- 12
        ts.data<-serieTiempo(mydata,f)
        x.fit<-aprendizaje(mydata,f,p)
        x.for<-pronostico(mydata,f,p)
        if(tipoR==1)
        {
          msimple<-HoltWinters(x=x.fit,alpha = NULL, beta = FALSE, gamma = FALSE)
          paste("SSE: ",msimple$SSE)
          #AIC(x.for$model)
          #AIC(x$model)

        }

        else if(tipoR==2)
        {

          mdouble<-HoltWinters(x=x.fit,alpha = NULL, beta = NULL, gamma = FALSE)
          paste("SSE: ",mdouble$SSE)
          #mdouble
        }
        else if(tipoR==3)
        {
          mtripleAdd<-HoltWinters(x=x.fit,seasonal = "additive")
          paste("SSE: ",mtripleAdd$SSE)
          #mtripleAdd
        }
        else if(tipoR==4)
        {

          mtripleMult<-HoltWinters(x=x.fit,seasonal = "multiplicative")
          paste("SSE: ",mtripleMult$SSE)
          #mtripleMult

        }
        else if (tipoR==5)
        {
          t<- seq(1:length(ts.data))                          # Variable independiente t: Tiempo
          rlineal <- lm(ts.data~t)
          paste("AIC: ",AIC(rlineal))
        }
        else if (tipoR==6)
        {
          t<- seq(1:length(ts.data))                          # Variable independiente t: Tiempo
          tt<- t*t
          rcuadratica <- lm(ts.data~t+tt)
          paste("AIC: ",AIC(rcuadratica))
        }
        else if (tipoR==7)
        {
          t<- seq(1:length(ts.data))                          # Variable independiente t: Tiempo
          tt<- t*t
          ttt<-t*t*t
          rcubica <- lm(ts.data~t+tt+ttt)
          paste("AIC: ",AIC(rcubica))
        }
        else if (tipoR==8)
        {
          T=length(ts.data)           # Variable independiente t: Tiempo
          t=seq(1:(T-p))
          It=seasonaldummy(x.fit)
          restacional = lm(x.fit ~ t + It)
          paste("AIC: ",AIC(restacional))
        }

      })

      # output$error <- renderPrint({
      #
      #   mydata=inFile()
      #   if (is.null(mydata))
      #     return(NULL)
      #   tipoR <- as.numeric(input$t_suavizado)
      #   ##f <- as.numeric(input$frecuencia_usuario)
      #   f <- 12
      #   ##p <- as.numeric(input$n)
      #   p <- 12
      #   ts.data<-serieTiempo(mydata,f)
      #   x.fit<-aprendizaje(mydata,f,p)
      #   x.for<-pronostico(mydata,f,p)
      #   if(tipoR==1)
      #   {
      #     msimple<-HoltWinters(x=x.fit,alpha = NULL, beta = FALSE, gamma = FALSE)
      #     paste("Error:",msimple$SSE)
      #
      #   }
      #
      #   else if(tipoR==2)
      #   {
      #
      #     mdouble<-HoltWinters(x=x.fit,alpha = NULL, beta = NULL, gamma = FALSE)
      #     paste("Error:",mdouble$SSE)
      #   }
      #   else if(tipoR==3)
      #   {
      #     mtripleAdd<-HoltWinters(x=x.fit,seasonal = "additive")
      #     paste("Error:",mtripleAdd$SSE)
      #   }
      #   else if(tipoR==4)
      #   {
      #
      #     mtripleMult<-HoltWinters(x=x.fit,seasonal = "multiplicative")
      #     paste("Error:",mtripleMult$SSE)
      #
      #   }
      #   else
      #   {
      #     NULL
      #   }
      #
      # })
      ################

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
      #output$ensayo <- renderPrint({
      #  datos <- input$file1
      #  if (is.null(datos))
      #    muestra <- var
      #  else{
      #    if(input$header)
      #    {
      #      muestra = scan(datos$datapath,skip=1)
      #    }
      #    else{
      #      muestra = scan(datos$datapath)
      #    }
      #  }
      #  #summary(muestra)
      #  summary(muestra)
      #
      #})


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


#Serie sintetica 1
#T1<-200
#ss<-rep(0,T1)
#for(t1 in 1:T1){
#  #y[t]<-cos(2*3.1416*((t/12)+runif(1,min=0,max=1)))
#  ss[t1]<-cos(2*3.1416*((t1/12)+0.1*runif(1,min=0,max=1)))
#}
#plot(ss,type = 'l')


#Serie sintetica 2
#TT<-40
#yy<-rep(0,TT)
#for(tt in 1:TT){
#  l1=0
#  l2=0
#  l3=0
#  if((tt+3)%%4==0){
#    l1=1
#  }
#  if((tt+2)%%4==0){
#    l2=1
#  }
#  if((tt+1)%%4==0){
#    l3=1
#  }
#  yy[tt]=10+(0.8*tt)+0.05*tt*tt + 15*l1 + 10.0*l2 + 3*l3 + rnorm(1)
#}
#plot(x=c(1:TT), y=yy,type = 'l')



#Serie sintetica 3
#T3<-100
#y3<-rep(0,T3)
#e3<-rep(0,T)
#for(t3 in 1:100){
#  e3[t3]=rnorm(1,mean = 0,sd = 1)
#  y3[t3]=10+(0.7*t3)+e3[t3]
#}
#plot(y3,type = 'l')

#binner(NULL)
#binner(ss)
#binner(yy)
#binner(y3)
