library(shiny)
library(data.table)
library(ggplot2)
library(plotly)
library(shinydashboard)
library("htmltools")
library("bsplus")
library(dplyr)
library(shinyjs)
library(unbalanced)
library(class)

df <- fread('creditcard.csv',header=T)
df$Class <- factor(df$Class)
df$Time<- df$Time  %>% scale() %>% as.data.frame()
df$Amount<- df$Amount  %>% scale() %>% as.data.frame()

ui <- fluidPage(
    useShinyjs(),
    includeCSS("customStyle.css"),
    bs_carousel(id = "the_beatles", use_indicators = TRUE, use_controls=FALSE) %>%
      bs_append(
        content = bs_carousel_image(src = "FruadDetection.jpg"),
        caption = bs_carousel_caption("Credit Card Fraud Detection", "Anonymized credit card transactions labeled as fraudulent or genuine")
    ),
    dashboardPage(skin = "black",
      dashboardHeader(),
      dashboardSidebar(disable = TRUE),
        dashboardBody(
          fluidRow(
            infoBoxOutput("noFraudBox"),
            infoBoxOutput("fraudBox"),
            infoBoxOutput("missingValue")
          ),
          fluidPage(
            fluidRow(
              column(width = 6,
                     box(
                       title = "Class", width = NULL, status = "danger", background = "navy",
                       plotlyOutput("classGraph")
                     )  
              ),
              column(width = 6,
                     box(
                       title = "Data Table", width = NULL, height = NULL, status = "danger", background = "navy",
                       div(style = 'overflow-x: scroll', tableOutput('contents'))
                     )                     
              )
            ),
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL, height = NULL, status = "danger", background = "navy",
                       fluidRow(
                         column(width = 12,
                                box(
                                  width = NULL, height = NULL, status = "danger", background = "navy",
                                  radioButtons("sampling", "Choose Sampling", selected = 1,
                                               c("Under Sampling" = 1))
                                )
                         ),
                         column(width = 6,
                                box(
                                  width = NULL, height = NULL, status = "danger", background = "navy",
                                  sliderInput(inputId = "perc",
                                              label = "perc",
                                              value= 50, min = 10,
                                              max = 50, step = 1
                                  )
                                )
                         ),
                         column(width = 6,
                                box(
                                  width = NULL, height = NULL, status = "danger", background = "navy",
                                  sliderInput(inputId = "subset",
                                              label = "Spliting set",
                                              value = 0.70, min = 0.60,
                                              max = 0.80, step = 0.01
                                  )
                                )
                         ),
                         column(width = 12,
                                tags$head(
                                  tags$style(HTML('#go{background-color:#DD4B39; border-color: #DD4B39;color: #FEF9F9;} #boxUnder{text-align: center;}'))
                                ),
                                box(
                                  id = "boxUnder", width = NULL, status = "danger", background = "navy",
                                  actionButton(inputId = "go",
                                               label = "Generate Graph after Undersampling and Make Train-Test set"),                                
                                )
                         ),
                         column(id="underSamplingColumn", width = 6,
                                box(
                                  title = "Class after do under Sampling", width = NULL, status = "danger", background = "navy",
                                  plotlyOutput("underSamplingGraph")
                                )
                         ),
                         column(id="underSamplingSummary", width = 6,
                                box(
                                  title = "Summary", width = NULL, status = "danger", background = "navy",
                                  verbatimTextOutput("underSamplingSummary")
                                )
                         ),
                         column(width = 12,
                                box(
                                  width = NULL, height = NULL, status = "danger", background = "navy",
                                  radioButtons("algorithm", "Choose Algorithm", selected = 0,
                                               c("None" = 0,
                                                 "K-nearest Neighbors" = 1,
                                                 "Logistic Regression" = 2
                                               ))
                                )
                         ),
                         column(id="KNNColumn", width = 12,
                                tags$head(
                                  tags$style(HTML('#goKNN{background-color:#DD4B39; border-color: #DD4B39;color: #FEF9F9;}'))
                                ),
                                box(
                                  width = NULL, height = NULL, status = "danger", background = "navy",
                                  sliderInput(inputId = "inputK",
                                              label = "Input K",
                                              min = 1, max = 30,
                                              value = c(1, 18),
                                  ),
                                  actionButton(inputId = "goKNN",
                                               label = "OK"
                                  ),
                                  plotlyOutput("knnGraph")
                                )
                         ),
                         column(id="LogisticColumn", width = 12,
                                tags$head(
                                  tags$style(HTML('#goLogistic{background-color:#DD4B39; border-color: #DD4B39;color: #FEF9F9;}'))
                                ),
                                box(
                                  width = NULL, height = NULL, status = "danger", background = "navy",
                                  sliderInput(inputId = "inputCutOff",
                                              label = "Input Cutoff",
                                              min = 0.0, max = 1.0,
                                              value = c(0.1, 0.8)
                                  ),
                                  actionButton(inputId = "goLogistic",
                                               label = "OK"
                                  ),
                                  plotlyOutput("logisticGraph")
                                
                                )
                         )
                       )
                     )
              )
            )
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$noFraudBox <- renderValueBox({
      infoBox(
        "No Fraud", paste(as.data.frame(summary(df$Class))[1, 1]), icon = icon("credit-card"),
        color = "navy", fill = TRUE
      )
    })
    output$fraudBox <- renderValueBox({
      infoBox(
        "Fraud", paste(as.data.frame(summary(df$Class))[2, 1]), icon = icon("credit-card"),
        color = "navy", fill = TRUE
      )
    })
    output$missingValue <- renderValueBox({
      infoBox(
        "Missing Value", paste(is.na(df) %>% any), icon = icon("credit-card"),
        color = "navy", fill = TRUE
      )
    })
    output$contents <- renderTable(width = "auto",{
        head(df, n=11)
    })
    output$summaryData <- renderPrint({
      str(df)
    })
    output$classGraph <- renderPlotly({
      ggplot(df)+
      geom_bar(aes(x=Class, fill=Class)) +
      theme_bw()
    })
    observeEvent(input$perc, {
      showNotification(paste("percentage of sampling: ", input$perc), type = "message", duration = 3)
    })
    observe({
      if (input$go == 0) {
        hide("underSamplingColumn")
        hide("underSamplingSummary")
      } else {
        show("underSamplingColumn")
        show("underSamplingSummary")
      }
      if (input$goKNN == 0) {
        hide("knnGraph")
      } else {
        show("knnGraph")
      }
      if (input$goLogistic == 0) {
        hide("logisticGraph")
      } else {
        show("logisticGraph")
      }
    })
    newUnderSamplingData <- eventReactive(input$go, {
      set.seed(1234)
      df.ubUnder<-ubUnder(X=df[, -31], Y=df$Class, perc = as.numeric(input$perc),  method = "percPos")
      newData<-cbind(df.ubUnder$X, Class = df.ubUnder$Y) 
      train.id <- caTools::sample.split(newData$Class, SplitRatio = 0.70)
      newData.train <- subset(newData, train.id)
      newData.validate <- subset(newData, !train.id)
      newData.train.class <- newData$Class[train.id]
      newData.validate.class <- newData$Class[!train.id]
      return (list("newData" = newData, "newData.train" = newData.train, "newData.validate" = newData.validate, "newData.train.class"= newData.train.class, "newData.validate.class" = newData.validate.class))
    })
    output$underSamplingGraph <- renderPlotly({
      ggplot(newUnderSamplingData()$newData)+
        geom_bar(aes(x=Class, fill=Class)) +
        theme_bw()
    })
    output$underSamplingSummary <- renderPrint({
      summary(newUnderSamplingData()$newData$Class)
    })
    observeEvent(input$algorithm, {
     if (input$algorithm == 0) {
       hide("LogisticColumn")
       hide("KNNColumn")
     }else if (input$algorithm == 1) {
       show("KNNColumn")
       hide("LogisticColumn")
     } else if (input$algorithm == 2) {
       show("LogisticColumn")
       hide("KNNColumn")
     } else {
       hide("LogisticColumn")
       hide("KNNColumn")
     }
    })
    find.opt.k <- function(k) {
      set.seed(1234)
      predictied <- knn(newUnderSamplingData()$newData.train[, 1:30], newUnderSamplingData()$newData.validate[, 1:30], newUnderSamplingData()$newData.train.class, k = k)
      confusion.table <- table(predictied, newUnderSamplingData()$newData.validate.class)
      confusion.matrix <- data.frame(pred_Y = c(confusion.table[2, 2], confusion.table[2, 1]),
                                     pred_N = c(confusion.table[1, 2], confusion.table[1, 1]),
                                     row.names = c("Fraud", "No Fraud"))
      Accuracy <- (confusion.matrix[1, 1] + confusion.matrix[2 ,2])/ sum(confusion.matrix)
      Sensitivity <- confusion.matrix[1, 1]/sum(confusion.matrix[, 1])
      Specificity <- confusion.matrix[2, 2]/sum(confusion.matrix[, 2])
      df.perf <- c(Accuracy=Accuracy, Sensitivity=Sensitivity, Specificity=Specificity)
    }
    resultKNN <- eventReactive(input$goKNN, {
      vec.k <- input$inputK[1]:input$inputK[2]
      results <- sapply(vec.k, find.opt.k)
      results <- apply(results,1,unlist)
      results <- as.data.frame(results)
      results$k <- vec.k
      return(results)
    })
    output$knnGraph <- renderPlotly({
      ggplot(data=resultKNN())+
        geom_line(aes(x=k,y=Accuracy),size=1,color="red")+
        geom_line(aes(x=k,y=Sensitivity),size=1,color="blue")+
        geom_line(aes(x=k,y=Specificity),size=1,color="green")+
        ylab('performance')+
        theme_bw()
    })
    logisticPred <- eventReactive(input$algorithm, {
      if (input$algorithm == 2) {
        logistic_AIC = glm(formula = Class ~ V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + 
                             V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + 
                             V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + Amount,
                           family = binomial, data = newUnderSamplingData()$newData.validate)
        pred <- predict(logistic_AIC, newdata = newUnderSamplingData()$newData.validate, type = "response")
        return(pred)
      }
    })
    find.opt.cutoff <- function(cutoff) {
      perf.table <- table(newUnderSamplingData()$newData.validate$Class, logisticPred() > cutoff)
      
      if (ncol(perf.table) == 1) {
        if (colnames(perf.table) == "TRUE") {
          perf.table <- cbind(c(0, 0), perf.table)
        }else{
          perf.table <- cbind(perf.table, c(0, 0))
        }
      }
      
      conf.mat <-
        data.frame(
          pred_T = c(perf.table[2, 2], perf.table[1, 2]),
          pred_F = c(perf.table[2, 1], perf.table[1, 1]),
          row.names = c("act_T", "act_F")
        )
      ACC <-  (conf.mat[1, 1] + conf.mat[2, 2]) / sum(conf.mat)
      Sensitivity <-  conf.mat[1, 1] / sum(conf.mat[, 1])
      Specificity <-  conf.mat[2, 2] / (conf.mat[2, 1] + conf.mat[2, 2])
      df.perf <- c(ACC=ACC, Sensitivity=Sensitivity, Specificity=Specificity)
    }
    resultLogistic <- eventReactive(input$goLogistic, {
      cutoff <- seq(input$inputCutOff[1],input$inputCutOff[2],.1)
      results2 <- sapply(cutoff, find.opt.cutoff) %>% t %>% data.frame()
      results2$cutoff <- cutoff
      return(results2)
    })
    output$logisticGraph <- renderPlotly({
        ggplot(data=resultLogistic())+geom_line(aes(x=cutoff,y=ACC),size=1,color="red")+
        geom_line(aes(x=cutoff,y=Sensitivity),size=1,color="blue")+
        geom_line(aes(x=cutoff,y=Specificity),size=1,color="green")+
        ylab('performance')+
        theme_bw()
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
