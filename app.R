library(shiny)
library(shinydashboard)
library(tree)

trainingdata <- read.csv(file = "./datasets/processed_training_data.csv", sep = ",") 
trainingdata$class <- as.factor(trainingdata$class)

set.seed(3)
tree.trained = tree(as.factor(trainingdata$class)~., trainingdata)

restecg.result1 = 'normal'
restecg.result2 = 'having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV'
restecg.result3 = 'showing probable or definite left ventricular hypertrophy by Estes\'\ criteria'

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "HeartSpot"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information", tabName = "info", icon = icon("info")),
      menuItem("Prediction", tabName = "prediction", icon = icon("desktop")) #see the ?icon
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "info",
              column(width = 6,
                     box(width = NULL, status = "danger",
                         HTML("<h1 style='text-align: center; color:red; font-weight: bold;'>HeartSpot</h1>"),
                         HTML("<img src='image4.png' style='display: block; margin-left: auto; margin-right: auto; width: 80%;'/>"),
                         HTML("<p style='text-align: center; color:gray; font-weight: bold; font-style: italic;'>AI-powered Heart Disease prediction tool</p>")
                     )
              ),
              column(width = 6,
                     box(width = NULL, status = "danger",
                         h3("Description"),
                         HTML("<p align='justify'>The system you have accessed is an AI-powered web application build for health care professionals to predict the presence of Heart Disease 
                              in a patient. We have implemented a Decision Tree model and used the <a href='http://archive.ics.uci.edu/ml/datasets/Heart+Disease'>UCI Heart Disease dataset</a> to train and test this system.</p>"),
                         h3("Usage"),
                         p(align = "justify", "By navigating to the prediction tab, you will be able to enter the variables this tool needs to calculate its prediciton. In total, this tool needs information about 10 different
                           parameters. When all of them are entered, press the Apply button to get the prediction."),
                         h3("Background"),
                         HTML("<p align='justify'>We are Health Informatics students from Karolinska Institutet in Stockholm who have build this application as part of a course project. If you are interested 
                              in our source code or would like to get in touch with us, please have a look at our <a href='https://github.com/LeooLeoo/heartspot'>GitHub</a> repository.</p>")
                         )
                         )
                     ),
      tabItem(tabName = "prediction",
              fluidRow(
                column(width = 8,
                       h2("Decision Tree"),
                       box(width = NULL, status = "danger",
                           plotOutput('plot1'),
                           actionButton(inputId="button", label="Apply to input")
                       ),
                       h2("Interpretation"),
                       box(width = NULL, status = "danger",
                           htmlOutput("result")
                       )
                ),
                column(width = 4,
                       h2("Data Input"),
                       box(width = NULL, title = "Demographics", status = "danger",
                           numericInput('Age', 'Age','25', width = "50%"),  
                           selectInput('Sex', 'Gender',  c('male','female'), width = "50%")
                       ),
                       box(width = NULL, title = "Symptoms", status = "danger",
                           selectInput('CP', 'Chest Pain',  c('typical angina','atypical angina', 'non-anginal pain', 'asymptomatic'), width = "75%"),
                           selectInput('exang', 'Exercise-induced angina', c('no','yes'), width = "75%")
                       ),
                       box(width = NULL, title = "ECG", status = "danger",
                           sliderInput('oldpeak', 'ST depression',0,5,3.0,0.2),
                           numericInput('trestbps', 'Resting blood pressure in mmHg','120', width = "75%"),
                           numericInput('thalach', 'Maximum heart rate achieved per minute','180', width = "75%"),
                           selectInput('restecg', 'Resting electrocardiographic results',  c(restecg.result1, restecg.result2, restecg.result3), width = "75%")
                       ),
                       box(width = NULL, title = "Lab results", status = "danger",
                           numericInput('chol', 'Serum cholesterol in mg/dl','200', width = "50%"),
                           selectInput('fbs', 'Fasting blood glucose >120mg/dl', c('no','yes'), width = "50%")
                       )
                )
              )
      )
    ),
    tags$script("document.getElementById('button').addEventListener('click', function(){
                alert('Disclaimer: We cannot be held accountable for the accuracy or correctness of the following prediction and any actions undertaken because of the prediction are on your own responsibility.');
    });")
  )
)




server <- function(input, output) {
  #input
  sex <- reactive({
    if (input$Sex == 'male'){
      sex <- 1
    } else if(input$Sex == 'female'){
      sex <- 0
    } 
  })
  
  cp <- reactive({
    if (input$CP == 'typical angina'){
      cp <- 1
    } else if(input$CP == 'atypical angina'){
      cp <- 2
    } else if(input$CP == 'non-anginal pain'){
      cp <- 3
    } else if(input$CP == 'asymptomatic'){
      cp <- 4
    }
  })
  
  exang <- reactive({
    if (input$exang == 'no'){
      exang <- 0
    } else if(input$exang == 'yes'){
      exang <- 1
    } 
  })
  
  restecg <- reactive({
    if (input$restecg == restecg.result1){
      restecg <- 0
    } else if(input$restecg == restecg.result2){
      restecg <- 1
    } else if(input$restecg == restecg.result3){
      restecg <- 2
    }
  })
  
  fbs <- reactive({
    if (input$fbs == 'no'){
      fbs <- 0
    } else if(input$fbs == 'yes'){
      fbs <- 1
    } 
  })
  
  output$plot1 <- renderPlot({ 
    plot(tree.trained)
    text(tree.trained, pretty=0)
  })
  
  observeEvent(input$button, {
    #点击按钮后生存df
    userfeatures <- data.frame(age = as.integer(input$Age), 
                               sex = sex(), 
                               cp = cp(), 
                               trestbps = input$trestbps, 
                               chol = input$chol, 
                               fbs = fbs(), 
                               restecg = restecg(), 
                               thalach = input$thalach, 
                               exang = exang(), 
                               oldpeak = input$oldpeak)
    
    pred <- predict(tree.trained, userfeatures, type="class")
    
    output$result <- renderUI({ 
      
      result <- ""
      color <- ""
      
      if (pred == 1){
        result <- "high"
        color <- "red"
      } else{
        result <- "low"
        color <- "blue"
      }
      HTML(sprintf("
           <p align='justify'>Based on your input, this patient is at a <span style='color:%s; font-weight:bold;'>%s</span> 
                   risk of having or developing Heart Disease. If you would like to understand and recreate the prediction, you have 
                   to follow the corresponding path in the Decision Tree provided above. The following table explains the feature 
                   abbreviations.</p>
                   <img src='features.png' style='display: block; margin-left: auto; margin-right: auto; width: 80%%;'/>
                   ", color, result)
      )
    })
  })
}

shinyApp(ui = ui, server = server)
