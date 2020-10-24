#STAT 628 Homework 2 Group 16
#The bodyfat calculator Shiny App 
#Writer: Qingchuan Ji
#Examiner: Jie Sheng

#Using package
library(shiny)

#User interface for the BodyFat calculator
ui<-fluidPage(
  #Title
  titlePanel("BodyFat Calculator"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Please enter these three measurements of your body to calculate your bodyfat. (Due to the database 
               limits, this calclator can only calculate male's bodyfat now."),            
         
      numericInput("Abdomen",
                   label = h5("Abdomen circumference (cm)"),
                   value = ""),
      numericInput("Weight",
                   label = h5("Weight (lbs)"),
                   value = ""),
      numericInput("Wrist",
                   label = h5("Wrist circumference (cm)"),
                   value = ""),
      br(),
      actionButton("button", label = "Test your bodyfat / Retest") ),
    mainPanel(
      tabsetPanel(
        tabPanel("Bodyfat Calculation",
                 p(h3("Your body measurements:")),
                 textOutput("Abdomen"),
                 textOutput("Weight"),
                 textOutput("Wrist"),
                 br(),
                 p(h3("Your bodyfat:")),
                 textOutput("Bodyfat"),
                 textOutput("Advice")),
        tabPanel("Application introduction",
                 p(h3("Bodyfat Calculator:")),
                 br(),
                 helpText("This calculator provides a convenient way to calculte your bodyfat."),
                 br(),
                 helpText("The database of the calculator is a real data set of 252 men with 
                          measurements of their percentage of body fat and various body circumference measurements."),
                 br(),
                 HTML("<b>The linear model in the bodyfat calculator: </b>
                      <br> <br>
                      Bodyfat = -24.4911+0.8826*Abdomen-0.0913*Weight-1.1954*Wrist <br>
                      <br> <br>
                      <b>The variables in this model: </b>
                      <br> <br>
                      Bodyfat = The percent body fat <br>
                      Abdomen = Abdomen circumference (cm) <br>
                      Weight = Weight (lbs) <br>
                      Wrist = Wrist circumference (cm) ")
        
                 ),
        tabPanel("Remark",
                 helpText("This shiny app is used for STAT 628 Homework 2, producing by Group 16"),
                 br(),
                 HTML("<b>Writter: Qingchuan Ji </b>
                       <br> <br>
                       <b>Examiner: Jie Sheng </b>
                       <br> <br>
                       If you have any question about this shiny app, feel free to contract me at qji5@wisc,edu. <br>
                       I will be very happy to discuss this app with you!")
                 )
                 )
                 )
                 )
                 )
                 

#Shiny server for the bodyfat calculator
server<-function(input, output) {
  
  bodyfat <- reactiveValues()
  # Calculate the interest and amount    
  observe({
    input$button
    bodyfat$int <- isolate({
      -24.4911+0.8826*(input$Abdomen)-0.0913*(input$Weight)-1.1954*(input$Wrist)
    })
    })
  
  
  # Display body measurements entered
  output$Abdomen <- renderText({
    input$button
    validate(
    need(input$button, 'Abdomen circumference (cm):'))
    paste("Abdomen circumference (cm):", isolate(input$Abdomen))
  })
  br()
  output$Weight <- renderText({
    input$button
    validate(
    need(input$button , 'Weight (lbs):'))
    paste("Weight (lbs):", isolate(input$Weight))
  })
  br()
  output$Wrist <- renderText({
    input$button
    validate(
    need(input$button, 'Wrist circumference (cm):'))
    paste("Wrist circumference (cm):", isolate(input$Wrist))
  })

  
  # Display calculated bodyfat

  output$Bodyfat <- renderText({
    if(input$button == 0) {""}
    else if(is.na(bodyfat$int)){"Warning: Please enter right value of the measurements!"}
    
    else if(isolate(input$Abdomen)<=0|isolate(input$Weight)<=0|isolate(input$Wrist)<=0){
      "The data your entering contains non-positive value. Please check it!"}
    else {paste("Your bodyfat is:", bodyfat$int)
  }}
)
  output$Advice <- renderText({
    if(input$button == 0) {""}
      else if(is.na(bodyfat$int)){""}
      else if(bodyfat$int<=0 | bodyfat$int>100){
      "Warning: You badyfat calculation is less than 0 or larger than 100. Please check your entered values"}
      else if(bodyfat$int>0 & bodyfat$int<=2){
      "Your bodyfat is very low! This bodyfat may be impossible! "
    }
      else if(bodyfat$int>2 & bodyfat$int<=15){
      "Advice: If you are an athlete, this bodyfat is fine. If not, try to eat more food to protect your heart and muscle!"
    }
      else if(bodyfat$int>10 &bodyfat$int<=25){
      "Advice: Your body fat is good now. Try to keep this achievement!"
    }
      else if(bodyfat$int>25 &bodyfat$int<=50){
      "Advice: You bodyfat is high now. Try to eat more healthy food and do sports to decrease your bodyfat!"
    }
      else{"Advice: If the measurements of your body is true. Try to do the professional test on your body and ask experts for advice!"}
  })
  }

# Create and Run the Shiny app (Bodyfat Calculator)
shinyApp(ui = ui, server = server)


