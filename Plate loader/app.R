

library(shiny)
library(shinythemes)

# Define UI for application 
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
   
  #Title panel for the app 
  titlePanel("Barbell plate calculator") ,
  
  #Input for bar type
  selectInput(inputId = "barwt", label = "Select the bar type", c("20KG bar" = 20 , "15KG bar" = 15)),
   

  #Input for the total weight 
  numericInput(inputId = "weight",
                "Input the total weight that you want to lift: ",
                min = 20,
                max = 400,
                value = 100, step= 2.5) , 
  #Button to run the calculation 
  actionButton(inputId = "go" , "Calculate") ,
  
  br(),
  br(),
  
  #Output for instuctions and the number of each plate  
  tags$b(textOutput("instruc1")),
  tags$b(textOutput("instruc2")),
  textOutput("no20s"), 
  textOutput("no10s"),
  textOutput("no5s"),
  textOutput("no25s"),
  textOutput("no125s")
   
)

# Define server 
server <- function(input, output) {
  
  #Inputs to be put through the server once the button has been pressed
  barwt <- eventReactive(input$go, input$barwt )  
  inless20div2 <- eventReactive(input$go, {(input$weight - as.integer(barwt()))/2 }) 
  
  #Calculate the number of each plate required
  no20s <- reactive({inless20div2() %/% 20}) 
  no10s <- reactive({(inless20div2() - (20*no20s())) %/% 10})
  no5s <- reactive({(inless20div2() - (20*no20s()+10*no10s())) %/% 5})
  no25s <- reactive({(inless20div2() - (20*no20s()+10*no10s()+5*no5s())) %/% 2.5})
  no125s <- reactive({(inless20div2() - (20*no20s()+10*no10s()+5*no5s()+2.5*no25s())) %/% 1.25})
  
  #Instructions to be printed after a calculation has been performed
  instructions1 <- eventReactive(input$go, paste("For a combined weight of ",input$weight,"KG, 
                                                you need to load"))
  instructions2 <- eventReactive(input$go, paste("the following on each side of the",
                                                input$barwt, "KG barbell:"))
  
  #Render each instruction line
  output$instruc1 <- renderText(instructions1())
  output$instruc2 <- renderText(instructions2())
  output$no20s <- renderText(paste(no20s(),"x 20KG plates"))
  output$no10s <- renderText(paste(no10s(),"x 10KG plates"))
  output$no5s <- renderText(paste(no5s(),"x 5KG plates"))
  output$no25s <- renderText(paste(no25s(),"x 2.5KG plates"))
  output$no125s <- renderText(paste(no125s(),"x 1.25KG plates"))
   
}

# Run the application 
shinyApp(ui = ui, server = server)

