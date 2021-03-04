
library(shiny)
ui <- fluidPage(titlePanel("this is my app"),sidebarLayout(position="left",sidebarPanel(h1("Give the input"),sliderInput("one","select the number",min=1,max=100,value=25),textInput("three","Enter the HeadLine"),actionButton("two","clickhere")),
                              mainPanel(h6("this is mainpanel"),plotOutput("hist"),textOutput("histogram"))
                               
                              ))
server <- function(input,output){
  data <- reactive({
    input$one
  })
  data1 <- reactive({
    input$three
  })

  output$hist <- renderPlot({
    input$two
    isolate({
      hist(rnorm(data()),main = data1())
    })
  
  })
    
  
}
shinyApp(ui,server)