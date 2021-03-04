library(shiny)
library(shape)

ui <- fluidPage(
  actionButton("create","create 4 random numbers"),
            wellPanel(tableOutput("Random_Numbers")),          
  fluidRow(column(3,"1"),column(8,plotOutput("A"))),
  fluidRow(column(3,"2"),column(8,plotOutput("B"))),
  fluidRow(column(3,"3"),column(8,plotOutput("C"))),
  fluidRow(column(3,"4"),column(8,plotOutput("D")))
)

server <- function(input,output){
    data <- eventReactive(input$create,
      {
        
        random_numbers <- runif(4,0,1)
  })
    output$Random_Numbers <- renderTable({
      
      table<-data()
      table
      
    })
    
    output$A <- renderPlot({
      t<-data()
      if (t[1] < 0.25)
      {
        color="red"
      }
      else if (t[1]<= 0.5 && t[1] > 0.25)
      {
        color="blue"
      }
      else if (t[1]<= 0.75 && t[1] > 0.5)
      {
        color="black"
      }
      else 
      {
        color="green"
      }
      
      
      emptyplot(xlim = c(-2, 2), ylim = c(-2, 2))
      filledellipse(rx1 = 1, ry1 = 0.5, mid = c(1, 1) ,
                    col = shadepalette(endcol = color))
    })
    output$B <- renderPlot({
      t<-data()
      if (t[2] < 0.25)
      {
        color="red"
      }
      else if (t[2]<= 0.5 && t[2] > 0.25)
      {
        color="blue"
      }
      else if (t[2]<= 0.75 && t[2] > 0.5)
      {
        color="black"
      }
      else 
      {
        color="green"
      }
      emptyplot(xlim = c(-2, 2), ylim = c(-2, 2))
      filledellipse(rx1 = 1, ry1 = 0.5, mid = c(1, 1) ,
                    col = shadepalette(endcol = color))
    })
    output$C <- renderPlot({
      t<-data()
      if (t[3] < 0.25)
      {
        color="red"
      }
      else if (t[3]<= 0.5 && t[3] > 0.25)
      {
        color="blue"
      }
      else if (t[3]<= 0.75 && t[3] > 0.5)
      {
        color="black"
      }
      else 
      {
        color="green"
      }
      
      emptyplot(xlim = c(-2, 2), ylim = c(-2, 2))
      filledellipse(rx1 = 1, ry1 = 0.5, mid = c(1, 1) ,
                    col = shadepalette(endcol = color))
    })
    output$D <- renderPlot({
      t<-data()
      if (t[4] < 0.25)
      {
        color="red"
      }
      else if (t[4]<= 0.5 && t[4] > 0.25)
      {
        color="blue"
      }
      else if (t[4]< 0.75 && t[4] > 0.5)
      {
        color="black"
      }
      else 
      {
        color="green"
      }
      
      emptyplot(xlim = c(-2, 2), ylim = c(-2, 2))
      filledellipse(rx1 = 1, ry1 = 0.5, mid = c(1, 1) ,
                    col = shadepalette(endcol = color))
    })
    
    
  }


shinyApp(ui=ui,server = server)