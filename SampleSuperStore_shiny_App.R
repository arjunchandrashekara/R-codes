setwd("C:/Users/Arjun/Desktop")
Sys.getenv("R_ARCH")
Sys.setenv(JAVA_HOME= 'C:/Program Files/Java/jre1.8.0_261')
library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)

library(memoise)
library(shinyWidgets)
library(DT)
library(gt)

options(shiny.maxRequestSize=100*1024^2)

## Only run examples in interactive R sessions


ui <- fluidPage(tags$head(tags$style(
  HTML('
         #sidebar {
            background-color: blue;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }'))),# use a gradient in background
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom"
  ),setBackgroundImage(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSG2Kr_H3JRygdYmIjVQjmQk4rTSRbfZQqAWQ&usqp=CAU"),h1(id="big-heading", "R Shiny Assignment"),
  tags$style(HTML("#big-heading{color: red;}"))
  ,
  
  sidebarLayout(
    sidebarPanel(id="sidebar",
                 fileInput("file1", "Choose xls File",
                           accept = 
                             ".xls"
                 ),actionButton("import","Import the Datset"),
                 tags$hr()
                 ,hr(),h3("Related Links", br()),
                 
                 a(href = "https://youtu.be/SY7x2Ayj_b0",
                   "Instructions video for Rshiny"),
                 
                 br(),
                 
                 a(href = "http://www.datazymes.com/",
                   "About DataZymes Analytics")
                 
    ),
    
    mainPanel(h1(id="mainobjective","The Main Objective"),tags$style(HTML("#mainobjective{color: red;}")),h2(id="First_Line","1) Create a R shiny dashboard using the superstore data "),tags$style(HTML("#First_Line{color: red;}")),h3(id="Second_Line","2) The Superstore data is at Customer ID X Order ID X Order date level "),tags$style(HTML("#Second_Line{color: red;}")),h3(id="Third_Line","3) Refer to Definition sheet for information of each column."),tags$style(HTML("#Third_Line{color: red;}")),
              dataTableOutput("Super_Store",),actionButton("review","Continue to Review the Data"),plotOutput("Plot1"),downloadButton("downloadData", "Download"),actionButton("analysis","Analyse the Data"),dataTableOutput("kmeans")
    )
  )
)

server <- function(input, output) {
  new_data<- reactive({
    
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    df <- read_excel(inFile$datapath,col_names = TRUE)
    df <- as.data.frame(df)
    df
  })
  k_means <- reactive({
    aaa <- new_data()
    if (is.null(aaa))
      return(NULL)
    clustering <- scale(aaa[,c(18,19,20,21)])
    clusters <- kmeans(clustering, 5)
    aaa$cluster <- as.factor(clusters$cluster)
    aaa
    
    
    
    
  })
  output$Super_Store <- renderDataTable({
    
    input$import
    isolate({
      withProgress({
        setProgress(message="Creating the table......")
        wc_data <- new_data()
        datatable(wc_data, options = list(
          columnDefs = list(list(className = 'dt-center', targets = 5)),
          pageLength = 100,
          lengthMenu = c(5,10,100,150)
        ))
        
        
      })
    })
  })
  
  output$Plot1 <- renderPlot({
    input$review
    isolate({
      withProgress({
        setProgress(message = "Processing.......")
        yyy <- new_data()
        if(is.null(yyy))
          return(NULL)
        
        ggplot(data = yyy, aes(x = yyy$'Sales', y = yyy$'Profit'),xlab='Sales',ylab='Profit',main='Sales Vs Profit') +
          geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~x) +
          geom_point()
      })
    })
    
    
  })
  
 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$file1, ".xls", sep = "")
    },
    content = function(file) {
      write.table(new_data(), file, row.names = FALSE)
    }
  )
  output$kmeans <- renderDataTable({
    input$analysis
    isolate({
      withProgress({
        setProgress(message="Creating the table......")
     data <- k_means()
     
     data
    })
    })
    
  })
  
  
}
shinyApp(ui, server)
