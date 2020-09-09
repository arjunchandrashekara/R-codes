Sys.getenv("R_ARCH")
Sys.setenv(JAVA_HOME= 'C:/Program Files/Java/jre1.8.0_261')
library(shiny)
library(lubridate)
library(stringr)
library(ggplot2)
library(tm)
library(wordcloud)
library(topicmodels)
library(memoise)
library(shinyWidgets)
library(DT)
library(gt)
options(shiny.maxRequestSize=100*1024^2)




ui <- fluidPage(tags$head(tags$style(
  HTML('
         #sidebar {
            background-color: red;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }'))),# use a gradient in background
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom"
  ),setBackgroundImage(src = "https://educationalresearchtechniques.files.wordpress.com/2017/04/13.jpg?w=550"),h1(id="big-heading", "ExcelR COnversation Mining"),
  tags$style(HTML("#big-heading{color: blue;}")),
  
  sidebarLayout(
    sidebarPanel(id="sidebar",
                 fileInput("file1", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                 tags$hr(),h3("Exploratory Analysis Overall"),actionButton("one","date"),actionButton("two","day"),actionButton("three","month"),
                 actionButton("four","platform"),actionButton("five","region"),actionButton("six","country"),actionButton("seven","browser"),
                 h3("Exploratory Analysis in India"),actionButton("one1","date"),actionButton("two1","day"),actionButton("three1","month"),
                 actionButton("four1","platform"),actionButton("five1","region"),actionButton("six1","country"),actionButton("seven1","browser"),h3("Exploratory Analysis in Abroad"),actionButton("one2","date"),
                 actionButton("two2","day"),actionButton("three2","month"),
                 actionButton("four2","platform"),actionButton("five2","region"),actionButton("six2","country"),actionButton("seven2","browser"),checkboxInput("header", "Header", TRUE),
                 fileInput("wc","upload the text file for wordcloud",multiple = F,accept="text/plain"),fileInput("stp_wrds","stopwords",multiple = F,accept="text/plain"),
                 numericInput("k","select the number of topic",value=3,min=3,max=10,step=1),sliderInput("freq","Minimum Frequency:",min = 1000,  max = 10000, value = 500),
                 sliderInput("max","Maximum Number of Words:",min = 1,  max = 300,  value = 100),
                 actionButton("update","create word cloud"),hr(),h3("Related Videos", br()),
                 
                 a(href = "https://youtu.be/SY7x2Ayj_b0",
                   "Instructions video for Rshiny"),
                 
                 br(),
                 
                 a(href = "https://youtu.be/qvt2UMZIZqY",
                   "About ExcelR"),h6("Powered by:"),tags$img(src="https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcSMCRLZFOPa-JNhB3MbykjsFS9nLIecCtc9hw&usqp=CAU",height=100,width=100)
                 
    ),
    
    mainPanel(h1(id="mainobjective","The Main Objective"),tags$style(HTML("#mainobjective{color: red;}")),h2(id="topicmining","Topic Mining & Exploratory Analysis for improving the"),tags$style(HTML("#topicmining{color: red;}")),h3(id="RA","1) Resource Allocation"),tags$style(HTML("#RA{color: red;}")),h3(id="CM","2) Content Modification"),tags$style(HTML("#CM{color: red;}")),h3(id="SI","3) Service Improvement"),tags$style(HTML("#SI{color: red;}")),
              plotOutput("date"),downloadButton('date_d', 'Download Data'),
              plotOutput("day"),downloadButton('day_d', 'Download Data'),plotOutput("month"),downloadButton('month_d', 'Download Data'),plotOutput("platform"),downloadButton('platform_d', 'Download Data'),
              plotOutput("region"),downloadButton('region_d', 'Download Data'),plotOutput("country"),downloadButton('country_d', 'Download Data'),
              plotOutput("browser"),downloadButton('browser_d', 'Download Data'),
              plotOutput("date1"),downloadButton('date1_d', 'Download Data'),
              plotOutput("day1"),downloadButton('day1_d', 'Download Data'),plotOutput("month1"),downloadButton('month1_d', 'Download Data'),
              plotOutput("platform1"),downloadButton('platform1_d', 'Download Data'),plotOutput("region1"),downloadButton('region1_d', 'Download Data'),
              plotOutput("country1"),downloadButton('country1_d', 'Download Data'),
              plotOutput("browser1"),downloadButton('browser1_d', 'Download Data'),plotOutput("date2"),downloadButton('date2_d', 'Download Data'),
              plotOutput("day2"),downloadButton('day2_d', 'Download Data'),plotOutput("month2"),downloadButton('month2_d', 'Download Data'),plotOutput("platform2"),downloadButton('platform2_d', 'Download Data'),plotOutput("region2"),downloadButton('region2_d', 'Download Data'),
              plotOutput("country2"),downloadButton('country2_d', 'Download Data'),
              plotOutput("browser2"),downloadButton('browser2_d', 'Download Data'),plotOutput("unigram"),downloadButton('unigram_d', 'Download Data'),dataTableOutput("LDA"),downloadButton('LDA_d', 'Download Data'),
    )
  )
)


server <- function(input, output) {
  
  
  new_data<- reactive({
    
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    df <-  read.csv(inFile$datapath, header = input$header)
    new<-as.data.frame(str_split_fixed(df[,2], "T", 2))
    Date <- new$V1
    Day <-weekdays(as.Date(new$V1,'%Y-%m-%d'))
    Month <- month(as.Date(new$V1),label = TRUE,abbr = F)
    
    cbind(df,Date,Day,Month)
    
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    
    
    
  })
  
 
  

  output$date <- renderPlot({
    input$one
    isolate({
      withProgress({
        setProgress(message = "Processing.......")
        yyy <- new_data()
        if (is.null(yyy))
          return(NULL)
        
        one <- as.data.frame(table(yyy$Date))
        subset_one <- subset(one,one$Freq>1750)
        ggplot(subset_one, 
               aes(x = Freq, 
                   y = Var1,fill = Freq)) +
          geom_bar(stat = "identity")+scale_fill_continuous(type = "viridis")+geom_text(aes(label = Freq),vjust=1)
      })
    })
    
  }) 
  

  
  output$date_d<- downloadHandler(
    filename =  function() {
      paste("date_overall","png", sep=".")
    },
    content = function(file) {
      yyy <- new_data()
      one <- as.data.frame(table(yyy$Date))
      subset_one <- subset(one,one$Freq>1750)
      png(file)
      print(ggplot(subset_one, 
             aes(x = Freq, 
                 y = Var1,fill = Freq)) +
        geom_bar(stat = "identity")+scale_fill_continuous(type = "viridis")+geom_text(aes(label = Freq),vjust=1))
      dev.off()
      
    }
  )
 
  
  output$day <- renderPlot({
    input$two
    isolate({
      withProgress({
        setProgress(message = "Processing........")
        yyy<- new_data()
        if (is.null(yyy))
          return(NULL)
        
        ggplot(yyy, aes(y = factor(Day))) +
          geom_bar(fill = c("darkred"))+geom_text(stat='count', aes(label=..count..))
      })
    })
    
    })
  output$day_d<- downloadHandler(
    filename =  function() {
      paste("day_overall","png", sep=".")
    },
    content = function(file) {
      yyy <- new_data()
      png(file)
      print(ggplot(yyy, aes(y = factor(Day))) +
              geom_bar(fill = c("darkred"))+geom_text(stat='count', aes(label=..count..)))
      dev.off()
      
    }
  )
  output$month <- renderPlot({
    input$three
    isolate({
      withProgress({
        setProgress(message = "Processing.......")
        yyy<- new_data()
        if (is.null(yyy))
          return(NULL)
        
        ggplot(yyy, aes(x = factor(Month))) +
          geom_bar(fill="gold")+geom_text(stat='count', aes(label=..count..))
      })
    })
    
    
  })
  output$month_d<- downloadHandler(
    filename =  function() {
      paste("month_overall","png", sep=".")
    },
    content = function(file) {
      yyy <- new_data()
      png(file)
      print(ggplot(yyy, aes(x = factor(Month))) +
              geom_bar(fill="gold")+geom_text(stat='count', aes(label=..count..)))
      dev.off()
      
    }
  )
  output$platform <- renderPlot({
    input$four
    isolate({
      withProgress({
        setProgress(message = "Processing.........")
        yyy<- new_data()
        if (is.null(yyy))
          return(NULL)
        
        ggplot(yyy, aes(y = factor(Platform))) +
          geom_bar(fill="khaki4")+geom_text(stat='count', aes(label=..count..))
      })
    })
    
    
  })
  output$platform_d<- downloadHandler(
    filename =  function() {
      paste("platform_overall","png", sep=".")
    },
    content = function(file) {
      yyy <- new_data()
      png(file)
      print(ggplot(yyy, aes(y = factor(Platform))) +
              geom_bar(fill="khaki4")+geom_text(stat='count', aes(label=..count..)))
      dev.off()
      
    }
  )
  
   output$region <- renderPlot({
    input$five
    isolate({
      withProgress({
        setProgress(message = "Processing........")
        yyy<- new_data()
        if (is.null(yyy))
          return(NULL)
        
        two <- as.data.frame(table(yyy$Region))
        subset_two <- subset(two,two$Freq>500)
        ggplot(subset_two, 
               aes(x = Freq, 
                   y = Var1,fill=Freq)) +
          geom_bar(stat = "identity")+scale_fill_continuous(type = "gradient")+geom_text(aes(label = Freq))
      })
    })
    
    
  })
   
   output$region_d<- downloadHandler(
     filename =  function() {
       paste("region_overall","png", sep=".")
     },
     content = function(file) {
       yyy <- new_data()
       two <- as.data.frame(table(yyy$Region))
       subset_two <- subset(two,two$Freq>500)
       png(file)
       
       print(ggplot(subset_two, 
              aes(x = Freq, 
                  y = Var1,fill=Freq)) +
         geom_bar(stat = "identity")+scale_fill_continuous(type = "gradient")+geom_text(aes(label = Freq)))
       dev.off()
     }
   )
  output$country <- renderPlot({
    input$six
    isolate({
      withProgress({
        setProgress(message = "Processing.......")
        yyy<- new_data()
        input$six
        if (is.null(yyy))
          return(NULL)
        
        three <- as.data.frame(table(yyy$country.name))
        subset_three <- subset(three,three$Freq>100)
        ggplot(subset_three, 
               aes(x = Freq, 
                   y = Var1,fill=Freq)) +
          geom_bar(stat = "identity")+scale_fill_continuous(type = "gradient")+geom_text(aes(label = Freq))
      })
    })
    
    
  })
  output$country_d<- downloadHandler(
    filename =  function() {
      paste("country_overall","png", sep=".")
    },
    content = function(file) {
      yyy <- new_data()
      three <- as.data.frame(table(yyy$country.name))
      subset_three <- subset(three,three$Freq>100)
      png(file)
      
      print(ggplot(subset_three, 
                   aes(x = Freq, 
                       y = Var1,fill=Freq)) +
              geom_bar(stat = "identity")+scale_fill_continuous(type = "gradient")+geom_text(aes(label = Freq)))
      dev.off()
    }
  )
  output$browser <- renderPlot({
    input$seven
    isolate({
      withProgress({
        setProgress(message = "Processing......")
        yyy<- new_data()
        if (is.null(yyy))
          return(NULL)
        ggplot(yyy, aes(y = factor(Browser))) +
          geom_bar(fill="black")+geom_text(stat='count', aes(label=..count..))
      })
    })
  })
  
  output$browser_d<- downloadHandler(
    filename =  function() {
      paste("browser_overall","png", sep=".")
    },
    content = function(file) {
      yyy <- new_data()
      png(file)
      print(ggplot(yyy, aes(y = factor(Browser))) +
              geom_bar(fill="black")+geom_text(stat='count', aes(label=..count..)))
      dev.off()
      
    }
  )
  
  
  
  india_data<- reactive({
    
  
    inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      df <-  read.csv(inFile$datapath, header = input$header)
      new<-as.data.frame(str_split_fixed(df[,2], "T", 2))
      Date <- new$V1
      Day <-weekdays(as.Date(new$V1,'%Y-%m-%d'))
      Month <- month(as.Date(new$V1),label = TRUE,abbr = F)
      
      conv_data<- cbind(df,Date,Day,Month)
      x <- subset(conv_data,conv_data$country.name==" India")
      
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      
      
      
  }) 
  output$date1 <- renderPlot({
    input$one1
    isolate({
      withProgress({
        setProgress(message = "Processing.......")
        yyy <- india_data()
        if (is.null(yyy))
          return(NULL)
        
        one <- as.data.frame(table(yyy$Date))
        subset_one <- subset(one,one$Freq>1500)
        ggplot(subset_one, 
               aes(x = Freq, 
                   y = Var1,fill = Freq)) +
          geom_bar(stat = "identity")+scale_fill_continuous(type = "viridis")+geom_text(aes(label = Freq),vjust=1)
      })
    })
    
  }) 
  output$date1_d <- downloadHandler(
    filename =  function() {
      paste("date_india","png", sep=".")
    },
    content = function(file) {
      yyy <- india_data()
      one <- as.data.frame(table(yyy$Date))
      subset_one <- subset(one,one$Freq>1500)
      png(file)
      print(ggplot(subset_one, 
                   aes(x = Freq, 
                       y = Var1,fill = Freq)) +
              geom_bar(stat = "identity")+scale_fill_continuous(type = "viridis")+geom_text(aes(label = Freq),vjust=1))
      dev.off()
      
    }
  )
  
  output$day1 <- renderPlot({
    input$two1
    isolate({
      withProgress({
        setProgress(message = "Processing........")
        yyy<- india_data()
        if (is.null(yyy))
          return(NULL)
        
        ggplot(yyy, aes(y = factor(Day))) +
          geom_bar(fill = c("darkred"))+geom_text(stat='count', aes(label=..count..))
      })
    })
    
    
    
  })
  output$day1_d <- downloadHandler(
    filename =  function() {
      paste("day_india","png", sep=".")
    },
    content = function(file) {
      yyy <- india_data()
      png(file)
      print(ggplot(yyy, aes(y = factor(Day))) +
                     geom_bar(fill = c("darkred"))+geom_text(stat='count', aes(label=..count..)))
      dev.off()
      
    }
  )
  output$month1 <- renderPlot({
    input$three1
    isolate({
      withProgress({
        setProgress(message = "Processing.......")
        yyy<- india_data()
        if (is.null(yyy))
          return(NULL)
        
        ggplot(yyy, aes(x = factor(Month))) +
          geom_bar(fill="gold")+geom_text(stat='count', aes(label=..count..))
      })
    })
    
    
  })
  output$month1_d <- downloadHandler(
    filename =  function() {
      paste("month_india","png", sep=".")
    },
    content = function(file) {
      yyy <- india_data()
      png(file)
      print(ggplot(yyy, aes(x = factor(Month))) +
              geom_bar(fill="gold")+geom_text(stat='count', aes(label=..count..)))
      dev.off()
      
    }
  )
  output$platform1 <- renderPlot({
    input$four1
    isolate({
      withProgress({
        setProgress(message = "Processing.........")
        yyy<- india_data()
        if (is.null(yyy))
          return(NULL)
        
        ggplot(yyy, aes(y = factor(Platform))) +
          geom_bar(fill="khaki4")+geom_text(stat='count', aes(label=..count..))
      })
    })
    
    
  })
  output$platform1_d <- downloadHandler(
    filename =  function() {
      paste("platform_india","png", sep=".")
    },
    content = function(file) {
      yyy <- india_data()
      png(file)
      print(ggplot(yyy, aes(y = factor(Platform))) +
              geom_bar(fill="khaki4")+geom_text(stat='count', aes(label=..count..)))
      dev.off()
      
    }
  )
  
  output$region1 <- renderPlot({
    input$five1
    isolate({
      withProgress({
        setProgress(message = "Processing........")
        yyy<- india_data()
        if (is.null(yyy))
          return(NULL)
        
        two <- as.data.frame(table(yyy$Region))
        subset_two <- subset(two,two$Freq>200)
        ggplot(subset_two, 
               aes(x = Freq, 
                   y = Var1,fill=Freq)) +
          geom_bar(stat = "identity")+scale_fill_continuous(type = "gradient")+geom_text(aes(label = Freq))
      })
    })
    
    
  })
  output$region1_d <- downloadHandler(
    filename =  function() {
      paste("region_india","png", sep=".")
    },
    content = function(file) {
      yyy <- india_data()
      two <- as.data.frame(table(yyy$Region))
      subset_two <- subset(two,two$Freq>200)
      png(file)
      print(ggplot(subset_two, 
                   aes(x = Freq, 
                       y = Var1,fill=Freq)) +
              geom_bar(stat = "identity")+scale_fill_continuous(type = "gradient")+geom_text(aes(label = Freq)))
      dev.off()
      
    }
  )
  output$country1 <- renderPlot({
    input$six1
    isolate({
      withProgress({
        setProgress(message = "Processing.......")
        yyy<- india_data()
        input$six
        if (is.null(yyy))
          return(NULL)
        
        three <- as.data.frame(table(yyy$country.name))
        subset_three <- subset(three,three$Freq>100)
        ggplot(subset_three, 
               aes(x = Freq, 
                   y = Var1,fill=Freq)) +
          geom_bar(stat = "identity")+scale_fill_continuous(type = "gradient")+geom_text(aes(label = Freq))
      })
    })
    
    
  })
  output$country1_d <- downloadHandler(
    filename =  function() {
      paste("country_india","png", sep=".")
    },
    content = function(file) {
      yyy <- india_data()
      three <- as.data.frame(table(yyy$country.name))
      subset_three <- subset(three,three$Freq>100)
      png(file)
      print(ggplot(subset_three, 
            aes(x = Freq, 
                y = Var1,fill=Freq)) +
        geom_bar(stat = "identity")+scale_fill_continuous(type = "gradient")+geom_text(aes(label = Freq)))
      dev.off()
      
    })
  output$browser1 <- renderPlot({
    input$seven1
    isolate({
      withProgress({
        setProgress(message = "Processing......")
        yyy<- india_data()
        if (is.null(yyy))
          return(NULL)
        ggplot(yyy, aes(y = factor(Browser))) +
          geom_bar(fill="black")+geom_text(stat='count', aes(label=..count..))
      })
    })
  })
  output$browser1_d <- downloadHandler(
    filename =  function() {
      paste("browser_india","png", sep=".")
    },
    content = function(file) {
      yyy <- india_data()
      
      png(file)
      print(ggplot(yyy, aes(y = factor(Browser))) +
              geom_bar(fill="black")+geom_text(stat='count', aes(label=..count..)))
  dev.off()
  
    }
  )
  abroad_data<- reactive({
    
    
    inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      df <-  read.csv(inFile$datapath, header = input$header)
      new<-as.data.frame(str_split_fixed(df[,2], "T", 2))
      Date <- new$V1
      Day <-weekdays(as.Date(new$V1,'%Y-%m-%d'))
      Month <- month(as.Date(new$V1),label = TRUE,abbr = F)
      
      conv_data<- cbind(df,Date,Day,Month)
      y <- subset(conv_data,conv_data$country.name!=" India")
      
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      
      
      
  }) 
  output$date2 <- renderPlot({
    input$one2
    isolate({
      withProgress({
        setProgress(message = "Processing.......")
        yyy <- abroad_data()
        if (is.null(yyy))
          return(NULL)
        
        one <- as.data.frame(table(yyy$Date))
        subset_one <- subset(one,one$Freq>300)
        ggplot(subset_one, 
               aes(x = Freq, 
                   y = Var1,fill = Freq)) +
          geom_bar(stat = "identity")+scale_fill_continuous(type = "viridis")+geom_text(aes(label = Freq),vjust=1)
      })
    })
    
  }) 
  
  output$date2_d <- downloadHandler(
    filename =  function() {
      paste("date_abroad","png", sep=".")
    },
    content = function(file) {
      yyy <- abroad_data()
      one <- as.data.frame(table(yyy$Date))
      subset_one <- subset(one,one$Freq>300)
      png(file)
      print(ggplot(subset_one, 
                   aes(x = Freq, 
                       y = Var1,fill = Freq)) +
              geom_bar(stat = "identity")+scale_fill_continuous(type = "viridis")+geom_text(aes(label = Freq),vjust=1))
      dev.off()
      
    })
  
  output$day2 <- renderPlot({
    input$two2
    isolate({
      withProgress({
        setProgress(message = "Processing........")
        yyy<- abroad_data()
        if (is.null(yyy))
          return(NULL)
        
        ggplot(yyy, aes(y = factor(Day))) +
          geom_bar(fill = c("darkred"))+geom_text(stat='count', aes(label=..count..))
      })
    })
    
    
    
  })
  output$day2_d <- downloadHandler(
    filename =  function() {
      paste("day_abroad","png", sep=".")
    },
    content = function(file) {
      yyy <- abroad_data()
      
      png(file)
      print(ggplot(yyy, aes(y = factor(Day))) +
              geom_bar(fill = c("darkred"))+geom_text(stat='count', aes(label=..count..)))
      dev.off()
      
    })
  
  output$month2 <- renderPlot({
    input$three2
    isolate({
      withProgress({
        setProgress(message = "Processing.......")
        yyy<- abroad_data()
        if (is.null(yyy))
          return(NULL)
        
        ggplot(yyy, aes(x = factor(Month))) +
          geom_bar(fill="gold")+geom_text(stat='count', aes(label=..count..))
      })
    })
    
    
  })
  output$month2_d <- downloadHandler(
    filename =  function() {
      paste("month_abroad","png", sep=".")
    },
    content = function(file) {
      yyy <- abroad_data()
      
      png(file)
      print(ggplot(yyy, aes(x = factor(Month))) +
              geom_bar(fill="gold")+geom_text(stat='count', aes(label=..count..)))
      dev.off()
      
    })
  output$platform2 <- renderPlot({
    input$four2
    isolate({
      withProgress({
        setProgress(message = "Processing.........")
        yyy<- abroad_data()
        if (is.null(yyy))
          return(NULL)
        
        ggplot(yyy, aes(y = factor(Platform))) +
          geom_bar(fill="khaki4")+geom_text(stat='count', aes(label=..count..))
      })
    })
    
    
  })
  
  output$platform2_d <- downloadHandler(
    filename =  function() {
      paste("platform_abroad","png", sep=".")
    },
    content = function(file) {
      yyy <- abroad_data()
      
      png(file)
      print(ggplot(yyy, aes(y = factor(Platform))) +
              geom_bar(fill="khaki4")+geom_text(stat='count', aes(label=..count..)))
      dev.off()
      
    })
  output$region2 <- renderPlot({
    input$five2
    isolate({
      withProgress({
        setProgress(message = "Processing........")
        yyy<- abroad_data()
        if (is.null(yyy))
          return(NULL)
        
        two <- as.data.frame(table(yyy$Region))
        subset_two <- subset(two,two$Freq>150)
        ggplot(subset_two, 
               aes(x = Freq, 
                   y = Var1,fill=Freq)) +
          geom_bar(stat = "identity")+scale_fill_continuous(type = "gradient")+geom_text(aes(label = Freq))
      })
    })
    
    
  })
  
  output$region2_d <- downloadHandler(
    filename =  function() {
      paste("region_abroad","png", sep=".")
    },
    content = function(file) {
      yyy <- abroad_data()
      two <- as.data.frame(table(yyy$Region))
      subset_two <- subset(two,two$Freq>150)
      
      
      png(file)
      print(ggplot(subset_two, 
                   aes(x = Freq, 
                       y = Var1,fill=Freq)) +
              geom_bar(stat = "identity")+scale_fill_continuous(type = "gradient")+geom_text(aes(label = Freq)))
      dev.off()
      
    })
  output$country2 <- renderPlot({
    input$six2
    isolate({
      withProgress({
        setProgress(message = "Processing.......")
        yyy<- abroad_data()
  
        if (is.null(yyy))
          return(NULL)
        
        three <- as.data.frame(table(yyy$country.name))
        subset_three <- subset(three,three$Freq>200)
        ggplot(subset_three, 
               aes(x = Freq, 
                   y = Var1,fill=Freq)) +
          geom_bar(stat = "identity")+scale_fill_continuous(type = "gradient")+geom_text(aes(label = Freq))
      })
    })
    
    
  })
  
  output$country2_d <- downloadHandler(
    filename =  function() {
      paste("country_abroad","png", sep=".")
    },
    content = function(file) {
      yyy <- abroad_data()
      three <- as.data.frame(table(yyy$country.name))
      subset_three <- subset(three,three$Freq>200)
      
      
      png(file)
      print(ggplot(subset_three, 
                   aes(x = Freq, 
                       y = Var1,fill=Freq)) +
              geom_bar(stat = "identity")+scale_fill_continuous(type = "gradient")+geom_text(aes(label = Freq)))
      dev.off()
      
    })
  output$browser2 <- renderPlot({
    input$seven2
    isolate({
      withProgress({
        setProgress(message = "Processing......")
        yyy<- abroad_data()
        if (is.null(yyy))
          return(NULL)
        ggplot(yyy, aes(y = factor(Browser))) +
          geom_bar(fill="black")+geom_text(stat='count', aes(label=..count..))
      })
    })
  })
  
  output$browser2_d <- downloadHandler(
    filename =  function() {
      paste("browser_abroad","png", sep=".")
    },
    content = function(file) {
      
      yyy <- abroad_data()
      
      png(file)
      print(ggplot(yyy, aes(y = factor(Browser))) +
              geom_bar(fill="black")+geom_text(stat='count', aes(label=..count..)))
      dev.off()
      
    })
  
  wc_data <- reactive(
    {
      input$update
      
      isolate(
        {
          withProgress(
            {
              setProgress(message = "Prodessing Corpus......")
              wc_file <- input$wc
              if(!is.null(wc_file)){
                wc_text <- readLines(wc_file$datapath)
              }
              else
              {
                wc_text <- "A word cloud is an image made of words that together resemble a cloudy shape.The size of a word shows how important it is e.g. how often it appears in a text - its frequency.People typically use word clouds to easily produce a summary of large documents (reports, speeches), to create art on a topic (gifts, displays) or to visualise data (tables, surveys)."
              }
              stops <- input$stp_wrds
              if(!is.null(stops)){
                wc_stp <- readLines(stops$datapath)
              }
              else
              {
                wc_stp <- ""
              }
              filesss <- wc_text
              stops <- wc_stp
              mydata.corpus <- Corpus(VectorSource(filesss))
              mydata.corpus <- tm_map(mydata.corpus,tolower)
              mydata.corpus <- tm_map(mydata.corpus,removePunctuation)
              mydata.corpus <- tm_map(mydata.corpus,removeNumbers)
              mydata.corpus <- tm_map(mydata.corpus,removeWords,stops)
              mydata.corpus <- tm_map(mydata.corpus,stripWhitespace)
              mydata.corpus <- tm_map(mydata.corpus,removeWords,stopwords("english"))
              mydata.corpus <- tm_map(mydata.corpus,removeWords,c("mounica","patel","visitor","ananya"))
              
              dtm <- DocumentTermMatrix(mydata.corpus)
              dtm.new <- removeSparseTerms(dtm,0.99)
              row_totals <- apply(dtm.new,1,sum)
              dtm.new <- dtm.new[row_totals>0,]
              dtm.new
              
              
              
              
            })
        })
    })
  
  
  
  
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$unigram <- renderPlot({
    
    withProgress({
      setProgress(message="creating the cloud......")
      wc_corpus <- wc_data()
      freq1 <- colSums(as.matrix(wc_corpus))
      pal <- brewer.pal(8,"Dark2")
      wordcloud(names(freq1), freq1, min.freq=input$freq,max.words=input$max,colors = pal)
      
      
    })
  })
  output$unigram_d <- downloadHandler(
    filename = function(){
      paste("unigram","png", sep=".")
    },
    content = function(file){
      wc_corpus <- wc_data()
      freq1 <- colSums(as.matrix(wc_corpus))
      pal <- brewer.pal(8,"Dark2")
      png(file)
      print(wordcloud(names(freq1),freq1,min.freq=input$freq,max.words=input$max,colors = pal))
      dev.off()
    }
  )
  
  output$LDA <- renderDataTable({
    withProgress({
      setProgress(message="Creating the topics......")
      wc_table<- wc_data()
      k <- input$k
      terms(LDA(wc_table,k,control=list(seed=1234)),10) 
      
      
      
    })
  })
  
  output$LDA_d <- downloadHandler(
    filename = function(){
      paste("topics","csv",sep = ".")
    },
    content = function(file){
      wc_table<- wc_data()
      k <- input$k
      topic_file <- terms(LDA(wc_table,k,control=list(seed=1234)),10)
      write.csv(file,topic_file.csv)
      
    }
  )
  
}



shinyApp(ui, server)

