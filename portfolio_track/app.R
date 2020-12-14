library(shiny)
library(tidyverse)
library(tidyquant)
library(shinythemes)
library(shinycssloaders)
library(ggthemes)

#Read dataset
full_stock <- read.csv("full_stock_cleaned.csv")
full_exchange <- read.csv("full_exchange.csv")

fpx <- read.csv("fpx_stock.csv")
ipo <- read.csv("ipo_stock.csv")
csd <- read.csv("csd_stock.csv")
spak <- read.csv("spak_stock.csv")

FPX <- read.csv("fpx_return.csv")%>%select(-2)
IPO <- read.csv("ipo_return.csv")%>%select(-2)
CSD <- read.csv("csd_return.csv")%>%select(-2)
SPAK <- read.csv("spak_return.csv")%>%select(-2)

SP500 <- read.csv("sp500_return.csv")%>%select(-2)
NASDAQ <- read.csv("nasdaq_return.csv")%>%select(-2)
DJI <- read.csv("dji_return.csv")%>%select(-2)

# Define UI
ui <- fluidPage(
  
  #Navbar structure for UI
  navbarPage("Portfolio Tracking", theme = shinytheme("yeti"),
             tabPanel("Portfolio List", fluid = TRUE, icon = icon("landmark"),
                      
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel("IPO ETF"),
                          
                          fluidRow(column(6,
                                          
                                          #Select which ETF
                                          selectInput(inputId = "ETFName",
                                                      label = "Fund Name",
                                                      choices = c("FPX","IPO","CSD","SPAK"),
                                                      selected = "FPX",
                                                      width = '400px'
                                          )
                                          
                          )
                          
                          )
                          
                        ),#End of sidebarPanel
                        mainPanel(
                          
                          tabsetPanel(
                            tabPanel(
                              "ETF Stock",
                          
                          titlePanel(textOutput("text1")), 
                          tags$style("#text1{
                                 font-size: 40px
                                 }"
                          ),
                          
                          titlePanel(textOutput("text2")), 
                          tags$style("#text2{
                                 font-size: 20px
                                 }"
                          ),
                          
                          br(),
                          
                          fluidRow(
                            withSpinner(DT::dataTableOutput(outputId = "ETFTable"))
                                   )
                              )
                          )
                        )
                        
                      )
                      
             ),#First tabPanel End
             
             
             
             tabPanel("Build Portfolio", fluid = TRUE, icon = icon("hand-holding-usd"),
                      
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel("US Stocks"),
                          

                                  #Select which year
                                  selectizeInput(inputId = "stockName",
                                                 label = "Company Name",
                                                 choices = sort(unique(full_stock$company)),
                                                 width = '400px',
                                                 multiple = TRUE
                                                ),
                                  
                                  #Text
                                  "Select number of shares based on 2020-07-01 stock price",   
                                  
                                  hr(),
                          
                                  #Output Total
                                  textOutput("Total"),
                                  tags$style(type="text/css", "#Total {white-space: pre-wrap;}"),
                                  
                                  hr(),
                          
                                  #Select weight
                                  uiOutput("weight"),
                                  
                                  #Assign name of the portfolio
                                  uiOutput("AssignName")
                          
                          
                        ),#End of sidebarPanel
                        mainPanel(
                          
                          tabsetPanel(
                            tabPanel(
                              "All Stock",
                              titlePanel(textOutput("text3")), 
                              tags$style("#text3{
                                 font-size: 40px
                                 }"
                              ),
                              
                              br(),
                              
                              fluidRow(
                                withSpinner(DT::dataTableOutput(outputId = "StockTable"))
                              )
                            ),#End of first tabPanel
                            
                            tabPanel(
                              "Selected Stock",
                              titlePanel(textOutput("text4")), 
                              tags$style("#text4{
                                 font-size: 40px
                                 }"
                              ),
                              
                              fluidRow(
                              column(width = 2, offset = 9, conditionalPanel(
                                condition = "input.stockName.length >= 1",
                                downloadButton("SavePortfolio","Download Portfolio",icon("download"))
                              )
                              )
                              ),
                              
                              br(),
                              
                              fluidRow(
                                withSpinner(DT::dataTableOutput(outputId = "SelectedStockTable"))
                              )
                            ),#End of second tabPanel
                            
                            tabPanel(
                              "Portfolio Performance",
                              titlePanel(textOutput("text5")), 
                              tags$style("#text5{
                                 font-size: 40px
                                 }"
                              ),
                              
                              fluidRow(
                                column(9, conditionalPanel(
                                  condition = "input.stockName.length >= 1",
                                  checkboxGroupInput("portfolioDisplay",
                                               "Display",
                                               choices = c("Selected","FPX","IPO","CSD",
                                                           "SPAK","NASDAQ","SP500","DJI"
                                                           ),
                                               selected = "Selected",
                                               inline = TRUE)
                                )
                                ),
                                
                                column(3,
                                       
                                       actionButton("selectall","Check All",icon("check-square")),
                                       
                                       actionButton("unselectall","Uncheck All",icon("trash-alt"))
                                  
                                )
                              ),
                              
                              br(),
                                
                              conditionalPanel(
                                condition = "input.stockName.length >= 1",
                                withSpinner(plotOutput("Curve"))
                              ),
                              
                              br(),
                              
                              conditionalPanel(
                                condition = "input.stockName.length >= 1",
                                withSpinner(DT::dataTableOutput(outputId = "ReturnTable"))
                              )
                              
                              
                            )#End of third tabPanel
                          )
                          
                        )
                        
                      )
                      
             )#Second tabPenal end
             
             
  )
  
)

# Define server logic
server <- function(input, output, session) {
  
  #Create Text1
  output$text1 <- renderText({

      paste("Top 10 Holdings from ",input$ETFName,sep = "")

  })
  
  #Create ETFTable
  output$ETFTable <- DT::renderDataTable(
    
    DT::datatable({
      
      #Assign value to `data` based on input
      if(input$ETFName=="FPX"){
        data <- fpx[,c(2,4)]
      }
      else if(input$ETFName=="IPO"){
        data <- ipo[,c(2,4)]
      }
      else if(input$ETFName=="CSD"){
        data <- csd[,c(2,4)]
      }
      else{
        data <- spak[,c(2,4)]
      }

      data
      
    })
    
  )
  
  
  #Deal with weight
  output$weight <- renderUI({
    
    #Build slider based on input
    n <- length(input$stockName)
    
    
    if(n>=1){
      #create vector
      numericname1 <- rep("",n)
      numericname2 <- rep("",n)
      
      #Store price
      tmp <- full_stock %>% 
                filter(company %in% input$stockName,
                       date == "2020-07-01")
      
    }
    
    #build name
    if(n>=1){
      for(i in 1:n){
        numericname1[i] <- paste("share_value",i,sep = "")
        numericname2[i] <- paste("Share for ",input$stockName[i],sep = "")
      }
    }

    if(n>=1){
      
        lapply(seq(1,n,1),function(i){
          
          price <- tmp %>%
            filter(company == input$stockName[i]) %>%
            select(9)
          
          #Use div to return multiple fludiRow
          div(
          fluidRow(
                  column(8,
                  #Note `isolate` is extremely important. 
                  #It prevents numericInput(created by UI) from refreshing
                  numericInput(numericname1[i],numericname2[i],value = isolate(input[[numericname1[i]]]))
                  )
          ),
          fluidRow(
                  column(5,
                    paste("Price: $",round(price,2),sep = "")
                  ),
                  column(7,
                    paste("Total: $",round(input[[numericname1[i]]]*price,0),sep = "")
                  )
          ),
          hr()
          )

        })
      
    }
    
  })
  
  #Portfolio Total
  
  output$Total <- renderText({
    
    n <- length(input$stockName)
    price <- rep(0,n)
    numericname1 <- rep("",n)
    
    #Store price
    tmp <- full_stock %>% 
      filter(company %in% input$stockName,
             date == "2020-07-01")
    
    #Get name
    if(n>=1){
      for(i in 1:n){
        numericname1[i] <- paste("share_value",i,sep = "")
        price[i] <- tmp %>%
          filter(company == input$stockName[i]) %>%
          select(9)
      }
    }
    
    #Calculate
    total <- 0
    if(n>=1){
    for(i in 1:n){
      if(!is.na(input[[numericname1[i]]])){
      total <- total + input[[numericname1[i]]]*as.numeric(price[i])
      }
    }
      total <- round(total,0)
    }
    
    if(n >= 1){
      if(total <= 250000){
        paste("Initial position: $250000","\n","Portfolio total: $",total,sep = "")
      }
      else{
        paste("Initial position: $250000","\n","Portfolio total: $",
              total,"\n","Out of Budget!!!",sep = "")
      }
    }
    
    
  })
  
  #AssignName
  output$AssignName <- renderUI({
    
    n <- length(input$stockName)
    
    if(n>=1){
      textInput("assignname","Portfolio Name")
    }
    
  })
  
  #Create Text2
  output$text2 <- renderText({
    
    #Assign value to `text` based on input
    if(input$ETFName=="FPX"){
      text <- paste("First Trust U.S. Equity Opportunities ETF")
    }
    else if(input$ETFName=="IPO"){
      text <- paste("Renaissance IPO ETF")
    }
    else if(input$ETFName=="CSD"){
      text <- paste("Invesco S&P Spin-Off ETF")
    }
    else{
      text <- paste("Defiance Next Gen SPAC Derived ETF")
    }
    
    text
    
  })
  
  
  #Create Text3
  output$text3 <- renderText({
    
    paste("US Stocks Information")
    
  })
  
  #Create StockTable
  output$StockTable <- DT::renderDataTable(
    
    DT::datatable({
      
      #Assign value to `data
      
      tmp <- full_exchange %>%
                filter(symbol %in% unique(full_stock$symbol))
      
      data <- tmp[,-c(1,2,4)]
      
      data
      
    })
    
  )
  
  #Create Text4
  output$text4 <- renderText({
    
    paste("Selected Stocks Information")
    
  })
  
  
  #Click download
  selectedData <- reactive({
    #Calculate weight
    n <- length(input$stockName)
    price <- rep(0,n)
    numericname1 <- rep("",n)
    price2 <- rep(0,n)
    
    #Store price
    if(n>=1){
      tmp <- full_stock %>% 
        filter(company %in% input$stockName,
               date == "2020-07-01")
    }
    
    #Get name
    if(n>=1){
      for(i in 1:n){
        numericname1[i] <- paste("share_value",i,sep = "")
        price[i] <- tmp %>%
          filter(company == input$stockName[i]) %>%
          select(9)
      }
    }
    
    #Calculate
    total <- 0
    if(n>=1){
      for(i in 1:n){
        if(!is.na(input[[numericname1[i]]])){
          price2[i] <- input[[numericname1[i]]]*as.numeric(price[i])
          total <- total + price2[i]
        }
      }
    }
    
    #Calculate weight
    if(n>=1){
      weight <- data.frame(company = input$stockName, weighting = paste(round(100*price2/total,2),"%",sep = ""))
    }
    
    
    #Assign value to `data` 
    if(n>=1){
      data <- full_exchange[,-c(1,2,4)]
      data <- data %>%
        filter(company %in% input$stockName)
      data <- left_join(data,weight,by = "company")
    }
     data
  })
  
  output$SavePortfolio <- downloadHandler(
    
    filename = function(){
      paste(input$assignname,".csv",sep = "")
    },
    content = function(file){
      write.csv(selectedData(),file,row.names = FALSE)
    }
    
  )

  
  #Create SelectedStockTable
  output$SelectedStockTable <- DT::renderDataTable(
    
    DT::datatable({
      
      n <- length(input$stockName)
      
      if(n>=1){
        selectedData()
      }
      
    })
    
  )
  
  #Build a reactive stored data
  TotalData <- reactive({
    
    #Get selected list
    portfolio_list <- input$portfolioDisplay
    
    n <- length(portfolio_list)
    
    #build all data into one
    data <- rbind(FPX,IPO,CSD,SPAK,SP500,NASDAQ,DJI)
    
    #Get full
    stock <- full_stock %>%
      select(c(2,3,9,10)) %>%
      mutate(date = as.Date(date)) %>%
      fill(adjusted) 
    
    #Form data based on selected
    if(n>=1){
      if(portfolio_list[1]=="Selected"){
        
        #Call data
        tmp <- selectedData()
        
        #Portfolio
        ticker <- tmp$company
        
        #weighting
        wts <- as.numeric(str_sub(tmp$weight,1,length(tmp$weight)-1))/100
        
        #Filter based on portfolio and calculate daily return
        tmp <- stock %>%
          filter(company %in% ticker) %>%
          group_by(company) %>%
          tq_transmute(select  = adjusted, 
                       mutate_fun = periodReturn, 
                       period = "daily",
                       col_rename = "Return")
        
        #Form portfolio table
        wts_tbl <- tibble(company = ticker,
                          wts = wts)
        #join
        ret_data <- left_join(tmp,wts_tbl, by = "company")
        
        #Get weighting daily return
        ret_data <- ret_data %>%
          mutate(wt_return = wts * Return)
        
        #Get sum daily return
        port_ret <- ret_data %>%
          group_by(date) %>%
          summarise(port_ret = sum(wt_return))
        
        #Get cumulative return
        port_cumulative_ret <- port_ret %>%
          mutate(cr = cumprod(1 + port_ret),
                 type = "Selected",
                 date = as.character(date)) %>%
          select(-2)
        
        data <- rbind(data,port_cumulative_ret)
        
      }
      
    }
    
    data_sub <- data %>%
      filter(type %in% portfolio_list) %>%
      mutate(cr = (cr-1)*100)
    
    data_sub
    
  })
  
  
  TotalDataPlot <- reactive({
    
    #Get selected list
    portfolio_list <- input$portfolioDisplay
    
    n <- length(portfolio_list)
    
    data_sub <- TotalData()
    
    #Build plot
    if(n==1){
      plot <- ggplot(data = data_sub,aes(x = date,y = cr,group=1,color=type)) +
        geom_line() +
        ylab("Cumulative Returns %")+
        xlab("Date From July 1 to November 30")+
        theme(axis.text.x=element_blank(),
              axis.ticks.x = element_blank(),
              panel.grid.minor = element_blank())
    }
    
    if(n>=1){
      plot <- ggplot(data = data_sub,aes(x = date,y = cr,group=type,color=type)) +
        geom_line() +
        ylab("Cumulative Returns %")+
        xlab("Date From July 1 to November 30")+
        theme(axis.text.x=element_blank(),
              axis.ticks.x = element_blank(),
              panel.grid.minor = element_blank())
    }
    
    plot
    
  })
  
  #Select which
  ##Use value to control
  value1 <- reactiveValues(count = 0)
  
  value2 <- reactiveValues(count = 0)
  
  ##Update after click
  observe({
    
    if(input$selectall == 0) {return(NULL)}
    else if (input$selectall > value1$count)
    {
      variables<-list("Selected"="Selected","FPX"="FPX","IPO"="IPO","CSD"="CSD",
                      "SPAK"="SPAK","NASDAQ"="NASDAQ","SP500"="SP500","DJI"="DJI")
      updateCheckboxGroupInput(session,"portfolioDisplay","Display",choices=variables,selected=unlist(variables),inline = TRUE)
      value1$count = value1$count + 1
    }
    
    if(input$unselectall == 0) return(NULL)
    else if (input$unselectall > value2$count)
    {
      variables<-list("Selected"="Selected","FPX"="FPX","IPO"="IPO","CSD"="CSD",
                      "SPAK"="SPAK","NASDAQ"="NASDAQ","SP500"="SP500","DJI"="DJI")
      updateCheckboxGroupInput(session,"portfolioDisplay","Display",choices=variables,inline = TRUE)
      value2$count = value2$count + 1
    }
  })
  
  #Create Plot
  output$Curve <- renderPlot({
    
    #Get selected list
    portfolio_list <- input$portfolioDisplay
    
    n <- length(portfolio_list)
    
    if(n>=1){
      
      plot <- TotalDataPlot()
      
      plot
      
    }
  
  })

  #Create ReturnTable
  output$ReturnTable <- DT::renderDataTable(
    
    DT::datatable({
      
      #Get selected list
      portfolio_list <- input$portfolioDisplay
      
      n1 <- length(portfolio_list)
      
      if(n1>=1){
        
        data <- TotalData() %>%
                  mutate(cr = paste(round(cr,2),"%",sep = "")) %>%
                  rename(`cumulative return` = cr)
          
          data
          
        }
        
      
    })
    
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

