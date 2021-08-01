library(shiny)
library(shinydashboard)
library(ggplot2)
library(dashboardthemes)
library(hrbrthemes)
library(dplyr)
library(lessR)
library(wordcloud)
library(packcircles)
library(viridis)
library(ggiraph)

data <- read.csv2("C://Users//dhruv//Downloads//archive (1)//subsaharan.csv")

ui<-dashboardPage(
  dashboardHeader(title = "Impact of COVID 19 on Subsaharan Countries",titleWidth = 500),
  dashboardSidebar(
    disable = TRUE
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "purple_gradient"
    ),
   
    fluidRow(
     
    column(
      5,
      box(
        title = "Employment status during Jan - March 2020",
        height = 460,
        width = NULL,
        status = "danger",
        selectizeInput("country","Select country", unique(data$Country),
                       multiple = FALSE),
        plotOutput("employment",height = 320)
      ),
      
      fluidRow(
        column(
          12,
          box(
            title="Income change post COVID outbreak",
            status = "warning",
            width = NULL,
            height = 330,
            ggiraphOutput("bubble",height = 270,width = "100%")
          )
        )  
      )
      ),
   
    column(
      7,
      fluidRow(
      column(
        7,
      box(
        title="Loss of Job during the pandemic",
        status = "success",
        width = "200px",
        height = 330,
        fluidRow(
          column(9,
                 plotOutput("donut",width = 350,height = 260)),
          column(3,
                 radioButtons(
                   "radio","Gender", unique(data$Gender))
          )
        )
      )),
     column(
       5,
       height=300,
       valueBoxOutput("info1",width = NULL),
       valueBoxOutput("info2",width = NULL),
       valueBoxOutput("info3",width = NULL),
     )
      ),
      
      fluidRow(
        column(
          8,
          height = 400,
          box(
            title = "Effects of the pandemic on Job sectors",
            status="info",
            width=NULL,
            plotOutput("job")
          )
        ),
        column(
          4,
          br(),
          box(
            width = NULL,
            title="Select Job Loss", status="danger", solidHeader = TRUE,
          selectInput("select1","", unique(data$JobLoss),selected = "Yes")
          ),
          box(
            width = NULL,
            title="Informal Worker",status = "warning",solidHeader = TRUE,
          selectInput("select2","",unique(data$InformalWorker),selected = "No")
          )
          
        )
         
         
          
      )
        
      )
      

      
    )

    )
  )
  


server<- function(input,output,session){
  custom_theme = theme(
    axis.text.x = element_text(size=12,angle=30, hjust=1),
    )
  custom_theme2 = theme(
    axis.text.x = element_text(size = 12)
  )
  
  table1<-reactive({
    filter(data,Gender == input$radio)
  })
  table2<-reactive({
    filter(data,Country==input$country)
  })
  
  table3<- reactive({
    filter(data,JobLoss==input$select1 & InformalWorker==input$select2)
  })
  
  govt <- reactive({
    (sum(table2()$GovernmentPriority=="Reopening the economy")/ nrow(table2()))*100
  })
  
  loan <- reactive({
    (sum(table2()$COVIDLoans == "Yes")/nrow(table2()))*100
  })
  
  rate<-reactive({
    names(which.max(table(table2()$Expense_Concern_Rating)))
    
  })
  
  
  output$employment<-renderPlot({
    ggplot(table2(), aes(fill=Gender, x=factor(EmploymentType)))+geom_bar()+scale_fill_viridis(discrete = T)+theme_ipsum()+xlab("")+ custom_theme

  })
  
  output$donut<- renderPlot({
    PieChart(JobLoss, data=table1(),
             rows = (JobLoss!="N/A"),
             fill="viridis",
             main=NULL)
  })

  output$job<- renderPlot({
    if(input$select2=="No"){
      tab<-filter(table3(), JobType!="N/A")
      ggplot(tab,aes(x=factor(JobType)))+geom_bar(fill="steelblue")+theme_ipsum()+xlab("")+ coord_flip()+ custom_theme2
    }
    else{
      temp<-filter(table3(),Informal_Work_Type!="N/A")
      temp<-count(temp,Informal_Work_Type)
      wordcloud(words = temp$Informal_Work_Type,freq = temp$n,min.freq = 1,colors = c("orange","green", "yellow","red"),scale=c(5,1))

    }
  })
  
  output$bubble<-renderggiraph({
    d<-filter(data,IncomeChange!="N/A")
    d<-count(d,IncomeChange)
    d$text <- paste0("Salary: ", d$IncomeChange, "\n", round((d$n/sum(d$n))*100),"%")
    
    packing<- circleProgressiveLayout(d$n, sizetype = "area")
    d<-cbind(d,packing)
    dat.gg<- circleLayoutVertices(packing,npoints=50)
    p<-ggplot()+
      geom_polygon_interactive(data=dat.gg, aes(x, y, group = id, fill=as.factor(id),tooltip = d$text[id]), colour = "black", alpha = 0.6) +
      scale_fill_manual(values = viridis(nrow(d))) +
      
      geom_text(data = d, aes(x, y, size=12, label = IncomeChange)) +
      scale_size_continuous(range = c(2,4)) +
      
      # General theme:
      theme_void() + 
      theme(legend.position="none") +
      coord_equal()
    widg <- ggiraph(ggobj = p, width_svg = 15, height_svg = 7)
    widg
  })
  
  output$info1<- renderValueBox({
    valueBox(
      rate(), "Expense concern rating",icon("exclamation-sign",lib = "glyphicon"),color="purple",
     
    )
    })
  output$info2<- renderValueBox({
    valueBox(
      paste0(loan(),"%"), "People took Covid Loans",icon("thumbs-up", lib="glyphicon"),color="blue",
      
    )
  })
  output$info3<- renderValueBox({
    valueBox(
      paste0(govt(),"%"), paste0("Priority to reviving the economy"),icon("briefcase", lib="glyphicon"),color="green",
    )
  })
  
  
}


shinyApp(ui, server)

d<-filter(data,IncomeChange!="N/A")
d<-count(d,IncomeChange)
d$text <- paste0("Salary: ", d$IncomeChange, "\n", round((d$n/sum(d$n))*100),"%")

packing<- circleProgressiveLayout(d$n, sizetype = "area")
d<-cbind(d,packing)
dat.gg<- circleLayoutVertices(packing,npoints=50)
p<-ggplot()+
  geom_polygon_interactive(data=dat.gg, aes(x, y, group = id, fill=as.factor(id),tooltip = d$text[id]), colour = "black", alpha = 0.6) +
  scale_fill_manual(values = viridis(nrow(d))) +
  
  geom_text(data = d, aes(x, y, size=n, label = IncomeChange)) +
  scale_size_continuous(range = c(2,4)) +
  
  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()
widg <- ggiraph(ggobj = p, width_svg = 7, height_svg = 7)
widg
