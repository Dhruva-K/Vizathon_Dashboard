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

