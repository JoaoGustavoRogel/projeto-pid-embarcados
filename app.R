library(shinydashboard)
library(rmatio)
library(ggplot2)
library(dplyr)
library(R.matlab)
library(matlib)
library(control)

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "blue"


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("PID", tabName = "dashboard", icon = icon("dashboard"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
            fluidRow(
              box(
                title = "Amostras",
                status = "primary",
                plotOutput("plot1", height = 240),
                height = 300
              ),
              
              tabBox(
                height = 300,
                tabPanel("Informações",
                  htmlOutput("informacoes")
                ),
                tabPanel("Mínimos Quadrados",
                         plotOutput("min_quad", heigh = 240)
                )
              )
            ),
            
            # Boxes with solid headers
            fluidRow(
              box(
                title = "Arquivo com amostras", width = '100%', solidHeader = TRUE, status = "primary",
                fileInput("amostras", "Amostras", multiple = FALSE, width = '100%')
              )
            )
    )
  )
)

messages <- dropdownMenu(type = "messages",
                         messageItem(
                           from = "Sales Dept",
                           message = "Sales are steady this month."
                         ),
                         messageItem(
                           from = "New User",
                           message = "How do I register?",
                           icon = icon("question"),
                           time = "13:45"
                         ),
                         messageItem(
                           from = "Support",
                           message = "The new server is ready.",
                           icon = icon("life-ring"),
                           time = "2014-12-01"
                         )
)

notifications <- dropdownMenu(type = "notifications", badgeStatus = "warning",
                              notificationItem(
                                text = "5 new users today",
                                icon("users")
                              ),
                              notificationItem(
                                text = "12 items delivered",
                                icon("truck"),
                                status = "success"
                              ),
                              notificationItem(
                                text = "Server load at 86%",
                                icon = icon("exclamation-triangle"),
                                status = "warning"
                              )
)

tasks <- dropdownMenu(type = "tasks", badgeStatus = "success",
                      taskItem(value = 90, color = "green",
                               "Documentation"
                      ),
                      taskItem(value = 17, color = "aqua",
                               "Project X"
                      ),
                      taskItem(value = 75, color = "yellow",
                               "Server deployment"
                      ),
                      taskItem(value = 80, color = "red",
                               "Overall project"
                      )
)

header <- dashboardHeader(
  title = "Projeto PID",
  messages,
  notifications,
  tasks
)

ui <- dashboardPage(header, sidebar, body, skin = skin)

server <- function(input, output) {
  
  set.seed(122)
  

  
  output$plot1 <- renderPlot({
    
    if (is.null(input$amostras))
      return()

    raw_base <- read.mat(input$amostras$datapath)
    df_base <- data.frame("resp" = raw_base$resp0_3,
                          "degrau" = raw_base$degrau0_3,
                          "tempo" = raw_base$tempo0_3)
    
    df_base %>% ggplot(aes(x=tempo, y=resp)) +
      geom_line()
  })
  
  output$informacoes  <- renderUI({
    if (is.null(input$amostras))
      return()
    
    raw_base <- read.mat(input$amostras$datapath)
    df_base <- data.frame("resp" = raw_base$resp0_3,
                          "degrau" = raw_base$degrau0_3,
                          "tempo" = raw_base$tempo0_3)
    
    ts = df_base$tempo[2] - df_base$tempo[1]
    
    dados <-  readMat(input$amostras$datapath)
    
    tempo <- dados[[3]]
    degrau <- dados[[1]]
    resp <- dados[[2]]
    
    size = length(tempo)
    f <- cbind(resp[1, 1:size-1], degrau[1, 1:size-1])
    j <- cbind(resp[1, 2:size])
    m <- solve(t(f) %*% f) %*% t(f) %*% j
    a1 <- m[[1]]
    b1 <- m[[2]]
    sys <- tf(b1, c(1, a1), Ts = ts)
    
    
    show <- paste(paste("<br/>Tempo de amostragem:", ts), "s<br/><br/><br/><br/>")
    
    func_1 <- paste0("<p><strong>Função de Transferência:</strong></p><p><span style='text-decoration: underline;'>&nbsp; ", b1)
    func_2 <- paste0("&nbsp; &nbsp;<br /></span>&nbsp; &nbsp;s&nbsp; &nbsp; +&nbsp; &nbsp; ", a1)
    func_3 <- "<span style='text-decoration: underline;'><br /><br /></span></p>"
    
    show <- paste0(show, func_1)
    show <- paste0(show, func_2)
    show <- paste0(show, func_3)
    
    HTML(
      show
    )
    
    
    
  })
  
  output$min_quad <- renderPlot({
    if (is.null(input$amostras))
      return()
    
    raw_base <- read.mat(input$amostras$datapath)
    df_base <- data.frame("resp" = raw_base$resp0_3,
                          "degrau" = raw_base$degrau0_3,
                          "tempo" = raw_base$tempo0_3)
    
    ts = df_base$tempo[2] - df_base$tempo[1]
    
    dados <-  readMat(input$amostras$datapath)
    
    tempo <- dados[[3]]
    degrau <- dados[[1]]
    resp <- dados[[2]]
    
    size = length(tempo)
    f <- cbind(resp[1, 1:size-1], degrau[1, 1:size-1])
    j <- cbind(resp[1, 2:size])
    m <- solve(t(f) %*% f) %*% t(f) %*% j
    a1 <- m[[1]]
    b1 <- m[[2]]
    sys <- tf(b1, c(1, a1), Ts = ts)
    
    resp_sys <- step(sys, df_base$tempo)
    df_resp <- data.frame(
      "resp" = resp_sys$y %>% as.vector(),
      "tempo" = resp_sys$t %>% as.vector()
    )
    
    ggplot() +
      geom_line(aes(x=df_resp$tempo, y=df_resp$resp), size = 0.8) +
      xlim(0, 350) +
      ylim(0, 0.007) + 
      ylab("resp") + 
      xlab("tempo")
    
  })
  
  output$scatter2 <- renderPlot({
    spread <- as.numeric(input$spread) / 100
    x <- rnorm(1000)
    y <- x + rnorm(1000) * spread
    plot(x, y, pch = ".", col = "red")
  })
  
}

shinyApp(ui, server)