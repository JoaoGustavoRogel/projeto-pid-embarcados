library(shinydashboard)
library(rmatio)
library(ggplot2)
library(dplyr)
library(R.matlab)
library(matlib)
library(control)
library(plotly)

source("functions.R")

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "blue"


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("PID", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Equipe 7", icon = icon("user-friends")),
    menuSubItem("André Fillipi"),
    menuSubItem("João Gustavo"),
    menuItem(""),
    menuItem("Especificações", icon = icon("info")),
    menuSubItem("Overshoot (%) 5"),
    menuSubItem("Acomodação (s) 60")
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
                         plotOutput("min_quad", height = 240)
                )
              )
            ),
            
            
            fluidRow(
              box(
                title = "Arquivo com amostras", width = '100%', solidHeader = TRUE, status = "primary",
                fileInput("amostras", "Amostras", multiple = FALSE, width = '100%')
              )
            ),
            
            fluidRow(
              tabBox(
                height = 440,
                width = '100%',
                tabPanel("Resposta Malha Aberta",
                         plotlyOutput("plot_malha_aberta", height = 440)
                ),
                tabPanel("Resposta Malha Fechada",
                         plotlyOutput("plot_malha_fechada", height = 440)
                ),
                tabPanel("Resposta Malha Fechada com Ganho",
                         plotlyOutput("plot_malha_fechada_ganho", height = 440)
                )
              )
            )
            
    )
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
  
  output$plot_malha_aberta <- renderPlotly({
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
    
    resp_malha_aberta <- control::step(sys)
    
    df_plot_malha_aberta <- data.frame(
      "tempo" = resp_malha_aberta$t %>% as.vector(),
      "resp" = resp_malha_aberta$y %>% as.vector()
    )
    
    p <- ggplot(df_plot_malha_aberta, aes(x=tempo, y=resp)) + 
      geom_line()
    
    ggplotly(p)
  })
  
  output$plot_malha_fechada <- renderPlotly({
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
    
    size <- length(tempo)
    f <- cbind(resp[1, 1:size-1], degrau[1, 1:size-1])
    j <- cbind(resp[1, 2:size])
    m <- solve(t(f) %*% f) %*% t(f) %*% j
    a1 <- m[[1]]
    b1 <- m[[2]]
    sys <- tf(b1, c(1, a1), Ts = ts)
    
    resp_malha_fechada <- get_resp(1, 0, 0, ts, size, a1, b1)
    
    df_plot_malha_fechada <- data.frame(
      "resp" = resp_malha_fechada,
      "tempo" = tempo %>% as.vector()
    )
    
    p <- ggplot(df_plot_malha_fechada, aes(x=tempo, y=resp)) + 
      geom_line()
    
    ggplotly(p)
  })
  
  output$plot_malha_fechada_ganho <- renderPlotly({
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
    
    size <- length(tempo)
    f <- cbind(resp[1, 1:size-1], degrau[1, 1:size-1])
    j <- cbind(resp[1, 2:size])
    m <- solve(t(f) %*% f) %*% t(f) %*% j
    a1 <- m[[1]]
    b1 <- m[[2]]
    sys <- tf(b1, c(1, a1), Ts = ts)
    
    found = FALSE
    resp_malha_fechada_ganho <- NULL
    
    # operações com ganhos
    for(kp in seq(from = 1, to = 10, by = 1)){
      for(ki in seq(from = 0, to = 5, by = 0.01)){
        kd <- 0.2
        aux <- get_resp(kp, ki, kd, ts, size, a1, b1)
        found <- (check_overshoot(aux, 0.1, size) && check_time(aux, tempo, 60, size, tempo))
        if(found){
          resp_malha_fechada_ganho <- aux
          kp_f <- kp
          ki_f <- ki
          kd_f <- kd
          break
        }
      }
      if(found){
        break
      }
    }

    df_plot_malha_fechada_ganho <- data.frame(
      "resp" = resp_malha_fechada_ganho,
      "tempo" = tempo %>% as.vector()
    )
    
    p <- ggplot(df_plot_malha_fechada_ganho, aes(x=tempo, y=resp)) + 
      geom_line()
    
    ggplotly(p)
    
  })
  
}

shinyApp(ui, server)