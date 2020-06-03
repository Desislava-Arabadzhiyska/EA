library(shiny)
library(shinydashboard)
library(tidyverse)

# function for calculating EA
#EA (t +1) = EA(t) + (λEA(t) + k(RP)dt + N(0,σ))
get_ea <- function(lambd = 2.3, k = 1.5, time_incr = 0.001, noise_sd = 0.003, nRP = 4, RP = 0.25, nDT = 0.4, max_time = 1.300){
  #EA_table <- array(0, dim=c(max_time*1000, nRP))
  for (nrps in 1:nRP) {
    EA <- rep(0, max_time*1000)
    i <- 1
    t <- 0
    while (abs(EA[i])<1 & t <(max_time -nDT)) {
      EA_incr<-(lambd*EA[i] + k*RP)*time_incr + noise_sd*rnorm(1) #EA_incr<-(lambd*EA[i] + k*RP[nrps])*time_incr + noise_sd*rnorm(1)
      t = t + time_incr
      EA[i + 1] = EA[i] + EA_incr
      i = i+1
    }
    #EA_table[,nrps] <- EA
  }
  #EA_table
  EA
}

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(title = "Evidence Accumulation app"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Parameter selection", tabName="paramtab", icon=icon("eye")),
      menuItem("Results", tabName="resulttab", icon=icon("poll-h"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "paramtab",
        fluidRow(
          # the tab's content will go in here put info in rolls each row is 12 colum
          box(width = 12,
              title = "EA parameters", 
              numericInput("lambd", "lambda", min = 0, value = 2),
              numericInput("k", "k", min = 0, value = 1),
              numericInput("time_incr", "time increment", min = 0, value = 0.001),
              numericInput("noise_sd", "sigma", min = 0, value = 0.0003),
              numericInput("nDT", "Non-decision time (in seconds)", min = 0, value = 0.4),
              numericInput("max_time", "Maximum decision time (in seconds)", min = 0, value = 1.3),
              selectInput("RP", "Number of Reward Probability levels",
                          choices = list("4" = 1, "5" = 2,
                                         "6" = 3),selected = 1),
              conditionalPanel(
                condition = "input.RP == '1'",
                numericInput("RP1", "Reward probability 1", min = 0, value = 0),
                numericInput("RP2", "Reward probability 2", min = 0, value = 0),
                numericInput("RP3", "Reward probability 3", min = 0, value = 0),
                numericInput("RP4", "Reward probability 4", min = 0, value = 0)
              ),
              conditionalPanel(
                condition = "input.RP == '2'",
                numericInput("RP1", "Reward probability 1", min = 0, value = 0),
                numericInput("RP2", "Reward probability 2", min = 0, value = 0),
                numericInput("RP3", "Reward probability 3", min = 0, value = 0),
                numericInput("RP4", "Reward probability 4", min = 0, value = 0),
                numericInput("RP5", "Reward probability 5", min = 0, value = 0)
              ),
              conditionalPanel(
                condition = "input.RP == '3'",
                numericInput("RP1", "Reward probability 1", min = 0, value = 0),
                numericInput("RP2", "Reward probability 2", min = 0, value = 0),
                numericInput("RP3", "Reward probability 3", min = 0, value = 0),
                numericInput("RP4", "Reward probability 4", min = 0, value = 0),
                numericInput("RP5", "Reward probability 5", min = 0, value = 0),
                numericInput("RP6", "Reward probability 6", min = 0, value = 0)
              )
          ) 
        )
      ),
      tabItem(
        tabName = "resulttab",
        fluidRow(
          #splitLayout(cellWidths = c("25%", "25%", "25%", "25%"),
          plotOutput("plot1"),
          plotOutput("plot2"),
           plotOutput("plot3"),
           plotOutput("plot4"),
            conditionalPanel(
              condition = "input.RP == '2'",
              plotOutput("plot5")            
            ),
            conditionalPanel(
              condition = "input.RP == '3'",
              plotOutput("plot5"), 
              plotOutput("plot6")
          )
          #)
        )
      )
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    b <- get_ea(input$lambd, input$k, input$time_incr, input$noise_sd, input$RP, RP = input$RP1, input$nDT, input$max_time)
    EA1_t <- data.frame(b)
    EA1 <- head(EA1_t, as.numeric(which(abs(EA1_t)==max(abs(EA1_t)))))
         a <- 1:nrow(EA1)
     plot(a,EA1[,1], 'l', xlab = "Time", ylab = "EA")
     
     
  })
  
  output$plot2 <- renderPlot({
    b <- get_ea(input$lambd, input$k, input$time_incr, input$noise_sd, input$RP, RP = input$RP2, input$nDT, input$max_time)
    EA1_t <- data.frame(b)
    EA1 <- head(EA1_t, as.numeric(which(abs(EA1_t)==max(abs(EA1_t)))))
    a <- 1:nrow(EA1)
    c <- paste0('Time', ncol(b))
    plot(a,EA1[,1], 'l', xlab = c, ylab = "EA")
    
    
  })
  
   output$plot3 <- renderPlot({
  b <- get_ea(input$lambd, input$k, input$time_incr, input$noise_sd, input$RP, RP = input$RP3, input$nDT, input$max_time)
  EA1_t <- data.frame(b)
  EA1 <- head(EA1_t, as.numeric(which(abs(EA1_t)==max(abs(EA1_t)))))
  a <- 1:nrow(EA1)
  c <- paste0('Time', ncol(b))
  plot(a,EA1[,1], 'l', xlab = c, ylab = "EA")
     
   })
   
   output$plot4 <- renderPlot({
 b <- get_ea(input$lambd, input$k, input$time_incr, input$noise_sd, input$RP, RP = input$RP4, input$nDT, input$max_time)
  EA1_t <- data.frame(b)
  EA1 <- head(EA1_t, as.numeric(which(abs(EA1_t)==max(abs(EA1_t)))))
  a <- 1:nrow(EA1)
  c <- paste0('Time', ncol(b))
  plot(a,EA1[,1], 'l', xlab = c, ylab = "EA")
   })
   
   output$plot5 <- renderPlot({
  b <- get_ea(input$lambd, input$k, input$time_incr, input$noise_sd, input$RP, RP = input$RP5, input$nDT, input$max_time)
  EA1_t <- data.frame(b)
  EA1 <- head(EA1_t, as.numeric(which(abs(EA1_t)==max(abs(EA1_t)))))
  a <- 1:nrow(EA1)
  c <- paste0('Time', ncol(b))
  plot(a,EA1[,1], 'l', xlab = c, ylab = "EA")
   })
   
   output$plot6 <- renderPlot({
  b <- get_ea(input$lambd, input$k, input$time_incr, input$noise_sd, input$RP, RP = input$RP6, input$nDT, input$max_time)
  EA1_t <- data.frame(b)
  EA1 <- head(EA1_t, as.numeric(which(abs(EA1_t)==max(abs(EA1_t)))))
  a <- 1:nrow(EA1)
  c <- paste0('Time', ncol(b))
  plot(a,EA1[,1], 'l', xlab = c, ylab = "EA")
   })
}

shinyApp(ui = ui, server = server)