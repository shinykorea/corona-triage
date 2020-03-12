
library(shiny)
library(googlesheets4)
library(DT)
library(lubridate)
library(dplyr)

ui <- fluidPage(
  headerPanel("Corona19 Patient Daily Survey"),
  uiOutput("controls"),
  mainPanel(
    dataTableOutput(outputId = 'tab1') 
  )
)

server <- function(input, output, session) {
  
  sheets_deauth()
  
  tab = read_sheet("http://docs.google.com/spreadsheets/d/188LunvsxTa2zqudNwAVDj-cuVpV5jm4y-3LTaj-azQE/edit?usp=sharing")
  names(tab) <- c("res_time", "name", "birth", "initial_res_yn", "gender", "age", "basis_sick", "temperature", "breathe_hard_yn", "breathe_cnt", "pulse_cnt", "oxygen","bloodpressure")
  tab <- tab %>% mutate(res_time = as_date(tab$res_time), birth = as_date(tab$birth))
  
  output$controls <- renderUI({
    sidebarPanel(
      dateRangeInput(inputId = "dt", label = "date", start = min(tab$res_time), end = max(tab$res_time),width = "100%")
    )
  })
  
  output$tab1 = renderDataTable(tab)
  
}

shinyApp(ui, server)

