library(shinymaterial)
library(shiny)
library(shinyjs)
library(readxl)
library(DT)
library(dplyr)
library(htmltools)
library(ggplot2)
library(gridExtra)
library(shinymanager) ## login
library(highcharter) ## interactive graph

## Make login DB

# credentials <- data.frame(
#  user = c("admin", "user1"),
#  password = c("admin", "user1"),
#  admin = c(T, F),
#  stringsAsFactors = FALSE)

# create_db(credentials_data = credentials, sqlite_path = "database.sqlite")

triage <- function(v) {
  age <- disease <- temperature <- count <- oxygen <- pressure <- breath <- 0

  if (v$Age >= 60) age <- 1
  if (v$Age >= 70) age <- 2
  if (v$Age >= 80) age <- 3

  if (v$Disease) disease <- 3

  if (v$Temperature <= 36) temperature <- 1
  if (v$Temperature >= 37.5) temperature <- 1
  if (v$Temperature > 38) temperature <- 2
  if (v$Temperature > 39) temperature <- 3

  if (v$BreathCount <= 11) count <- 1
  if (v$BreathCount <= 8) count <- 3

  if (v$Oxygen <= 95) oxygen <- 1
  if (v$Oxygen <= 93) oxygen <- 2
  if (v$Oxygen <= 91) oxygen <- 3

  if (v$BloodPressure <= 110) pressure <- 1
  if (v$BloodPressure <= 100) pressure <- 2
  if (v$BloodPressure <= 90) pressure <- 3

  breath <- as.integer((breath %in% 9:11) + 2 * (breath <= 8))

  point <- sum(age, disease, temperature, count, oxygen, pressure, breath)

  return(point)
}

ui <- material_page(
  nav_bar_color = "deep-purple lighten-1",
  color = "#311b92",
  title = "corona-triage <a href = 'https://github.com/shinykorea/corona-triage' target='_blank'> <i class='material-icons' style = 'font-size:1.3em;'> info </i> </a>",
  useShinyjs(),
  tags$head(tags$style(type = "text/css", "table.dataTable tr.selected td, table.dataTable td.selected {background-color: #d1c4e9 !important;}")),
  tags$head(tags$style(type = "text/css", "th, td {text-align:center !important;}")),
  tags$head(tags$style(type = "text/css", "html, body {height:100% !important;}")),
  tags$head(tags$style(type = "text/css", ".indicator {height:0.3em;}")),
  tags$head(tags$style(type = "text/css", "table {table-layout:fixed; width:100%;}")),
  tags$head(tags$style(type = "text/css", ".container-fab {height: 3em; background-color: #7e57c2;}")),
  tags$head(tags$style(type = "text/css", ".btn.btn-default.action-button.buttons-fab.shiny-bound-input { background-color: #b388ff;}")),
  tags$head(tags$style(type = "text/css", ".tabs .tab a, .tabs .tab a:hover, .tabs .tab a.active {font-size:1.5em; color : #311b92;}")),
  material_tabs(
    tabs = c(
      "자가" = "home",
      "생활치료센터" = "facility",
      "하급병실" = "hospital_general",
      "상급병실" = "hospital_premium"
    ),
    color = "#311b92"
  ),

  # Define tab content
  material_tab_content(
    tab_id = "home",
    tags$h1("자가"),
    material_row(
      material_column(
        width = 7,
        material_card(
          depth = 3,
          fileInput(
            inputId = "file",
            label = tags$a(href = "https://github.com/shinykorea/corona-triage/blob/master/Example.xlsx?raw=true", tags$div(HTML(paste("엑셀(xlsx)", tags$span(style = "color:black", "업로드"), sep = "")))),
            accept = ".xlsx",
            multiple = FALSE
          ),

          DT::dataTableOutput("tab1", width = "100%")
        )
      ),
      material_column(
        width = 5,
        material_card(
          tags$h2("시군구 지도 필요"),
          p("자가: 시군구 보건소에서 매일 데이터 보내주는 시나리오."),
          p("우리는 데이터를 받아서 당일 업데이트 현황을 지도에 보여줘야 함."),
          p("예) 당일 업데이트된 사람 60%, 50% 미만이면 빨간색")
        )
      )
    ),

    material_card(
      depth = 4,
      material_row(
        height = "100%",
        material_column(
          material_card(
            title = htmlOutput("pat", style = "text-align:center"),
            divider = TRUE,
            DT::dataTableOutput("tab2")
          ),
          width = 7
        ),
        material_column(
          highchartOutput("img", height = "600px"),
          width = 5
        )
      ),
      div(style = "height:3em;")
    )
  ),
  material_tab_content(
    tab_id = "facility",
    tags$h2("생활치료센터")
  ),
  material_tab_content(
    tab_id = "hospital_general",
    tags$h2("상급병실")
  ),
  material_tab_content(
    tab_id = "hospital_premium",
    tags$h2("하급병실")
  )
)

# Wrap your UI with secure_app
# ui <- secure_app(ui, enable_admin = T)  개발땐 생략, 배포시 적용

# f7d794 YELLOW  1 #ffffc0
# f19066 ORANGE  2 #ffbb85
# e66767 RED     3 #ff8686

# ede7f6 purple 1
# d1c4e9 purple 2
# b39ddb purple 3

getColor <- function(Data, Type) {
  col1 <- "#FFC312" # yellow #FFC312
  col2 <- "#EA2027" # red #EA2027
  col3 <- "#6F1E51" # purple #6F1E51
  colBasic <- "#A3CB38" # Green #A3CB38
  if (Type == "Point") {
    res <-
      sapply(Data[[Type]], function(i) {
        v <- colBasic
        if (i > 5) v <- col1
        if (i > 10) v <- col2
        if (i > 15) v <- col3
        return(v)
      }, USE.NAMES = FALSE)
  }
  if (Type == "Temperature") {
    res <-
      sapply(Data[[Type]], function(i) {
        v <- colBasic
        if (i <= 36) v <- col1
        if (i >= 37.5) v <- col1
        if (i > 38) v <- col2
        if (i > 39) v <- col3
        return(v)
      }, USE.NAMES = FALSE)
  }
  if (Type == "Oxygen") {
    res <-
      sapply(Data[[Type]], function(i) {
        v <- colBasic
        if (i <= 95) v <- col1
        if (i <= 93) v <- col2
        if (i <= 91) v <- col3
        return(v)
      }, USE.NAMES = FALSE)
  }
  if (Type == "BreathCount") {
    res <-
      sapply(Data[[Type]], function(i) {
        v <- colBasic
        if (i <= 11) v <- col1
        if (i <= 8) v <- col3
        return(v)
      }, USE.NAMES = FALSE)
  }
  if (Type == "BloodPressure") {
    res <-
      sapply(Data[[Type]], function(i) {
        v <- colBasic
        if (i <= 110) v <- col1
        if (i <= 100) v <- col2
        if (i <= 90) v <- col3
        return(v)
      }, USE.NAMES = FALSE)
  }

  return(data.frame(Date = Data$Date, y = Data[[Type]], color = res))
}

styleDT <- function(age, disease, temperature, count, oxygen, pressure, breath, point) {
  col1 <- "#ede7f6"
  col2 <- "#d1c4e9"
  col3 <- "#b39ddb"
  JS(paste0("function(row, data, index){
            // Age 
            if(data[", age, "] > 60){$(row).find('td:eq(", age, ")').css({'background-color' : '", col1, "', 'font-size' : '1.2em'});}
            if(data[", age, "] > 70){$(row).find('td:eq(", age, ")').css({'background-color' : '", col2, "', 'font-size' : '1.5em'});}
            if(data[", age, "] > 80){$(row).find('td:eq(", age, ")').css({'background-color' : '", col3, "', 'font-size' : '1.8em'});}
            
            // Diesease
            
            if(data[", disease, "]){$(row).find('td:eq(", disease, ")').css({'background-color' : '", col3, "', 'font-size' : '1.8em'});}
            
            // Temperature
            if(data[", temperature, "] <= 36 ){$(row).find('td:eq(", temperature, ")').css({'background-color' : '", col1, "', 'font-size' : '1.2em'});}
            if(data[", temperature, "] >= 37.5 ){$(row).find('td:eq(", temperature, ")').css({'background-color' : '", col1, "', 'font-size' : '1.2em'});}
            if(data[", temperature, "] > 38){$(row).find('td:eq(", temperature, ")').css({'background-color' : '", col2, "', 'font-size' : '1.5em'});}
            if(data[", temperature, "] > 39){$(row).find('td:eq(", temperature, ")').css({'background-color' : '", col3, "', 'font-size' : '1.8em'});}
        
            // BreathCount    
            if(data[", count, "] <= 11){$(row).find('td:eq(", count, ")').css({'background-color' : '", col1, "', 'font-size' : '1.2em'});}
            if(data[", count, "] <= 8){$(row).find('td:eq(", count, ")').css({'background-color' : '", col3, "', 'font-size' : '1.8em'});}
            
            // Oxygen
            if(data[", oxygen, "] <= 95){$(row).find('td:eq(", oxygen, ")').css({'background-color' : '", col1, "', 'font-size' : '1.2em'});}
            if(data[", oxygen, "] <= 93){$(row).find('td:eq(", oxygen, ")').css({'background-color' : '", col2, "', 'font-size' : '1.5em'});}
            if(data[", oxygen, "] <= 91){$(row).find('td:eq(", oxygen, ")').css({'background-color' : '", col3, "', 'font-size' : '1.8em'});}
            
            // BloodPressure
            if(data[", pressure, "] <= 110){$(row).find('td:eq(", pressure, ")').css({'background-color' : '", col1, "', 'font-size' : '1.2em'});}
            if(data[", pressure, "] <= 100){$(row).find('td:eq(", pressure, ")').css({'background-color' : '", col2, "', 'font-size' : '1.5em'});}
            if(data[", pressure, "] <= 90){$(row).find('td:eq(", pressure, ")').css({'background-color' : '", col3, "', 'font-size' : '1.8em'});}
            
            // Breath
            if(data[", breath, "]){$(row).find('td:eq(", breath, ")').css({'background-color' : '", col2, "', 'font-size' : '1.5em'});}
            
            // Point
            if(data[", point, "] > 5){$(row).find('td:eq(", point, ")').css({'background-color' : '", col1, "', 'font-size' : '1.2em'});}
            if(data[", point, "] > 10){$(row).find('td:eq(", point, ")').css({'background-color' : '", col2, "', 'font-size' : '1.5em'});}
            if(data[", point, "] > 15){$(row).find('td:eq(", point, ")').css({'background-color' : '", col3, "', 'font-size' : '1.8em'});}
            
        }"))
}

server <- function(input, output, session) {

  ## Apply login DB
  res_auth <- secure_server(
    check_credentials = check_credentials("database.sqlite")
  )


  tab <- newtab <- ""

  output$tab1 <- renderDataTable({
    inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    }

    tab <- readxl::read_excel(inFile$datapath)

    tab$Sex <- as.factor(tab$Sex)
    tab$Occurrence <- as.Date(tab$Occurrence)
    tab$Confirm <- as.Date(tab$Confirm)
    tab$Date <- as.Date(tab$Date)

    triages <- sapply(1:nrow(tab), function(i) {
      triage(tab[i, ])
    })
    # hide file upload
    shinyjs::runjs('$(".form-group.shiny-input-container").hide()')

    tab$Point <- triages
    tab <<- tab

    newtab <<- tab %>%
      filter(Date == max(Date)) %>%
      select(-City, -Town, -Occurrence, -Confirm)


    dtobj <- datatable(
      newtab,
      escape = FALSE,
      caption = "전체 환자: 시설",
      options = list(
        rowCallback = styleDT(3, 4, 5, 6, 7, 8, 9, 11),
        dom = "tip",
        rowsGroup = list(0), # Place, not Sex!
        order = list(list(10, "desc"))
      ),
      selection = "single",
      filter = "top",
      rownames = FALSE
    )
    path <- file.path(getwd(), "www")
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0",
      path,
      script = "dataTables.rowsGroup.js"
    )
    dtobj$dependencies <- c(dtobj$dependencies, list(dep))
    dtobj
  })

  observeEvent(input$tab1_rows_selected, {
    selected <- input$tab1_rows_selected # check none selected
    thisTab <- tab %>% filter(Name == newtab$Name[selected])

    output$pat <- renderText(
      paste0(
        HTML('<i class = "material-icons" style= "font-size : 2.5rem">face</i> '), # icon
        thisTab$Name[1], " / ", # name
        thisTab$Sex[1], " / ", # sex
        "Confirmed in ", thisTab$Confirm[1], " / ", # confirmed
        thisTab$Town[1], " / ", # town
        thisTab$Place[1]
      )
    )

    output$tab2 <- renderDataTable({
      dtobj <-
        datatable(
          thisTab[, -(1:7)],
          rownames = FALSE,
          selection = "none",
          options = list(
            rowsGroup = list(0, 1), # Age, Disease
            rowCallback = styleDT(0, 1, 2, 3, 4, 5, 6, 8),
            dom = "tip",
            autoWidth = FALSE,
            order = list(list(7, "desc"))
          )
        )
      path <- file.path(getwd(), "www")
      dep <- htmltools::htmlDependency(
        "RowsGroup", "2.0.0",
        path,
        script = "dataTables.rowsGroup.js"
      )
      dtobj$dependencies <- c(dtobj$dependencies, list(dep))
      dtobj
    })

    output$img <- renderHighchart({
      highchart() %>%
        hc_xAxis(type = "datetime", title = list(text = "Day")) %>%
        hc_yAxis_multiples(
          list(top = "0%", height = "20%", title = list(text = "Point"), lineWidth = 3),
          list(top = "20%", height = "20%", title = list(text = "Temp"), showFirstLabel = T, showLastLabel = T, opposite = T),
          list(top = "40%", height = "20%", title = list(text = "SpO2"), showFirstLabel = T, showLastLabel = T),
          list(top = "60%", height = "20%", title = list(text = "RR"), showFirstLabel = T, showLastLabel = T, opposite = T),
          list(top = "80%", height = "20%", title = list(text = "SBP"), showFirstLabel = T, showLastLabel = T)
        ) %>%
        hc_add_series(getColor(thisTab, "Point"), "line", hcaes(Date, y, color = color), name = "Point", marker = list(radius = 8)) %>%
        hc_add_series(getColor(thisTab, "Temperature"), "line", hcaes(Date, y, color = color), name = "Temp", marker = list(radius = 8), yAxis = 1) %>%
        hc_add_series(getColor(thisTab, "Oxygen"), "line", hcaes(Date, y, color = color), name = "SpO2", marker = list(radius = 8), yAxis = 2) %>%
        hc_add_series(getColor(thisTab, "BreathCount"), "line", hcaes(Date, y, color = color), name = "RR", marker = list(radius = 8), yAxis = 3) %>%
        hc_add_series(getColor(thisTab, "BloodPressure"), "line", hcaes(Date, y, color = color), name = "SBP", marker = list(radius = 8), yAxis = 4) %>%
        hc_exporting(enabled = T) %>%
        hc_tooltip(valueDecimals = 1, shared = T, crosshairs = T)
    })
  })
}

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
