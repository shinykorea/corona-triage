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
library(googlesheets4) # google sheets
library(gargle) # googlesheets encoding

## Make login DB

# credentials <- data.frame(
#  user = c("admin", "user1"),
#  password = c("admin", "user1"),
#  admin = c(T, F),
#  stringsAsFactors = FALSE)

# create_db(credentials_data = credentials, sqlite_path = "database.sqlite")

myButton <- function(inputId, label, width = NULL, onClick = NULL, ...) {
  value <- restoreInput(id = inputId, default = NULL)
  tags$button(
    id = inputId, style = if (!is.null(width)) {
      paste0("width: ", validateCssUnit(width), ";")
    },
    type = "button", class = "btn btn-default action-button",
    onClick = onClick,
    `data-val` = value, list(label),
    ...
  )
}


triage <- function(v) {
  
  PT = PCF = PCO = PM = 0
  
  ##### PT
  
  T = v$T
  if(T <= 35) PT = 3 
  if(T <= 36 || T >39) PT = max(2,PT)
  if(T >= 38 ) PT = max(1,PT)

  ##### PCF
  HB = v$HB
  DHB = v$DHB
  CP = v$CP
  F = v$F
  C = v$C
  BL = v$BL
  HHB = v$HHB
  
  if(HB){
    if(sum(DHB, CP, F, C, BL, HHB)) PCF = 3
    else PCF = 2
  }
  
  O = v$O
  if(O<=93) PCF = 3
  if(O<=95) PCF = max(2,PCF)
  
  BC = v$BC
  if(BC >=25 || BC <=8) PCF = 3
  if(BC >=21 || BC <=11) PCF = max(2,PCF)
  
  P = v$P
  if(P > 110 || P <= 40) PCF = 3
  if(P >= 100 ) PCF = max(2,PCF)
  if(P >= 101 || P <= 110) PCF = max(2,PCF)
  
  ##### PCO
  
  CO = v$CO
  if(!CO) PCO = 3
  
  ##### PM
  
  PO=v$PO
  PE=v$PE	
  TR=v$TR	
  CH=v$CH	
  CC=v$CC	
  AC=v$AC	
  W=v$W	
  SA=v$SA	
  FE=v$FE
  
  if(sum(PO,PE,TR,C,CC,AC,W,SA,FE)>=4) PM = 3
  else if(sum(PO,PE,TR,C,CC,AC,W,SA,FE)>=1) PM = max(1,PM)
  
  return(c(PT, PCF, PCO, PM, max(PT, PCF, PCO, PM)))
}

ui <- function(){
  material_page(
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
        "구글시트예시" = 'google'
      ),
      color = "#311b92"
    ),
    
    # Define tab content
    material_tab_content(
      tab_id = "home",
      tags$h1("자가"),
      div( # navigator
        material_column(
          myButton(inputId = 'totab1',label = 'tab1',onClick = 'location.href = "#tab1"'),
          myButton(inputId = 'totab2',label = 'tab2',onClick = 'location.href = "#tab2"'),
          myButton(inputId = 'toimg',label = 'img',onClick = 'location.href = "#img"'),
        ),
        id = 'home_navigator',
        style = 'position:fixed;bottom:3em;width:100%;z-index:999;'
        
      ),
      material_row(
        material_column(
          width = 12,
          material_card(
            depth = 3,
            DT::dataTableOutput("tab1", width = "100%")
          )
        ),
      ),
      
      material_row(
        height = "100%",
        material_column(
          material_card(
            title = htmlOutput("pat", style = "text-align:center"),
            divider = TRUE,
            DT::dataTableOutput("tab2")
          ),
          width = 12
        ),
            
        material_column(
          material_card(
            title = HTML(paste0('Visualize among time. ',
                          '<span style = "background-color:#EA2027">Red</span> : 3, ',
                         '<span style = "background-color:#F79F1F">Orange</span> : 2, ',
                         '<span style = "background-color:#FFC312">Yellow</span> : 1, ',
                         '<span style = "background-color:#A3CB38">Green</span> : 0')),
            highchartOutput("img", height = "600px"),
            depth = 3
          ),
          width = 12
        )
      ),
      div(style = "height:3em;")
      
    ),
    material_tab_content(
      tab_id = "facility",
      tags$h2("생활치료센터")
    ),
    
    material_tab_content(
      tab_id = "google",
      tags$h2("구글시트예시"),
      material_row(
        material_column(
          width = 12,
          material_card(
            title = 'date',
            depth = 3,
            material_row(
              material_column(
                shinymaterial::material_date_picker(input_id = 'date1', label = 'From'),
                width = 6
              ),
              material_column(
                shinymaterial::material_date_picker(input_id = 'date2', label = 'Until'),
                width = 6
              ),
              material_button('dateapply', 'apply', icon = 'access_time')
            )
          )
        ),
        material_column(
          width = 12,
          material_card(
            title='gtab',
            depth = 3,
            DT::dataTableOutput("tab_google", width = "100%"),
          )
        )
        
      )
    )
  )
}
  
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
  col2 <- "#EE5A24" # orange #EE5A24
  col3 <- "#EA2027" # red #EA2027
  colBasic <- "#A3CB38" # Green #A3CB38
  
  if (Type == "TRI") {
    res <-
      sapply(Data[[Type]], function(i) {
        v <- colBasic
        if (i == 1) v <- col1
        if (i == 2) v <- col2
        if (i == 3) v <- col3
        return(v)
      }, USE.NAMES = FALSE)
  }
  if (Type == "T") {
    res <-
      sapply(Data[[Type]], function(T) {
        if(T <= 35) return(col3)
        if(T <= 36 || T >39) return(col2)
        if(T >= 38 ) return(col1)
        return(colBasic)
      }, USE.NAMES = FALSE)
  }
  if (Type == "O") {
    res <-
      sapply(Data[[Type]], function(O) {
        if(O<=93) return(col3)
        if(O<=95) return(col2)
        return(colBasic)
      }, USE.NAMES = FALSE)
  }
  if (Type == "BC") {
    res <-
      sapply(Data[[Type]], function(BC) {
        if(BC >=25 || BC <=8) return(col3)
        if(BC >=21 || BC <=11) return(col2)
        return(colBasic)
      }, USE.NAMES = FALSE)
  }
  if (Type == "P") {
    res <-
      sapply(Data[[Type]], function(P) {
        if(P > 110 || P <= 40) return(col3)
        if(P >= 100 ) return(col2)
        if(P >= 101 || P <= 110) return(col2)
        return(colBasic)
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

  # deauthorize google sheets
  sheets_deauth()
  
  ## Apply login DB
  res_auth <- secure_server(
    check_credentials = check_credentials("database.sqlite")
  )

  tab <- newtab <- ""

  output$tab1 <- renderDataTable({
    
    tab <- readxl::read_xlsx('Example.xlsx')

    tab$Occurrence <- as.Date(tab$Occurrence)
    tab$Confirm <- as.Date(tab$Confirm)
    tab$Date <- as.Date(tab$Date)
    tab$S <- as.factor(tab$S)
    #tab$D <- as.factor(tab$D)
    
    TRIS <- sapply(1:nrow(tab), function(i) {
      triage(tab[i, ])
    })
    rownames(TRIS) = c('PT','PCF','PCO','PM','TRI')
    
    tab = tab %>% cbind(t(TRIS))
    
    # hide file upload
    shinyjs::runjs('$(".form-group.shiny-input-container").hide()')

    tab <<- tab

    newtab <<- tab %>%
      filter(Date == max(Date)) %>% # recent day
      select(Place, Name, S, A, -City, -Town, -Occurrence, -Confirm, PT, PCF, PCO, PM, TRI)

    ########################
    # MERGING COLUMN  : 8  #
    # ORDER COLUMN    : 8  #
    ########################
    
    dtobj <- datatable(
      newtab,
      escape = FALSE,
      caption = "전체 환자: 시설",
      options = list(
        #rowCallback = styleDT(3, 4, 5, 6, 7, 8, 9, 11),
        dom = "tip",
        rowsGroup = list(8), # TRIAGE
        order = list(list(8, "desc"))
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
    tt <- thisTab <- tab %>% filter(Name == newtab$Name[selected])
    
    # print(head(thisTab))
    
    output$pat <- renderText({
      txt = paste0(
        HTML('<i class = "material-icons" style= "font-size : 2.5rem">face</i> '), # icon
        thisTab$Name[1], " / ", # name
        thisTab$S[1], " / ", # sex
        "Confirmed in ", thisTab$Confirm[1], " / ", # confirmed
        thisTab$Town[1], " / ", # town
        thisTab$Place[1], ' / '
      )
      if(thisTab$TRI[1]==3) txt = paste0(txt,'상급병상우선')
      if(thisTab$TRI[1]==2) txt = paste0(txt,'병상우선')
      if(thisTab$TRI[1]<=1) txt = paste0(txt,'가정')
      txt
    })

    output$tab2 <- renderDataTable({
      
      thisTab = thisTab %>% select(-City, -Town, -Place, -Occurrence, -Confirm, -Name, -S, -A, -PT, -PCF, -PCO, -PM)
      minus = c()
      for(i in 3:ncol(thisTab)){
        if(sum(thisTab[,i])==0) minus= c(minus, i)
        thisTab[which(thisTab[,i]=='TRUE'),i] = 'T'
        thisTab[which(thisTab[,i]=='FALSE'),i] = 'F'
      }
      
      thisTab = thisTab[,-minus]
      
      TRIIDX = which(colnames(thisTab)=='TRI') - 1
      DIDX = which(colnames(thisTab)=='D') - 1
      
      dtobj <-
        datatable(
          thisTab,
          rownames = FALSE,
          selection = "none",
          options = list(
            rowsGroup = list(DIDX), # Disease
            #rowCallback = styleDT(0, 1, 2, 3, 4, 5, 6, 8),
            dom = "tip",
            autoWidth = FALSE,
            order = list(list(TRIIDX, "desc"))
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
      thisTab = tt
      highchart() %>%
        hc_xAxis(type = "datetime", title = list(text = "Day")) %>%
        hc_yAxis_multiples(
          list(top = "0%", height = "20%", title = list(text = "TRI"), lineWidth = 3),
          list(top = "20%", height = "20%", title = list(text = "T"), showFirstLabel = T, showLastLabel = T, opposite = T),
          list(top = "40%", height = "20%", title = list(text = "O"), showFirstLabel = T, showLastLabel = T),
          list(top = "60%", height = "20%", title = list(text = "BC"), showFirstLabel = T, showLastLabel = T, opposite = T),
          list(top = "80%", height = "20%", title = list(text = "P"), showFirstLabel = T, showLastLabel = T)
        ) %>%
        hc_add_series(getColor(thisTab, "TRI"), "line", hcaes(Date, y, color = color), name = "TRI", marker = list(radius = 8)) %>%
        hc_add_series(getColor(thisTab, "T"), "line", hcaes(Date, y, color = color), name = "T", marker = list(radius = 8), yAxis = 1) %>%
        hc_add_series(getColor(thisTab, "O"), "line", hcaes(Date, y, color = color), name = "O", marker = list(radius = 8), yAxis = 2) %>%
        hc_add_series(getColor(thisTab, "BC"), "line", hcaes(Date, y, color = color), name = "BC", marker = list(radius = 8), yAxis = 3) %>%
        hc_add_series(getColor(thisTab, "P"), "line", hcaes(Date, y, color = color), name = "P", marker = list(radius = 8), yAxis = 4) %>%
        hc_exporting(enabled = T) %>%
        hc_tooltip(valueDecimals = 1, shared = T, crosshairs = T)
    })
  })
  
  
  gtab = read_sheet("http://docs.google.com/spreadsheets/d/188LunvsxTa2zqudNwAVDj-cuVpV5jm4y-3LTaj-azQE/edit?usp=sharing")
  names(gtab) <- c("res_time", "name", "birth", "initial_res_yn", "gender", "age", "basis_sick", "temperature", "breathe_hard_yn", "breathe_cnt", "pulse_cnt", "oxygen","bloodpressure")
  gtab <- gtab %>% mutate(res_time = as_date(gtab$res_time), birth = as_date(gtab$birth))
  
  output$tab_google = renderDataTable(
    datatable(
      gtab,
      escape = FALSE,
      #caption = "전체 환자: 시설",
      options = list(
        #rowCallback = styleDT(3, 4, 5, 6, 7, 8, 9, 11),
        dom = "tip"#,
        #rowsGroup = list(8), # TRIAGE
        #order = list(list(8, "desc"))
      ),
      #selection = "single",
      filter = "top",
      rownames = FALSE
    )
  )
  
  observeEvent(input$dateapply,{
    if(input$dateapply==0){return(NULL)}
    
    asdate = function(v){
      v = strsplit(v,' ')[[1]]
      
      month = v[1]
      day = strsplit(v[2],',')[[1]][1]
      year = as.character(v[3])
      
      if(month =='Jan'){month = 1}
      if(month =='Feb'){month = 2}
      if(month =='Mar'){month = 3}
      if(month =='Apr'){month = 4}
      if(month =='May'){month = 5}
      if(month =='Jun'){month = 6}
      if(month =='Jul'){month = 7}
      if(month =='Aug'){month = 8}
      if(month =='Sep'){month = 9}
      if(month =='Oct'){month = 10}
      if(month =='Nov'){month = 11}
      if(month =='Dec'){month = 12}
      month = as.character(month)
      
      return(as.Date.character(paste0(year,'-',month,'-',day)))
    }
    date1 = asdate(input$date1)
    date2 = asdate(input$date2)
    
    output$tab_google = renderDataTable(
      datatable(
        gtab %>% filter(res_time >= date1) %>% filter(res_time <= date2),
        escape = FALSE,
        #caption = "전체 환자: 시설",
        options = list(
          #rowCallback = styleDT(3, 4, 5, 6, 7, 8, 9, 11),
          dom = "tip"#,
          #rowsGroup = list(8), # TRIAGE
          #order = list(list(8, "desc"))
        ),
        #selection = "single",
        filter = "top",
        rownames = FALSE
      )
    )
  })
  
  
  
}

shinyApp(ui = ui(), server = server, options = list(launch.browser = TRUE))
