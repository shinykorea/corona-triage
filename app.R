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

# important -------------------------------- must be development version
# devtools::install_github('r-lib/gargle')
library(gargle) # googlesheets encoding,

library(lubridate) # date handling
library(tibble)

## Make login DB

# credentials <- data.frame(
#  user = c("admin", "user1"),
#  password = c("admin", "user1"),
#  admin = c(T, F),
#  stringsAsFactors = FALSE)

# create_db(credentials_data = credentials, sqlite_path = "database.sqlite")

material_infobox <- function(width, offset = 0, contents, Infotitle, Cardcolor, boxid, hover = TRUE, hide = FALSE) {
  title <- HTML(paste0("<span style='font-weight:bold; font-size:1.2vw;margin:auto;color:#FFF;'>", Infotitle, "</span>&nbsp;&nbsp;")) # Main Title with white color
  
  box <- shiny::tags$div(
    class = "card z-depth-3",
    shiny::tags$div(
      class = "card-content action-button",
      id = boxid,
      style = paste0("background-color:", Cardcolor, "; padding : 1em; ", ifelse(hover, "cursor:pointer;", ""), ifelse(hide, "display:none;", "")),
      shiny::tags$span(class = "card-title", style = "text-align:center", title),
      shiny::tags$div(
        HTML(paste0("<div style = 'text-align:center;'><span style='font-size:1.2vw; color:#FFF;'>", contents, "</span></div>"))
      )
    )
  )
  
  material_column(
    width = width,
    offset = offset,
    box
  )
}

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
  PT <- PCO <- 0
  POT <- 0
  if (is.na(v$호흡곤란)) {
    PCF <- ""
    # NO POT ADD
  }
  else {
    PCF <- v$호흡곤란
    POT <- POT + PCF
  }
  
  if (is.na(v$가벼운불안)) {
    PM <- ""
    # NO POT ADD
  }
  else {
    PM <- v$가벼운불안
    POT <- POT + PM
  }
  
  ##### PT
  
  T <- v$체온
  if (!is.na(T)) {
    if (T <= 35) PT <- 3
    if (T <= 36 || T > 39) PT <- max(2, PT)
    if (T > 38) PT <- max(1, PT)
    POT <- POT + PT
  }
  else {
    PT <- ""
    # NO POT ADD
  }
  
  ##### PCO
  
  CO <- v$의식저하
  if (is.na(CO)) {
    PCO <- ""
    # NO POT ADD
  }
  else {
    if (CO) PCO <- 3
    POT <- POT + PCO
  }
  
  return(c(PT, PCF, PCO, PM, POT))
}

ui <- function() {
  material_page(
    nav_bar_color = "deep-purple lighten-1",
    color = "#311b92",
    title = paste0(
      "G-CoMS 확진자 건강관리 시스템<a href = 'https://github.com/shinykorea/corona-triage' target='_blank'> ",
      "<i class='material-icons' style = 'font-size:1.3em;'> info </i> </a>"
    ),
    useShinyjs(),
    tags$head(tags$style(type = "text/css", "table.dataTable tr.selected td, table.dataTable td.selected {background-color: #d1c4e9 !important;}")),
    
    tags$head(HTML("<title>G-CoMS 확진자 건강관리 시스템</title>")),
    
    ## Change Font Here ---------------------------------------------------
    tags$head(includeCSS("www/includeGGfont.css")),
    tags$head(includeCSS("www/customcss.css")),
    
    material_tabs(
      tabs = c(
        "생활치료센터" = "facility" # ,
        # "가정" = "home" # ,
        # "구글시트예시" = 'google'
      ),
      color = "#311b92"
    ),
    
    # Define Pat content
    material_tab_content(
      tab_id = "facility",
      # navigator
      div(
        material_column(
          myButton(inputId = "toinfo", label = "요약", onClick = 'location.href = "#infoboxGroup"'),
          myButton(inputId = "totab1", label = "전체환자", onClick = 'location.href = "#tab1"'),
          myButton(inputId = "totab2", label = "선택환자", onClick = 'location.href = "#tab2"'),
          myButton(inputId = "toimg", label = "시간별", onClick = 'location.href = "#img"'),
        ),
        id = "home_navigator",
        style = "position:fixed;bottom:3em;width:100%;z-index:999;"
      ),
      material_row(
        div(style = "height:1em"),
        uiOutput(outputId = "infoboxGroup"),
        material_infobox(
          width = 2, offset = 5,
          contents = "",
          Infotitle = "테이블 리셋",
          Cardcolor = "#000000",
          boxid = "resetBox", hide = TRUE
        ) # reset
      ),
      material_row(
        material_column(
          width = 12,
          material_card(
            depth = 3,
            title = "",
            DT::dataTableOutput("tab1"),
            actionButton(inputId = "unorder", label = "정렬 해제", style = "right: -90%; position: relative; margin-top: 1em; display:none;")
          )
        ),
      ),
      material_row(
        height = "100%",
        material_column(
          material_card(
            title = htmlOutput("pat", style = "text-align:center"),
            divider = TRUE,
            DT::dataTableOutput("tab0"),
            div(class = "divider", style = "margin-top:3em;"),
            DT::dataTableOutput("tab2")
          ),
          width = 12
        ),
        material_column(
          material_card(
            title = HTML(paste0(
              "시간별 지수 변화, ",
              '<span style = "background-color:#ff6363">　</span> : 3 ',
              '<span style = "background-color:#ff9d9d">　</span> : 2 ',
              '<span style = "background-color:#35a4c6">　</span> : 1 ',
              '<span style = "background-color:#35a4c6">　</span> : 0'
            )),
            divider = TRUE,
            highchartOutput("img", height = "800px"),
            depth = 3
          ),
          width = 12
        )
      ),
      div(style = "height:3em;")
    ),
    # material_tab_content(
    # tab_id = "home",
    # material_row(
    # div(style = "height:1em"),
    # uiOutput(outputId = "infoboxGroup2")
    # ),
    # material_row(
    # material_column(
    # width = 12,
    # material_card(
    # title = "",
    # depth = 3,
    # DT::dataTableOutput("tab_google"),
    # )
    # )
    # ),
    
    # material_row(
    # height = "100%",
    # material_column(
    # material_card(
    # title = htmlOutput("pat2", style = "text-align:center"),
    # divider = TRUE,
    # DT::dataTableOutput("tab_google2")
    # ),
    # width = 12
    # ),
    
    # material_column(
    # material_card(
    # title = HTML(paste0(
    # "시간별 지수 변화, ",
    #' <span style = "background-color:#ff6363">　</span> : 3 ',
    #' <span style = "background-color:#f6a21c">　</span> : 2 ',
    #' <span style = "background-color:#ffed82">　</span> : 1 ',
    #' <span style = "background-color:#35bdbb">　</span> : 0'
    # )),
    # highchartOutput("img2", height = "600px"),
    # depth = 3
    # ),
    # width = 12
    # )
    # )
    # )
  )
}

# Wrap your UI with secure_app
ui <- secure_app(ui(), enable_admin = T)  #개발땐 생략, 배포시 적용

getColor <- function(Data, Type) {
  col1 <- "#35a4c6" # yellow
  col2 <- "#ff9d9d" # orange
  col3 <- "#ff6363" # red
  colBasic <- "#35a4c6" # emerald
  
  if (Type == "중증도") {
    res <-
      sapply(Data[[Type]], function(i) {
        v <- colBasic
        if (is.na(i)) {
          return(colBasic)
        }
        if (i == 1) v <- col1
        if (i == 2) v <- col2
        if (i >= 3) v <- col3
        return(v)
      }, USE.NAMES = FALSE)
  }
  if (Type == "체온") {
    res <-
      sapply(Data[[Type]], function(T) {
        if (is.na(T)) {
          return(colBasic)
        }
        if (T <= 35) {
          return(col3)
        }
        if (T <= 36 || T > 39) {
          return(col2)
        }
        if (T >= 38) {
          return(col1)
        }
        return(colBasic)
      }, USE.NAMES = FALSE)
  }
  if (Type == "산소포화도") {
    res <-
      sapply(Data[[Type]], function(O) {
        if (is.na(O)) {
          return(colBasic)
        }
        if (O <= 93) {
          return(col3)
        }
        if (O <= 95) {
          return(col2)
        }
        return(colBasic)
      }, USE.NAMES = FALSE)
  }
  if (Type == "호흡수") {
    res <-
      sapply(Data[[Type]], function(BC) {
        if (is.na(BC)) {
          return(colBasic)
        }
        if (BC >= 25 || BC <= 8) {
          return(col3)
        }
        if (BC >= 21 || BC <= 11) {
          return(col2)
        }
        return(colBasic)
      }, USE.NAMES = FALSE)
  }
  if (Type == "맥박") {
    res <-
      sapply(Data[[Type]], function(P) {
        if (is.na(P)) {
          return(colBasic)
        }
        if (P > 110 || P <= 40) {
          return(col3)
        }
        if (P >= 100) {
          return(col2)
        }
        if (P >= 101 || P <= 110) {
          return(col2)
        }
        return(colBasic)
      }, USE.NAMES = FALSE)
  }
  
  
  return(data.frame(날짜 = Data[["날짜"]], y = Data[[Type]], color = res))
}

styleDT <- function(point, change) {
  col2 <- "#ff9d9d" # orange
  col3 <- "#ff6363" # red
  col4 <- "#91c320" # green
  JS(paste0("function(row, data, index){
            // Point
            if(data[", point, "] == 2 ){$(row).find('td:eq(", point, ")').css({'background-color' : '", col2, "'});}
            if(data[", point, "] >= 3 ){$(row).find('td:eq(", point, ")').css({'background-color' : '", col3, "',});}
            
            // change
            if(data[", change, "] == '-' ){$(row).find('td:eq(", change, ")').css({'color' : '", col4, "'});}
            if(data[", change, "] == '+' ){$(row).find('td:eq(", change, ")').css({'color' : '", col3, "',});}
        }"))
}

styleDT2 <- function(point) {
  temp <- 4
  mental <- 5
  anxiety <- 6
  dyspnea <- 7
  col2 <- "#ff9d9d" # orange
  col3 <- "#ff6363" # red
  col4 <- "#ffaaa5" #
  JS(paste0("function(row, data, index){
            // Point
            if(data[", point, "] == 2 ){$(row).find('td:eq(", point, ")').css({'background-color' : '", col2, "'});}
            if(data[", point, "] >= 3 ){$(row).find('td:eq(", point, ")').css({'background-color' : '", col3, "',});}
            
            // temp
            if(data[", temp, "] == null ){$(row).find('td:eq(", temp, ")').css({'background-color' : '", col4, "',});} 
            
            // mental
            if(data[", mental, "].length == 0 ){$(row).find('td:eq(", mental, ")').css({'background-color' : '", col4, "',});} 
            
            // anxiety
            if(data[", anxiety, "].length == 0 ){$(row).find('td:eq(", anxiety, ")').css({'background-color' : '", col4, "',});} 
            
            // dyspnea
            if(data[", dyspnea, "].length == 0 ){$(row).find('td:eq(", dyspnea, ")').css({'background-color' : '", col4, "',});} 
            
        }"))
}

asDate <- function(i) {
  i <- strsplit(as.character(i), "")[[1]]
  as.Date(paste0(
    paste0(i[1:4], collapse = ""), "-",
    paste0(i[5:6], collapse = ""), "-",
    paste0(i[7:8], collapse = "")
  ), origin = "1970-01-01")
}

readSurvey <- function() {
  sheets_auth("") # hide.
  Link <- "" # hide.
  Survey <- read_sheet(Link)
  
  colnames(Survey) <- c(
    "시간", "목적", "이름", "주민등록번호", "확진일자",
    "입소일자", "센터", "보건소", "60세", "24개월",
    "개월", "보호자", "기저질병", "초기산소", "독립생활",
    "거주지", "고위험군동거"
  )
  return(data.frame(Survey))
}

readPat <- function() {
  sheets_auth("") # hide.
  Link <- "" # hide.
  
  sheets <- sheets_sheets(Link)
  sheets <- sheets[which(lubridate::as_date(sheets) <= Sys.Date())]
  
  Pat <- c()
  withProgress(
    message = "데이터 읽는 중 (용인)",
    for (i in 1:length(sheets)) {
      incProgress(1 / length(sheets), detail = paste0(sheets[i], " 시트 읽는 중"))
      PatTemp <- read_sheet(Link, sheet = sheets[i]) # first sheets
      PatTemp$temperature <- as.numeric(unlist(PatTemp$temperature))
      PatTemp$mental <- as.numeric(unlist(PatTemp$mental))
      PatTemp$anxiety <- as.numeric(unlist(PatTemp$anxiety))
      PatTemp$dyspnea <- as.numeric(unlist(PatTemp$dyspnea))
      PatTemp$sao2 <- as.numeric(unlist(PatTemp$sao2))
      PatTemp$HR <- as.numeric(unlist(PatTemp$HR))
      PatTemp$PCR <- as.numeric(unlist(PatTemp$PCR))
      Pat <- rbind(Pat, PatTemp)
    }
  )
  
  colnames(Pat) <- c(
    "주민등록번호", "이름", "체온", "의식저하", "가벼운불안",
    "호흡곤란", "산소포화도", "호흡수", "맥박", "PCR",
    "퇴원여부", "센터", "입력날짜", "차수"
  )
  
  today <- Sys.Date()
  
  Age <- sapply(1:nrow(Pat), function(i) {
    # Year
    if (substr(Pat$주민등록번호[i], 7, 7) > 2) { ## after 2000
      day <- (today - as.Date(paste0("20", substr(Pat$주민등록번호[i], 1, 6)), "%Y%m%d"))[[1]]
    }
    else { # before 2000
      day <- (today - as.Date(paste0("19", substr(Pat$주민등록번호[i], 1, 6)), "%Y%m%d"))[[1]]
    }
    floor(day / 365)
  })
  
  ### 중증도 계산 --------------------------------
  
  TRIS <- sapply(1:nrow(Pat), function(i) {
    Pat[i, ] %>%
      select(체온, 호흡곤란, 의식저하, 가벼운불안) %>%
      triage()
  })
  rownames(TRIS) <- c("체온지수", "심폐지수", "의식지수", "심리지수", "중증도")
  TRIS <- data.frame(t(TRIS), stringsAsFactors = FALSE)
  
  Pat <- Pat %>% cbind(나이 = Age)
  Pat <- Pat %>% cbind(TRIS)
  Pat$중증도 <- as.numeric(Pat$중증도)
  
  ## 날짜 decompose ---------------
  
  DATE <- sapply(1:nrow(Pat), function(i) {
    if (Pat$차수[i] == 1) {
      return(paste0(Pat$입력날짜[i], " 1차"))
    }
    return(paste0(Pat$입력날짜[i], " 2차"))
  }, USE.NAMES = FALSE)
  
  Pat <- Pat %>%
    cbind(DATE) %>%
    rename(날짜 = DATE)
  
  Pat$날짜 <- as.character(Pat$날짜)
  
  # 생년월일
  
  Birth <- sapply(Pat$주민등록번호, function(i) {
    (i - i %% 10000000) / 10000000
  })
  
  Sex <- sapply(Pat$주민등록번호, function(i) {
    i <- i %% 10000000
    i <- (i - i %% 1000000) / 1000000
    ifelse(i %% 2 == 1, "남", "여")
  })
  
  Pat <- Pat %>%
    cbind(Birth) %>%
    rename(생년월일 = Birth) %>%
    cbind(Sex) %>%
    rename(성별 = Sex)
  
  return(data.frame(Pat))
}

readPat2 <- function() {
  sheets_auth("") # hide.
  Link <- "" # hide.
  
  sheets <- sheets_sheets(Link)
  sheets <- sheets[which(lubridate::as_date(sheets) <= Sys.Date())]
  
  Pat <- c()
  
  withProgress(
    message = "데이터 읽는 중 (이천)",
    for (i in 1:length(sheets)) {
      incProgress(1 / length(sheets), detail = paste0(sheets[i], " 시트 읽는 중"))
      PatTemp <- read_sheet(Link, sheet = sheets[i]) # first sheets
      PatTemp$temperature <- as.numeric(unlist(PatTemp$temperature))
      PatTemp$mental <- as.numeric(unlist(PatTemp$mental))
      PatTemp$anxiety <- as.numeric(unlist(PatTemp$anxiety))
      PatTemp$dyspnea <- as.numeric(unlist(PatTemp$dyspnea))
      PatTemp$sao2 <- as.numeric(unlist(PatTemp$sao2))
      PatTemp$HR <- as.numeric(unlist(PatTemp$HR))
      PatTemp$PCR <- as.numeric(unlist(PatTemp$PCR))
      Pat <- rbind(Pat, PatTemp)
    }
  )
  
  colnames(Pat) <- c(
    "주민등록번호", "이름", "체온", "의식저하", "가벼운불안",
    "호흡곤란", "산소포화도", "호흡수", "맥박", "PCR",
    "퇴원여부", "센터", "입력날짜", "차수"
  )
  
  today <- Sys.Date()
  
  Age <- sapply(1:nrow(Pat), function(i) {
    # Year
    if (substr(Pat$주민등록번호[i], 7, 7) > 2) { ## after 2000
      day <- (today - as.Date(paste0("20", substr(Pat$주민등록번호[i], 1, 6)), "%Y%m%d"))[[1]]
    }
    else { # before 2000
      day <- (today - as.Date(paste0("19", substr(Pat$주민등록번호[i], 1, 6)), "%Y%m%d"))[[1]]
    }
    floor(day / 365)
  })
  
  ### 중증도 계산 --------------------------------
  
  TRIS <- sapply(1:nrow(Pat), function(i) {
    Pat[i, ] %>%
      select(체온, 호흡곤란, 의식저하, 가벼운불안) %>%
      triage()
  })
  rownames(TRIS) <- c("체온지수", "심폐지수", "의식지수", "심리지수", "중증도")
  TRIS <- data.frame(t(TRIS), stringsAsFactors = FALSE)
  
  Pat <- Pat %>% cbind(나이 = Age)
  Pat <- Pat %>% cbind(TRIS)
  Pat$중증도 <- as.numeric(Pat$중증도)
  
  ## 날짜 decompose ---------------
  
  DATE <- sapply(1:nrow(Pat), function(i) {
    if (Pat$차수[i] == 1) {
      return(paste0(Pat$입력날짜[i], " 1차"))
    }
    return(paste0(Pat$입력날짜[i], " 2차"))
  }, USE.NAMES = FALSE)
  
  Pat <- Pat %>%
    cbind(DATE) %>%
    rename(날짜 = DATE)
  
  Pat$날짜 <- as.character(Pat$날짜)
  
  # 생년월일
  
  Birth <- sapply(Pat$주민등록번호, function(i) {
    (i - i %% 10000000) / 10000000
  })
  
  Sex <- sapply(Pat$주민등록번호, function(i) {
    i <- i %% 10000000
    i <- (i - i %% 1000000) / 1000000
    ifelse(i %% 2 == 1, "남", "여")
  })
  
  Pat <- Pat %>%
    cbind(Birth) %>%
    rename(생년월일 = Birth) %>%
    cbind(Sex) %>%
    rename(성별 = Sex)
  
  return(data.frame(Pat))
}

server <- function(input, output, session) {
  
  # off scientific notation ( 주민번호 )
  options(scipen = 999)
  
  ## Apply login DB
  res_auth <- secure_server(
    check_credentials = check_credentials("database.sqlite"),
    timeout = 60 * 24 * 30
  )
  
  newtab <- ""
  
  #Pat <- reactive(readPat())
  #Pat <- reactive(rbind(Pat(), readPat2()))
  #Survey <- reactive(readSurvey())
  
  Pat <- reactive({
    rbind(readPat(), readPat2()) %>% 
      inner_join(readSurvey(), by = c("주민등록번호", "센터", "이름"))
  })
  
  output$tab1 <- renderDataTable({
    
    ## 증감 계산 -------------------------------------------------------------
    
    discharged <- Pat() %>%
      filter(!is.na(퇴원여부)) %>%
      select(이름) %>%
      unlist(use.names = FALSE)
    
    newtab <<- Pat() %>%
      group_by(이름) %>%
      filter(!is.na(체온)) %>%
      filter(!is.na(의식저하)) %>%
      filter(!is.na(가벼운불안)) %>%
      filter(!is.na(호흡곤란)) %>%
      filter(!이름 %in% discharged) %>%
      filter(날짜 == max(날짜)) %>% # recent data
      select(주민등록번호, 이름, 성별, 나이, 센터, 체온지수, 의식지수, 심리지수, 심폐지수, 중증도, 날짜)
    
    temp <- Pat() %>%
      group_by(이름) %>%
      top_n(2, wt = 날짜) %>%
      select(이름, 날짜, 중증도)
    
    newtab$이름 <- as.character(newtab$이름)
    
    change <- sapply(unique(temp$이름), function(i) {
      k <- temp %>% filter(이름 == i)
      if (nrow(k) == 1) {
        return(".")
      } # 변화없음
      if (k$중증도[2] == k$중증도[1]) {
        return(".")
      } # 변화없음
      if (k$중증도[2] > k$중증도[1]) {
        return("+")
      } # 증가
      return("-") # 감소
    })
    
    temp <- data.frame(이름 = names(change), 증감 = change, stringsAsFactors = FALSE, row.names = NULL)
    
    newtab <- newtab %>% inner_join(temp)
    newtab <- newtab %>% select(-날짜)
    newtab <<- newtab
    
    rm(temp)
    
    ########################
    # MERGING COLUMN  : 8  #
    # ORDER COLUMN    : 8  #
    ########################
    
    newtab$이름 <- as.factor(newtab$이름)
    newtab$센터 <- as.factor(newtab$센터)
    newtab$증감 <- as.factor(newtab$증감)
    
    output$infoboxGroup <- renderUI({
      higher <- Pat() %>%
        group_by(이름) %>%
        filter(날짜 == max(날짜)) %>%
        filter(중증도 >= 3) %>%
        nrow()
      
      pat <- Pat() %>%
        group_by(이름) %>%
        filter(날짜 == max(날짜)) %>%
        filter(중증도 == 2) %>%
        nrow()
      
      lastTime1 <- Pat() %>%
        filter(센터 != "용인") %>%
        filter(!is.na(체온)) %>%
        filter(!is.na(의식저하)) %>%
        filter(!is.na(가벼운불안)) %>%
        filter(!is.na(호흡곤란)) %>%
        filter(날짜 == max(날짜)) %>%
        select(날짜)
      
      lastTime2 <- Pat() %>%
        filter(센터 == "용인") %>%
        filter(!is.na(체온)) %>%
        filter(!is.na(의식저하)) %>%
        filter(!is.na(가벼운불안)) %>%
        filter(!is.na(호흡곤란)) %>%
        filter(날짜 == max(날짜)) %>%
        select(날짜)
      
      if (nrow(lastTime1) == 0) {
        lastTime1 <- "데이터 없음"
      }
      else {
        lastTime1 <- lastTime1[, 1]
        lastTime1 <- strsplit(lastTime1, "")[[1]]
        lastTime1 <- paste0(
          as.numeric(paste0(lastTime1[5:6], collapse = "")), "월 ", # 03 -> 3, 10 -> 10
          paste0(lastTime1[7:8], collapse = ""), "일 ",
          paste0(lastTime1[10:11], collapse = "") # 1차, 2차
        )
      }
      
      if (nrow(lastTime2) == 0) {
        lastTime2 <- "데이터 없음"
      }
      else {
        lastTime2 <- lastTime2[, 1]
        lastTime2 <- strsplit(lastTime2, "")[[1]]
        lastTime2 <- paste0(
          as.numeric(paste0(lastTime2[5:6], collapse = "")), "월 ", # 03 -> 3, 10 -> 10
          paste0(lastTime2[7:8], collapse = ""), "일 ",
          paste0(lastTime2[10:11], collapse = "") # 1차, 2차
        )
      }
      
      tagList(
        material_infobox(
          width = 2, offset = 2,
          contents = paste0(higher, "명"),
          Infotitle = "상급의료기관 배정 필요",
          Cardcolor = "#ff6363",
          boxid = "higherBox"
        ), # pink
        material_infobox(
          width = 2, contents = paste0(pat, "명"),
          Infotitle = "의료기관 배정 필요",
          Cardcolor = "#ff9d9d",
          boxid = "patBox"
        ), # sky
        material_infobox(
          width = 2, contents = lastTime2,
          Infotitle = "업데이트시간(용인)",
          Cardcolor = "#35a4c6",
          boxid = "timeBox1"
        ), # green
        
        material_infobox(
          width = 2, contents = lastTime1,
          Infotitle = "업데이트시간(기타)",
          Cardcolor = "#35a4c6",
          boxid = "timeBox2"
        ) # purple
      )
    })
    
    shinyjs::show("unorder")
    
    dtobj <- datatable(
      newtab,
      escape = FALSE,
      options = list(
        # styleDT : 체온지수, 심폐지수, 의식지수, 심리지수, 중증도, 증감의 인덱스 - 1
        rowCallback = styleDT(9, 10),
        dom = "tip",
        order = list(list(9, "desc")),
        pageLength = 50
      ),
      
      selection = "single",
      # filter = "top",
      rownames = FALSE
    )
    dtobj
  })
  
  observeEvent(input$timeBox1, {
    output$tab1 <- renderDataTable(
      datatable(
        newtab %>% filter(센터 == "용인"),
        escape = FALSE,
        options = list(
          # styleDT : 체온지수, 심폐지수, 의식지수, 심리지수, 중증도, 증감의 인덱스 - 1
          rowCallback = styleDT(9, 10),
          dom = "tip",
          order = list(list(9, "desc")),
          pageLength = 50
        ),
        selection = "single",
        # filter = "top",
        rownames = FALSE
      )
    )
    shinyjs::show("resetBox")
    shinyjs::hide("unorder")
  })
  
  observeEvent(input$timeBox2, {
    output$tab1 <- renderDataTable(
      datatable(
        newtab %>% filter(센터 == "이천"),
        escape = FALSE,
        options = list(
          # styleDT : 체온지수, 심폐지수, 의식지수, 심리지수, 중증도, 증감의 인덱스 - 1
          rowCallback = styleDT(9, 10),
          dom = "tip",
          order = list(list(9, "desc")),
          pageLength = 50
        ),
        selection = "single",
        # filter = "top",
        rownames = FALSE
      )
    )
    shinyjs::show("resetBox")
    shinyjs::hide("unorder")
  })
  
  observeEvent(input$unorder, {
    output$tab1 <- renderDataTable(
      datatable(
        newtab,
        escape = FALSE,
        options = list(
          # styleDT : 체온지수, 심폐지수, 의식지수, 심리지수, 중증도, 증감의 인덱스 - 1
          rowCallback = styleDT(9, 10),
          dom = "tip",
          # order = list(list(9, "desc")),
          pageLength = 50
        ),
        selection = "single",
        # filter = "top",
        rownames = FALSE
      )
    )
  }) # 정렬 해제
  
  observeEvent(input$resetBox, { # 초기화
    
    output$tab1 <- renderDataTable(
      datatable(
        newtab,
        escape = FALSE,
        options = list(
          # styleDT : 체온지수, 심폐지수, 의식지수, 심리지수, 중증도, 증감의 인덱스 - 1
          rowCallback = styleDT(9, 10),
          dom = "tip",
          order = list(list(9, "desc")),
          pageLength = 50
        ),
        selection = "single",
        # filter = "top",
        rownames = FALSE
      )
    )
    shinyjs::hide("resetBox")
    shinyjs::show("unorder")
  })
  
  observeEvent(input$higherBox, { # 중증도 3
    shinyjs::show("resetBox")
    shinyjs::hide("unorder")
    output$tab1 <- renderDataTable({
      datatable(
        newtab %>% filter(중증도 >= 3),
        escape = FALSE,
        options = list(
          # styleDT : 체온지수, 심폐지수, 의식지수, 심리지수, 중증도, 증감의 인덱스 - 1
          rowCallback = styleDT(9, 10),
          dom = "tip",
          order = list(list(9, "desc")),
          pageLength = 50
        ),
        selection = "single",
        # filter = "top",
        rownames = FALSE
      )
    })
  })
  
  observeEvent(input$patBox, { # 중증도 2
    shinyjs::show("resetBox")
    shinyjs::hide("unorder")
    output$tab1 <- renderDataTable({
      datatable(
        newtab %>% filter(중증도 == 2),
        escape = FALSE,
        options = list(
          # styleDT : 체온지수, 심폐지수, 의식지수, 심리지수, 중증도, 증감의 인덱스 - 1
          rowCallback = styleDT(9, 10),
          dom = "tip",
          order = list(list(9, "desc")),
          pageLength = 50
        ),
        selection = "single",
        # filter = "top",
        rownames = FALSE
      )
    })
  })
  
  # specific table -------------------------------------------------------------
  observeEvent(input$tab1_rows_selected, {
    selected <- input$tab1_rows_selected # check none selected
    tt <- thisTab <- Pat() %>%
      filter(이름 == newtab$이름[selected]) %>%
      arrange(desc(날짜))
    
    # specific table title ----------------------------------------------------
    output$pat <- renderText({
      txt <- paste0(
        HTML('<i class = "material-icons" style= "font-size : 2.5rem">face</i> '), # icon
        thisTab$이름[1], " / ",
        thisTab$성별[1], " / ",
        thisTab$나이[1], " / ",
        thisTab$센터[1], "센터 / "
      )
      
      if (thisTab$중증도[1] >= 3) txt <- paste0(txt, "상급의료기관 배정 필요")
      if (thisTab$중증도[1] == 2) txt <- paste0(txt, "의료기관 배정 필요")
      if (thisTab$중증도[1] <= 1) txt <- paste0(txt, "생활치료센터 유지")
      
      HTML(txt)
    })
    
    output$tab0 <- renderDataTable({
      tabzero <- thisTab %>% select(확진일자, 입소일자, 보건소, X24개월, 개월, 보호자, 기저질병, 독립생활, 거주지, 고위험군동거)
      colnames(tabzero) <- c(
        "확진일자", "입소일자", "실거주지 보건소", "(소아환자) 24개월 미만여부", "(소아환자) 개월 수",
        "(소아환자) 보호자 유무", "기저질환",
        "가정 내 독립생활 가능 여부", "적절한 거주지 유무", "고위험군과의 동거 여부"
      )
      tabzero <- tabzero[1, ]
      tabzero[1] <- lubridate::as_date(tabzero[[1]][1][[1]])
      tabzero[2] <- lubridate::as_date(tabzero[[2]][1][[1]])
      
      datatable(
        tabzero[1, ],
        rownames = FALSE,
        selection = "none",
        options = list(
          dom = "tip",
          autoWidth = FALSE,
          pageLength = 50,
          bInfo = FALSE,
          bPaginate = FALSE
        )
      )
    })
    
    # specific table content -------------------------------------------------
    output$tab2 <- renderDataTable({
      thisTab <- thisTab %>%
        select(주민등록번호, 입력날짜, 차수, 중증도, PCR, 체온, 의식지수, 심리지수, 심폐지수, 호흡곤란, 산소포화도, 호흡수, 맥박) %>%
        inner_join(newtab %>% select(주민등록번호)) %>%
        select(-주민등록번호, -이름)
      
      TRIIDX <- which(colnames(thisTab) == "중증도") - 1
      
      dtobj <-
        datatable(
          thisTab,
          rownames = FALSE,
          escape = FALSE,
          selection = "none",
          options = list(
            rowCallback = styleDT2(7),
            dom = "tip",
            autoWidth = FALSE,
            # order = list(list(TRIIDX, "desc")),
            pageLength = 50
          )
        ) %>% formatRound(columns = "체온", digits = 1)
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
      thisTab <- tt
      
      
      thisTab$날짜 <- thisTab$날짜
      #  datetime_to_timestamp(lubridate::ymd_hm(thisTab$날짜))
      
      thisTab <- thisTab %>% arrange(날짜)
      
      highchart() %>%
        hc_xAxis(
          type = "category",
          title = list(text = "일", style = list(fontSize = "20px")),
          labels = list(
            format = "{value}",
            # text = thisTab$날짜,
            style = list(fontSize = "20px")
          )
        ) %>%
        hc_yAxis_multiples(
          list(
            top = "0%", height = "50%",
            title = list(text = "중증도", style = list(fontSize = "20px")),
            labels = list(style = list(fontSize = "20px")), lineWidth = 3
          ),
          list(
            top = "50%", height = "50%",
            title = list(text = "체온(°C)", style = list(fontSize = "20px")),
            labels = list(style = list(fontSize = "20px")),
            showFirstLabel = T, showLastLabel = T, opposite = T
          ) # ,
          # list(
          # top = "40%", height = "20%",
          # title = list(text = "산소포화도(%)", style = list(fontSize = "20px")),
          # labels = list(style = list(fontSize = "20px")),
          # showFirstLabel = T, showLastLabel = T
          # ),
          # list(
          # top = "60%", height = "20%",
          # title = list(text = "호흡수(/분)", style = list(fontSize = "20px")),
          # labels = list(style = list(fontSize = "20px")),
          # showFirstLabel = T, showLastLabel = T, opposite = T
          # ),
          # list(
          # top = "80%", height = "20%",
          # title = list(text = "맥박(/분)", style = list(fontSize = "20px")),
          # labels = list(style = list(fontSize = "20px")),
          # showFirstLabel = T, showLastLabel = T
          # )
        ) %>%
        hc_add_series(getColor(thisTab, "중증도"), "line", hcaes("날짜", y, color = color), name = "중증도", marker = list(radius = 8)) %>%
        hc_add_series(getColor(thisTab, "체온"), "line", hcaes("날짜", y, color = color), name = "체온", marker = list(radius = 8), yAxis = 1) %>%
        # hc_add_series(getColor(thisTab, "산소포화도"), "line", hcaes("날짜", y, color = color), name = "산소포화도", marker = list(radius = 8), yAxis = 2) %>%
        # hc_add_series(getColor(thisTab, "호흡수"), "line", hcaes("날짜", y, color = color), name = "호흡수", marker = list(radius = 8), yAxis = 3) %>%
        # hc_add_series(getColor(thisTab, "맥박"), "line", hcaes("날짜", y, color = color), name = "맥박", marker = list(radius = 8), yAxis = 4) %>%
        hc_legend(itemStyle = list(fontSize = "20px")) %>%
        hc_exporting(enabled = T) %>%
        hc_tooltip(
          shared = T,
          crosshairs = T,
          style = list(fontSize = "20px"),
          headerFormat = '<span style="font-size: 20px; color: black;">{point.key}</span><br/>'
        )
    })
  })
  
  gtab <- readxl::read_excel("Example_update.xlsx")
  
  # gtab = read_sheet("http://docs.google.com/spreadsheets/d/188LunvsxTa2zqudNwAVDj-cuVpV5jm4y-3LTaj-azQE/edit?usp=sharing")
  # names(gtab) <- c("res_time", "name", "birth", "initial_res_yn", "gender", "age", "basis_sick", "temperature", "breathe_hard_yn", "breathe_cnt", "pulse_cnt", "oxygen","bloodpressure")
  # gtab <- gtab %>% mutate(res_time = as_date(gtab$res_time), birth = as_date(gtab$birth))
  
  gtab <- gtab %>% select(-id, -birthyear) # remove id, birthyear == name and age
  
  # data.frame(Date = lapply(gtab$datetime, asDate))
  D <- c()
  
  gtab$datetime <- lubridate::ymd_hm(gtab$datetime)
  # gtab$date = transform(data.frame(Date = gtab$date), Date = as.Date(as.character(Date), "%Y%m%d"))
  
  gtab$sex <- as.factor(gtab$sex)
  gtab$ori_center <- as.factor(gtab$ori_center)
  gtab$res_center <- as.factor(gtab$res_center)
  gtab$disease <- as.factor(gtab$disease)
  # gtab$num = as.factor(gtab$num)
  gtab$dyspnea <- as.factor(gtab$dyspnea)
  gtab$mental <- as.factor(gtab$mental)
  gtab$anxiety <- as.factor(gtab$anxiety)
  gtab$inde_resi <- as.factor(gtab$inde_resi)
  gtab$apt_resi <- as.factor(gtab$apt_resi)
  gtab$highrisk_g <- as.factor(gtab$highrisk_g)
  
  getPT <- function(T) {
    PT <- 0
    if (T <= 35) PT <- 3
    if (T <= 36 || T > 39) PT <- max(2, PT)
    if (T >= 38) PT <- max(1, PT)
    return(PT)
  }
  
  gtab <- gtab %>% tibble::add_column(체온지수 = sapply(gtab$temperature, getPT)) # PT
  
  gtab <- gtab %>%
    rename(심폐지수 = dyspnea) %>%
    rename(심리지수 = mental) %>%
    rename(의식지수 = anxiety) %>%
    rename(중증도 = num) %>%
    rename(성별 = sex) %>%
    rename(나이 = age) %>%
    rename(이름 = name) %>%
    rename(장소 = res_center) %>%
    rename(날짜 = datetime) %>%
    rename(체온 = temperature) %>%
    rename(기저질병여부 = disease) %>%
    rename(확진날짜 = confirmdate)
  
  # remove all NA column
  rem <- c()
  for (i in 1:ncol(gtab)) {
    if (length(which(is.na(gtab[, i]))) == nrow(gtab)) { # all na
      rem <- c(rem, i)
    }
  }
  
  gtab <- gtab %>% select(-rem)
  
  output$tab_google <- renderDataTable(
    datatable(
      gtab %>% select(장소, 이름, 성별, 나이, 체온지수, 심폐지수, 의식지수, 심리지수, 중증도),
      escape = FALSE,
      options = list(
        dom = "tip",
        pageLength = 50
      ),
      selection = "single",
      filter = "top",
      rownames = FALSE
    )
  )
  
  observeEvent(input$tab_google_rows_selected, {
    selected <- input$tab_google_rows_selected # check none selected
    tt <- thisTab <- gtab %>% filter(이름 == gtab$이름[selected])
    
    output$pat2 <- renderText({
      txt <- paste0(
        HTML('<i class = "material-icons" style= "font-size : 2.5rem">face</i> '), # icon
        thisTab$이름[1], " / ", # name
        thisTab$성별[1], " / ", # sex
        thisTab$확진날짜[1], "에 확진", " / ", # confirmed
        thisTab$장소[1], " / "
      )
      if (thisTab$중증도[1] == 3) txt <- paste0(txt, "상급병상우선")
      if (thisTab$중증도[1] == 2) txt <- paste0(txt, "병상우선")
      if (thisTab$중증도[1] <= 1) txt <- paste0(txt, "가정")
      txt
    })
    
    output$tab_google2 <- renderDataTable({
      thisTab <- thisTab %>% select(날짜, 기저질병여부, 체온, inde_resi, apt_resi, highrisk_g, 중증도)
      
      TRIIDX <- which(colnames(thisTab) == "중증도") - 1
      DIDX <- which(colnames(thisTab) == "기저질병여부") - 1
      
      dtobj <-
        datatable(
          thisTab,
          colnames = c("날짜", "질병", "체온", "inde_resi", "apt_resi", "고위험군", "중증도"),
          rownames = FALSE,
          selection = "none",
          options = list(
            dom = "tip",
            autoWidth = FALSE,
            pageLength = 50
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
    
    output$img2 <- renderHighchart({
      thisTab <- tt
      
      thisTab$날짜 <- datetime_to_timestamp(thisTab$날짜)
      highchart() %>%
        hc_xAxis(type = "datetime", title = list(text = "Day", style = list(fontSize = "20px")), labels = list(style = list(fontSize = "20px"))) %>%
        hc_yAxis_multiples(
          list(top = "0%", height = "50%", title = list(text = "중증도", style = list(fontSize = "20px")), labels = list(style = list(fontSize = "20px")), lineWidth = 3),
          list(top = "50%", height = "50%", title = list(text = "체온(°C)", style = list(fontSize = "20px")), labels = list(style = list(fontSize = "20px")), showFirstLabel = T, showLastLabel = T, opposite = T)
        ) %>%
        hc_add_series(getColor(thisTab, "중증도"), "line", hcaes(날짜, y, color = color), name = "중증도", marker = list(radius = 8)) %>%
        hc_add_series(getColor(thisTab, "체온"), "line", hcaes(날짜, y, color = color), name = "체온", marker = list(radius = 8), yAxis = 1) %>%
        hc_legend(itemStyle = list(fontSize = "20px")) %>%
        hc_exporting(enabled = T) %>%
        hc_tooltip(valueDecimals = 1, shared = T, crosshairs = T, style = list(fontSize = "20px"), headerFormat = '<span style="font-size: 20px; color: black;">{point.key}</span><br/>')
    })
  })
}

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
