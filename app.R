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

  PCF <- v$호흡곤란
  PM <- v$가벼운불안

  ##### PT

  T <- v$체온
  if (T <= 35) PT <- 3
  if (T <= 36 || T > 39) PT <- max(2, PT)
  if (T > 38) PT <- max(1, PT)

  ##### PCO

  CO <- v$의식저하
  if (CO) PCO <- 3


  return(c(PT, PCF, PCO, PM, sum(PT, PCF, PCO, PM)))
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
        "생활치료센터" = "facility",
        "가정" = "home" # ,
        # "구글시트예시" = 'google'
      ),
      color = "#311b92"
    ),

    # Define Pat content
    material_tab_content(
      tab_id = "facility",
      # tags$h1("생활치료센터"),
      div( # navigator
        material_column(
          myButton(inputId = "toinfo", label = "info", onClick = 'location.href = "#infoboxGroup"'),
          myButton(inputId = "totab1", label = "tab1", onClick = 'location.href = "#tab1"'),
          myButton(inputId = "totab2", label = "tab2", onClick = 'location.href = "#tab2"'),
          myButton(inputId = "toimg", label = "img", onClick = 'location.href = "#img"'),
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
            DT::dataTableOutput("tab1")
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
            title = HTML(paste0(
              "Visualize among time. ",
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
    material_tab_content(
      tab_id = "home",
      material_row(
        div(style = "height:1em"),
        uiOutput(outputId = "infoboxGroup2")
      ),
      material_row(
        material_column(
          width = 12,
          material_card(
            title = "",
            depth = 3,
            DT::dataTableOutput("tab_google"),
          )
        )
      ),

      material_row(
        height = "100%",
        material_column(
          material_card(
            title = htmlOutput("pat2", style = "text-align:center"),
            divider = TRUE,
            DT::dataTableOutput("tab_google2")
          ),
          width = 12
        ),

        material_column(
          material_card(
            title = HTML(paste0(
              "Visualize among time. ",
              '<span style = "background-color:#ff6363">　</span> : 3 ',
              '<span style = "background-color:#f6a21c">　</span> : 2 ',
              '<span style = "background-color:#ffed82">　</span> : 1 ',
              '<span style = "background-color:#35bdbb">　</span> : 0'
            )),
            highchartOutput("img2", height = "600px"),
            depth = 3
          ),
          width = 12
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
  col1 <- "#35a4c6" # yellow
  col2 <- "#ff9d9d" # orange
  col3 <- "#ff6363" # red
  colBasic <- "#35a4c6" # emerald

  if (Type == "중증도") {
    res <-
      sapply(Data[[Type]], function(i) {
        v <- colBasic
        if (i == 1) v <- col1
        if (i == 2) v <- col2
        if (i >= 3) v <- col3
        return(v)
      }, USE.NAMES = FALSE)
  }
  if (Type == "체온") {
    res <-
      sapply(Data[[Type]], function(T) {
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
  # 중증도, 증감
  # index 형태로 주어져야함.
  # col1 <- "#ffed82" # yellow
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
  # 중증도
  # index 형태로 주어져야함.
  # col1 <- "#ffed82" # yellow
  col2 <- "#ff9d9d" # orange
  col3 <- "#ff6363" # red
  col4 <- "#91c320" # green
  JS(paste0("function(row, data, index){
            // Point
            if(data[", point, "] == 2 ){$(row).find('td:eq(", point, ")').css({'background-color' : '", col2, "'});}
            if(data[", point, "] >= 3 ){$(row).find('td:eq(", point, ")').css({'background-color' : '", col3, "',});}
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

  Pat <- read_sheet(Link, sheet = sheets[1]) # first sheets
  for (i in 2:length(sheets)) {
    Pat <- rbind(Pat, read_sheet(Link, sheet = sheets[i]))
  }

  colnames(Pat) <- c(
    "주민등록번호", "이름", "확진일자", "기저질환", "센터", "체온",
    "의식저하", "가벼운불안", "호흡곤란", "산소포화도", "호흡수",
    "맥박", "중증도", "퇴원여부", "입력날짜", "차수"
  )
  Pat <- Pat %>% select(-중증도) # remove 중증도도

  ### disease decompose ---------------------------

  temp <- t(sapply(Pat$기저질환, function(i) {
    res <- rep(0, 9)
    if (!is.na(i)) {
      v <- as.numeric(strsplit(i, split = ",")[[1]])
      if (length(v)) {
        res[as.numeric(strsplit(i, split = ",")[[1]])] <- 1
      }
    }

    names(res) <- c("당뇨", "만성 신질환", "만성 간질환", "만성 폐질환", "만성 심혈관 질환", "혈액암", "항암치료 암환자", "면역억제제 복용", "HIV 환자")
    return(res)
  }, USE.NAMES = FALSE))

  Pat <- Pat %>% cbind(temp)

  Pat <- Pat %>% select(-기저질환)

  ### 중증도 계산 --------------------------------

  TRIS <- sapply(1:nrow(Pat), function(i) {
    Pat[i, ] %>%
      select(체온, 호흡곤란, 의식저하, 가벼운불안) %>%
      triage()
  })
  rownames(TRIS) <- c("체온지수", "심폐지수", "의식지수", "심리지수", "중증도")

  Pat <- Pat %>% cbind(t(TRIS))

  ## 날짜 decompose ---------------

  DATE <- sapply(1:nrow(Pat), function(i) {
    if (Pat$차수[i] == 1) {
      return(paste0(Pat$입력날짜[i], "0900"))
    }
    return(paste0(Pat$입력날짜[i], "2100"))
  }, USE.NAMES = FALSE)

  Pat <- Pat %>%
    cbind(DATE) %>%
    select(-입력날짜, -차수) %>%
    rename(날짜 = DATE)

  Pat$날짜 <- as.character(Pat$날짜)

  # 생년월일

  Birth <- sapply(Pat$주민등록번호, function(i) {
    (i - i %% 10000000) / 10000000
  })

  Sex <- sapply(Pat$주민등록번호, function(i) {
    i <- i %% 10000000
    i <- (i - i %% 1000000) / 1000000
    ifelse(i %% 2 == 1, "M", "F")
  })

  Pat <- Pat %>%
    cbind(Birth) %>%
    rename(생년월일 = Birth) %>%
    cbind(Sex) %>%
    rename(성별 = Sex)

  return(data.frame(Pat))
}



server <- function(input, output, session) {

  # off scientific notation
  options(scipen = 999)

  # deauthorize google sheets
  sheets_deauth()

  ## Apply login DB
  res_auth <- secure_server(
    check_credentials = check_credentials("database.sqlite")
  )

  newtab <- ""

  Pat <- readPat()
  Survey <- readSurvey()
  Pat <- Pat %>% inner_join(Survey, by = c("주민등록번호", "센터", "이름", "확진일자"))

  output$tab1 <- renderDataTable({

    ## 증감 계산 -------------------------------------------------------------

    # Pat column name -------------------------------------------------------------------------------------------
    # 주민등록번호 이름 성별 생년월일 확진일자 센터 체온 의식저하 가벼운불한 호흡곤란 산소포화도 호흡수 맥박 퇴원여부
    # 입력날짜 차수 당뇨 만성 신질환 만성 간질환 만성 폐질환 만성 심혈관 질환 혈액암 항암치료 암환자 면역억제제 복용 HIV 환자
    # 체온지수 심폐지수 의식지수 심리지수 중증도

    newtab <<- Pat %>%
      group_by(이름) %>%
      filter(날짜 == max(날짜)) %>% # recent data
      select(주민등록번호, 이름, 생년월일, 확진일자, 센터, 체온지수, 의식지수, 심리지수, 심폐지수, 중증도, 날짜)

    temp <- Pat %>%
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
      higher <- Pat %>%
        group_by(이름) %>%
        filter(날짜 == max(날짜)) %>%
        filter(중증도 >= 3) %>%
        nrow()

      pat <- Pat %>%
        group_by(이름) %>%
        filter(날짜 == max(날짜)) %>%
        filter(중증도 == 2) %>%
        nrow()

      lastTime1 <- Pat %>%
        filter(센터 == "이천") %>%
        filter(날짜 == max(날짜)) %>%
        select(날짜)
      lastTime2 <- Pat %>%
        filter(센터 == "용인") %>%
        filter(날짜 == max(날짜)) %>%
        select(날짜)
      lastTime1 <- lastTime1[, 1]
      lastTime2 <- lastTime2[, 1]
      lastTime1 <- strsplit(lastTime1, "")[[1]]
      lastTime1 <- paste0(
        paste0(lastTime1[6], collapse = ""), "월 ",
        paste0(lastTime1[7:8], collapse = ""), "일 ",
        ifelse(lastTime1[9] == 0, "1차", "2차")
      )

      lastTime2 <- strsplit(lastTime2, "")[[1]]
      lastTime2 <- paste0(
        paste0(lastTime2[6], collapse = ""), "월 ",
        paste0(lastTime2[7:8], collapse = ""), "일 ",
        ifelse(lastTime2[9] == 0, "1차", "2차")
      )

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
          Infotitle = "업데이트시간(이천)",
          Cardcolor = "#35a4c6",
          boxid = "timeBox2"
        ) # purple
      )
    })

    dtobj <- datatable(
      newtab,
      escape = FALSE,
      options = list(
        # styleDT : 체온지수, 심폐지수, 의식지수, 심리지수, 중증도, 증감의 인덱스 - 1
        rowCallback = styleDT(9, 10),
        dom = "tip",
        order = list(list(9, "desc"))
      ),
      selection = "single",
      # filter = "top",
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

  observeEvent(input$timeBox1, {
    output$tab1 <- renderDataTable(
      datatable(
        newtab %>% filter(센터 == "용인"),
        escape = FALSE,
        options = list(
          # styleDT : 체온지수, 심폐지수, 의식지수, 심리지수, 중증도, 증감의 인덱스 - 1
          rowCallback = styleDT(9, 10),
          dom = "tip",
          order = list(list(9, "desc"))
        ),
        selection = "single",
        # filter = "top",
        rownames = FALSE
      )
    )
    shinyjs::show("resetBox")
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
          order = list(list(9, "desc"))
        ),
        selection = "single",
        # filter = "top",
        rownames = FALSE
      )
    )
    shinyjs::show("resetBox")
  })

  observeEvent(input$resetBox, { # 초기화
    output$tab1 <- renderDataTable(
      datatable(
        newtab,
        escape = FALSE,
        options = list(
          # styleDT : 체온지수, 심폐지수, 의식지수, 심리지수, 중증도, 증감의 인덱스 - 1
          rowCallback = styleDT(9, 10),
          dom = "tip",
          order = list(list(9, "desc"))
        ),
        selection = "single",
        # filter = "top",
        rownames = FALSE
      )
    )
    shinyjs::hide("resetBox")
  })

  observeEvent(input$higherBox, { # 중증도 3
    shinyjs::show("resetBox")
    output$tab1 <- renderDataTable({
      datatable(
        newtab %>% filter(중증도 >= 3),
        escape = FALSE,
        options = list(
          # styleDT : 체온지수, 심폐지수, 의식지수, 심리지수, 중증도, 증감의 인덱스 - 1
          rowCallback = styleDT(9, 10),
          dom = "tip",
          order = list(list(9, "desc"))
        ),
        selection = "single",
        # filter = "top",
        rownames = FALSE
      )
    })
  })

  observeEvent(input$patBox, { # 중증도 2
    shinyjs::show("resetBox")
    output$tab1 <- renderDataTable({
      datatable(
        newtab %>% filter(중증도 == 2),
        escape = FALSE,
        options = list(
          # styleDT : 체온지수, 심폐지수, 의식지수, 심리지수, 중증도, 증감의 인덱스 - 1
          rowCallback = styleDT(9, 10),
          dom = "tip",
          order = list(list(9, "desc"))
        ),
        selection = "single",
        # filter = "top",
        rownames = FALSE
      )
    })
  })

  # output$infoboxGroup2 = renderUI({

  # aged = gtab %>%
  # filter(date == max(date)) %>%
  # filter(A>=60) %>%
  # nrow()

  # pat = gtab %>%
  # filter(Date == max(Date)) %>%
  # filter(TRI >=2) %>%
  # nrow()

  # tagList(
  # material_infobox(width = 2,offset = 3,contents = pat ,Infotitle = '중증환자수',Cardcolor = 'pink accent-2'), # red
  # material_infobox(width = 2,contents = aged ,Infotitle = '고령자수',Cardcolor =  'teal darken-3'), # green
  # material_infobox(width = 2,contents = '20/03/14', Infotitle = '업데이트시간',Cardcolor =  'deep-purple') # purple,
  # )
  # })


  # specific table -------------------------------------------------------------
  observeEvent(input$tab1_rows_selected, {
    selected <- input$tab1_rows_selected # check none selected
    tt <- thisTab <- Pat %>%
      filter(이름 == newtab$이름[selected]) %>%
      arrange(desc(날짜))

    # specific table title ----------------------------------------------------
    output$pat <- renderText({
      txt <- paste0(
        HTML('<i class = "material-icons" style= "font-size : 2.5rem">face</i> '), # icon
        thisTab$이름[1], " / ",
        thisTab$성별[1], " / ",
        thisTab$생년월일[1], " / ",
        thisTab$센터[1], "센터 / "
      )
      if (thisTab$중증도[1] >= 3) txt <- paste0(txt, "상급의료기관 배정")
      if (thisTab$중증도[1] == 2) txt <- paste0(txt, "의료기관 배정")
      if (thisTab$중증도[1] <= 1) txt <- paste0(txt, "가정")
      txt
    })

    # specific table content -------------------------------------------------
    output$tab2 <- renderDataTable({
      thisTab <- thisTab %>%
        select(이름, 확진일자, 체온, 의식지수, 심리지수, 심폐지수, 산소포화도, 호흡수, 맥박, 중증도, 날짜) %>%
        inner_join(newtab %>% select(이름, 증감)) %>%
        select(-이름, -증감)

      TRIIDX <- which(colnames(thisTab) == "중증도") - 1


      dtobj <-
        datatable(
          thisTab,
          # colnames=c("날짜", "D", "체온", "산소포화도", "호흡수", "맥박", "의식저하", "두근거림", "떨림", "숨가쁨", "질식감","가슴불편", "메스꺼움", "어지러움", "중증도"),
          rownames = FALSE,
          selection = "none",
          options = list(
            rowCallback = styleDT2(8),
            # 체온지수, 심폐지수, 의식지수, 심리지수, 중증도, 증감
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
      thisTab <- tt

      thisTab$날짜 <- datetime_to_timestamp(lubridate::ymd_hm(thisTab$날짜))

      highchart() %>%
        hc_xAxis(type = "datetime", title = list(text = "Day", style = list(fontSize = "20px")), labels = list(style = list(fontSize = "20px"))) %>%
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
        hc_tooltip(valueDecimals = 1, shared = T, crosshairs = T, style = list(fontSize = "20px"), headerFormat = '<span style="font-size: 20px; color: black;">{point.key}</span><br/>')
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
      # colnames=c("장소", "이름", "성별", "나이", "체온", "심폐기능", "의식수준", "심리상태", "중증도"),
      escape = FALSE,
      # caption = "전체 환자: 시설",
      options = list(
        # rowCallback = styleDT(3, 4, 5, 6, 7, 8, 9, 11),
        dom = "tip" # ,
        # rowsGroup = list(8)#,
        # order = list(list(8, "desc"))
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
        # thisTab$Town[1], " / ", # town
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
            # rowsGroup = list(DIDX), # Disease
            # rowCallback = styleDT(0, 1, 2, 3, 4, 5, 6, 8),
            dom = "tip",
            autoWidth = FALSE # ,
            # order = list(list(TRIIDX, "desc"))
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

shinyApp(ui = ui(), server = server, options = list(launch.browser = TRUE))
