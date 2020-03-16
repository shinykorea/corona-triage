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
library(lubridate) # date handling
library(tibble)

## Make login DB

# credentials <- data.frame(
#  user = c("admin", "user1"),
#  password = c("admin", "user1"),
#  admin = c(T, F),
#  stringsAsFactors = FALSE)

# create_db(credentials_data = credentials, sqlite_path = "database.sqlite")

material_infobox <- function(width, offset = 0, contents, Infotitle, Cardcolor) {
  
  title = HTML( paste0("<span style='font-weight:bold; color:#FFF;'>", Infotitle, "</span>&nbsp;&nbsp;" ) ) # Main Title with white color
  material_column(
    width = width,
    offset = offset,
    shiny::tags$div(
      class = "card z-depth-3", 
      shiny::tags$div(
        class = "card-content", 
        style = paste0('background-color:' ,Cardcolor),
        shiny::tags$span(class = "card-title", title), 
        shiny::tags$div(
          HTML(paste0("<div class='text-right'><span style='font-size:28px; text-align:center; color:#FFF;'>", contents, "</span></div>"))))
      )
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
  PT <- PCF <- PCO <- PM <- 0

  ##### PT

  T <- v$체온
  if (T <= 35) PT <- 3
  if (T <= 36 || T > 39) PT <- max(2, PT)
  if (T >= 38) PT <- max(1, PT)

  ##### PCF
  HB <- v$호흡곤란
  DHB <- v$심한호흡곤란
  CP <- v$가슴통증
  F <- v$실신
  C <- v$고열
  BL <- v$입술
  HHB <- v$호흡곤란심화

  if (HB) {
    if (sum(DHB, CP, F, C, BL, HHB)) {
      PCF <- 3
    } else {
      PCF <- 2
    }
  }

  O <- v$산소포화도
  if (O <= 93) PCF <- 3
  if (O <= 95) PCF <- max(2, PCF)

  BC <- v$호흡수
  if (BC >= 25 || BC <= 8) PCF <- 3
  if (BC >= 21 || BC <= 11) PCF <- max(2, PCF)

  P <- v$맥박
  if (P > 110 || P <= 40) PCF <- 3
  if (P >= 100) PCF <- max(2, PCF)
  if (P >= 101 || P <= 110) PCF <- max(2, PCF)

  ##### PCO

  CO <- v$의식
  if (!CO) PCO <- 3

  ##### PM

  PO <- v$두근거림
  PE <- v$발한
  TR <- v$몸떨림
  CH <- v$질식
  CC <- v$가슴불편
  AC <- v$복부불편
  W <- v$어지러움
  SA <- v$감각이상
  FE <- v$두려움

  if (sum(PO, PE, TR, C, CC, AC, W, SA, FE) >= 4) {
    PM <- 3
  } else if (sum(PO, PE, TR, C, CC, AC, W, SA, FE) >= 1) PM <- max(1, PM)

  return(c(PT, PCF, PCO, PM, max(PT, PCF, PCO, PM)))
}

ui <- function() {
  material_page(
    nav_bar_color = "deep-purple lighten-1",
    color = "#311b92",
    title = paste0("corona-triage <a href = 'https://github.com/shinykorea/corona-triage' target='_blank'> ",
                   "<i class='material-icons' style = 'font-size:1.3em;'> info </i> </a>"),
    useShinyjs(),
    tags$head(tags$style(type = "text/css", "table.dataTable tr.selected td, table.dataTable td.selected {background-color: #d1c4e9 !important;}")),
    
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

    # Define tab content
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
        uiOutput(outputId = "infoboxGroup")
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
              '<span style = "background-color:#f6a21c">　</span> : 2 ',
              '<span style = "background-color:#ffed82">　</span> : 1 ',
              '<span style = "background-color:#35bdbb">　</span> : 0'
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
  col1 <- "#ffed82" # yellow
  col2 <- "#f6a21c" # orange
  col3 <- "#ff6363" # red 
  colBasic <- "#35bdbb" # emerald

  if (Type == "중증도") {
    res <-
      sapply(Data[[Type]], function(i) {
        v <- colBasic
        if (i == 1) v <- col1
        if (i == 2) v <- col2
        if (i == 3) v <- col3
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

styleDT <- function(age, temperature, breath, concious, mental, point) {
  # 나이, 체온, 심폐기능, 의식, 심리, 중증도
  # index 형태로 주어져야함.
  col1 <- "#ffed82" # yellow
  col2 <- "#f6a21c" # orange
  col3 <- "#ff6363" # red

  JS(paste0("function(row, data, index){
            // Age 
            if(data[", age, "] > 60){$(row).find('td:eq(", age, ")').css({'background-color' : '", col1, "'});}
            if(data[", age, "] > 70){$(row).find('td:eq(", age, ")').css({'background-color' : '", col2, "'});}
            if(data[", age, "] > 80){$(row).find('td:eq(", age, ")').css({'background-color' : '", col3, "', 'color' : '#FFF'});}
            
            // Temperature
            if(data[", temperature, "] == 1){$(row).find('td:eq(", temperature, ")').css({'background-color' : '", col1, "'});}
            if(data[", temperature, "] == 2){$(row).find('td:eq(", temperature, ")').css({'background-color' : '", col2, "'});}
            if(data[", temperature, "] == 3){$(row).find('td:eq(", temperature, ")').css({'background-color' : '", col3, "'});}
            
            // Breath
            if(data[", breath, "] == 1){$(row).find('td:eq(", breath, ")').css({'background-color' : '", col1, "'});}
            if(data[", breath, "] == 2){$(row).find('td:eq(", breath, ")').css({'background-color' : '", col2, "'});}
            if(data[", breath, "] == 3){$(row).find('td:eq(", breath, ")').css({'background-color' : '", col3, "'});}
            
            // Concious
            if(data[", concious, "] == 1 ){$(row).find('td:eq(", concious, ")').css({'background-color' : '", col1, "'});}
            if(data[", concious, "] == 2 ){$(row).find('td:eq(", concious, ")').css({'background-color' : '", col2, "'});}
            if(data[", concious, "] == 3 ){$(row).find('td:eq(", concious, ")').css({'background-color' : '", col3, "'});}
            
            // Mental
             
            if(data[", mental, "] == 1 ){$(row).find('td:eq(", mental, ")').css({'background-color' : '", col1, "'});}
            if(data[", mental, "] == 2 ){$(row).find('td:eq(", mental, ")').css({'background-color' : '", col2, "'});}
            if(data[", mental, "] == 3 ){$(row).find('td:eq(", mental, ")').css({'background-color' : '", col3, "'});}
            
            // Point
            if(data[", point, "] == 1 ){$(row).find('td:eq(", point, ")').css({'background-color' : '", col1, "'});}
            if(data[", point, "] == 2 ){$(row).find('td:eq(", point, ")').css({'background-color' : '", col2, "'});}
            if(data[", point, "] == 3 ){$(row).find('td:eq(", point, ")').css({'background-color' : '", col3, "',});}
            
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
    tab <- readxl::read_xlsx("Example.xlsx")

    tab$증상발현날짜 <- as.Date(tab$증상발현날짜)
    tab$확진날짜 <- as.Date(tab$확진날짜)
    tab$날짜 <- as.Date(tab$날짜)
    tab$성별 <- as.factor(tab$성별)
    # tab$D <- as.factor(tab$D)

    TRIS <- sapply(1:nrow(tab), function(i) {
      triage(tab[i, ])
    })
    rownames(TRIS) <- c("체온지수", "심폐지수", "의식지수", "심리지수", "중증도")

    tab <- tab %>% cbind(t(TRIS))

    # hide file upload
    # shinyjs::runjs('$(".form-group.shiny-input-container").hide()')

    tab <<- tab

    newtab <<- tab %>%
      filter(날짜 == max(날짜)) %>% # recent day
      select(장소, 이름, 성별, 나이, -도, -시, -증상발현날짜, -확진날짜, 체온지수, 심폐지수, 의식지수, 심리지수, 중증도)

    ########################
    # MERGING COLUMN  : 8  #
    # ORDER COLUMN    : 8  #
    ########################

    dtobj <- datatable(
      newtab,
      # colnames=c("장소", "이름", "성별", "나이", "체온", "심폐기능", "의식수준", "심리상태", "중증도"),
      escape = FALSE,
      # caption = "전체 환자: 시설",
      options = list(
        # styleDT : 나이, 체온, 심폐기능, 의식수준, 심리상태, 중증도 의 인덱스 - 1
        rowCallback = styleDT(3, 4, 5, 6, 7, 8),
        dom = "tip",
        # rowsGroup = list(8), # TRIAGE
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

  output$infoboxGroup <- renderUI({
    aged <- tab %>%
      filter(날짜 == max(날짜)) %>%
      filter(나이 >= 60) %>%
      nrow()

    pat <- tab %>%
      filter(날짜 == max(날짜)) %>%
      filter(중증도 >= 2) %>%
      nrow()

    tagList(
      material_infobox(width = 2, offset = 3, contents = pat, Infotitle = "중증환자수", Cardcolor = "#d492b2"), # pink
      material_infobox(width = 2, contents = aged, Infotitle = "고령자수", Cardcolor = "#439e5b"), # green
      material_infobox(width = 2, contents = "20/03/14", Infotitle = "업데이트시간", Cardcolor = "#02adea") # sky,
    )
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


  observeEvent(input$tab1_rows_selected, {
    selected <- input$tab1_rows_selected # check none selected
    tt <- thisTab <- tab %>% filter(이름 == newtab$이름[selected])

    output$pat <- renderText({
      txt <- paste0(
        HTML('<i class = "material-icons" style= "font-size : 2.5rem">face</i> '), # icon
        thisTab$이름[1], " / ", # name
        thisTab$성별[1], " / ", # sex
        thisTab$확진날짜[1], "에 확진", " / ", # confirmed
        thisTab$시[1], " / ", # town
        thisTab$장소[1], " / "
      )
      if (thisTab$중증도[1] == 3) txt <- paste0(txt, "상급병상우선")
      if (thisTab$중증도[1] == 2) txt <- paste0(txt, "병상우선")
      if (thisTab$중증도[1] <= 1) txt <- paste0(txt, "가정")
      txt
    })

    output$tab2 <- renderDataTable({
      thisTab <- thisTab %>% select(-도, -시, -장소, -증상발현날짜, -확진날짜, -이름, -성별, -나이, -체온지수, -심폐지수, -의식지수, -심리지수)
      minus <- c()
      for (i in 3:ncol(thisTab)) {
        if (sum(thisTab[, i]) == 0) minus <- c(minus, i)
        thisTab[which(thisTab[, i] == "TRUE"), i] <- "T"
        thisTab[which(thisTab[, i] == "FALSE"), i] <- "F"
      }

      thisTab <- thisTab[, -minus]

      TRIIDX <- which(colnames(thisTab) == "중증도") - 1
      DIDX <- which(colnames(thisTab) == "기저질병여부") - 1

      dtobj <-
        datatable(
          thisTab,
          # colnames=c("날짜", "D", "체온", "산소포화도", "호흡수", "맥박", "의식저하", "두근거림", "떨림", "숨가쁨", "질식감","가슴불편", "메스꺼움", "어지러움", "중증도"),
          rownames = FALSE,
          selection = "none",
          options = list(
            # rowsGroup = list(DIDX), # Disease
            # rowCallback = styleDT(0, 1, 2, 3, 4, 5, 6, 8),
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


      highchart() %>%
        hc_xAxis(type = "datetime", title = list(text = "Day", style = list(fontSize = "20px")), labels = list(style = list(fontSize = "20px"))) %>%
        hc_yAxis_multiples(
          list(top = "0%", height = "20%", title = list(text = "중증도", style = list(fontSize = "20px")), labels = list(style = list(fontSize = "20px")), lineWidth = 3),
          list(top = "20%", height = "20%", title = list(text = "체온(°C)", style = list(fontSize = "20px")), labels = list(style = list(fontSize = "20px")), showFirstLabel = T, showLastLabel = T, opposite = T),
          list(top = "40%", height = "20%", title = list(text = "산소포화도(%)", style = list(fontSize = "20px")), labels = list(style = list(fontSize = "20px")), showFirstLabel = T, showLastLabel = T),
          list(top = "60%", height = "20%", title = list(text = "호흡수(/분)", style = list(fontSize = "20px")), labels = list(style = list(fontSize = "20px")), showFirstLabel = T, showLastLabel = T, opposite = T),
          list(top = "80%", height = "20%", title = list(text = "맥박(/분)", style = list(fontSize = "20px")), labels = list(style = list(fontSize = "20px")), showFirstLabel = T, showLastLabel = T)
        ) %>%
        hc_add_series(getColor(thisTab, "중증도"), "line", hcaes("날짜", y, color = color), name = "중증도", marker = list(radius = 8)) %>%
        hc_add_series(getColor(thisTab, "체온"), "line", hcaes("날짜", y, color = color), name = "체온", marker = list(radius = 8), yAxis = 1) %>%
        hc_add_series(getColor(thisTab, "산소포화도"), "line", hcaes("날짜", y, color = color), name = "산소포화도", marker = list(radius = 8), yAxis = 2) %>%
        hc_add_series(getColor(thisTab, "호흡수"), "line", hcaes("날짜", y, color = color), name = "호흡수", marker = list(radius = 8), yAxis = 3) %>%
        hc_add_series(getColor(thisTab, "맥박"), "line", hcaes("날짜", y, color = color), name = "맥박", marker = list(radius = 8), yAxis = 4) %>%
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

  asDate <- function(i) {
    i <- strsplit(as.character(i), "")[[1]]
    as.Date(paste0(
      paste0(i[1:4], collapse = ""), "-",
      paste0(i[5:6], collapse = ""), "-",
      paste0(i[7:8], collapse = "")
    ), origin = "1970-01-01")
  }
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

  gtab <- gtab %>% select(-all_of(rem))

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
