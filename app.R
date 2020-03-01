library(shinymaterial)
library(shiny)
library(shinyjs)
library(readxl)
library(DT)
library(dplyr)
library(htmltools)
library(ggplot2)
library(gridExtra)

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

  if (v$Breath) breath <- 2

  point <- sum(age, disease, temperature, count, oxygen, pressure, breath)

  return(point)
}

ui <- function() {
  material_page(
    title = "corona-triage",
    useShinyjs(),
    tags$head(tags$style(type = "text/css", "th, td {text-align:center !important;}")),
    material_card(
      depth = 3,
      fileInput(inputId = "file", label = "upload Excel", accept = ".xlsx", multiple = FALSE),
      DT::dataTableOutput("tab1"),
      actionButton("btn", label = "button", style = "display:none;")
    ),
    material_card(
      depth = 4,
      DT::dataTableOutput("tab2"),
      plotOutput('img'),
    )
  )
}

# f7d794 YELLOW  1 #ffffc0
# f19066 ORANGE  2 #ffbb85
# e66767 RED     3 #ff8686

styleDT <- function(age, disease, temperature, count, oxygen, pressure, breath, point) {
  JS(paste0("function(row, data, index){
            // Age 
            if(data[", age, "] > 60){$(row).find('td:eq(", age, ")').css({'background-color' : '#ffffc0', 'font-size' : '1.2em'});}
            if(data[", age, "] > 70){$(row).find('td:eq(", age, ")').css({'background-color' : '#ffbb85', 'font-size' : '1.5em'});}
            if(data[", age, "] > 80){$(row).find('td:eq(", age, ")').css({'background-color' : '#ff8686', 'font-size' : '1.8em'});}
            
            // Diesease
            if(data[", disease, "]){$(row).find('td:eq(", disease, ")').css({'background-color' : '#ff8686', 'font-size' : '1.8em'});}
            
            // Temperature
            if(data[", temperature, "] <= 36 ){$(row).find('td:eq(", temperature, ")').css({'background-color' : '#ffffc0', 'font-size' : '1.2em'});}
            if(data[", temperature, "] >= 37.5 ){$(row).find('td:eq(", temperature, ")').css({'background-color' : '#ffffc0', 'font-size' : '1.2em'});}
            if(data[", temperature, "] > 38){$(row).find('td:eq(", temperature, ")').css({'background-color' : '#ffbb85', 'font-size' : '1.5em'});}
            if(data[", temperature, "] > 39){$(row).find('td:eq(", temperature, ")').css({'background-color' : '#ff8686', 'font-size' : '1.8em'});}
        
            // BreathCount    
            if(data[", count, "] <= 11){$(row).find('td:eq(", count, ")').css({'background-color' : '#ffffc0', 'font-size' : '1.2em'});}
            if(data[", count, "] <= 8){$(row).find('td:eq(", count, ")').css({'background-color' : '#ff8686', 'font-size' : '1.8em'});}
            
            // Oxygen
            if(data[", oxygen, "] <= 95){$(row).find('td:eq(", oxygen, ")').css({'background-color' : '#ffffc0', 'font-size' : '1.2em'});}
            if(data[", oxygen, "] <= 93){$(row).find('td:eq(", oxygen, ")').css({'background-color' : '#ffbb85', 'font-size' : '1.5em'});}
            if(data[", oxygen, "] <= 91){$(row).find('td:eq(", oxygen, ")').css({'background-color' : '#ff8686', 'font-size' : '1.8em'});}
            
            // BloodPressure
            if(data[", pressure, "] <= 110){$(row).find('td:eq(", pressure, ")').css({'background-color' : '#ffffc0', 'font-size' : '1.2em'});}
            if(data[", pressure, "] <= 100){$(row).find('td:eq(", pressure, ")').css({'background-color' : '#ffbb85', 'font-size' : '1.5em'});}
            if(data[", pressure, "] <= 90){$(row).find('td:eq(", pressure, ")').css({'background-color' : '#ff8686', 'font-size' : '1.8em'});}
            
            // Breath
            if(data[", breath, "]){$(row).find('td:eq(", breath, ")').css({'background-color' : '#ffbb85', 'font-size' : '1.5em'});}
            
            // Point
            if(data[", point, "] > 5){$(row).find('td:eq(", point, ")').css({'background-color' : '#ffffc0', 'font-size' : '1.2em'});}
            if(data[", point, "] > 10){$(row).find('td:eq(", point, ")').css({'background-color' : '#ffbb85', 'font-size' : '1.5em'});}
            if(data[", point, "] > 15){$(row).find('td:eq(", point, ")').css({'background-color' : '#ff8686', 'font-size' : '1.8em'});}
            
        }"))
}

server <- function(input, output, session) {
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
    datatable(
      newtab,
      escape = FALSE,
      options = list(
        rowCallback = styleDT(2, 3, 4, 5, 6, 7, 8, 10),
        dom = "ltip",
        autoWidth = TRUE,
        order = list(list(10, "desc"))
      ),
      selection = "single",
      filter = "top",
      rownames = FALSE,
      callback = JS(
        'table.on("click.dt", "tr", function() {
                var data = table.row(this).data();
                $("#btn").show();
                })'
      )
    )
  })

  observeEvent(input$btn, {
    if (input$btn == 0) {
      return(NULL)
    }

    selected <- input$tab1_rows_selected # check none selected
    shinyjs::runjs('$("#btn").hide()')
    thisTab = tab %>% filter(Name == newtab$Name[selected])
    output$tab2 <- renderDataTable({
      dtobj <- datatable(
        thisTab,
        rownames = FALSE,
        selection = "none",
        options = list(
          rowsGroup = list(0, 1, 2, 3, 4, 5, 6, 7, 12),
          rowCallback = styleDT(6, 7, 8, 9, 10, 11, 12, 14),
          dom = "ltip",
          autoWidth = TRUE
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
    
    output$img = renderPlot(grid.arrange(
        ggplot(thisTab, aes(x = Date, y = Temperature)) + 
            geom_line() + geom_point(),
        ggplot(thisTab, aes(x = Date, y = BreathCount)) + 
            geom_line() + geom_point(),
        ggplot(thisTab, aes(x = Date, y = Oxygen)) + 
            geom_line() + geom_point(),
        ggplot(thisTab, aes(x = Date, y = BloodPressure)) + 
            geom_line() + geom_point(),
        ggplot(thisTab, aes(x = Date, y = Point)) + 
            geom_line() + geom_point(),
        nrow = 1
    ))
  })
}

shinyApp(ui = ui(), server = server, options = list(launch.browser = TRUE))
