



#' @title frontend
#' @description frontend by shiny app.
#' @param host The IP address that Shiny should listen
#' @param port A port number that Shiny will listen
#' @importFrom shiny shinyApp
#' @export
front <- function(host = "0.0.0.0", port = 3000) {

    options(shiny.port = port)
    options(shiny.launch.browser = FALSE)
    options(shiny.host = host)

    #' @importFrom shinydashboard
    #'  dashboardPage
    #'  dashboardHeader
    #'  dashboardSidebar
    #'  sidebarMenu
    #'  dashboardBody
    #'  tabItems
    #'  tabItem
    #'  menuItem
    #' @importFrom shiny
    #'  column
    #'  fluidRow
    #'  fileInput
    #'  icon
    #'  uiOutput
    #' @importFrom shinyWidgets
    #'  switchInput
    #'  actionBttn
    #' @importFrom DT DTOutput
    ui <- dashboardPage(
        skin = "purple",
        dashboardHeader(title = "e4ts frontend"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("import", tabName = "import", icon = icon("file-import")),
                menuItem("create", tabName = "create", icon = icon("table"))

            )
        ),
        dashboardBody(
            tabItems(
                tabItem(
                    tabName = "import",
                    column(5,
                        fluidRow(
                            fileInput(
                                "fileselector",
                                "file selection",
                                accept = ".csv"
                                )
                        ),

                        fluidRow(
                            DTOutput("targetfile")
                        )

                    )
                ),

                tabItem(
                    tabName = "create",
                    column(3,
                        fluidRow(
                            uiOutput("dimensions"),
                            uiOutput("measure"),
                            uiOutput("method"),
                            switchInput(
                                inputId = "using_weight",
                                label = "using weight",
                                labelWidth = "100px"
                            ),
                            uiOutput("weight"),
                            uiOutput("missing"),
                            switchInput(
                                inputId = "calc_total",
                                label = "calculate total",
                                labelWidth = "100px"
                            ),
                            actionBttn(
                                inputId = "create",
                                label = "Create Table",
                                style = "simple",
                                color = "primary",
                                icon = icon("table")
                            )
                        )
                    ),

                    column(7,
                        fluidRow(
                            DTOutput("stattable")
                        )
                    )

                )

            )
        )
    )

    #' @importFrom DT renderDataTable
    #' @importFrom shiny
    #'  observeEvent
    #'  renderUI
    #'  reactiveValues
    #' @importFrom data.table fread
    #' @importFrom shinyWidgets
    #'  pickerInput
    serv <- function(input, output, session) {

        local <- reactiveValues(
            target = NULL,
            stat = NULL
        )

        observeEvent(input$create, {


            dimensions <- input$dimension

            measure_new <- paste0(
                "measure_",
                input$method,
                "$new(name = input$measure)"
                )
            measure <- eval(parse(text = measure_new))

            weight <- ifelse(input$using_weight, input$weight, NULL)
            calc_total <- input$calc_total

            st <- stattable(
                df = local$target,
                dimensions = dimensions,
                measure = measure,
                weight = weight,
                calc_total = calc_total
                ) %>%
                select(-one_of("func"))

            local$stat <- st

        })



        observeEvent(input$fileselector, {

            local$target <- fread(
                input$fileselector$datapath,
                stringsAsFactors = FALSE,
                colClasses = "character",
                encoding = "UTF-8"
            )

        })


        output$dimensions <- renderUI({
            pickerInput(
                inputId = "dimension",
                label = "dimension",
                multiple = TRUE,
                choices = names(local$target),
                options = list(
                    size = 5,
                    `live-search`= TRUE
                )
            )
        })

        output$measure <- renderUI({
            pickerInput(
                inputId = "measure",
                label = "measure",
                multiple = FALSE,
                choices = names(local$target),
                options = list(
                    size = 5,
                    `live-search` = TRUE
                )
            )
        })



        output$method <- renderUI({
            pickerInput(
                inputId = "method",
                label = "method",
                multiple = FALSE,
                choices = c("sum", "average", "var", "sd", "min", "max"),
                options = list(
                    size = 5
                )
            )
        })

        output$weight <- renderUI({
            pickerInput(
                inputId = "weight",
                label = "weight",
                multiple = FALSE,
                choices = names(local$target),
                options = list(
                    size = 5
                )
            )
        })


        output$missing <- renderUI({
            pickerInput(
                inputId = "missing",
                label = "missing value convert",
                multiple = FALSE,
                choices = c("0"),
                options = list(
                    size = 5
                )
            )
        })

        output$targetfile <- DT::renderDataTable(
            local$target,
            selection = "single",
            rownames = FALSE
        )

        output$stattable <- DT::renderDataTable(
            local$stat,
            selection = "single",
            rownames = FALSE
        )



    }

    shiny::shinyApp(ui, serv)



}
