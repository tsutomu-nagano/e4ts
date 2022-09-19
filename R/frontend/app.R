## app.R ##
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(e4ts)
library(shiny)



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
                            "処理対象のファイルを選択してください。",
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

server <- function(input, output, session) {

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

        weight <- ifelse(input$using_weight, input$weight, "")
        calc_total <- input$calc_total

        st <- stattable(
            df = local$target,
            dimensions = dimensions,
            measure = measure,
            weight = weight,
            calc_total = calc_total
            ) %>%
            select(-func)

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
            # choices = statlist$statname,
            choices = names(local$target),
            # choicesOpt = list(
            #     subtext = paste(
            #         "statcode",
            #         statlist$statcode,
            #         sep = ":"
            #     )
            # ),
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
            # choices = statlist$statname,
            choices = names(local$target),
            # choicesOpt = list(
            #     subtext = paste(
            #         "statcode",
            #         statlist$statcode,
            #         sep = ":"
            #     )
            # ),
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
            # choices = statlist$statname,
            choices = c("sum", "average", "ver", "sd", "min", "max"),
            # choicesOpt = list(
            #     subtext = paste(
            #         "statcode",
            #         statlist$statcode,
            #         sep = ":"
            #     )
            # ),
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
            # choices = statlist$statname,
            choices = names(local$target),
            # choicesOpt = list(
            #     subtext = paste(
            #         "statcode",
            #         statlist$statcode,
            #         sep = ":"
            #     )
            # ),
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
            # choices = statlist$statname,
            choices = c("0"),
            # choicesOpt = list(
            #     subtext = paste(
            #         "statcode",
            #         statlist$statcode,
            #         sep = ":"
            #     )
            # ),
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

shinyApp(ui, server)