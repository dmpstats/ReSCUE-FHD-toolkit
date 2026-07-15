#' guide_tour UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags fluidRow actionButton
mod_app_tour_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      ns("start_guide"),
      label = tagList(
        bsicons::bs_icon("play-circle"),
        "Start Tutorial"
      ),
      class = "arrow-btn-faded",
      style = "width:225px"
    )

    # tags$details(
    #   tags$summary("Get a Guided Tour"),
    #   tags$div(
    #     class = "innerrounded rounded",
    #     align = "center",
    #     fluidRow(
    #       col_12(
    #         actionButton(
    #           ns("start_guide"),
    #           "How to use this app",
    #           class = "modbutton"
    #         )
    #       )
    #     )
    #   )
    # )
  )
}

#' guide_tour Server Functions
#'
#' @noRd
#' @importFrom shiny NS observeEvent
#' @importFrom conductor Conductor
#' @importFrom golem invoke_js
mod_app_tour_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # namespacing under main tabs namespaces
    land_tab_ns <- NS("landing_page")
    data_tab_ns <- NS("data_select")
    analysis_tab_ns <- NS("data_analysis")

    # construt guide
    guide <- Conductor$new(
      defaultStepOptions = list(
        floatingUIOptions = list()
      ) #,
      #progress = TRUE
    )

    # Add guide steps
    guide <- guide$step(
      el = "#app-version-container",
      title = "App Version",
      text = "The app version is displayed here.",
      position = "bottom-start"
    )$step(
      el = "[data-value='nav-home']", # using data-value as selector
      title = "Navigation Bar",
      text = "This allows you to navigate between different sections of the app. You can click on the icons to go to the desired section."
    )$step(
      el = paste0("#", land_tab_ns("tutorial_selectdata")),
      title = "Select Data",
      text = "Click this button to go to the Data Selection tab, where you can choose the data you want to analyze. This is the first step in using the app.",
      canClickTarget = FALSE,
      position = "left-start",
      tabId = "main-nav",
      tab = "nav-home",
      #cancelIcon = list(enabled = TRUE, label = "Close")
    )$step(
      el = paste0("#", data_tab_ns("card_data_select")),
      title = "Data Selection",
      text = "This is the Data Selection tab. Here, you can select the data you want to analyze.",
      position = "left",
      tabId = "main-nav",
      tab = "nav-data-select"
    )$step(
      el = paste0("#", data_tab_ns("go_analysis")),
      title = "Go to Analysis",
      text = "Once you have selected your data, click this button to go to the Data Analysis tab, where you can visualize and analyze the selected data.",
      position = "left-start",
      tabId = "main-nav",
      tab = "nav-data-select"
    )$step(
      el = paste0("#", analysis_tab_ns("card_fhdplot")),
      title = "Selected FHDs",
      text = "Here you can vizualise the selected flight height distributions (FHDs) based on your data selection.",
      position = "right-start",
      tabId = "main-nav",
      tab = "nav-analysis"
    )

    # initialize and start the guided tour when the button is clicked
    observeEvent(input$start_guide, {
      # invoke_js(
      #   "drive",
      #   list(arg = TRUE)
      # )
      guide$init()$start()
    })

    guide
  })
}
