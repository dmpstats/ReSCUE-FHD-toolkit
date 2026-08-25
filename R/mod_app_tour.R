#' guide_tour UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags fluidRow actionButton
mod_app_tour_ui <- function(id, style = "width:18vw; height: 5vh;") {
  ns <- NS(id)
  tagList(
    actionButton(
      ns("start_guide"),
      label = tagList(
        bsicons::bs_icon("play-circle"),
        "Tutorial"
      ),
      class = "not-arrow-btn",
      style = style
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
      exitOnEsc = TRUE,
      keyboardNavigation = TRUE,
      defaultStepOptions = list(
        floatingUIOptions = list(),
        modalOverlayOpeningPadding = 8
      ) #,
      #progress = TRUE
    )

    # Add guide steps
    guide <- guide$step(
      title = "Welcome to ReSCUEApp!",
      text = "This short guided tour will walk you through the main steps of the app, from selecting your flight-height distributions to visualise, compare and download your results and session details.

      <br><br><small style='color: #999;'>
      Tip: Press <strong>Esc</strong> at any time exit the tour.
      ",
      buttons = list(
        list(text = "Next", action = "next")
      )
    )$step(
      el = paste0("#", land_tab_ns("proj_overview_panel")),
      title = "The ReSCUE Toolkit",
      text = "Start here for an overview of the ReSCUE Project and this toolkit app. Click any section heading to expand it and learn more about the app, its objectives, and the team behind it."
    )$step(
      el = paste0("#", land_tab_ns("link_guide")),
      title = "User Guide",
      text = "For further detailed instructions on how to use the app and its features, click the 'User Guide' button.<br><br>The User Guide is a comprehensive resource for understanding the app's functionality and will provide you with step-by-step instructions.",
      position = "auto",
      canClickTarget = FALSE
    )$step(
      el = paste0("#", land_tab_ns("go_data")),
      title = "Ready to Start?",
      text = "Click this button to go to the Selection tab, where you can choose the Flight Height Distributions (FHDs) you want to explore. <br><br>This is the first step in using the app.",
      canClickTarget = FALSE,
      position = "auto",
      tabId = "main-nav",
      tab = "nav-home"
    )$step(
      el = paste0("#", data_tab_ns("map_selection_card")),
      title = "Selection Tool",
      text = "This interactive map allows you to select FHDs for visualisation. Each shaded region is a Flight Height Distribution for a given species, measuring method, season, etc.
      <br><br>Have a go! Click on a region to see its details and add it to your selection.",
      position = "left-start",
      tabId = "main-nav",
      tab = "nav-data-select"
    )$step(
      el = paste0("#", data_tab_ns("selected_fhd_card")),
      title = "Your Current Selection",
      text = "FHDs you've added from the map appear here. Click a row to highlight an FHD, then click the trash icon to remove it from your selection.
      <br><br> You can select up to 10 FHDs for analysis at a time.
      <br><br><small style='color: #999;'> Haven't selected an FHD yet? Go back to the previous step to select one from the map.</small>",
      position = "left-start"
    )$step(
      el = paste0("#", data_tab_ns("map_filters")),
      title = "Filter FHDs",
      text = "Use these filters to narrow down the available FHDs based on your criteria. <br><br>For example, you can filter by species and season to find the FHDs that are most relevant to your analysis.",
      position = "right-start"
    )$step(
      el = paste0("#", data_tab_ns("upload_data")),
      title = "Upload Your Own FHD",
      text = "Optionally, upload your own flight-height distribution to include it alongside the built-in FHDs, for comparison.",
      position = "top",
      canClickTarget = FALSE
    )$step(
      el = paste0("#", data_tab_ns("go_analysis")),
      title = "Explore Selection",
      text = "When you're ready, click here to navigate to the Visualisation tab, where you can explore, compare and export your selected FHDs.",
      position = "left-start",
      canClickTarget = FALSE,
      tabId = "main-nav",
      tab = "nav-data-select",
      onHide = "Shiny.setInputValue('tour_load_demo_fhd', Math.random(), {priority: 'event'});"
    )$step(
      el = paste0("#", analysis_tab_ns("card_selected_fhds")),
      title = "Selected FHDs",
      text = "The FHDs you selected are listed here.<br><br>Click <strong>Details</strong> to see an FHD's metadata, or <strong>Covariates</strong> to inspect it by specific covariate levels.",
      position = "right-start",
      buttons = list(list(text = "Next", action = "next"))
    )$step(
      el = paste0("#", analysis_tab_ns("card_prop_crh")),
      title = "Proportion at Collision Risk Height",
      text = "This card shows the proportion of birds at collision risk height (CRH) across your selected FHDs under a pre-defined air gap and rotor radius.      <br><br>Use <strong>Turbine Parameters</strong> to change the default settings, and thus update the risk height envelope.
      <br><br>The <strong>Summaries</strong> tab provides percentile summaries.
      <br><br>The <strong>Air Gap Sensitivity</strong> tab shows how the proportion changes with incremental shifts in selected air gap.",
      position = "right-start"
    )$step(
      el = paste0("#", analysis_tab_ns("card_fhdplot")),
      title = "FHD Visualisation",
      text = "This section plots your selected FHDs, with the collision risk height envelope shaded based on the specified air gap and rotor radius.
      <br><br>Use <strong>Hide legend</strong> or <strong>Facet plot</strong> to adjust the display. You can also click legend entries to show or hide individual FHDs.",
      position = "left-start"
    )$step(
      el = paste0("#", analysis_tab_ns("card_download")),
      title = "Download Options",
      text = "Select an FHD from the dropdown and choose which outputs to include - SCRM data, the FHD plot, and/or metadata - then click <strong>Download</strong> to export them as a zip file.",
      position = "left-start",
      canClickTarget = FALSE,
      tabId = "main-nav",
      tab = "nav-analysis"
    )$step(
      el = "#main-nav",
      title = "Navigation Bar",
      text = "The navigation bar lets you move between the main sections of the app. You can also access the documentation, settings and app version from here.",
      position = "bottom",
      canClickTarget = FALSE
    )$step(
      el = "[data-value='nav-home']",
      #title = "Home",
      text = "This button gets you back to the starting point.",
      position = "bottom",
      canClickTarget = FALSE
    )$step(
      el = "[data-value='nav-data-select']",
      #title = "FHD Selection",
      text = "This one takes you back to the FHD selection section.",
      position = "bottom",
      canClickTarget = FALSE
    )$step(
      el = "[data-value='nav-analysis']",
      #title = "Visualisation & Export",
      text = "While clicking here sends you to the FHD visualisation and download section.",
      position = "bottom",
      canClickTarget = FALSE
    )$step(
      el = "li.nav-item.dropdown:has(.bi-book-fill)",
      #title = "Documentation",
      text = "The <strong>Documentation</strong> menu gives you access to <strong>Data Sources</strong>, which provides information about the FHD database and its provenance, the full <strong>User Guide</strong>, and the <strong>Metadata Builder</strong> tool.",
      position = "bottom",
      canClickTarget = FALSE
    )$step(
      el = "li.nav-item.dropdown:has(.bi-gear-fill)",
      #title = "Settings",
      text = "The <strong>Settings</strong> menu lets you report a bug or reset your session to start fresh.",
      position = "bottom",
      canClickTarget = FALSE
    )$step(
      el = "#app-version-container",
      title = "App Version",
      text = "The current version of ReSCUEApp is shown here. This is useful when reporting bugs or checking that you are using the latest release. Click the version number to view the app's release notes and history.",
      position = "bottom",
      canClickTarget = FALSE
    )$step(
      title = "You're all set!",
      text = paste0(
        "That's the full walkthrough. From here, you can:
      <br><br>&bull; Navigate to the <strong>Select Flight Height Distributions</strong> tab to tweak your selection.
      <br>&bull; Adjust <strong>Turbine Parameters</strong> or covariate selections to explore different scenarios.
      <br>&bull; Use the info buttons  ",
        as.character(bsicons::bs_icon("info-circle")),
        "  on each section for more detail.
      <br>&bull; <strong>Download</strong> your results once you're happy with your selection.
      <br><br><small style='color: #999;'>You can restart this tour at any time from the 'Start Tutorial' button on the home page.</small>"
      ),
      tabId = "main-nav",
      tab = "nav-home",
      buttons = list(
        list(action = "back", secondary = TRUE, text = "Previous"),
        list(action = "next", text = "Finish")
      )
    )

    # initialize and start the guided tour when the button is clicked
    observeEvent(input$start_guide, {
      guide$init(session = session)$start(session = session)
    })

    guide
  })
}
