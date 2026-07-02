#' data_analysis UI Function
#'
#' @description A shiny Module handling app data analysis.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_fillable(
					bslib::layout_columns(
						col_widths = c(6, 6),

						column(
							12,
							# First card: selected data
							bslib::card(
								bslib::card_header("Selected Data"),
								bslib::card_body(DT::DTOutput(ns("dummy_dt")))
							),

							# Second card: defining turbine parameters
							bslib::card(
								bslib::card_header("Turbine Parameters"),
								bslib::card_body(
									fluidRow(
										bslib::layout_columns(
											col_widths = c(4, 4, 4),
											numericInput(
												"hub_height",
												"Hub Height (m)",
												value = 100,
												min = 0
											),
											numericInput(
												"rotor_diameter",
												"Rotor Diameter (m)",
												value = 80,
												min = 0
											),
											numericInput(
												"cut_in_speed",
												"Cut-in Speed (m/s)",
												value = 3.5,
												min = 0
											)
										)
									)
								)
							),

							# Third card: analysis results
							bslib::card(
								bslib::card_header("Analysis Results"),
								bslib::card_body("Analysis results content will go here")
							)
						),

						# Second column will contain the plot and output distributions
						column(
							12,
							bslib::card(
								bslib::card_header("Flight Height Distribution"),
								bslib::card_body("Flight height distribution plot will go here")
							),

							# Second card will contain download options
							bslib::card(
								bslib::card_header("Download Options"),
								bslib::card_body("Download options content will go here")
							)
						)
					)
				)
  )
}
    
#' data_analysis Server Functions
#'
#' @param id Module ID
#' @param nav_id The ID of the parent navigation bar (e.g., "main-nav"). This is required to allow the module to control navigation between tabs.
#' @param parent_session The session object of the parent Shiny app. This is required to allow the module to control navigation between tabs.
#' 
#' 
#' @noRd 
mod_data_analysis_server <- function(id, nav_id = "main-nav", parent_session, selected_data, reset_trigger = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Render the dummy data passed from the previous
    output$dummy_dt <- DT::renderDataTable({
      selected_data()
    })

    # Listen for reset trigger from parent app
    observeEvent(reset_trigger(), {
      # Any module-level state should be reset here
      # This example just re-renders the table, which updates when selected_data changes
    })
    
  })
}
    
## To be copied in the UI
# mod_data_analysis_ui("data_analysis_1")
    
## To be copied in the server
# mod_data_analysis_server("data_analysis_1")
