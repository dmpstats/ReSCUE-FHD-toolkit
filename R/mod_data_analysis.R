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

				tagList(
					# 12,
					# First card: selected data
					bslib::card(
						bslib::card_header("Selected Data", class = "text-bg-primary"),
						bslib::card_body(DT::DTOutput(ns("dummy_dt"))),
						# Ensure this card doesn't cover more than 30% of the page height
						style = "max-height: 30vh; overflow-y: auto;",
						class = "card border-primary mb-3 bg-light"
					),
					# Second card: defining turbine parameters
					bslib::card(
						bslib::card_header(
							style = "display:flex; align-items:center; justify-content:space-between;",
							class = "text-bg-primary",
							htmltools::span("Turbine Parameters"),
							bslib::toolbar(
								bslib::toolbar_input_select(
									id = ns("use_turb"),
									label = "Use Turbine Parameters",
									choices = c("On", "Off"),
									class = "bg-light text-primary"
								)
							)
						),
						bslib::card_body(
							fluidRow(
								bslib::layout_columns(
									col_widths = c(6, 6),
									numericInput(
										ns("rotor_min"),
										"Minimum Rotor Height (m)",
										value = 50,
										min = 0
									),
									numericInput(
										ns("rotor_max"),
										"Maximum Rotor Height (m)",
										value = 70,
										min = 0
									)
								)
							)
						),
						class = "card border-primary mb-3 bg-light",
						style = "max-height: 16vh; overflow-y: auto;"
					),

					# Third card: analysis results
					bslib::card(
						bslib::card_header("Analysis Results", class = "text-bg-success"),
						bslib::card_body("Analysis results content will go here"),
						class = "card border-success mb-3 bg-light"
					)
				),

				# Second column will contain the plot and output distributions
				tagList(
					# 12,
					bslib::card(
						id = ns("card_fhdplot"),
						bslib::card_header(
							"Flight Height Distribution", 
							class = "text-bg-primary"
						),
						bslib::card_body(
							plotly::plotlyOutput(ns("dummy_plot")),
							uiOutput(ns("debug"))
						),
						class = "card border-primary mb-3 bg-light"
					),

					# Second card will contain download options
					bslib::card(
						bslib::card_header("Download Options", class = "text-bg-primary"),
						bslib::card_body("Download options content will go here"),
						class = "card border-primary mb-3 bg-light"
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
mod_data_analysis_server <- function(
	id,
	nav_id = "main-nav",
	parent_session,
	selected_data
) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns

		# Render the dummy data passed from the previous
		output$dummy_dt <- DT::renderDataTable(
			{
				selected_data()
			},
			options = list(
				paging = FALSE,
				searching = FALSE,
				info = FALSE,
				scrollY = "200px",
				scrollCollapse = TRUE
			)
		)

		# Simulate some dummy data
		dummy_data <- reactive({
			req(selected_data())
			if (nrow(selected_data()) == 0) {
				return(NULL)
			}
			dummy_fheight_dists(
				max_height = 100,
				seed = 123,
				n = nrow(selected_data())
			)
		})

		output$dummy_plot <- plotly::renderPlotly({
			req(selected_data())
			req(dummy_data())
			dummy_fheight_plot(
				dummy_data(),
				risk_min = input$rotor_min,
				risk_max = input$rotor_max
			)
		})
	})
}
