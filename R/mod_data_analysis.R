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
						bslib::card_header(
							"Selected Data",
							class = "text-bg-primary"
						),
						bslib::card_body(DT::DTOutput(ns("selected_data_dt"))),
						# Ensure this card doesn't cover more than 30% of the page height
						style = "max-height: 30vh; overflow-y: auto;",
						class = "card border-primary mb-3 bg-light"
					),
					# Second card: analysis results
					bslib::navset_card_underline(
						title = "Analysis",
						bslib::nav_spacer(),
						bslib::nav_panel(
							title = "Risk Height"
						),
						bslib::nav_panel(
							title = "Compare Distributions"
						),
						bslib::nav_item(
							actionButton(
								ns("test"),
								icon = bsicons::bs_icon("gear"),
								label = "Turbine Parameters",
								class = "btn-success"
							) |>
								bslib::popover(
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
						),
						full_screen = TRUE
					) |>
						htmltools::tagAppendAttributes(
							class = "border-success mb-3 bg-light header-primary"
						)
				),

				# Second column will contain the plot and output distributions
				tagList(
					# 12,
					bslib::card(
						id = ns("card_fhdplot"),
						bslib::card_header(
							"Flight Height Distribution",
							class = "text-bg-primary",
							bslib::toolbar_spacer()
						),
						bslib::card_body(
							# class = "card-body-white",
							plotly::plotlyOutput(ns("dummy_plot")),
							uiOutput(ns("debug"))
						),
						class = "card border-primary mb-3 card-body-white"
					),

					# Second card will contain download options
					bslib::card(
						bslib::card_header("Download Options", class = "text-bg-primary"),
						bslib::card_body("Download options content will go here"),
						class = "card border-primary mb-3 bg-light"
					)
				)
			),
			div(
				class = "d-flex flex-column align-items-start gap-2 my-3",
				actionButton(
					ns("go_data_selection"),
					label = tagList(
						bsicons::bs_icon("play-circle"),
						"Data Selection"
					),
					full_screen = TRUE,
					class = "left-arrow-btn"
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
#' @noRd
mod_data_analysis_server <- function(
	id,
	nav_id = "main-nav",
	parent_session,
	selected_data
) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns

		# Get the draws and metadata

		# Render the dummy data passed from the previous
		output$selected_data_dt <- DT::renderDataTable(
			{
				req(selected_data$metadata)
				req(nrow(selected_data$metadata) > 0)
				selected_data$metadata |>
					dplyr::select(
						name_common,
						method,
						spatial_scale,
						temporal_scale,
						season,
						year,
						region,
						site,
						group,
						input_type
					)
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
			req(selected_data$metadata)
			if (nrow(selected_data$metadata) == 0) {
				return(NULL)
			}
			dummy_fheight_dists(
				max_height = 100,
				seed = 123,
				n = nrow(selected_data$metadata)
			)
		})

		output$dummy_plot <- plotly::renderPlotly({
			req(selected_data$metadata)
			req(dummy_data())
			dummy_fheight_plot(
				dummy_data(),
				risk_min = input$rotor_min,
				risk_max = input$rotor_max
			)
		})

		# React to go to data selection tab --------------
		observeEvent(
			input$go_data_selection,
			{
				bslib::nav_select(
					id = nav_id,
					selected = "nav-data-select",
					session = parent_session
				)
			}
		)
	})
}
