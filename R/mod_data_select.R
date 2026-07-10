#' data_select UI Function
#'
#' @description A shiny Module allowing users to select flight-height datasets for analysis and comparison
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_select_ui <- function(id) {
	ns <- NS(id)
	tagList(
		bslib::page_fillable(
			bslib::layout_columns(
				col_widths = c(6, 6),
				class = "h-100",
				bslib::accordion(
					multiple = FALSE,
					class = "card border-primary mb-3 bg-light h-80",
					# Map selection tab ----------
					bslib::accordion_panel(
						title = "Map Selection",
						br(),
						mod_map_picker_ui(ns("map_picker_1")),
						class = "bg-light"
					),

					# Table selection tab ----------
					bslib::accordion_panel(
						title = "Table Selection",
						br(),
						mod_table_picker_ui(ns("table_picker_1")),
						class = "bg-light"
					),

					# User upload tab ----------
					bslib::accordion_panel(
						title = "Data Upload",
						br(),
						mod_user_upload_ui(ns("data_upload_1")),
						class = "bg-light"
					),
					# bslib::nav_item(
					# 	mod_help_button_ui(ns("select_data"))
					# ),
				),

				# Right-hand side: show selected data and go to analysis button
				tagList(
					# 12,
					bslib::card(
						bslib::card_header("Selected Data", 
						class = "text-bg-primary",
						bslib::toolbar(
							mod_help_button_ui(ns("select_data"), type = "toolbar")
						)
					),
						bslib::card_body(DT::DTOutput(ns("show_dt"))),
						class = "card border-primary mb-3 bg-light",
						full_screen = TRUE,
						height = "30vh"
					),
					bslib::card(
						bslib::card_header
						("Flight Height Preview",
						class = "text-bg-primary"
					),
						bslib::card_body(
							div(
								plotly::plotlyOutput(ns("dummy_plot"), height = "35vh"),
								class = "rounded-box"
							)
						),
						class = "card border-primary mb-3 bg-light"
					),
					div(
						class = "d-flex flex-column align-items-center gap-2 my-3",
						actionButton(
							ns("go_analysis"),
							label = tagList(
								bsicons::bs_icon("play-circle"),
								"Start Analysis"
							),
							full_screen = TRUE,
							class = "arrow-btn"
						)
					)
				)
			)
		)
	)
}

#' data_select Server Functions
#'
#' @param id Module ID
#' @param nav_id The ID of the parent navigation bar (e.g., "main-nav"). This is required to allow the module to control navigation between tabs.
#' @param parent_session The session object of the parent Shiny app. This is required to allow the module to control navigation between tabs.
#'
#' @noRd
mod_data_select_server <- function(id, nav_id = "main-nav", parent_session) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns

		# Data selection sub-modules  ------------
		# We receive the reactive inputs from each one:
		map_data <- mod_map_picker_server("map_picker_1")
		table_data <- mod_table_picker_server("table_picker_1")
		user_data <- mod_user_upload_server("data_upload_1")

		# Helpfile modules
		mod_help_button_server("select_data", help_file = "select_data", size = "l")

		# Stack these and render them on the RHS
		selected_data <- reactive({
			dplyr::bind_rows(
				map_data(),
				table_data(),
				user_data()
			) |>
				dplyr::select(
					dplyr::any_of(
						c("species_id", "method", "region", "season")
					)
				)
		})
		output$show_dt <- DT::renderDataTable({
			selected_data()
		},
		options = list(
			paging = FALSE,
			searching = FALSE,
			info = FALSE,
			scrollY = "200px",
			scrollCollapse = TRUE
		))

		# For now, plot a dummy flight-height preview
		output$dummy_plot <- plotly::renderPlotly({
			dumdat <- dummy_fheight_dists(max_height = 100, seed = 123, n = 1)
			dummy_fheight_plot(dumdat, risk_min = NULL, risk_max = NULL)
		})

		# React to next-page button --------------
		observeEvent(
			input$go_analysis,
			{

				# If the user has not selected any data, show a modal warning
				if (is.null(selected_data()) | nrow(selected_data()) == 0) {
					shiny::showNotification(
						"No data selected!",
						type = "error",
						duration = 3
					)
					return()
				}

				bslib::nav_select(
					id = nav_id,
					selected = "Analysis",
					session = parent_session
				)
			}
		)

		# Return the selected data as a reactive --------
		return(selected_data)
	})
}
