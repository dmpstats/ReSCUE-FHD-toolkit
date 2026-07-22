#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import conductor
#' @import sf
#' @noRd
app_server <- function(input, output, session) {
	# Track Mode and switch when required
	mode <- reactiveVal("flight")

	restore_payload <- reactive({
		req(input$restore_file)
		readRDS(input$restore_file$datapath)
	})

	# Call the top-level metadata
	metadata <- lapply(metadata_files, readRDS) |>
		# need to nest covariates list into a higher-level list so that list-columns are
		# preserved
		purrr::map(
			function(x) {
				#browser()
				x$covariates <- list(x$covariates %||% NULL)
				x
			}
		) |>
		purrr::map(tibble::as_tibble_row) |>
		purrr::list_rbind()

	# Modules -----------------

	mod_landing_page_server(
		"landing_page",
		nav_id = "main-nav",
		parent_session = session
	)

	data_select_output <- mod_data_select_server(
		"data_select",
		nav_id = "main-nav",
		parent_session = session,
		metadata_tbl = metadata,
		restore_payload = restore_payload
	)

	mod_data_analysis_server(
		"data_analysis",
		nav_id = "main-nav",
		parent_session = session,
		selected_data = data_select_output$selected_data
	)

	# React to page-reset button --------------

	observeEvent(
		input$reset_app,
		{
			session$reload()
		}
	)

	# Save session --------------

	observeEvent(
		input$save_session,
		{
			# Open a modal with download instructions and a downloadButton#
			showModal(
				modalDialog(
					title = "Save Session",
					div(
						p("Click the button below to download your session data."),
						br(),
						br(),
						# This is ugly, but the downloadButton doesn't work properly in the modal
						downloadLink(
							outputId = "download_session",
							label = tags$span(
								bsicons::bs_icon("download"),
								"Download Session Data"
							)
						),
						style = "text-align: center; margin-top: 40px;"
					),
					easyClose = TRUE,
					size = "m"
				)
			)
		}
	)
	output$download_session <- downloadHandler(
		filename = "rescue_session.rds",
		content = function(file) {
			saveRDS(
				list(
					map_data = data_select_output$map_data(),
					current_tab = input[["main-nav"]]
				),
				file
			)
		}
	)

	# Restore session --------------

	observeEvent(
		input$restore_session,
		{
			showModal(
				modalDialog(
					title = "Restore Session",
					p(
						"Upload your previously saved session data file (.rds) to restore your session."
					),
					div(
						fileInput(
							inputId = "restore_file",
							label = "Upload your session data file (.rds)",
							accept = c(".rds"),
							width = "80%"
						),
						style = "text-align: center; margin-top: 20px; width: 100%;"
					),
					easyClose = TRUE,
					size = "m"
				)
			)
		}
	)

	# When the restore file is uploaded, close the modal ---------
	observeEvent(
		restore_payload(),
		{
			removeModal()
			# Switch to the previously-selected tab
			bslib::nav_select(
				id = "main-nav",
				selected = restore_payload()$current_tab
			)
		}
	)

	# Helper functions
	mod_version_button_server("app_version")
}
