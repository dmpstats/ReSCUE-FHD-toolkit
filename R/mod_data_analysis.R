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
						bslib::card_body(
							uiOutput(ns("fhd_config_table"))
						),
						# Ensure this card doesn't cover more than 30% of the page height
						style = "max-height: 35vh; overflow-y: auto;",
						class = "card border-primary mb-3 bg-light"
					),
					# Second card: analysis results
					bslib::navset_card_underline(
						title = "Analysis",
						bslib::nav_spacer(),
						bslib::nav_panel(
							title = "Risk Height",
							bslib::input_switch(
								id = ns("show_as_percentages"),
								value = TRUE,
								label = "Show as percentages"
							),
							DT::DTOutput(ns("heightshift_table"))
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
						bslib::card_header(
							"Download Options",
							class = "text-bg-primary"
						),
						bslib::card_body(
							"Download options content will go here"
						),
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

		# ── Helpers ───────────────────────────────────────────────────────────────
		# Sanitise fhd_id into a valid Shiny input-ID fragment.
		make_safe_id <- function(x) gsub("[^A-Za-z0-9]", "_", x)

		# ── Per-row popover table ─────────────────────────────────────────────────
		output$fhd_config_table <- renderUI({
			req(selected_data$metadata)
			meta <- selected_data$metadata
			if (nrow(meta) == 0) {
				return(
					tags$p(
						class = "text-muted small py-2 mb-0",
						bsicons::bs_icon("info-circle"),
						" No datasets selected."
					)
				)
			}

			rows <- lapply(seq_len(nrow(meta)), function(i) {
				row <- meta[i, ]
				fhd_id <- row$fhd_id
				safe_id <- make_safe_id(fhd_id)
				covs <- row$covariates[[1]] # named list or NULL

				# ---- Tab 1: Covariates ----------------------------------------
				covariates_tab <- if (is.null(covs) || length(covs) == 0) {
					tags$p(
						class = "text-muted small mb-0 py-1",
						bsicons::bs_icon("info-circle"),
						" No covariates available for this dataset."
					)
				} else {
					cov_blocks <- lapply(names(covs), function(cov_name) {
						cov_meta <- covs[[cov_name]]
						switch_id <- paste0(
							"cov_switch_",
							safe_id,
							"_",
							cov_name
						)
						levels_id <- paste0(
							"cov_levels_",
							safe_id,
							"_",
							cov_name
						)
						cov_label <- cov_meta$label %||% cov_name
						cov_levels <- cov_meta$levels %||% character(0)

						tags$div(
							class = "mb-2",
							tags$div(
								class = "d-flex align-items-center justify-content-between",
								tags$span(
									class = "small fw-semibold",
									cov_label
								),
								bslib::input_switch(
									id = ns(switch_id),
									label = "Use",
									value = FALSE
								)
							),
							conditionalPanel(
								condition = paste0(
									"input['",
									ns(switch_id),
									"'] === true"
								),
								tags$div(
									class = "ms-1 mt-1",
									checkboxGroupInput(
										inputId = ns(levels_id),
										label = tags$span(
											class = "small text-muted",
											"Levels:"
										),
										choices = cov_levels,
										selected = cov_levels,
										inline = TRUE
									)
								)
							)
						)
					})

					tagList(!!!cov_blocks)
				}

				# ---- Popover content: covariates only --------------------------------
				popover_content <- tags$div(
					style = "min-width: 280px;",
					covariates_tab
				)

				# ---- List-group item -----------------------------------------
				tags$li(
					class = paste(
						"list-group-item d-flex align-items-center",
						"justify-content-between gap-2 px-2 py-2"
					),
					# LHS: Details button + label
					tags$div(
						class = "d-flex align-items-center gap-2 overflow-hidden",
						tags$button(
							class = "btn btn-sm btn-outline-info flex-shrink-0",
							onclick = paste0(
								"Shiny.setInputValue('",
								ns("det_btn_click"),
								"', '",
								fhd_id,
								"', {priority:'event'})"
							),
							bsicons::bs_icon("card-text"),
							" Details"
						),
						tags$div(
							class = "overflow-hidden",
							tags$span(
								class = "fw-semibold small d-block lh-sm text-truncate",
								row$name_common
							),
							tags$span(
								class = "text-muted",
								style = "font-size: 0.72rem;",
								paste0(row$method, " \u00b7 ", row$season)
							)
						)
					),
					# RHS: Configure button + popover
					actionButton(
						inputId = ns(paste0("cfg_btn_", safe_id)),
						label = tagList(
							bsicons::bs_icon("sliders"),
							" Configure"
						),
						class = "btn btn-sm btn-outline-secondary flex-shrink-0"
					) |>
						bslib::popover(
							title = row$name_common,
							placement = "right",
							popover_content
						)
				)
			})

			tags$ul(
				class = "list-group list-group-flush",
				!!!rows
			)
		})

		# ── Step 1: Convert raw draws to fhd_array objects ───────────────────────
		fhd_arrays <- reactive({
			req(selected_data$draws)
			purrr::imap(selected_data$draws, function(draws_df, fhd_id) {
				fhd_df_to_array(
					draws_df,
					height_col = "height",
					draw_col = "draw_id",
					prob_col = "probability"
				)
			})
		})

		# ── Step 2: Read per-FHD covariate selections from dynamic inputs ─────────
		# Returns a named list (one entry per fhd_id).
		# Each entry is a named list: cov_name -> NULL (drop) or character vector of levels.
		cov_selections <- reactive({
			req(selected_data$metadata)
			meta <- selected_data$metadata

			purrr::map(
				seq_len(nrow(meta)),
				function(i) {
					row <- meta[i, ]
					fhd_id <- row$fhd_id
					safe_id <- make_safe_id(fhd_id)
					covs <- row$covariates[[1]]

					if (is.null(covs) || length(covs) == 0) {
						return(list())
					}

					purrr::map(names(covs), function(cov_name) {
						switch_id <- paste0(
							"cov_switch_",
							safe_id,
							"_",
							cov_name
						)
						levels_id <- paste0(
							"cov_levels_",
							safe_id,
							"_",
							cov_name
						)
						using_cov <- isTRUE(input[[switch_id]])

						if (using_cov) {
							lvls <- input[[levels_id]]
							# Treat an empty level selection the same as "drop"
							if (length(lvls) == 0) NULL else lvls
						} else {
							NULL
						}
					}) |>
						setNames(names(covs))
				}
			) |>
				setNames(meta$fhd_id)
		})

		# ── Step 3: Slice each array by covariate selections → labelled data frames
		processed_fhds <- reactive({
			req(fhd_arrays(), cov_selections())

			purrr::imap(fhd_arrays(), function(arr, fhd_id) {
				sel <- cov_selections()[[fhd_id]]

				sliced_df <- rlang::inject(
					slice_fhd(
						fhd_array = arr,
						!!!sel,
						out_format = "df",
						seed = 8421L # fixed seed for reproducible resampling
					)
				)

				sliced_df$fhd_id <- fhd_id
				sliced_df
			})
		})

		# ── Step 4: Combine all processed FHDs into one long data frame ───────────
		plot_ready_data <- reactive({
			req(processed_fhds())
			# The columns we can always expect are height, draw_id, probability, fhd_id.
			# Any additional columns are covariates.
			# Create a new column identifying each unique FHD, i.e. unique
			# combination of fhd_id and covariate levels.
			alldat <- dplyr::bind_rows(processed_fhds(), .id = "fhd_id")
			expected_cols <- c("height", "draw_id", "probability", "fhd_id")
			unexpected_cols <- setdiff(
				names(processed_fhds()[[1]]),
				expected_cols
			)
			uuid <- alldat |>
				dplyr::select(dplyr::all_of(c("fhd_id", unexpected_cols))) |>
				dplyr::distinct() |>
				dplyr::mutate(unique_fhd = dplyr::row_number())
			alldat |>
				dplyr::left_join(uuid, by = c("fhd_id", unexpected_cols))
		})

		# -- Step 5: Generate the height-shift FHD table -------------
		heightshift_data <- reactive({
			req(plot_ready_data())
			heightshift(
				plot_ready_data(),
				height_col = "height",
				prob_col = "probability",
				id_col = "unique_fhd",
				draw_id_col = "draw_id",
				risk_min = input$rotor_min,
				risk_max = input$rotor_max,
				type = ifelse(input$show_as_percentages, "percentage", "probability"),
				round = ifelse(input$show_as_percentages, 2, 4)
			)
		})
		# Make the table
		output$heightshift_table <- DT::renderDataTable(
			{
				percjs <- if (input$show_as_percentages) {
					"function(data, type, row) {
						if(type === 'display' && data !== null) {
							var num = parseFloat(data);
							if(!isNaN(num)) {
								var prefix = num > 0 ? '+' : '';
								return prefix + data + '%';
							}
						}
						return data;
					}"
				} else {
					NULL
				}

				req(heightshift_data())
				num_cols <- ncol(heightshift_data())
				out <- DT::datatable(
					heightshift_data(),
					rownames = FALSE,
					extensions = c("Buttons", "FixedHeader"),
					options = list(
						dom = "Bfrt",
						buttons = c("copy", "csv", "excel", "pdf", "print"),
						fixedHeader = TRUE,
						pageLength = -1,
						searching = FALSE,
						lengthMenu = c(5, 10, 25, 50, 100),
						scrollX = TRUE,
						columnDefs = list(
							list(
								targets = 1:(num_cols - 1), # All columns except first (0-indexed)
								render = DT::JS(percjs)
							)
						)
					)
				)

				if (input$show_as_percentages) {
					out <- out |>
						DT::formatStyle(
							columns = 2:ncol(heightshift_data()),
							color = DT::styleInterval(
								0,
								c("green", "red")
							)
						)
				}

				out
			},
			# Use the "compact" style to reduce padding
			class = "compact stripe hover row-border",
			# Colour values green if negative and red if positive
			callback = DT::JS(
				"function(settings, json) {",
				"  $(this.api().table().header()).css({'background-color': '#f8f9fa', 'color': '#212529'});",
				"}"
			)
		)

		# ── FHD plot ──────────────────────────────────────────────────────────────
		output$dummy_plot <- plotly::renderPlotly({
			req(plot_ready_data())

			# Identify covariates being *used* across any FHD
			used_covs <- cov_selections() |>
				purrr::map(~ names(Filter(\(x) !is.null(x), .x))) |>
				purrr::reduce(union, .init = character(0))

			plt <- fhd_baseplot(
				risk_min = input$rotor_min,
				risk_max = input$rotor_max
			)

			fhd_ids <- unique(plot_ready_data()$fhd_id)
			for (id in fhd_ids) {
				fhd_subset <- dplyr::filter(plot_ready_data(), fhd_id == id)
				# Only pass used_covs that actually exist as columns in this subset
				plot_by_cov <- intersect(used_covs, names(fhd_subset))

				plt <- add_fhd(
					plot = plt,
					fhd_data = fhd_subset,
					id_col = "fhd_id",
					height_col = "height",
					draw_col = "draw_id",
					prob_col = "probability",
					plot_by_cov = if (length(plot_by_cov) > 0) {
						plot_by_cov
					} else {
						NULL
					}
				)
			}

			plt
		})

		# ── Details modal ────────────────────────────────────────────────────────
		observeEvent(input$det_btn_click, {
			fhd_id <- input$det_btn_click
			row <- selected_data$metadata[
				selected_data$metadata$fhd_id == fhd_id,
			]

			details_fields <- c(
				"fhd_id",
				"name_common",
				"name_scientific",
				"method",
				"spatial_scale",
				"temporal_scale",
				"season",
				"year",
				"country",
				"region",
				"site",
				"group",
				"crm_recommended",
				"input_type"
			)
			covs <- row$covariates[[1]]
			cov_names_str <- {
				cn <- names(covs)
				if (length(cn) == 0) "None" else paste(cn, collapse = ", ")
			}

			details_table <- tags$table(
				class = "table table-sm table-striped",
				tags$tbody(
					lapply(
						intersect(details_fields, names(row)),
						function(col) {
							tags$tr(
								tags$th(
									style = "width: 35%; white-space: nowrap;",
									col
								),
								tags$td(as.character(row[[col]]))
							)
						}
					),
					tags$tr(
						tags$th("covariates"),
						tags$td(cov_names_str)
					)
				)
			)

			showModal(modalDialog(
				title = tagList(
					bsicons::bs_icon("card-text"),
					" ",
					row$name_common
				),
				details_table,
				easyClose = TRUE,
				footer = modalButton("Close"),
				size = "m"
			))
		})

		# ── Navigation ────────────────────────────────────────────────────────────
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
