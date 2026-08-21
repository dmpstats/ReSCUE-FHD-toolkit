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
						# bslib::card_header(
						# 	"Selected Data",
						# 	class = "text-bg-primary"
						# ),
						bslib::card_body(
							uiOutput(ns("fhd_config_table"))
						),
						# Ensure this card doesn't cover more than 40% of the page height
						style = "height: 35vh; overflow-y: auto;",
						class = "card border-primary mb-3 bg-light"
					),
					# Second card: analysis results
					bslib::navset_card_underline(
						title = "Proportion at CRH",
						bslib::nav_spacer(),
						bslib::nav_panel(
							title = "Summary",
							DT::DTOutput(ns("fhd_summaries"))
						),
						bslib::nav_panel(
							title = "Risk Height",
							fluidRow(
								bslib::input_switch(
									id = ns("show_as_percentages"),
									value = TRUE,
									label = "Percentage change",
									width = "40%"
								),
								bslib::input_switch(
									id = ns("condensed_table"),
									value = TRUE,
									label = "5m increments",
									width = "40%"
								),
								shinyWidgets::radioGroupButtons(
									ns("heightshift_output_type"),
									label = NULL,
									choiceNames = list(
										bsicons::bs_icon("table"),
										bsicons::bs_icon("graph-up")
									),
									choiceValues = list("table", "plot"),
									selected = "table",
									direction = "horizontal",
									size = "sm",
									justified = TRUE,
									width = "20%"
								)
							),
							# p(
							# 	"This table shows changes to the proportion of birds at collision-height
							# 	if the turbine height is increased or decreased. The first column shows the unique FHD identifiers, and the remaining columns show the probabilities of FHD risk for each height shift."
							# ),
							# Add a conditionalPanel that changes description based on input$show_as_percentages
							conditionalPanel(
								condition = paste0(
									"input['",
									ns("show_as_percentages"),
									"'] === true"
								),
								p(
									"This table shows changes to the percentage of birds at collision-height
									compared to the rotor sweapt area of your turbine for increases/decreases to the risk-height window. The first column shows the unique FHD identifiers, and the remaining columns show the percentage change in FHD risk for each height shift."
								)
							),
							conditionalPanel(
								condition = paste0(
									"input['",
									ns("show_as_percentages"),
									"'] === false"
								),
								p(
									"This table shows the raw proportion of birds at collision-height if the turbine height is increased or decreased. The first column shows the unique FHD identifiers, and the remaining columns show the change in FHD risk for each height shift."
								)
							),
							conditionalPanel(
								condition = paste0(
									"input['",
									ns("heightshift_output_type"),
									"'] === 'table'"
								),
								div(
									DT::DTOutput(ns("heightshift_table")),
									style = "height: 25vh; overflow-y: auto; overflow-x: auto;"
								)
							),
							conditionalPanel(
								condition = paste0(
									"input['",
									ns("heightshift_output_type"),
									"'] === 'plot'"
								),
								plotly::plotlyOutput(ns("heightshift_plot"), height = "25vh")
							)
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
										ns("airgap"),
										strong("Air Gap (m)"),
										value = 50,
										min = 0
									),
									numericInput(
										ns("rotor_radius"),
										strong("Rotor Radius (m)"),
										value = 10,
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
							bslib::toolbar(
								mod_help_button_ui(ns("help_fhd"), type = "toolbar")
							),
							bslib::toolbar_spacer(),
							bslib::input_switch(
								id = ns("hide_legend"),
								label = "Hide legend",
								value = FALSE
							),
							bslib::input_switch(
								id = ns("facet_plot"),
								label = "Facet plot",
								value = FALSE
							)
						),
						bslib::card_body(
							# class = "card-body-white",
							plotly::plotlyOutput(ns("fhd_plot")),
							uiOutput(ns("debug"))
						),
						class = "card border-primary mb-3 card-body-white"
					),

					# Second card will contain download options
					bslib::card(
						bslib::card_header(
							"Download Options",
							class = "text-bg-primary",
							bslib::toolbar(
								mod_help_button_ui(ns("help_download"), type = "toolbar")
							)
						),
						bslib::card_body(
							# Add a drop-down to select the FHD to output
							bslib::layout_columns(
								tagList(
									selectInput(
										ns("selected_fhd"),
										"Select FHD to download",
										choices = NULL,
										width = "100%"
									),
									downloadButton(
										ns("download_btn"),
										"Download",
										icon = bsicons::bs_icon("download"),
										class = "btn-success",
										width = "100%"
									),
									uiOutput(ns("covar_warning"))
								),
								shinyWidgets::prettyCheckboxGroup(
									ns("download_contents"),
									"Select contents to download",
									choices = c(
										"FHD Data" = "data",
										"FHD Plot" = "plot",
										"Metadata" = "metadata"
									),
									width = "100%",
									selected = c("data", "plot", "metadata"),
									status = "success"
								)
								# checkboxGroupInput(
								# 	ns("download_contents"),
								# 	"Select contents to download",
								# 	choices = c(
								# 		"FHD Data" = "data",
								# 		"FHD Plot" = "plot",
								# 		"Metadata" = "metadata"
								# 	),
								# 	width = "100%",
								# 	selected = c("data", "plot", "metadata")
								# )
							)
						),

						class = "card border-primary mb-3 bg-light"
					)
				)
			),
			# Add a button as an absolutePanel in the bottom-left corner
			shiny::absolutePanel(
				actionButton(
					ns("go_data_selection"),
					label = tagList(
						bsicons::bs_icon("play-circle"),
						"Data Selection"
					),
					full_screen = TRUE,
					class = "left-arrow-btn"
				),
				left = 10,
				bottom = 10,
				draggable = TRUE
			)
			# div(
			# 	class = "d-flex flex-column align-items-start gap-2 my-3",
			# 	actionButton(
			# 		ns("go_data_selection"),
			# 		label = tagList(
			# 			bsicons::bs_icon("play-circle"),
			# 			"Data Selection"
			# 		),
			# 		full_screen = TRUE,
			# 		class = "left-arrow-btn"
			# 	)
			# )
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

		# webshot2/chromote require a real Chromium-based browser. If Chrome
		# isn't found (e.g. on Windows machines that only have Edge), point
		# chromote at the first Chromium-based browser we can find so PNG
		# export doesn't fail outright.
		ensure_chromote_browser <- function() {
			if (nzchar(Sys.getenv("CHROMOTE_CHROME"))) {
				return(invisible(TRUE))
			}

			found <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
			if (!is.null(found) && nzchar(found)) {
				return(invisible(TRUE))
			}

			invisible(FALSE)
		}

		# ── Error state ───────────────────────────────────────────────────────────
		# Single reactive value used to surface processing errors as a toast.
		# NULL means "no error"; any other value is shown as the toast message.
		# Call `set_error(msg)` from within a reactive/observer to populate it.
		error_state <- reactiveVal(NULL)
		set_error <- function(msg) error_state(msg)

		# Show/hide the toast whenever error_state changes.
		observeEvent(
			error_state(),
			{
				if (!is.null(error_state())) {
					bslib::show_toast(
						bslib::toast(
							header = "Warning",
							error_state(),
							icon = bsicons::bs_icon("exclamation-triangle-fill"),
							type = "warning",
							duration_s = 0,
							id = "fhd_processing_warning",
							position = "bottom-right"
						)
					)
				} else {
					bslib::hide_toast("fhd_processing_warning")
				}
			},
			ignoreNULL = FALSE
		)

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
					switch_ids <- vapply(
						names(covs),
						function(cov_name) {
							paste0("cov_switch_", safe_id, "_", cov_name)
						},
						character(1)
					)

					cov_blocks <- lapply(seq_along(covs), function(ci) {
						cov_name <- names(covs)[ci]
						cov_meta <- covs[[cov_name]]
						switch_id <- switch_ids[[ci]]
						levels_id <- paste0(
							"cov_levels_",
							safe_id,
							"_",
							cov_name
						)
						cov_label <- cov_meta$label %||% cov_name
						cov_levels <- cov_meta$levels %||% character(0)

						tagList(
							# Separator between covariate blocks (skip before first)
							if (ci > 1) tags$hr(class = "my-2") else NULL,
							tags$div(
								# Row: covariate name on left, "Use" label + switch on right
								class = "d-flex align-items-center justify-content-between gap-2",
								tags$span(class = "small fw-semibold", cov_label),
								tags$div(
									class = "d-flex align-items-center gap-1",
									# tags$span(class = "small text-muted", "Use"),
									bslib::input_switch(
										id = ns(switch_id),
										label = NULL,
										value = FALSE
									)
								)
							),
							# Level checkboxes — indented with a left border when visible
							conditionalPanel(
								condition = paste0(
									"input['",
									ns(switch_id),
									"'] === true"
								),
								tags$div(
									class = "border-start border-2 ps-2 ms-1 mt-1",
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
					style = "min-width: 320px;",
					covariates_tab
				)

				# ---- List-group item -----------------------------------------
				tags$li(
					class = paste(
						"list-group-item d-flex align-items-center",
						"justify-content-between gap-2 px-2 py-2"
					),
					# LHS: index badge + Details button + label
					tags$div(
						class = "d-flex align-items-center gap-2 overflow-hidden",
						tags$span(
							class = "badge bg-secondary flex-shrink-0",
							style = "font-size: 1.5rem;",
							i
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
								paste0(
									row$method,
									" \u00b7 ",
									row$season,
									" \u00b7 ",
									row$region
								)
							)
						)
					),
					# RHS: Button container
					tags$div(
						class = "d-flex gap-2",
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

			# Reset any stale error from a previous processing run, so a fixed
			# upstream issue doesn't leave the toast stuck on screen.
			error_state(NULL)

			purrr::imap(fhd_arrays(), function(arr, fhd_id) {
				sel <- cov_selections()[[fhd_id]]

				#browser()

				sliced_df <- tryCatch(
					{
						rlang::inject(
							slice_fhd(
								fhd_array = arr,
								!!!sel,
								out_format = "df",
								seed = 8421L # fixed seed for reproducible resampling
							)
						)
					},
					error = function(e) {
						cli::cli_alert_warning(
							"Failed to slice FHD {.val {fhd_id}}: {.val {e$message}}"
						)
						set_error(paste0(
							"Processing the selected FHDs failed for '",
							fhd_id,
							"': ",
							e$message
						))
						return(NULL)
					}
				)

				sliced_df$fhd_id <- fhd_id
				sliced_df
			})
		})

		# ── Step 4: Combine all processed FHDs into one long data frame ───────────
		plot_ready_data <- reactive({
			req(processed_fhds())
			req(length(processed_fhds()) > 0)
			# The columns we can always expect are height, draw_id, probability, fhd_id.
			# Any additional columns are covariates.
			# Create a new column identifying each unique FHD, i.e. unique
			# combination of fhd_id and covariate levels.
			alldat <- dplyr::bind_rows(processed_fhds(), .id = "fhd_id")
			expected_cols <- c("height", "draw_id", "probability", "fhd_id")
			unexpected_cols <- setdiff(
				lapply(
					processed_fhds(),
					names
				) |>
					unlist(),
				expected_cols
			)
			uuid <- alldat |>
				dplyr::select(dplyr::all_of(c("fhd_id", unexpected_cols))) |>
				dplyr::distinct() |>
				dplyr::group_by(fhd_id) |>
				dplyr::mutate(
					unique_fhd = if (length(unexpected_cols) == 0 || dplyr::n() == 1L) {
						# No covariates, or only one combination — use fhd_id directly
						fhd_id
					} else {
						# Multiple splits of the same FHD — append covariate values in brackets
						paste0(
							fhd_id,
							" [",
							apply(
								dplyr::pick(dplyr::all_of(unexpected_cols)),
								1,
								function(x) paste(na.omit(x), collapse = " \u00b7 ")
							),
							"]"
						)
					}
				) |>
				dplyr::ungroup()
			out <- alldat |>
				dplyr::left_join(uuid, by = c("fhd_id", unexpected_cols))

			# If there are >10 unique FHDs, filter to only the first 10,
			# and show a warning toast.
			if (length(unique(out$unique_fhd)) > 10) {
				set_error(
					"More than 10 unique FHDs were generated. Only the first 10 will be analysed."
				)
				out <- dplyr::filter(
					out,
					unique_fhd %in% unique(out$unique_fhd)[1:10]
				)
			}

			# Populate the FHD selection drop-down with the unique FHDs.
			updateSelectInput(
				parent_session,
				ns("selected_fhd"),
				choices = unique(out$unique_fhd),
				selected = unique(out$unique_fhd)[1]
			)

			out
		})

		# Populate error_state when the plot-ready data is not suitable; the
		# observer on error_state() above takes care of showing/hiding the toast.
		observeEvent(plot_ready_data(), {
			req(plot_ready_data())
			if (
				nrow(plot_ready_data()) == 0 ||
					!all(
						c("height", "probability", "unique_fhd", "draw_id") %in%
							names(plot_ready_data())
					)
			) {
				set_error(
					"Processing the selected FHDs failed. Please check the covariate selections and try again."
				)
			}
		})

		# -- Step 5: Generate the height-shift FHD table -------------

		heightshift_data <- reactive({
			req(plot_ready_data())

			# if the input data is bad, return an empty dataset
			if (
				nrow(plot_ready_data()) == 0 ||
					!all(
						c("height", "probability", "unique_fhd", "draw_id") %in%
							names(plot_ready_data())
					)
			) {
				return(list(perc = data.frame(), prob = data.frame()))
			}

			heightshift(
				plot_ready_data(),
				height_col = "height",
				prob_col = "probability",
				id_col = "unique_fhd",
				draw_id_col = "draw_id",
				# converted from airgap and rotor radius to min and max risk heights
				risk_min = input$airgap,
				risk_max = input$airgap + 2 * input$rotor_radius,
				round = c(4, 2),
				condensed_table = input$condensed_table
			)
		})

		# Make the table
		output$heightshift_table <- DT::renderDataTable(
			{
				type <- ifelse(input$show_as_percentages, "perc", "prob")
				req(heightshift_data())
				req(nrow(heightshift_data()[[type]]) > 0)

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

				num_cols <- ncol(heightshift_data()[[type]])
				out <- DT::datatable(
					heightshift_data()[[type]],
					rownames = FALSE,
					extensions = c("FixedHeader"),
					options = list(
						dom = "Bfrt",
						# buttons = c("copy", "csv", "excel", "pdf", "print"),
						fixedHeader = TRUE,
						pageLength = 10,
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
							columns = 2:ncol(heightshift_data()[[type]]),
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

		# ---- Step 5b: Heightshift plot ----
		output$heightshift_plot <- plotly::renderPlotly({
			req(heightshift_data())
			req(nrow(heightshift_data()[["perc"]]) > 0)
			heightshift_plot(
				heightshift_data = heightshift_data(),
				show_as_percentages = input$show_as_percentages
			)
		})

		# ---- Step 6: Generate the FHD summaries ----
		output$fhd_summaries <- DT::renderDataTable({
			req(plot_ready_data())
			if (
				nrow(plot_ready_data()) == 0 ||
					!all(
						c("height", "probability", "unique_fhd", "draw_id") %in%
							names(plot_ready_data())
					)
			) {
				return(DT::datatable(
					data.frame(
						Message = "No valid data available for summary."
					),
					rownames = FALSE,
					options = list(
						dom = "Bfrt",
						fixedHeader = TRUE,
						pageLength = 10,
						searching = FALSE,
						lengthMenu = c(5, 10, 25, 50, 100),
						scrollX = TRUE,
						scrollY = "30vh"
					)
				))
			}
			sumdat <- make_fhd_summary(
				plot_ready_data(),
				id_col = "unique_fhd",
				height_col = "height",
				prob_col = "probability",
				draw_col = "draw_id",
				risk_min = input$airgap,
				risk_max = input$airgap + (2 * input$rotor_radius)
			) #|>
			# # Add a % to all cols except the first
			# dplyr::mutate(
			# 	dplyr::across(
			# 		-1,
			# 		~ paste0(.x, "%")
			# 	)
			# )

			DT::datatable(
				sumdat,
				caption = htmltools::tags$caption(
					style = "caption-side: top; text-align: left;",
					"Proportion of birds at collision height (CRH) for each unique FHD, with 50% confidence intervals."
				),
				rownames = FALSE,
				extensions = c("FixedHeader"),
				options = list(
					dom = "Bfrt",
					fixedHeader = TRUE,
					pageLength = 10,
					searching = FALSE,
					lengthMenu = c(5, 10, 25, 50, 100),
					scrollX = TRUE,
					scrollY = "30vh"
				),
			) |>
				DT::formatRound(columns = 2:ncol(sumdat), digits = 3)
		})

		# ── FHD plot ──────────────────────────────────────────────────────────────
		# Build the plot as a reactive so it can be reused for rendering and export
		fhd_plot_object <- reactive({
			req(plot_ready_data())

			# Guard: return empty base plot if data is invalid
			bad_data <- nrow(plot_ready_data()) == 0 ||
				!all(
					c("height", "probability", "unique_fhd", "draw_id") %in%
						names(plot_ready_data())
				)

			if (bad_data) {
				return(fhd_baseplot(
					risk_min = input$airgap,
					risk_max = input$airgap + (2 * input$rotor_radius)
				))
			}

			# ── Faceted view ─────────────────────────────────────────────────────
			if (isTRUE(input$facet_plot)) {
				return(fhd_facet_plot(
					plot_data = plot_ready_data(),
					id_col = "fhd_id",
					unique_id_col = "unique_fhd",
					height_col = "height",
					draw_col = "draw_id",
					prob_col = "probability",
					risk_min = input$airgap,
					risk_max = input$airgap + (2 * input$rotor_radius),
					show_legend = !isTRUE(input$hide_legend)
				))
			}

			# ── Combined view ────────────────────────────────────────────────────
			used_covs <- cov_selections() |>
				purrr::map(~ names(Filter(\(x) !is.null(x), .x))) |>
				purrr::reduce(union, .init = character(0))

			max_prob <- 0
			meta <- selected_data$metadata
			plt <- fhd_baseplot(
				risk_min = input$airgap,
				risk_max = input$airgap + (2 * input$rotor_radius)
			)

			fhd_ids <- unique(plot_ready_data()$fhd_id)
			for (id in fhd_ids) {
				fhd_subset <- dplyr::filter(plot_ready_data(), fhd_id == id)
				# Only pass used_covs that actually exist as columns in this subset
				plot_by_cov <- intersect(used_covs, names(fhd_subset))
				# Row index in metadata — used as the legend prefix when covariates
				# are active so entries from different FHDs never collide (e.g. "1.cold.low")
				fhd_index <- match(id, meta$fhd_id)

				max_prob <- max(max_prob, max(fhd_subset$probability, na.rm = TRUE))

				plt <- add_fhd(
					plot = plt,
					fhd_data = fhd_subset,
					id_col = "fhd_id",
					height_col = "height",
					draw_col = "draw_id",
					prob_col = "probability",
					plot_by_cov = if (length(plot_by_cov) > 0) plot_by_cov else NULL,
					index = if (length(plot_by_cov) > 0) fhd_index else NULL
				)

				plt <- plt |>
					plotly::layout(
						yaxis = list(range = c(0, max_prob * 1.1))
					)
			}

			if (isTRUE(input$hide_legend)) {
				plt <- plt |> plotly::layout(showlegend = FALSE)
			}

			plt
		})

		# Render the plot reactive to the UI
		output$fhd_plot <- plotly::renderPlotly({
			fhd_plot_object()
		})

		# ---- Track FHDs with covars -----------------------------------------------
		fhds_with_active_covs <- reactive({
			req(cov_selections())

			# Return vector of fhd_ids that have ANY covariate actively selected
			cov_selections() |>
				purrr::map_lgl(function(fhd_covs) {
					# Check if ANY covariate has non-NULL (i.e., selected) levels
					any(!sapply(fhd_covs, is.null))
				}) |>
				# Get which are TRUE
				which() |>
				# Get the names (fhd_ids) of those TRUE entries
				names()
		})

		# Render a warning if the selected FHD is in fhds_with_active_covs
		output$covar_warning <- renderUI({
			req(input$selected_fhd)
			if (
				length(fhds_with_active_covs() > 0) &&
					stringr::str_detect(
						input$selected_fhd,
						paste(fhds_with_active_covs(), collapse = "|")
					)
			) {
				bslib::card(
					tags$p(
						"Warning: The selected FHD has covariate selections active. This makes it unsuitable for use in an SCRM analysis."
					),
					tags$p(
						"You may still download the outputs, but it should not be used in an SCRM analysis."
					),
					class = "card border-warning"
				)
			} else {
				NULL
			}
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

		# Download button handler --------------------------------------------------
		# Note: this must be a `downloadHandler()` bound to a `downloadButton()`
		# in the UI (not an `actionButton()` + separately-created output). Shiny
		# only triggers a browser download when the UI element is a
		# downloadButton/downloadLink whose outputId matches a downloadHandler;
		# defining the handler from inside an observeEvent on a plain
		# actionButton never gets requested by the browser, so nothing happens.
		output$download_btn <- downloadHandler(
			filename = function() {
				paste0(make.names(input$selected_fhd), ".zip")
			},
			content = function(file) {
				selected_fhd <- input$selected_fhd

				# Extract this fhd data
				fhd_data <- plot_ready_data() |>
					dplyr::filter(unique_fhd == selected_fhd)

				out <- prep_export(
					fhd_draws = fhd_data
				)
				metadata <- selected_data$metadata |>
					sf::st_drop_geometry() |>
					dplyr::mutate(
						ReSCUE_version = golem::get_golem_version(),
						export_time = Sys.time()
					) |>
					dplyr::left_join(
						plot_ready_data() |>
							dplyr::select(dplyr::all_of(c("fhd_id", "unique_fhd"))) |>
							dplyr::distinct(),
						by = "fhd_id"
					) |>
					dplyr::filter(
						unique_fhd == selected_fhd
					)

				# Create a temporary directory for export files
				temp_export_dir <- file.path(
					tempdir(),
					paste0("fhd_export_", Sys.time() |> format("%s"))
				)
				dir.create(temp_export_dir, showWarnings = FALSE, recursive = TRUE)
				on.exit(unlink(temp_export_dir, recursive = TRUE), add = TRUE)

				# Save metadata to tempdir
				metadata_path <- file.path(temp_export_dir, "metadata.json")
				jsonlite::write_json(metadata, metadata_path, pretty = TRUE)

				# Save draws to tempdir
				data_path <- file.path(temp_export_dir, "fhd_data.csv")
				readr::write_csv(out, data_path)

				# Export the current plot to PNG using the reactive plot object.
				# We render to a temporary HTML widget and screenshot it with
				# webshot2 (headless Chrome) rather than plotly::save_image(),
				# which depends on the Python "kaleido" package via reticulate
				# and is fragile/unavailable in many R environments.
				if (!ensure_chromote_browser()) {
					set_error(paste0(
						"Could not find a Chromium-based browser (Chrome or Edge) ",
						"to render the plot image. The FHD data and metadata will ",
						"still be exported, but the PNG plot will be omitted."
					))
				} else {
					plot_html_path <- file.path(temp_export_dir, "fhd_plot.html")
					htmlwidgets::saveWidget(
						fhd_plot_object(),
						file = plot_html_path,
						selfcontained = TRUE
					)

					plot_path <- file.path(temp_export_dir, "fhd_plot.png")
					tryCatch(
						webshot2::webshot(
							url = plot_html_path,
							file = plot_path,
							vwidth = 1000,
							vheight = 700,
							zoom = 2
						),
						error = function(e) {
							cli::cli_alert_warning(
								"Failed to export FHD plot to PNG: {.val {e$message}}"
							)
							set_error(paste0(
								"Exporting the plot image failed: ",
								e$message,
								". The FHD data and metadata will still be exported."
							))
						}
					)

					# The intermediate HTML isn't part of the deliverable
					unlink(plot_html_path)
				}

				# Zip the files in the export directory. Use relative paths to
				# avoid issues with zip::zip() and Windows path separators.
				# Change to the temp dir, zip relative to it, then restore —
				# restore explicitly (via tryCatch/finally) *before* the
				# temp-dir cleanup on.exit above runs, since Windows can
				# refuse to delete a directory that is still the process's
				# working directory.
				old_wd <- getwd()
				setwd(temp_export_dir)
				tryCatch(
					zip::zip(
						zipfile = file,
						files = list.files(full.names = FALSE)
					),
					finally = setwd(old_wd)
				)
			}
		)

		# Help button servers -----
		mod_help_button_server("help_fhd", help_file = "fhd_plot", size = "xl")
		mod_help_button_server(
			"help_download",
			help_file = "download_options",
			size = "xl"
		)
	})
}
