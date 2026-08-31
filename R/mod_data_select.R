#' data_select UI Function
#'
#' @description A shiny Module allowing users to select flight-height datasets for analysis and comparison
#'
#' @param id,input,output,session Internal parameters for {shiny}.#'
#'
#' @importFrom shiny NS tagList
#' @noRd
mod_data_select_ui <- function(id) {
	ns <- NS(id)
	tagList(
		tags$head(tags$style(
			type = "text/css",
			"
			/* Make dropdowns open upward for footer filters */
			.shiny-input-container:has(#data_select-species) .selectize-dropdown,
			.shiny-input-container:has(#data_select-method) .selectize-dropdown,
			.shiny-input-container:has(#data_select-season) .selectize-dropdown {
				bottom: 100% !important;
				top: auto !important;
			}
			/* Allow default_species/default_region dropdowns to escape container and open normally */
			.overflow-visible {
				overflow: visible !important;
			}
			.shiny-input-container:has(#data_select-default_region) .selectize-dropdown,
			.shiny-input-container:has(#data_select-default_species) .selectize-dropdown {
				bottom: auto !important;
				top: 100% !important;
			}
			/* Scoped to the map card only, so its footer selectize dropdowns can
			 * escape the card/tab boundaries without disabling overflow
			 * clipping/scrolling on other cards (e.g. the help text card). */
			.map-card, .map-card .card-body,
			.tab-content:has(.map-card), .tab-pane:has(.map-card) {
				overflow: visible !important;
			}
			/*
			 * When a selectize is open, elevate its entire control as a stacking context
			 * so the dropdown paints above sibling selectize inputs (z-index: 1) in the
			 * same container, without reaching the modal layer.
			 */
			.selectize-control.dropdown-active {
				position: relative;
				z-index: 100 !important;
			}
			/* Raise modal stack above bslib stacking contexts (bslib uses up to z-index 1070) */
			.modal-backdrop {
				z-index: 10000 !important;
			}
			.modal {
				z-index: 10005 !important;
			}
			"
		)),
		bslib::page_fillable(
			bslib::layout_columns(
				col_widths = c(6, 6),
				class = "h-100",
				tagList(
					bslib::card(
						id = ns("map_selection_card"),
						class = "card map-card border-primary mb-3 bg-light",
						height = "75vh",

						bslib::card_header(
							"Flight-Height Distributions",
							class = "text-bg-primary",
							bslib::toolbar(
								mod_help_button_ui(
									ns("select_data"),
									type = "toolbar"
								)
							)
						),
						bslib::card_body(
							leaflet::leafletOutput(
								ns("source_map"),
								# Fill the remainder of the space
								height = "75vh"
							),
							class = "p-0"
						),

						bslib::card_footer(
							id = ns("map_filters"),
							fluidRow(
								column(
									3,
									selectizeInput(
										ns("species"),
										label = "Species",
										choices = NULL,
										multiple = FALSE,
										options = list(
											hideSelected = FALSE,
											remove_button = TRUE
										)
									)
								),
								column(
									3,
									selectizeInput(
										ns("method"),
										label = "Method",
										choices = NULL,
										multiple = TRUE
									)
								),
								column(
									3,
									selectizeInput(
										ns("season"),
										label = "Season",
										choices = c(
											"Any" = "any",
											"Both" = "both",
											"Breeding" = "breeding",
											"Non-breeding" = "nonbreeding"
										),
										multiple = FALSE
									)
								),
								column(
									3,
									class = "d-flex align-items-center justify-content-center",
									actionButton(
										ns("advanced_filters"),
										label = tagList(
											bsicons::bs_icon("funnel"),
											"Advanced Filters"
										),
										class = "btn btn-dark w-100"
									) |>
										bslib::popover(
											# Add some additional filters
											selectizeInput(
												ns("crm_recommended"),
												label = "Recommended for CRM?",
												choices = c(
													"Yes" = TRUE,
													"No" = FALSE
												),
												multiple = TRUE
											),
											selectizeInput(
												ns("region"),
												label = "Region",
												choices = NULL,
												multiple = TRUE
											),
											placement = "top"
										)
								),
								fluidRow(
									tags$small(
										"The map provides BDMPS [Biological Defined Minimum Population Scale] regions for each individual species. Each region represents a single flight-height distribution."
									)
								)
							)
						)
					),

					# TEMP: DUMMY DATA WARNING
					bslib::card(
						bslib::card_header(
							# Add warning icon and text "Warning!"
							tags$span(
								bsicons::bs_icon("exclamation-triangle-fill"),
								"Warning!"
							),
						),
						tags$strong(
							"
							This is a prototype version of the ReSCUEApp. The FHDs shown here are not real data, and are only for demonstration purposes.",
							style = "font-size: 14px;"
						),
						class = "card bg-warning"
						# max_height = "10vh"
					)
				),

				# Right-hand side: show selected data and go to analysis button
				tagList(
					bslib::card(
						class = "card border-primary mb-3 bg-light",
						# max_height = "15vh",
						bslib::card_header(
							tags$span(
								bsicons::bs_icon("question-circle-fill"),
								"  How to Select Data"
							)
						),
						bslib::card_body(
							div(
								style = "font-size: 14px;",
								# Some information on this page
								tags$p(
									"Select flight-height distributions from the map You can filter the FHDs by species, method, season, and other criteria. Once you have selected the datasets you want to analyze, click 'Start Analysis' to proceed.",
								),
								tags$p(
									"Note: You can select up to 10 FHDs for analysis. If you select more than 10, only the first 10 will be analyzed."
								)
							)

							# Left-side: some text
							# bslib::layout_columns(
							# 	col_widths = c(8, 4),
							# 	HTML(
							# 		"<h4>Recommended Defaults</h4>
							# 		If you are running the analysis for a single species, and want to use the recommended defaults, you can do so here.
							# 		<br><br>
							# 		This will auto-load the recommended flight-height distribution for the selected species, and will override any other selections."
							# 	),
							# 	tagList(
							# 		# This selectizeInput should drop downwards, unlike the others
							# 		selectizeInput(
							# 			ns("default_region"),
							# 			label = "Select Region",
							# 			choices = c(
							# 				"North Sea",
							# 				"Norwegian Sea",
							# 				"Barents Sea",
							# 				"Atlantic Ocean"
							# 			)
							# 		),
							# 		selectizeInput(
							# 			ns("default_species"),
							# 			label = "Select Species",
							# 			choices = c(
							# 				"Puffin",
							# 				"Razorbill",
							# 				"Guillemot",
							# 				"Gannet",
							# 				"Shag",
							# 				"Kittiwake",
							# 				"Fulmar"
							# 			),
							# 			multiple = FALSE
							# 		),
							# 		actionButton(
							# 			ns("load_defaults"),
							# 			label = tagList(
							# 				bsicons::bs_icon("play-circle"),
							# 				"Load Defaults"
							# 			),
							# 			class = "not-arrow-btn"
							# 		)
							# 	)
							# )
						)
					),

					bslib::card(
						id = ns("selected_fhd_card"),
						bslib::card_header(
							"Selected Flight-Height Distributions",
							class = "text-bg-primary",
							bslib::toolbar(
								actionButton(
									ns("clear_selection"),
									label = bsicons::bs_icon("trash"),
									class = "btn btn-sm btn-light"
								) |>
									bslib::tooltip(
										"Clear the datasets selected (highlighted) in the table below.",
										placement = "bottom"
									)
								# mod_help_button_ui(ns("select_data"), type = "toolbar")
							)
						),
						bslib::card_body(DT::DTOutput(ns("show_selected"))),
						bslib::card_footer(
							# Some light-grey text to explain the table
							tags$small(
								"Click on a row to highlight it, then click the trash icon to remove it from the selection."
							)
						),
						class = "card border-primary mb-3 bg-light",
						full_screen = TRUE,
						# height = "30vh",
						# Keep horizontal and vertical scroll internal to the card
						style = "overflow-y: auto; overflow-x: auto;"
					),
					bslib::layout_columns(
						col_widths = c(6, 6),
						actionButton(
							ns("upload_data"),
							label = tagList(
								bsicons::bs_icon("cloud-upload"),
								"Upload Data"
							),
							class = "not-arrow-btn"
						) |>
							bslib::tooltip(
								"Upload your own flight-height dataset of a suitable format."
							),
						# actionButton(
						# 	ns("download_data"),
						# 	label = tagList(
						# 		bsicons::bs_icon("cloud-download"),
						# 		"Download Data"
						# 	),
						# 	class = "not-arrow-btn"
						# ) |>
						# 	bslib::tooltip(
						# 		"Download the selected flight-height datasets before analysis."
						# 	),
						actionButton(
							ns("go_analysis"),
							label = tagList(
								bsicons::bs_icon("play-circle"),
								"Visualise & Export"
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
#' @param metadata_tbl Data frame of all available FHD metadata.
#' @param restore_payload Optional reactive carrying a previously saved session payload.
#'
#' @noRd
mod_data_select_server <- function(
	id,
	nav_id = "main-nav",
	parent_session,
	metadata_tbl,
	restore_payload = NULL,
	tour_signal = NULL,
	demo_fhd_id = NULL
) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns

		# ── Row-level clearing: fhd_ids of user uploads to remove ───────────────
		remove_upload_ids <- reactiveVal(character(0))

		# Continuously run the user-upload module within this -------
		user_uploads <- mod_user_upload_server(
			id = "user_upload",
			remove_ids = reactive(remove_upload_ids())
		)

		# ---- Track some states -----------

		ready_to_download <- reactiveVal(FALSE)
		have_downloaded <- reactiveVal(FALSE)

		# ── Single source of truth: which fhd_ids are selected ──────────────────
		selected_ids <- reactiveVal(character(0))

		# ── Tour demo: auto-select a known FHD when the tour signals ─────────────
		if (!is.null(tour_signal) && !is.null(demo_fhd_id)) {
			tour_demo_fired <- reactiveVal(FALSE)
			observeEvent(
				tour_signal(),
				{
					if (tour_demo_fired()) {
						return(invisible(NULL))
					}
					tour_demo_fired(TRUE)
					if (!demo_fhd_id %in% selected_ids()) {
						selected_ids(c(selected_ids(), demo_fhd_id))
					}
					# Trigger go_analysis logic directly (avoids JS timing race)
					go_analysis_logic()
				},
				ignoreInit = TRUE
			)
		}

		# ── Filtered data: shared by map and DT ─────────────────────────────────
		# Empty selection for a filter = no constraint applied for that dimension.
		filtered_data <- reactive({
			data <- metadata_tbl
			if (length(input$species) > 0) {
				data <- dplyr::filter(data, species_id %in% input$species)
			}
			if (length(input$method) > 0) {
				data <- dplyr::filter(data, method %in% input$method)
			}
			if (length(input$season) > 0 && input$season != "any") {
				# Split the season string on commas to handle "breeding, nonbreeding"
				# or "nonbreeding, breeding" (order-insensitive)
				if (input$season == "both") {
					data <- dplyr::filter(
						data,
						season %in%
							c("nonbreeding, breeding", "breeding, nonbreeding")
					)
				} else {
					data <- dplyr::filter(data, season == input$season)
				}
			}
			if (length(input$crm_recommended) > 0) {
				data <- dplyr::filter(
					data,
					crm_recommended %in% input$crm_recommended
				)
			}
			if (length(input$region) > 0) {
				data <- dplyr::filter(data, region %in% input$region)
			}
			if (nrow(data) == 0) {
				bslib::show_toast(
					bslib::toast(
						header = "Warning",
						"No datasets match the selected filters. Please adjust your filters to see available datasets.",
						icon = bsicons::bs_icon("exclamation-triangle-fill"),
						type = "warning",
						duration_s = 0,
						id = "filter_warning",
						position = "bottom-right"
					)
				)
			} else {
				bslib::hide_toast("filter_warning")
			}

			data
		})

		# ── Filter choices (data-driven) ────────────────────────────────────────
		observe({
			updateSelectizeInput(
				session,
				"species",
				# choices = unique(metadata_tbl$species_id),
				choices = setNames(
					unique(metadata_tbl$species_id),
					unique(metadata_tbl$name_common)
				),
				selected = unique(metadata_tbl$species_id)[1],
				server = TRUE
			)
			updateSelectizeInput(
				session,
				"method",
				choices = unique(metadata_tbl$method),
				selected = character(0),
				server = TRUE
			)
			updateSelectizeInput(
				session,
				"region",
				choices = unique(metadata_tbl$region),
				selected = character(0),
				server = TRUE
			)
		})

		# ── One-shot trigger: fires once filter defaults are populated ──────────
		# renderLeaflet below depends on this instead of input$species directly,
		# so later changes to input$species do not cause the map to re-render.
		map_ready <- reactiveVal(FALSE)
		observeEvent(
			input$species,
			{
				map_ready(TRUE)
			},
			once = TRUE
		)

		# ── Map: initial render with all data markers ───────────────────────────
		output$source_map <- leaflet::renderLeaflet({
			req(map_ready())

			# Use isolate() to prevent map re-rendering on filter changes
			map <- leaflet::leaflet(
				options = leaflet::leafletOptions(
					attributionControl = FALSE,
					zoomControl = FALSE,
					minZoom = 3
				)
			) |>
				leaflet::addProviderTiles(
					leaflet::providers$CartoDB.DarkMatter,
					options = leaflet::providerTileOptions(
						key = "cb1_2637_1_9c44517e8d620b92dae4b2e4"
					)
				) |>
				leaflet::setView(lng = -3.5, lat = 56, zoom = 5)

			# Populate with initial data (isolated from filter changes)
			data <- isolate(filtered_data())
			map <- map |>
				add_fhd_polygons(
					data = data,
					selected_ids = isolate(selected_ids()),
					ns = ns
				)

			map
		})

		# ── Map markers: redrawn whenever filters or selection changes ───────────
		# After initial render, update markers when filters or selection change.
		observe({
			data <- filtered_data()

			leaflet::leafletProxy("source_map", session) |>
				leaflet::clearGroup("main_data") |>
				add_fhd_polygons(
					data = data,
					selected_ids = selected_ids(),
					ns = ns
				)
		})

		observe({
			# Add user datasets to the main, if there are any
			leaflet::leafletProxy("source_map", session) |>
				leaflet::clearGroup("user_data")
			req(user_uploads$metadata())
			userdat <- user_uploads$metadata() |>
				dplyr::bind_rows()
			req(nrow(userdat) > 0)
			userdat <- userdat |>
				dplyr::filter(!is.na(lon) & !is.na(lat))
			req(nrow(userdat) > 0)
			leaflet::leafletProxy("source_map", session) |>
				leaflet::addCircleMarkers(
					data = userdat,
					lng = ~lon,
					lat = ~lat,
					layerId = ~fhd_id,
					radius = 14,
					color = "white",
					weight = 4,
					group = "user_data",
					fillOpacity = 0.85,
					fillColor = "#009b12",
					popup = ~ paste0(
						"<strong>USER DATASET</strong><br/>",
						"<div style='width:200px;'>",
						"<strong>",
						species_id,
						"</strong><br/>",
						"<strong>Season: </strong>",
						season,
						"<br/>",
						"<strong>Method: </strong>",
						method
					)
				)
		})

		# ---- When a map marker's "Add Entry" button is clicked, update the selection ----
		observeEvent(input$map_add_btn, {
			fhd_id <- input$map_add_btn
			ids <- selected_ids()
			if (fhd_id %in% ids) {
				ids <- setdiff(ids, fhd_id)
			} else {
				ids <- c(ids, fhd_id)
			}
			selected_ids(ids)
		})

		# ── Combined selected data (main + user uploads), row order matches the
		# ── "show_selected" DT, so DT row indices can be mapped back to fhd_id.
		selected_data_combined <- reactive({
			metadata_tbl[metadata_tbl$fhd_id %in% selected_ids(), ] |>
				dplyr::bind_rows(
					user_uploads$metadata() |>
						dplyr::bind_rows()
				) |>
				as.data.frame()
		})

		# Show selected data ----
		output$show_selected <- DT::renderDT({
			data <- selected_data_combined() |>
				dplyr::select(
					dplyr::all_of(
						c(
							"fhd_id",
							"name_common",
							"method",
							"season"
						)
					)
				) |>
				dplyr::rename(
					"FHD ID" = fhd_id,
					"Species" = name_common,
					"Method" = method,
					"Season" = season
				)
			DT::datatable(
				data,
				selection = "multiple",
				options = list(
					pageLength = 5,
					lengthChange = FALSE,
					searching = FALSE,
					ordering = FALSE,
					info = FALSE
				),
				rownames = FALSE
			)
		})

		# ---- Show modal with more details when "More Details" button is clicked on map marker -----
		observeEvent(
			input$map_details_btn,
			{
				fhd_id <- input$map_details_btn
				details <- metadata_tbl[metadata_tbl$fhd_id == fhd_id, ] |>
					as.data.frame() |>
					dplyr::mutate(
						covariates = paste(
							names(covariates[[1]]),
							collapse = ", "
						),
						covariates = ifelse(
							covariates == "",
							"No covariates",
							covariates
						)
					) |>
					dplyr::select(-dplyr::any_of(c("sf_obj")))

				# prettify field names
				names(details) <- names(details) |>
					stringr::str_replace_all("_", " ") |>
					stringr::str_to_title()

				showModal(
					modalDialog(
						title = "Flight Height Dataset Details",
						# Add a warning that data is dummy
						bslib::card(
							tags$strong(
								"Warning: This is dummy data for demonstration purposes only. Obvious scientific errors may be present in the data."
							),
							class = "card bg-warning"
						),
						# Display details in a table format
						tags$table(
							class = "table table-striped",
							tags$tbody(
								lapply(names(details), function(name) {
									tags$tr(
										tags$th(name),
										tags$td(as.character(details[[name]]))
									)
								})
							)
						),
						easyClose = TRUE,
						icon = bsicons::bs_icon("info-circle"),
						size = "l"
					)
				)
			}
		)

		# ---- Clear selection button ----
		# Clears only the rows currently selected (highlighted) in the
		# "show_selected" DT, whether they originate from the main dataset or
		# from user uploads. If no rows are highlighted, prompt the user via
		# a toast rather than clearing anything.
		observeEvent(input$clear_selection, {
			rows <- input$show_selected_rows_selected

			if (length(rows) == 0) {
				bslib::show_toast(
					bslib::toast(
						header = "Nothing to clear",
						"No datasets are selected in the table. Highlight one or more rows first.",
						icon = bsicons::bs_icon("exclamation-triangle-fill"),
						type = "warning",
						id = "no_rows_selected_toast",
						position = "bottom-right"
					)
				)
				return(invisible(NULL))
			}

			# Modal to confirm and then clear
			showModal(
				modalDialog(
					title = "Clear Selected Datasets",
					tags$p(
						sprintf(
							"Are you sure you want to clear the %d dataset%s selected in the table?",
							length(rows),
							if (length(rows) == 1) "" else "s"
						)
					),
					footer = tagList(
						actionButton(
							ns("confirm_clear"),
							"Yes, clear selected",
							class = "btn btn-danger"
						),
						modalButton("Cancel")
					),
					easyClose = TRUE,
					size = "m"
				)
			)
		})
		observeEvent(input$confirm_clear, {
			rows <- input$show_selected_rows_selected
			ids_to_remove <- selected_data_combined()$fhd_id[rows]

			# Datasets from the main metadata table
			selected_ids(setdiff(selected_ids(), ids_to_remove))

			# Datasets from user uploads
			upload_ids <- names(user_uploads$metadata())
			ids_to_remove_uploads <- intersect(ids_to_remove, upload_ids)
			if (length(ids_to_remove_uploads) > 0) {
				remove_upload_ids(ids_to_remove_uploads)
			}

			removeModal()
		})

		# ----- Too-much-data toast ------------
		# If over 10 datasets are selected, show a warning toast that the analysis will only analyze the first 10 datasets.
		observeEvent(selected_ids(), {
			if (length(selected_ids()) > 10) {
				bslib::show_toast(
					bslib::toast(
						header = "Too many datasets selected",
						icon = bsicons::bs_icon("exclamation-triangle-fill"),
						"Only the first 10 datasets will be analyzed. Please deselect some datasets if you want to analyze fewer than 10.",
						type = "warning",
						id = "too_many_datasets_toast"
					)
				)
			} else {
				bslib::hide_toast("too_many_datasets_toast")
			}
		})

		# ── Navigation ───────────────────────────────────────────────────────────
		outputs <- reactiveValues()

		# define the logic of going to the analysis tab as a local function, to handle calls from both the "go_analysis" button and the tour demo.
		# TODO: Consider moving this function to a separate helper script
		go_analysis_logic <- function() {
			download_fhds <- metadata_tbl |>
				dplyr::filter(fhd_id %in% selected_ids()) |>
				dplyr::pull(fhd_id)
			downloads <- lapply(download_fhds, function(fhd_id) {
				readRDS(paste0("data-dummy/draws/", fhd_id, ".rds"))
			})
			names(downloads) <- download_fhds
			outputs$draws <- c(
				downloads,
				user_uploads$draws()
			)
			outputs$metadata <- metadata_tbl |>
				dplyr::filter(fhd_id %in% selected_ids()) |>
				dplyr::bind_rows(
					user_uploads$metadata() |>
						dplyr::bind_rows()
				)

			bslib::nav_select(
				id = nav_id,
				selected = "nav-analysis",
				session = parent_session
			)
		}

		observeEvent(input$go_analysis, {
			go_analysis_logic()
		})

		# ==== User-upload module as a modal dialog ====
		observeEvent(
			input$upload_data,
			{
				showModal(
					modalDialog(
						title = "Upload Flight Height Dataset",
						mod_user_upload_ui(ns("user_upload")),
						easyClose = TRUE,
						size = "xl"
					)
				)
			}
		)

		# Help button servers -----
		mod_help_button_server(
			"select_data",
			help_file = "select_data",
			size = "xl"
		)

		return(list(
			selected_data = outputs,
			user_fhds = NULL
		))
	})
}
