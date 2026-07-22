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
		tags$head(tags$style(
			type = "text/css",
			paste0(
				".selectize-dropdown {
                                                     bottom: 100% !important;
                                                     top:auto!important;
                                                 }}"
			)
		)),
		bslib::page_fillable(
			bslib::layout_columns(
				col_widths = c(6, 6),
				class = "h-100",
				bslib::card(
					class = "card border-primary mb-3 bg-light",
					bslib::card_header(
						"Data Selection",
						class = "text-bg-primary",
						bslib::toolbar(
							mod_help_button_ui(ns("select_data"), type = "toolbar")
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
						fluidRow(
							column(
								3,
								selectizeInput(
									ns("species"),
									label = "Species",
									choices = NULL,
									multiple = TRUE,
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
										"Breeding" = "breeding",
										"Non-breeding" = "non_breeding",
										"Both" = "both"
									),
									multiple = TRUE
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
										p("We will import extended filters here."),
										placement = "top"
									)
							)
						)
					)
				),

				# Right-hand side: show selected data and go to analysis button
				tagList(
					bslib::card(
						bslib::card_header(
							"Selected Data",
							class = "text-bg-primary",
							bslib::toolbar(
								mod_help_button_ui(ns("select_data"), type = "toolbar")
							)
						),
						bslib::card_body(DT::DTOutput(ns("show_selected"))),
						class = "card border-primary mb-3 bg-light",
						full_screen = TRUE,
						height = "30vh"
					),
					bslib::layout_columns(
						col_widths = c(6, 6),
						actionButton(
							ns("Upload Data"),
							label = tagList(
								bsicons::bs_icon("cloud-upload"),
								"Upload Data"
							),
							class = "not-arrow-btn"
						) |>
							bslib::tooltip(
								"Upload your own flight-height dataset of a suitable format."
							),
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
#' @param metadata_tbl Data frame of all available FHD metadata.
#' @param restore_payload Optional reactive carrying a previously saved session payload.
#'
#' @noRd
mod_data_select_server <- function(
	id,
	nav_id = "main-nav",
	parent_session,
	metadata_tbl,
	restore_payload = NULL
) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns

		# ── Filter choices (data-driven) ────────────────────────────────────────
		observe({
			updateSelectizeInput(
				session,
				"species",
				choices = unique(metadata_tbl$species_id),
				selected = character(0),
				server = TRUE
			)
			updateSelectizeInput(
				session,
				"method",
				choices = unique(metadata_tbl$method),
				selected = character(0),
				server = TRUE
			)
		})

		# ── Single source of truth: which fhd_ids are selected ──────────────────
		selected_ids <- reactiveVal(character(0))

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
			if (length(input$season) > 0) {
				data <- dplyr::filter(data, season %in% input$season)
			}
			data
		})

		# ── Map: initial render with all data markers ───────────────────────────
		output$source_map <- leaflet::renderLeaflet({
			data <- metadata_tbl # Start with all data
			ids <- selected_ids()

			leaflet::leaflet(
				data = data,
				options = leaflet::leafletOptions(
					attributionControl = FALSE,
					zoomControl = FALSE,
					minZoom = 3
				)
			) |>
				leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) |>
				leaflet::setView(lng = -3.5, lat = 56, zoom = 5) |>
				leaflet::addCircleMarkers(
					lng = ~lon,
					lat = ~lat,
					layerId = ~fhd_id,
					radius = 10,
					color = "black",
					weight = 1,
					fillOpacity = 0.85,
					fillColor = ifelse(data$fhd_id %in% ids, "steelblue", "grey"),
					popup = ~ paste0(
						"<div style='width:200px;'>",
						"<strong>",
						species_id,
						"</strong><br/>",
						"Season: ",
						season,
						"<br/>",
						"<button ",
						"onclick=\"Shiny.setInputValue('",
						ns("map_add_btn"),
						"', '",
						fhd_id,
						"', {priority:'event'})\" ",
						"class='btn btn-sm btn-primary' ",
						"style='margin-top:8px;width:100%;'>",
						ifelse(fhd_id %in% ids, "Remove Entry", "Add Entry"),
						"</button>",
						"</div>"
					)
				)
		})

		# ── Map markers: redrawn whenever filters or selection changes ───────────
		# After initial render, update markers when filters or selection change.
		observe({
			data <- filtered_data()
			ids <- selected_ids()

			leaflet::leafletProxy("source_map", session) |>
				leaflet::clearMarkers() |>
				leaflet::addCircleMarkers(
					data = data,
					lng = ~lon,
					lat = ~lat,
					layerId = ~fhd_id,
					radius = 10,
					color = "black",
					weight = 1,
					fillOpacity = 0.85,
					fillColor = ifelse(data$fhd_id %in% ids, "steelblue", "grey"),
					popup = ~ paste0(
						"<div style='width:200px;'>",
						"<strong>",
						species_id,
						"</strong><br/>",
						"Season: ",
						season,
						"<br/>",
						"<button ",
						"onclick=\"Shiny.setInputValue('",
						ns("map_add_btn"),
						"', '",
						fhd_id,
						"', {priority:'event'})\" ",
						"class='btn btn-sm btn-primary' ",
						"style='margin-top:8px;width:100%;'>",
						ifelse(fhd_id %in% ids, "Remove Entry", "Add Entry"),
						"</button>",
						"</div>"
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

		# Show selected data ----
		output$show_selected <- DT::renderDT({
			data <- metadata_tbl[metadata_tbl$fhd_id %in% selected_ids(), ] |>
				dplyr::select(
					dplyr::all_of(
						c(
							"fhd_id",
							"species_id",
							"method",
							"season"
						)
					)
				)
			DT::datatable(
				data,
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

		# ── Navigation ───────────────────────────────────────────────────────────
		observeEvent(input$go_analysis, {
			# ADD LATER: modal warning if selected_ids() is empty
			bslib::nav_select(
				id = nav_id,
				selected = "nav-analysis",
				session = parent_session
			)
		})

		# ── Return ───────────────────────────────────────────────────────────────
		return(list(
			selected_data = reactive({
				metadata_tbl[metadata_tbl$fhd_id %in% selected_ids(), ]
			}),
			uploaded_data = NULL
		))
	})
}
