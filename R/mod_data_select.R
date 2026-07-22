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
										"Any" = "any",
										"Both" = "nonbreeding, breeding",
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
								actionButton(
									ns("clear_selection"),
									label = bsicons::bs_icon("trash"),
									class = "btn btn-sm btn-light"
								) |>
									bslib::tooltip(
										"Clear all selected datasets.",
										placement = "bottom"
									),
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

		# Continuously run the user-upload module within this -------
		user_uploads <- mod_user_upload_server(
			id = "user_upload",
			clear_trigger = reactive(input$clear_all_uploads)
		)

		# ---- Track some states -----------

		ready_to_download <- reactiveVal(FALSE)
		have_downloaded <- reactiveVal(FALSE)

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
			updateSelectizeInput(
				session,
				"region",
				choices = unique(metadata_tbl$region),
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
			if (length(input$season) > 0 && input$season != "any") {
				data <- dplyr::filter(data, season %in% input$season)
			}
			if (length(input$crm_recommended) > 0) {
				data <- dplyr::filter(data, crm_recommended %in% input$crm_recommended)
			}
			if (length(input$region) > 0) {
				data <- dplyr::filter(data, region %in% input$region)
			}
			data
		})

		# ── Map: initial render with all data markers ───────────────────────────
		output$source_map <- leaflet::renderLeaflet({
			data <- metadata_tbl # Start with all data
			# ids <- selected_ids()

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
					group = "main_data",
					color = "black",
					weight = 1,
					fillOpacity = 0.85,
					fillColor = "grey",
					popup = ~ paste0(
						"<div style='width:200px;'>",
						"<strong>",
						species_id,
						"</strong><br/>",
						"<strong>Season: </strong>",
						season,
						"<br/>",
						"<strong>Method: </strong>",
						method,
						"<br/>",
						"<strong>Recommended for CRM: </strong>",
						ifelse(
							crm_recommended,
							# Green text for yes, red for no
							"<span style='color:green;'>Yes</span>",
							"<span style='color:red;'>No</span>"
						),
						"<br/>",
						"<button ",
						"onclick=\"Shiny.setInputValue('",
						ns("map_add_btn"),
						"', '",
						fhd_id,
						"', {priority:'event'})\" ",
						"class='btn btn-sm btn-primary' ",
						"style='margin-top:8px;width:100%;'>",
						"Select Dataset", # because initially no entries are selected
						"</button>",
						# Add a button for 'More Details'
						"<button ",
						"onclick=\"Shiny.setInputValue('",
						ns("map_details_btn"),
						"', '",
						fhd_id,
						"', {priority:'event'})\" ",
						"class='btn btn-sm btn-outline-primary' ",
						"style='margin-top:8px;width:100%;'>",
						"More Details",
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
				leaflet::clearGroup("main_data") |>
				leaflet::addCircleMarkers(
					data = data,
					lng = ~lon,
					lat = ~lat,
					layerId = ~fhd_id,
					radius = 10,
					color = "black",
					weight = 1,
					group = "main_data",
					fillOpacity = 0.85,
					fillColor = ifelse(data$fhd_id %in% ids, "#ffa134", "grey"),
					popup = ~ paste0(
						"<div style='width:200px;'>",
						"<strong>",
						species_id,
						"</strong><br/>",
						"<strong>Season: </strong>",
						season,
						"<br/>",
						"<strong>Method: </strong>",
						method,
						"<br/>",
						"<strong>Recommended for CRM: </strong>",
						ifelse(
							crm_recommended,
							# Green text for yes, red for no
							"<span style='color:green;'>Yes</span>",
							"<span style='color:red;'>No</span>"
						),
						"<br/>",
						"<button ",
						"onclick=\"Shiny.setInputValue('",
						ns("map_add_btn"),
						"', '",
						fhd_id,
						"', {priority:'event'})\" ",
						ifelse(
							fhd_id %in% ids,
							"class='btn btn-sm btn-success' ",
							"class='btn btn-sm btn-primary' "
						),
						# "class='btn btn-sm btn-primary' ",
						"style='margin-top:8px;width:100%;'>",
						ifelse(fhd_id %in% ids, "Deselect Dataset", "Select Dataset"),
						"</button>",
						# Add a button for 'More Details'
						"<button ",
						"onclick=\"Shiny.setInputValue('",
						ns("map_details_btn"),
						"', '",
						fhd_id,
						"', {priority:'event'})\" ",
						"class='btn btn-sm btn-outline-primary' ",
						"style='margin-top:8px;width:100%;'>",
						"More Details",
						"</button>",
						"</div>"
					)
				)
		})

		observe({
			# Add user datasets to the main, if there are any
			req(user_uploads$metadata())
			userdat <- user_uploads$metadata() |>
				dplyr::bind_rows()
			req(nrow(userdat) > 0)
			leaflet::leafletProxy("source_map", session) |>
				leaflet::clearGroup("user_data") |>
				leaflet::addCircleMarkers(
					data = userdat,
					lng = ~lon,
					lat = ~lat,
					layerId = ~fhd_id,
					radius = 10,
					color = "black",
					weight = 1,
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

		# Show selected data ----
		output$show_selected <- DT::renderDT({
			data <- metadata_tbl[metadata_tbl$fhd_id %in% selected_ids(), ] |>
				dplyr::bind_rows(
					user_uploads$metadata() |>
						dplyr::bind_rows()
				) |>
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

		# ---- Show modal with more details when "More Details" button is clicked on map marker -----
		observeEvent(
			input$map_details_btn,
			{
				fhd_id <- input$map_details_btn
				details <- metadata_tbl[metadata_tbl$fhd_id == fhd_id, ] |>
					dplyr::mutate(
						covariates = paste(names(covariates[[1]]), collapse = ", "),
						covariates = ifelse(
							covariates == "",
							"No covariates",
							covariates
						)
					) |>
					dplyr::select(-dplyr::any_of(c("sf_obj")))
				showModal(
					modalDialog(
						title = "Flight Height Dataset Details",
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
		observeEvent(input$clear_selection, {
			# Modal to confirm and then clear
			showModal(
				modalDialog(
					title = "Clear Selection",
					tags$p(
						"Are you sure you want to clear all selected datasets? ",
						br(),
						tags$strong("This will include any user-uploaded datasets.")
					),
					footer = tagList(
						actionButton(
							ns("confirm_clear"),
							"Yes, clear selection",
							class = "btn btn-outline-danger"
						),
						modalButton("Cancel")
					),
					easyClose = TRUE,
					size = "m"
				)
			)
		})
		observeEvent(input$confirm_clear, {
			selected_ids(character(0))
			removeModal()
		})

		# ----- Merge the user-uploaded data -----------
		#' The user can upload a dataset at any point. Reactively merge
		#' the user-uploaded data with the main metadata table. This allows the user to select their own datasets for analysis.

		# ── Navigation ───────────────────────────────────────────────────────────
		outputs <- reactiveValues()
		observeEvent(input$go_analysis, {
			# Download the FHDs for the selected datasets (not including user-uploads)
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

		return(list(
			selected_data = outputs,
			uploaded_data = NULL
		))
	})
}
