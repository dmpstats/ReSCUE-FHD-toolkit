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

				bslib::card(
					bslib::card_header(
						"Data Selection",
						class = "text-bg-primary",
						bslib::toolbar(
							bslib::popover(
								actionButton(
									ns("advanced_filters"),
									label = tagList(
										bsicons::bs_icon("funnel"),
										"Advanced Filters"
									),
									class = "btn btn-light btn-sm"
								),
								p("We will import extended filters here.")
							),
							mod_help_button_ui(ns("select_data"), type = "toolbar")
						)
					),
					bslib::card_body(
						fluidRow(
							selectizeInput(
								ns("species"),
								label = "Species",
								choices = c("Dummy", "Dummy2"),
								width = "33%"
							),
							selectizeInput(
								ns("method"),
								label = "Method",
								choices = c("Dummy", "Dummy2"),
								multiple = TRUE,
								width = "33%"
							),
							selectizeInput(
								ns("season"),
								label = "Season",
								choices = c(
									"Breeding" = "breeding",
									"Non-breeding" = "non_breeding",
									"Both" = "both"
								),
								selected = "both",
								width = "33%"
							)
						 
						),
						div(
							leaflet::leafletOutput(ns("source_map"), height = "60vh"),
							class = "rounded-box"
						),
					),
					class = "card border-primary mb-3 bg-light",
				),

				# Right-hand side: show selected data and go to analysis button
				tagList(
					# 12,
					bslib::card(
						bslib::card_header(
							"Selected Data",
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
							bslib::tooltip("Upload your own flight-height dataset of a suitable format."),
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
mod_data_select_server <- function(
	id,
	nav_id = "main-nav",
	parent_session,
	metadata_tbl,
	restore_payload = NULL
) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns

		# Data selection sub-modules  ------------

		## Adjusting filters to input data
		observe({
			# Species filter
			updateSelectizeInput(
				session = session,
				inputId = "species",
				choices = unique(metadata_tbl$Species),
				selected = unique(metadata_tbl$Species)[1],
				server = TRUE
			)
			# Method filter
			updateSelectizeInput(
				session = session,
				inputId = "method",
				choices = unique(metadata_tbl$method),
				selected = unique(metadata_tbl$method)[1],
				server = TRUE
			)
		})

		## Generate map 
		output$source_map <- leaflet::renderLeaflet({
      map <- leaflet::leaflet(
				options = leaflet::leafletOptions(
					attributionControl=FALSE,
					zoomControl = FALSE,
					minZoom = 3
				)
			) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) |>
        leaflet::setView(lng = -3.5, lat = 56, zoom = 5) |>
				# Add markers for the metadata coords
				leaflet::addCircleMarkers(
					data = metadata_tbl,
					lng = ~lon,
					lat = ~lat,
					layerId = ~fhd_id,
					radius = 10,
					color = "black",
					fillColor = "grey",
					fillOpacity = 0.8,
					weight = 1,
					label = ~ paste0(
						"<div style='width: 200px;'>",
						"<strong>",
						species_id,
						"</strong><br/>",
						"Season: ",
						season,
						"<br/>",
						"<button class='map-add-btn' data-row='",
						i,
						"' type='button' class='btn btn-sm btn-primary' style='margin-top: 8px; width: 100%;'>Add Entry</button>",
						"</div>"
					)
				)
			
		})

		# Dynamically add circleMarkers 
		

		# React to next-page button --------------
		observeEvent(
			input$go_analysis,
			{
				# If the user has not selected any data, show a modal warning
				
				# ADD THIS LATER

				bslib::nav_select(
					id = nav_id,
					selected = "nav-analysis",
					session = parent_session
				)
			}
		)

		# Return the selected data as a reactive --------
		return(
			list(
				
			)
		)
	})
}
