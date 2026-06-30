#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd
app_ui <- function(request) {
	tagList(
		golem_add_external_resources(),
		bslib::page_navbar(
			title = "ReSCUEApp",
			id = "main-nav",
			theme = bslib::bs_theme(
				version = 5,
				preset = "flatly"
			),

			# NAVBAR -----------------

			# Provide a link to the GitHub repository
			bslib::nav_item(
				tags$a(
					href = "https://github.com",
					target = "_blank",
					bsicons::bs_icon("github", size = "2em"),
					"aria-label" = "GitHub repository"
				)
			),
			bslib::nav_spacer(),
			bslib::nav_item(
				shiny::actionButton(
					"dl_session",
					"Download Session",
					class = "btn-outline-primary"
				)
			),

			# Tab 1: Welcome ==============================

			bslib::nav_panel(
				"Welcome Page",
				icon = bsicons::bs_icon("house"),
				bslib::page_fillable(
					bslib::layout_columns(
						col_widths = c(6, 6),
						# LHS: A card containing project information.
						bslib::card(
							bslib::card_header("What is ResCUE?",

							# DON'T FORGET TO REMOVE THIS - DUMMY EXAMPLE
																mod_help_button_ui("dummy_help")
						),
							bslib::card_body(
								lorem::ipsum(paragraphs = 2)
							)
						),
						column(
							12,
							bslib::card(
								bslib::card_header("Project Partners"),
								bslib::card_body("Logos")
							),
							div(
								class = "d-flex flex-column align-items-center gap-2 my-3",
								actionButton(
									"link_guide",
									label = tagList(
										bsicons::bs_icon("info-circle"),
										"User Guide"
									),
									class = "arrow-btn-faded",
									style = "width:225px"
								),
								actionButton(
									"restore_session",
									label = tagList(
										bsicons::bs_icon("arrow-counterclockwise"),
										"Restore Session"
									),
									class = "arrow-btn-faded",
									style = "width:225px"
								),
								actionButton(
									"go_data",
									label = tagList(
										bsicons::bs_icon("play-circle"),
										"Select Data"
									),
									class = "arrow-btn",
									style = "width:260px"
								)
							)
						)
					)
				)
			),

			# Tab 2: Data Selection =======================

			bslib::nav_panel(
				"Data Selection",
				icon = bsicons::bs_icon("map"),
				bslib::page_fillable(
					bslib::layout_columns(
						col_widths = c(6, 6),
						bslib::navset_card_underline(
							## MAP MODULE WILL REPLACE THIS =========
							bslib::nav_panel(
								title = "Map Selection",
								leaflet::leafletOutput("source_map", height = "300px"),
								fluidRow(
									bslib::layout_columns(
										col_widths = c(4, 4, 4),
										textInput("species_id", "Species"),
										textInput("region", "Region"),
										textInput("something_else", "Something else")
									)
								)
							),
							# =============================

							bslib::nav_panel(
								title = "Table Selection",
								"Table selection content will go here"
							),
							bslib::nav_panel(
								title = "Data Upload",
								"Data upload content will go here"
							)
						),
						column(
							12,
							bslib::card(
								bslib::card_header("Selected Data"),
								bslib::card_body("Selected data content will go here")
							),
							bslib::card(
								bslib::card_header("Flight Height Preview"),
								bslib::card_body("Flight height preview content will go here")
							)
						)
					)
				)
			),

			# Tab 3: Analysis =============================

			bslib::nav_panel(
				"Analysis",
				icon = bsicons::bs_icon("bar-chart"),
				bslib::page_fillable(
					bslib::layout_columns(
						col_widths = c(6, 6),

						column(
							12,
							# First card: selected data
							bslib::card(
								bslib::card_header("Selected Data"),
								bslib::card_body(DT::DTOutput("dummy_dt"))
							),

							# Second card: defining turbine parameters
							bslib::card(
								bslib::card_header("Turbine Parameters"),
								bslib::card_body(
									fluidRow(
										bslib::layout_columns(
											col_widths = c(4, 4, 4),
											numericInput(
												"hub_height",
												"Hub Height (m)",
												value = 100,
												min = 0
											),
											numericInput(
												"rotor_diameter",
												"Rotor Diameter (m)",
												value = 80,
												min = 0
											),
											numericInput(
												"cut_in_speed",
												"Cut-in Speed (m/s)",
												value = 3.5,
												min = 0
											)
										)
									)
								)
							),

							# Third card: analysis results
							bslib::card(
								bslib::card_header("Analysis Results"),
								bslib::card_body("Analysis results content will go here")
							)
						),

						# Second column will contain the plot and output distributions
						column(
							12,
							bslib::card(
								bslib::card_header("Flight Height Distribution"),
								bslib::card_body("Flight height distribution plot will go here")
							),

							# Second card will contain download options
							bslib::card(
								bslib::card_header("Download Options"),
								bslib::card_body("Download options content will go here")
							)
						)
					)
				)

				# Tab 4: Data Sources ============================
			)
		)
	)
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
	add_resource_path(
		"www",
		app_sys("app/www")
	)

	tags$head(
		favicon(),
		bundle_resources(
			path = app_sys("app/www"),
			app_title = "ReSCUEApp"
		),
		tags$style(HTML(
			"
      .arrow-btn {
        background: var(--bs-primary); color: var(--bs-white); font-weight: bold;
        border: none; padding: 12px 30px 12px 20px; font-size: 1.1rem;
        clip-path: polygon(0 0, 85% 0, 100% 50%, 85% 100%, 0 100%);
        overflow: visible;
        cursor: pointer; margin: 4px;
      }
      .arrow-btn:hover { filter: brightness(0.85); }
      .arrow-btn-red {
        background: var(--bs-danger); color: var(--bs-white); font-weight: bold;
        border: none; padding: 6px 14px; font-size: 0.85rem;
        cursor: pointer;
      }
			.arrow-btn-faded {
							background: var(--bs-light); color: var(--bs-dark); font-weight: bold;
				border: none; padding: 12px 30px 12px 20px; font-size: 1.1rem;
				clip-path: polygon(0 0, 85% 0, 100% 50%, 85% 100%, 0 100%);
				overflow: visible;
				cursor: pointer; margin: 4px;
			}
			.arrow-btn-faded:hover { filter: brightness(0.85); }
			.arrow-icon { position: absolute; right: -10px; top: 50%; transform: translateY(-50%); }

      /* Help button */
      .btn.btn-help {
        background: none !important;
        border: none !important;
        box-shadow: none !important;
        padding: 0 0 0 4px;
        color: var(--bs-secondary);
        font-size: 0.9em;
        line-height: 1;
        vertical-align: middle;
        opacity: 0.6;
        transition: opacity 0.2s, color 0.2s;
      }
      .btn.btn-help:hover,
      .btn.btn-help:focus {
        background: none !important;
        box-shadow: none !important;
        color: var(--bs-primary) !important;
        opacity: 1;
      }
    "
		))
	)
}
