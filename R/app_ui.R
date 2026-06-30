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
							bslib::card_header("What is ResCUE?"),
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
									"User Guide",
									class = "arrow-btn-faded",
									icon = icon("info-circle", class = "arrow-icon"),
									style = "width:225px"
								),
								actionButton(
									"restore_session",
									"Restore Session",
									class = "arrow-btn-faded",
									icon = icon("arrow-rotate-left", class = "arrow-icon"),
									style = "width:225px"
								),
								actionButton(
									"go_data",
									"Select Data",
									class = "arrow-btn",
									icon = icon("play-circle", class = "arrow-icon"),
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
				icons = bsicons::bs_icon("map"),
				bslib::page_fillable(
					bslib::layout_columns(
						col_widths = c(6, 6),
						bslib::navset_card_underline(
							bslib::nav_panel(
								title = "Map Selection",
								"Map selection content will go here"
							),
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
				icon = bsicons::bs_icon("bar-chart")
				# Content will go here
			),

			# Tab 4: Data Sources ============================
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
		tags$style(HTML("
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
    "))
	)
}
