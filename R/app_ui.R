#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import bslib
#' @import conductor
#' @import conductor
#' @noRd
app_ui <- function(request) {
	tagList(
		golem_add_external_resources(),
		shinybusy::use_busy_spinner(
			spin = "circles-to-rhombuses",
			position = "bottom-right"
		),
		bslib::page_navbar(
			title = span("ReSCUEApp", style = "color: #ffffff;"),
			id = "main-nav",
			theme = bslib::bs_theme(
				version = 5,
				preset = "flatly",
				primary = "#002e40",
				secondary = "#b5cbca",
				success = "#ffa134",
				bg = "#113d4e",
				fg = "#ffffff"
			) |>
				bslib::bs_add_variables(
					"border-radius" = "1rem",
					# Set the tooltip colour to Success
					"tooltip-bg" = "var(--bs-success)"
				),
			# padding = c("1.5rem", "1.5rem", "100px", "1.5rem"),
			navbar_options = bslib::navbar_options(
				style = "height: 3rem;",
				#	position = "fixed-bottom",
				underline = FALSE
			),

			# NAVBAR -----------------

			bslib::nav_item(
				tags$a(
					href = "https://github.com",
					target = "_blank",
					bsicons::bs_icon("github", size = "2em") |>
						bslib::tooltip(
							placement = "bottom",
							"Go to GitHub Repository"
						),
					"aria-label" = "GitHub repository"
				)
			),
			bslib::nav_item(
				div(
					id = "app-version-container",
					mod_version_button_ui("app_version")
				)
			),
			bslib::nav_spacer(),

			# Tab 1: Welcome ==============================

			bslib::nav_panel(
				title = "",
				icon = bsicons::bs_icon(
					"house-fill",
					size = "1.5em"
				) |>
					bslib::tooltip(
						placement = "bottom",
						"Welcome Page"
					),
				value = "nav-home",
				mod_landing_page_ui("landing_page")
			),

			# Tab 2: Data Selection =======================

			bslib::nav_panel(
				title = "",
				icon = bsicons::bs_icon(
					"funnel-fill",
					size = "1.5em"
				) |>
					bslib::tooltip(
						placement = "bottom",
						"Data Selection"
					),
				value = "nav-data-select",
				mod_data_select_ui("data_select")
			),
			# Tab 3: Analysis =============================

			bslib::nav_panel(
				title = "",
				icon = bsicons::bs_icon(
					"bar-chart-fill",
					size = "1.5em"
				) |>
					bslib::tooltip(
						placement = "bottom",
						"Analysis"
					),
				value = "nav-analysis",
				mod_data_analysis_ui("data_analysis")
			),

			# Tab 4: Data Sources ============================

			bslib::nav_panel(
				title = "",
				icon = bsicons::bs_icon(
					"database-fill",
					size = "1.5em"
				) |>
					bslib::tooltip(
						placement = "bottom",
						"Data Sources"
					),
				bslib::page_fillable(
					# A .md with the sources will go here
					bslib::card(
						bslib::card_header(
							h2("Data Sources"),
							class = "text-bg-primary"
						),
						shiny::includeMarkdown("inst/app/md/sources.md"),
						class = "card border-primary mb-3 bg-light"
					)
				)
			),

			# Drop-down for save/restore options  =======================

			bslib::nav_menu(
				title = NULL,
				icon = bsicons::bs_icon(
					"gear-fill",
					size = "1.5em",
					title = "Settings"
				) |>
					bslib::tooltip(
						placement = "bottom",
						"Settings"
					),
				align = "right",
				bslib::nav_item(
					shiny::actionLink(
						"save_session",
						label = tagList(
							bsicons::bs_icon("download"),
							"Save Session"
						)
					)
				),
				bslib::nav_item(
					shiny::actionLink(
						"restore_session",
						label = tagList(
							bsicons::bs_icon("upload"),
							"Restore Session"
						)
					)
				),
				# bslib::nav_item(
				# 	shiny::actionLink(
				# 		"switch_mode",
				# 		label = tagList(
				# 			bsicons::bs_icon("tsunami"),
				# 			"Switch to Tidal"
				# 		)
				# 	)
				# ),
				bslib::nav_item(
					# Add a link to the GitHub repo to report a bug
					tags$a(
						href = "https://github.com", # ADD CORRECT LINK TO ISSUE PAGE ONCE PUBLIC
						target = "_blank",
						bsicons::bs_icon("bug-fill"),
						"Report a Bug"
					)
				),
				bslib::nav_item(
					shiny::actionLink(
						"reset_session",
						label = tagList(
							bsicons::bs_icon("arrow-counterclockwise"),
							"Reset Session"
						)
					)
				)
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
		favicon(ext = "png"),
		useConductor(),
		useConductor(),
		bundle_resources(
			path = app_sys("app/www"),
			app_title = "ReSCUEApp"
		),
		tags$style(HTML(
			"
			.rounded-box {
        border-radius: 15px;
				overflow: hidden;
      }
			.card-body-white {
        background-color: white !important;
      }
			.accordion-button {
        background-color: var(--bs-primary);
        color: white;
      }
      .accordion-button:not(.collapsed) {
        background-color: var(--bs-success);
        color: var(--bs-primary]);
      }
			.not-arrow-btn {
				background: var(--bs-dark); color: var(--bs-light); font-weight: bold;
				border: none; padding: 12px 30px 12px 20px; font-size: 1.1rem;
				overflow: visible;
				cursor: pointer; margin: 4px;
			}
      .arrow-btn {
        background: var(--bs-success); color: var(--bs-white); font-weight: bold;
        border: none; padding: 12px 30px 12px 20px; font-size: 1.1rem;
        clip-path: polygon(0 0, 85% 0, 100% 50%, 85% 100%, 0 100%);
        overflow: visible;
        cursor: pointer; margin: 4px;
      }
			.left-arrow-btn {
			  background: var(--bs-success); color: var(--bs-white); font-weight: bold;
				border: none; padding: 12px 20px 12px 30px; font-size: 1.1rem;
				clip-path: polygon(100% 0, 15% 0, 0 50%, 15% 100%, 100% 100%);
			  overflow: visible;
			  cursor: pointer; margin: 4px;
			}
      .arrow-btn:hover, .left-arrow-btn:hover { filter: brightness(0.85); }
      .arrow-btn-red {
        background: var(--bs-danger); color: var(--bs-white); font-weight: bold;
        border: none; padding: 6px 14px; font-size: 0.85rem;
        cursor: pointer;
      }
			.arrow-btn-faded {
							background: var(--bs-dark); color: var(--bs-light); font-weight: bold;
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
        color: var(--bs-success);
        font-size: 0.9em;
        line-height: 1;
        vertical-align: middle;
        opacity: 0.6;
        transition: opacity 0.2s, color 0.2s;#
      }
      .btn.btn-help:hover,
      .btn.btn-help:focus {
        background: none !important;
        box-shadow: none !important;
        color: var(--bs-primary) !important;
        opacity: 1;
      }

      /* Selectize dropdown z-index to sit above leaflet map */
      .selectize-dropdown {
        z-index: 9999 !important;
      }
      .selectize-input {
        z-index: 9998 !important;
      }
    "
		))
	)
}
