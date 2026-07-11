#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import bslib
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
			title = "ReSCUEApp",
			id = "main-nav",
			theme = bslib::bs_theme(
				version = 5,
				preset = "flatly"
			),
			# padding = c("1.5rem", "1.5rem", "100px", "1.5rem"),
			navbar_options = bslib::navbar_options(
				style = "height: 3.5rem;",
				#	position = "fixed-bottom",
				underline = FALSE
			),

			# NAVBAR -----------------

			bslib::nav_item(
				tags$a(
					href = "https://github.com",
					target = "_blank",
					bsicons::bs_icon("github", size = "2em"),
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
				),
				value = "nav-home",
				mod_landing_page_ui("landing_page")
			),

			# Tab 2: Data Selection =======================

			bslib::nav_panel(
				title = "", #"Data Selection",
				icon = bsicons::bs_icon("funnel-fill", size = "1.5em"),
				value = "nav-data-select",
				mod_data_select_ui("data_select")
			),
			# Tab 3: Analysis =============================

			bslib::nav_panel(
				"", #"Analysis",
				icon = bsicons::bs_icon("bar-chart-fill", size = "1.5em"),
				value = "nav-analysis",
				mod_data_analysis_ui("data_analysis")
			),

			# Tab 4: Data Sources ============================

			bslib::nav_panel(
				"", #"Data Sources",
				icon = bsicons::bs_icon("database-fill", size = "1.5em"),
				bslib::page_fillable(
					# A .md with the sources will go here
					bslib::card(
						shiny::includeMarkdown("inst/app/md/sources.md")
					)
				)
			),

			# Drop-down for save/restore options  =======================

			bslib::nav_menu(
				title = NULL,
				icon = bsicons::bs_icon("gear-fill", size = "1.5em"),
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
        transition: opacity 0.2s, color 0.2s;#
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
