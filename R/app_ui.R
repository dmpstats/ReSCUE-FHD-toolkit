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
		shinybusy::add_busy_spinner(
			spin = "spring",
			position = "bottom-right",
			height = "150px",
			width = "150px",
			timeout = 200,
			onstart = TRUE,
			color = "#ffa134"
		),
		# shinybusy::use_busy_gif(
		# 	# use 'birdflap.gif' in the www folder
		# 	src = "www/birdflap.gif",
		# 	position = "bottom-right",
		# 	height = "150px",
		# 	width = "150px"
		# ),
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
					href = "https://github.com/dmpstats/ReSCUE-FHD-toolkit",
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
						"FHD Selection"
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
						"Visualisation & Export"
					),
				value = "nav-analysis",
				mod_data_analysis_ui("data_analysis")
			),

			bslib::nav_menu(
				title = NULL,
				align = "right",
				icon = fontawesome::fa(
					#"file-lines",
					"folder-closed",
					height = "1.4em",
					prefer_type = "solid"
				) |>
					bslib::tooltip(
						placement = "bottom",
						"Documentation"
					),

				# Tab 4: Data Sources ============================

				bslib::nav_panel(
					title = "Data Sources",
					icon = fontawesome::fa(
						"database",
						height = "1.3em",
						margin_right = "0.3em",
						fill_opacity = 0.8
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

				bslib::nav_panel(
					title = "User Guide",
					value = "nav-user-guide",
					icon = fontawesome::fa(
						"book-open",
						height = "1.3em",
						margin_right = "0.3em",
						fill_opacity = 0.8
					),
					# use page_fillable() to make card fill the available viewport's vertical
					# space, ensuring the Tutorial button is always visible at bottom of the page
					bslib::page_fillable(
						tags$iframe(
							src = "html/ReSCUE_Toolkit_User_Guide_V1.0.html",
							style = "width: 100%; flex: 1 1 auto; border: none"
						),
						shiny::absolutePanel(
							actionButton(
								"start_tour",
								label = tagList(
									fontawesome::fa("route")
								),
								class = "circle-btn"
							) |>
								bslib::tooltip(
									placement = "bottom",
									"Start In-App Tour"
								),
							right = "30px",
							bottom = "30px",
							# right = "50px",
							# top = "60px",
							fixed = TRUE
						)
						# 	bslib::card(
						# 		bslib::card_body(
						# 			tags$iframe(
						# 				src = "html/ReSCUE_Toolkit_User_Guide_V1.0.html",
						# 				style = "width: 100%; height: 100%; border: none;"
						# 			)
						# 			# shiny::includeMarkdown(
						# 			# 	app_sys(
						# 			# 		"app",
						# 			# 		"md",
						# 			# 		"userguide.md",
						# 			# 	)
						# 			# )
						# 		),
						# 		bslib::card_footer(
						# 			class = "bg-primary",
						# 			bslib::toolbar(
						# 				bslib::toolbar_input_button(
						# 					id = "start_tour",
						# 					label = "In-App Tutorial",
						# 					icon = fontawesome::fa("route"),
						# 					show_label = TRUE,
						# 					tooltip = "Start Tour",
						# 					class = "btn btn-secondary fw-bold",
						# 					style = "font-size: 1.25rem; padding: 0.75rem 1.5rem; border-radius: 8px;"
						# 				),
						# 				align = "left"
						# 			) |>
						# 				htmltools::tagAppendAttributes(
						# 					style = "justify-content: center;"
						# 				)
						# 		),
						# 		class = "card border-primary bg-light"
						# 	)
						# 	# Sidebar with logos
						# 	# bslib::card(
						# 	# 	class = "d-flex flex-column align-items-center gap-3 h-100",
						# 	# 	logolink("dmp", height = 9),
						# 	# 	logolink("ne", height = 9),
						# 	# 	logolink("bto", height = 9),
						# 	# 	logolink("blackbawks", height = 9),
						# 	# 	logolink("niras", height = 9)
						# 	# )
					)
				),

				bslib::nav_panel(
					title = "Metadata Builder",
					value = "nav-metadata-builder",
					icon = fontawesome::fa(
						"screwdriver-wrench",
						height = "1.3em",
						margin_right = "0.3em",
						fill_opacity = 0.8
					),
					mod_metadata_builder_ui("metadata_builder")
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
				# bslib::nav_item(
				# 	shiny::actionLink(
				# 		"save_session",
				# 		label = tagList(
				# 			bsicons::bs_icon("download"),
				# 			"Save Session"
				# 		)
				# 	)
				# ),
				# bslib::nav_item(
				# 	shiny::actionLink(
				# 		"restore_session",
				# 		label = tagList(
				# 			bsicons::bs_icon("upload"),
				# 			"Restore Session"
				# 		)
				# 	)
				# ),
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
						href = "https://github.com/dmpstats/ReSCUE-FHD-toolkit/issues",
						target = "_blank",
						bsicons::bs_icon("bug-fill", size = "1.3em"),
						"Report a Bug",
						class = "nav-item-link"
					)
				),
				bslib::nav_item(
					shiny::actionLink(
						"reset_app",
						label = tagList(
							bsicons::bs_icon(
								"arrow-counterclockwise",
								size = "1.3em"
							),
							"Reset Session"
						),
						class = "nav-item-link"
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

	add_resource_path(
		"html",
		app_sys("app/html")
	)

	tags$head(
		favicon(ext = "png"),
		conductor::useConductor(),
		shinyjs::useShinyjs(),
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
				border: 2px solid white; padding: 12px 30px 12px 20px; font-size: 1.1rem; overflow: visible;
				cursor: pointer; margin: 4px;
			}
      .arrow-btn {
        background: var(--bs-success); color: var(--bs-white); font-weight: bold;
        border: 2px solid var(--bs-success); padding: 12px 30px 12px 20px; font-size: 1.1rem;
        clip-path: polygon(0 0, 85% 0, 100% 50%, 85% 100%, 0 100%);
        overflow: visible;
        cursor: pointer; margin: 4px;
      }
			.left-arrow-btn {
			  background: var(--bs-success); color: var(--bs-white); font-weight: bold;
				border: 2px solid var(--bs-success); padding: 12px 20px 12px 30px; font-size: 1.1rem;
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
        color: white;
        font-size: 0.9em;
        line-height: 1;
        vertical-align: middle;
        opacity: 0.7;
        transition: opacity 0.2s, color 0.2s;#
      }
      .btn.btn-help:hover,
      .btn.btn-help:focus {
        background: none !important;
        box-shadow: none !important;
        color: var(--bs-success) !important;
        opacity: 1;
      }
    "
		)),
		# Selecting a tab inside a nav_menu() leaves the parent dropdown open.
		# Shiny activates tabs with jQuery's .tab('show'), and bslib's bs3compat
		# shim fires 'shown.bs.tab' as a jQuery event, so this must be bound with
		# jQuery -- a native addEventListener would never fire. We strip the
		# 'show' classes directly because bootstrap.Dropdown.getInstance() returns
		# null for menus the user has not clicked yet.
		tags$script(HTML(
			"
			$(document).on('shown.bs.tab', function(e) {
				$('.navbar .dropdown-menu.show').removeClass('show');
				$('.navbar .nav-item.dropdown.show').removeClass('show');
				$('.navbar .dropdown-toggle[aria-expanded=\"true\"]')
					.attr('aria-expanded', 'false');
			});
			"
		))
	)
}
