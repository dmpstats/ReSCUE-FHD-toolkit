#' landing_page UI Function
#'
#' @description A shiny module containing the 'landing page' or welcome page for the app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_landing_page_ui <- function(id) {
	ns <- NS(id)
	tagList(
		bslib::page_fillable(
			# Add a page-wide card
			# bslib::card(
			# 	class = "bg-secondary",
			# 	full_screen = FALSE,
			# 	fill = FALSE, # <-- stops the card from growing to fill the page height
			# 	tags$a(
			# 		href = "google.com",
			# 		target = "_blank",
			# 		rel = "noopener noreferrer",
			# 		tags$img(
			# 			src = "www/ReSCUE_banner.png",
			# 			class = "logo-link",
			# 			alt = "ReSCUE Project Banner",
			# 			style = "max-width: 90vw; cursor: pointer; display: block; margin-left: auto; margin-right: auto;"
			# 		),
			# 	)
			# ),
			bslib::layout_columns(
				col_widths = c(6, 6),
				# LHS: A card containing project information.
				# bslib::card(
				# bslib::card_header(
				# "What is ResCUE?",
				# DON'T FORGET TO REMOVE THIS - DUMMY EXAMPLE
				# mod_help_button_ui(ns("dummy_help")),
				# class = "text-bg-primary d-flex justify-content-end"
				# ),
				# bslib::card_body(
				bslib::accordion(
					multiple = FALSE,
					class = "card border-primary mb-3 bg-light",
					height = "95%",
					bslib::accordion_panel(
						title = "Project Overview",
						icon = bsicons::bs_icon("info-circle"),
						shiny::includeMarkdown(app_sys(
							"app",
							"md",
							"landingpage",
							"project_overview.md"
						)),
						class = "bg-light"
					),
					bslib::accordion_panel(
						title = "About This App",
						icon = bsicons::bs_icon("app-indicator"),
						shiny::includeMarkdown(app_sys(
							"app",
							"md",
							"landingpage",
							"about_this_app.md"
						)),
						class = "bg-light"
					),
					bslib::accordion_panel(
						title = "What This App Does",
						icon = bsicons::bs_icon("question-circle"),
						shiny::includeMarkdown(app_sys(
							"app",
							"md",
							"landingpage",
							"what_this_app_does.md"
						)),
						class = "bg-light"
					),
					bslib::accordion_panel(
						title = "What This App Doesn't Do",
						icon = bsicons::bs_icon("file-x"),
						shiny::includeMarkdown(app_sys(
							"app",
							"md",
							"landingpage",
							"what_app_doesnt_do.md"
						)),
						class = "bg-light"
					),
					bslib::accordion_panel(
						title = "Contacts",
						icon = bsicons::bs_icon("envelope-fill"),

						# CONTACTS -----------------
						bslib::value_box(
							title = "ReSCUE Project Lead",
							value = "Eddie Cole",
							# Add a link to an email
							tags$a(
								href = "mailto:Eddie.Cole@naturalengland.org.uk",
								"Eddie.Cole@naturalengland.org.uk"
							),
							showcase = tags$a(
								href = "mailto:Eddie.Cole@naturalengland.org.uk",
								bsicons::bs_icon("envelope-fill", class = "text-secondary")
							)
						),
						bslib::value_box(
							title = "Developer",
							value = "Bruno Caneco",
							# Add a link to an email
							tags$a(
								href = "mailto:bruno@dmpstats.co.uk",
								"bruno@dmpstats.co.uk"
							),
							showcase = tags$a(
								href = "mailto:bruno@dmpstats.co.uk",
								bsicons::bs_icon("envelope-fill", class = "text-secondary")
							)
						),
						bslib::value_box(
							title = "Developer",
							value = "Callum Clarke",
							# Add a link to an email
							tags$a(
								href = "mailto:callum@dmpstats.co.uk",
								"callum@dmpstats.co.uk"
							),
							showcase = tags$a(
								href = "mailto:callum@dmpstats.co.uk",
								bsicons::bs_icon("envelope-fill", class = "text-secondary")
							)
						),
						class = "bg-light"
					)
				),
				# class = "card border-primary mb-3 bg-light"
				# )
				# ),
				column(
					12,
					# OWEC logo in its own card
					bslib::card(
						bslib::card_body(
							class = "d-flex justify-content-center align-items-center",
							logolink("owec", height = 35)
						),
						height = "22vh",
						class = "card border-primary mb-3 bg-secondary"
					),
					bslib::layout_columns(
						col_widths = c(4, 4, 4),
						# height = "22vh",
						bslib::card(
							bslib::card_body(
								class = "d-flex justify-content-center align-items-center",
								logolink("dmp")
							),
							# Disable scrolling
							style = "overflow-x: hidden; overflow-y: hidden;",
							class = "card border-primary bg-secondary"
						),
						bslib::card(
							bslib::card_body(
								class = "d-flex justify-content-center align-items-center",
								logolink("ne", height = 7)
							),
							class = "card border-primary bg-secondary"
						),
						bslib::card(
							bslib::card_body(
								class = "d-flex justify-content-center align-items-center",
								logolink("blackbawks")
							),
							class = "card border-primary bg-secondary"
						)
					),
					bslib::layout_columns(
						# height = "22vh",
						col_widths = c(4, 4, 4),
						bslib::card(
							bslib::card_body(
								class = "d-flex justify-content-center align-items-center",
								logolink("rescue")
							),
							class = "card border-primary bg-secondary"
						),
						bslib::card(
							bslib::card_body(
								class = "d-flex justify-content-center align-items-center",
								logolink("bto", height = 6)
							),
							class = "card border-primary bg-secondary"
						),
						bslib::card(
							bslib::card_body(
								class = "d-flex justify-content-center align-items-center",
								logolink("niras")
							),
							class = "card border-primary bg-secondary"
						)
					),
					bslib::layout_columns(
						col_widths = c(4, 4, 4),
									actionButton(
											ns("link_guide"),
											label = tagList(
												bsicons::bs_icon("info-circle"),
												"Guide"
											),
											class = "not-arrow-btn",
											style = "width: 100%;"
										),
													mod_app_tour_ui(
															ns("app_tour"),
															style = "width: 100%;"
														),
																actionButton(
																		ns("go_data"),
																		label = tagList(
																			bsicons::bs_icon("play-circle"),
																			"Start"
																		),
																		class = "arrow-btn"
																	)

					),
					# div(
					# 	class = "d-flex flex-column align-items-center justify-content-center gap-4 my-3",
					# 	# User Guide and Start Tutorial in a row
					# 	div(
					# 		class = "d-flex gap-3",
					# 		style = "width: 100%; max-width: 600px;",
					# 		# User Guide
					# 		div(
					# 			class = "flex-grow-1",
					# 			actionButton(
					# 				ns("link_guide"),
					# 				label = tagList(
					# 					bsicons::bs_icon("info-circle"),
					# 					"Guide"
					# 				),
					# 				class = "not-arrow-btn",
					# 				style = "width: 100%;"
					# 			)
					# 		),
					# 		# Start Tutorial
					# 		div(
					# 			class = "flex-grow-1",
					# 			mod_app_tour_ui(
					# 				ns("app_tour"),
					# 				style = "width: 100%;"
					# 			)
					# 		)
					# 	),
					# 	# Select Data - centered below
					# 	div(
					# 		id = ns("tutorial_selectdata"),
					# 		actionButton(
					# 			ns("go_data"),
					# 			label = tagList(
					# 				bsicons::bs_icon("play-circle"),
					# 				"Select Flight-Height Distributions"
					# 			),
					# 			class = "arrow-btn",
					# 			style = "width: 100%; max-width: 600px;"
					# 		)
					# 	)
				)
			)
		)
	)
}

#' landing_page Server Functions
#'
#' @param id Module ID
#' @param nav_id The ID of the parent navigation bar (e.g., "main-nav"). This is required to allow the module to control navigation between tabs.
#' @param parent_session The session object of the parent Shiny app. This is required to allow the module to control navigation between tabs.
#'
#' @noRd
mod_landing_page_server <- function(
	id,
	nav_id = "main-nav",
	parent_session
) {
	moduleServer(id, function(input, output, session) {
		# Helpfile submodules -----
		mod_help_button_server("dummy_help", help_file = "dummy_help")

		# React to next-page button --------------
		observeEvent(
			input$go_data,
			{
				bslib::nav_select(
					id = nav_id,
					selected = "nav-data-select",
					session = parent_session
				)
			}
		)

		# React to user guide button --------------
		observeEvent(
			input$link_guide,
			{
				# Navigate to the user guide tab
				bslib::nav_select(
					id = nav_id,
					selected = "nav-user-guide",
					session = parent_session
				)
			}
		)

		# Tour guide module  -------------------
		## Storing conductor object to tweak options or add response events later
		guide <- mod_app_tour_server("app_tour")
	})
}
