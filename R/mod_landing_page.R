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
			bslib::layout_columns(
				col_widths = c(7, 5),
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
					height = "90%",
					bslib::accordion_panel(
						title = "Project Overview",
						icon = bsicons::bs_icon("info-circle"),
						lorem::ipsum(paragraphs = 2),
						class = "bg-light"
					),
					bslib::accordion_panel(
						title = "Other Stuff",
						icon = bsicons::bs_icon("question-circle"),
						p("We can nest some other stuff in here too."),
						class = "bg-light"
					),
					bslib::accordion_panel(
						title = "Contacts",
						icon = bsicons::bs_icon("envelope-fill"),
						p("Maybe some email contacts too."),
						class = "bg-light"
					)
				),
				# class = "card border-primary mb-3 bg-light"
				# )
				# ),
				column(
					12,
					bslib::card(
						bslib::card_header(
							"Project Partners",
							class = "text bg-primary"
						),
						bslib::card_body(
							bslib::layout_columns(
								col_widths = c(4, 4, 4),
								logolink("dmp"),
								logolink("bto"),
								logolink("ne")
							),
							bslib::layout_columns(
								col_widths = c(6, 6),
								logolink("blackbawks"),
								logolink("niras")
							)
						),
						class = "card border-primary mb-3 bg-secondary"
					),
					bslib::layout_columns(
						div(
							class = "d-flex flex-column align-items-center gap-2 my-3",
							actionButton(
								ns("link_guide"),
								label = tagList(
									bsicons::bs_icon("info-circle"),
									"User Guide"
								),
								class = "arrow-btn-faded",
								style = "width:18vw; height: 5vh;"
							),
							actionButton(
								ns("restore_session"),
								label = tagList(
									bsicons::bs_icon("arrow-counterclockwise"),
									"Restore Session"
								),
								class = "arrow-btn-faded",
								style = "width:18vw; height: 5vh;"
							),
						),
						div(
							class = "d-flex flex-column align-items-center gap-2 my-3",
							# tour guide module
							mod_app_tour_ui(
								ns("app_tour"),
								style = "width:18vw; height: 5vh;"
							),
							div(
								id = ns("tutorial_selectdata"),
								actionButton(
									ns("go_data"),
									label = tagList(
										bsicons::bs_icon("play-circle"),
										"Select Data"
									),
									class = "arrow-btn",
									style = "width:18vw; height: 5vh;"
								)
							)
						)
					)
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
				# For now, we'll just open inst/app/md/userguide.md in a modal
				showModal(modalDialog(
					title = NULL,
					shiny::includeMarkdown(app_sys(
						"app",
						"md",
						"userguide.md"
					)),
					easyClose = TRUE,
					footer = modalButton("Close"),
					size = "xl"
				))
			}
		)

		# Tour guide module  -------------------
		## Storing conductor object to tweak options or add response events later
		guide <- mod_app_tour_server("app_tour")
	})
}
