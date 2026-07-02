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
						col_widths = c(6, 6),
						# LHS: A card containing project information.
						bslib::card(
							bslib::card_header(
								"What is ResCUE?",

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
								# bslib::card_header("Project Partners"),
								bslib::card_body(
									bslib::layout_columns(
										col_widths = c(3, 3, 3, 3),
										logolink("dmp"),
										logolink("bto"),
										logolink("ne"),
										logolink("blackbawks")
									)
								)
							),
							div(
								class = "d-flex flex-column align-items-center gap-2 my-3",
								actionButton(
									ns("link_guide"),
									label = tagList(
										bsicons::bs_icon("info-circle"),
										"User Guide"
									),
									class = "arrow-btn-faded",
									style = "width:225px"
								),
								actionButton(
									ns("restore_session"),
									label = tagList(
										bsicons::bs_icon("arrow-counterclockwise"),
										"Restore Session"
									),
									class = "arrow-btn-faded",
									style = "width:225px"
								),
								actionButton(
									ns("go_data"),
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
  )
}
    
#' landing_page Server Functions
#'
#' @param id Module ID
#' @param nav_id The ID of the parent navigation bar (e.g., "main-nav"). This is required to allow the module to control navigation between tabs.
#' @param parent_session The session object of the parent Shiny app. This is required to allow the module to control navigation between tabs.
#'
#' @noRd 
mod_landing_page_server <- function(id, nav_id = "main-nav", parent_session){
  moduleServer(id, function(input, output, session){

    # React to next-page button --------------
    observeEvent(
      input$go_data,
      {
        bslib::nav_select(
          id = nav_id,
          selected = "Data Selection",
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
					title     = NULL,
					shiny::includeMarkdown(app_sys("app", "md", "userguide.md")),
					easyClose = TRUE,
					footer    = modalButton("Close"),
					size      = "xl"
				))
			}
		)

  })
}
