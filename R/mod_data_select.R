#' data_select UI Function
#'
#' @description A shiny Module.
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
					),
					div(
						class = "d-flex flex-column align-items-center gap-2 my-3",
						actionButton(
							ns("go_analysis"),
							label = tagList(
								bsicons::bs_icon("play-circle"),
								"Start Analysis"
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

#' data_select Server Functions
#'
#' @noRd
mod_data_select_server <- function(id, nav_id = "main-nav", parent_session) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns

		# React to next-page button --------------
		observeEvent(
			input$go_analysis,
			{
				bslib::nav_select(
					id = nav_id,
					selected = "Analysis",
					session = parent_session
				)
			}
		)
	})
}
