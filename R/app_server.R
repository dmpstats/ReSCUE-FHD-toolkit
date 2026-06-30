#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

	# Dummy map: random offshore points around the UK
	set.seed(3847)
	n_pts <- 120
	dummy_pts <- data.frame(
		lat = runif(n_pts, 49.5, 61.5),
		lon = runif(n_pts, -8.5, 2.5)
	)

	output$source_map <- leaflet::renderLeaflet({
		leaflet::leaflet(dummy_pts) |>
			leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) |>
			leaflet::setView(lng = -3.5, lat = 56, zoom = 5) |>
			leaflet::addCircleMarkers(
				lng         = ~lon,
				lat         = ~lat,
				radius      = 9,
				color       = "#cccccc",
				weight      = 1,
				fillColor   = "#c8c8c8",
				fillOpacity = 0.75
			)
	})

	# Dummy: data table of selected data
	output$dummy_dt <- DT::renderDataTable({
		data.frame(
			Species = c("Puffin", "Guillemot", "Bald Eagle"),
			ID = "P1", "G1", "BE1",
			Dataset = c("Jonson et al.", "Jonson et al.", "Smith et al."),
			`Plotting Options` = "Miscellaneous"
		)
	})

	# Helper functions
	mod_help_button_server("dummy_help", help_file = "dummy_help")


}
