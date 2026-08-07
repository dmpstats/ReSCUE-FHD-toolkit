#' logolink
#'
#' @description A utils function for adding logos with associated links to the UI
#'
#' @return A div containing the logo image and link.
#'
#' @noRd
logolink <- function(company, tooltip = TRUE, height = 10) {
  if (
    !(company %in%
      c("dmp", "bto", "ne", "blackbawks", "niras", "owec", "rescue"))
  ) {
    stop(
      "Invalid company name. Please use one of the following: 'dmp', 'bto', 'ne', 'blackbawks', 'niras', 'owec', 'rescue', or modify logolink() for the new company."
    )
  }
  link <- switch(
    company,
    "dmp" = "https://dmpstats.co.uk/",
    "bto" = "https://www.bto.org/",
    "ne" = "https://www.gov.uk/government/organisations/natural-england/about",
    "blackbawks" = "https://blackbawks.net/",
    "niras" = "https://www.niras.com/",
    "owec" = "https://www.thecrownestate.co.uk/our-business/marine/offshore-wind-evidence-and-change-programme",
    "rescue" = "https://naturalengland.blog.gov.uk/2024/10/24/to-the-rescue-understanding-flight-heights-for-seabird-conservation-and-offshore-wind-expansion/",
    "Invalid"
  )
  logo_path <- paste0("www/logos/", company, ".png")
  out <- div(
    class = "d-flex justify-content-center align-items-center",
    style = "height: 100%; min-height: 150px;",
    tags$a(
      href = link,
      target = "_blank",
      rel = "noopener noreferrer",
      tags$img(
        src = logo_path,
        class = "logo-link",
        alt = paste0("Logo for ", company),
        style = paste0(
          "max-width: ",
          height,
          "vw; height: auto; cursor: pointer;"
        )
      )
    )
  )
  if (tooltip) {
    out <- out |>
      bslib::tooltip(
        switch(
          company,
          "dmp" = HTML(
            "<strong>DMP Statistical Solutions</strong> <br>ReSCUEApp Developers"
          ),
          "bto" = HTML(
            "<strong>British Trust for Ornithology</strong><br>ReSCUETools Developers"
          ),
          "ne" = HTML(
            "<strong>Natural England</strong><br>ReSCUE Project Lead"
          ),
          "blackbawks" = HTML(
            "<strong>Black Bawks Data Science</strong><br>ReSCUE Project Manager"
          ),
          "niras" = HTML(
            "<strong>NIRAS</strong><br>ReSCUE Development Consultants"
          ),
          "owec" = HTML(
            "<strong>OWEC / Crown Estate</strong><br>Project Funding Agency"
          ),
          "rescue" = HTML(
            "<strong>ReSCUE Project</strong><br>Project Website"
          ),
          "Invalid"
        ),
        placement = "bottom"
      )
  }

  out
}

#' Add FHD polygons to a leaflet map
#'
#' @description Helper function to add polygon layers for FHD data to a leaflet map.
#' Handles styling and popup creation with interactive buttons.
#'
#' @param map A leaflet map object or proxy to add polygons to.
#' @param data A data frame containing polygon data with columns: fhd_id, species_id,
#'   season, method, crm_recommended, and geometry.
#' @param selected_ids Character vector of currently selected FHD IDs.
#' @param ns A Shiny namespace function for generating input IDs.
#'
#' @return The updated leaflet map/proxy object with polygons added.
#'
#' @noRd
add_fhd_polygons <- function(map, data, selected_ids, ns) {
  if (is.null(data) || nrow(data) == 0) {
    return(map)
  }
  map |>
    leaflet::addPolygons(
      data = data,
      layerId = ~fhd_id,
      group = "main_data",
      color = "white",
      weight = 3,
      fillOpacity = 0.25,
      opacity = 1.0,
      fillColor = ifelse(data$fhd_id %in% selected_ids, "#ffa134", "grey"),
      label = ~region,
      popup = ~ paste0(
        "<div style='width:200px;'>",
        "<strong>",
        species_id,
        "</strong><br/>",
        "<strong>Season: </strong>",
        season,
        "<br/>",
        "<strong>Method: </strong>",
        method,
        "<br/>",
        "<strong>Recommended for CRM: </strong>",
        ifelse(
          crm_recommended,
          # Green text for yes, red for no
          "<span style='color:green;'>Yes</span>",
          "<span style='color:red;'>No</span>"
        ),
        "<br/>",

        # Add a section listing the covariates, which are in a nested list
        # (one list of named covariate specs per row of `data`)
        "<strong>Covariates: </strong>",
        vapply(
          covariates,
          function(covs) {
            if (length(covs) == 0) {
              return("None")
            }
            labels <- vapply(covs, function(cov) cov$label, character(1))
            paste0(
              "<ul>",
              paste0("<li>", labels, "</li>", collapse = ""),
              "</ul>"
            )
          },
          character(1)
        ),
        "<br/>",

        "<button ",
        "onclick=\"Shiny.setInputValue('",
        ns("map_add_btn"),
        "', '",
        fhd_id,
        "', {priority:'event'})\" ",
        ifelse(
          fhd_id %in% selected_ids,
          "class='btn btn-sm btn-success' ",
          "class='btn btn-sm btn-primary' "
        ),
        "style='margin-top:8px;width:100%;'>",
        ifelse(fhd_id %in% selected_ids, "Deselect Dataset", "Select Dataset"),
        "</button>",
        # Add a button for 'More Details'
        "<button ",
        "onclick=\"Shiny.setInputValue('",
        ns("map_details_btn"),
        "', '",
        fhd_id,
        "', {priority:'event'})\" ",
        "class='btn btn-sm btn-outline-primary' ",
        "style='margin-top:8px;width:100%;'>",
        "More Details",
        "</button>",
        "</div>"
      )
    )
}
