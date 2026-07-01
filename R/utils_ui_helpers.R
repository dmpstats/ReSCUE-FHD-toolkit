#' logolink 
#'
#' @description A utils function for adding logos with associated links to the UI
#'
#' @return A div containing the logo image and link.
#'
#' @noRd
logolink <- function(company) {
  if (!(company %in% c("dmp", "bto", "ne", "blackbawks"))) {
    stop("Invalid company name. Please use one of the following: 'dmp', 'bto', 'ne', 'blackbawks', or modify logolink() for the new company.")
  }
  link = switch(
    company,
    "dmp" = "https://dmpstats.co.uk/",
    "bto" = "https://www.bto.org/",
    "ne" = "https://www.gov.uk/government/organisations/natural-england/about",
    "blackbawks" = "https://blackbawks.net/",
    "Invalid"
  )
  logo_path <- paste0("www/logos/", company, ".png")
  div(
    class = "text-center",
    tags$a(
      href = link,
      target = "_blank",
      rel = "noopener noreferrer",
      tags$img(
        src = logo_path,
        alt = paste0("Logo for ", company),
        style = "max-width: 100%; height: auto; cursor: pointer;"
      )
    )
  )
}