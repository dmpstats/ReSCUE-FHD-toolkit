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
            "<strong>DMP Statistical Solutions</strong> <br>ReSCUETOols Developers"
          ),
          "bto" = HTML(
            "<strong>British Trust for Ornithology</strong><br>ReSCUE Research Partner"
          ),
          "ne" = HTML(
            "<strong>Natural England</strong><br>ReSCUE Project Lead"
          ),
          "blackbawks" = HTML(
            "<strong>Black Bawks Data Science</strong><br>CLARIFY ROLE"
          ),
          "niras" = HTML("<strong>NIRAS</strong><br>CLARIFY ROLE"),
          "owec" = HTML("<strong>OWEC</strong><br>Project Funder"),
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
