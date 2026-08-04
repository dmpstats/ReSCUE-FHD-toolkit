#' Run the FHD Metadata Builder app
#'
#' @description Launches a standalone Shiny app that walks a user through
#'   entering the fields of a single FHD `metadata` entry -- as defined in
#'   `data-raw/fhd_schema_tentative.R` and exemplified by the files in
#'   `data-dummy/metadata/` -- and lets them download the result as an `.rds`
#'   file ready to be paired with a `fhd_draws` object.
#'
#'   The app is self-contained (its own `ui`/`server`) so it can be launched
#'   on its own, e.g. `run_metadata_builder()`, without needing the rest of
#'   the ReSCUEApp golem app to be running.
#'
#' @param ... Arguments passed on to [shiny::shinyApp()] (e.g. `options`).
#'
#' @return A `shiny.appobj`, as returned by [shiny::shinyApp()]. Typically
#'   called for its side effect of launching the app.
#'
#' @export
#'
#' @importFrom shiny fluidRow column textInput numericInput selectInput selectizeInput radioButtons actionButton downloadButton downloadHandler verbatimTextOutput renderPrint reactiveVal req showModal removeModal modalDialog modalButton observeEvent showNotification icon shinyApp tagList tags NS renderText textOutput textAreaInput conditionalPanel uiOutput renderUI
#' @importFrom bslib page_fillable card card_header card_body layout_columns input_switch value_box
run_metadata_builder <- function(...) {
  ui <- run_metadata_builder_ui()
  server <- run_metadata_builder_server()
  shiny::shinyApp(ui, server, ...)
}

# Choices, kept here so UI and validation stay in sync -----------------------
.mb_choices <- list(
  method = c("LiDAR-DAS", "GPS", "Altimeter"),
  spatial_scale = c("site-specific", "regional", "national"),
  temporal_scale = c("monthly", "seasonal", "annual"),
  season = c("breeding", "nonbreeding"),
  input_type = c("rescue-library", "user-upload"),
  cov_type = c("numeric", "factor", "logical")
)

#' @noRd
run_metadata_builder_ui <- function() {
  bslib::page_fillable(
    title = "FHD Metadata Builder",
    theme = bslib::bs_theme(version = 5),

    bslib::layout_columns(
      col_widths = c(8, 4),

      # ---- Form -------------------------------------------------------
      bslib::card(
        bslib::card_header("Build a metadata entry", class = "text-bg-primary"),
        bslib::card_body(
          shiny::tags$h5("Identification"),
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::textInput(
              "fhd_id",
              "Dataset ID*",
              placeholder = "e.g. Kittiwake_Wales_V_Bay"
            ) |>
              bslib::tooltip(
                "This must match the filename of the corresponding draws file."
              ),
            shiny::textInput(
              "species_id",
              "Species ID*",
              placeholder = "e.g. species_kittiwake"
            ),
            shiny::selectizeInput(
              "group",
              "Species group",
              choices = c("auk", "diver", "gull"),
              options = list(
                create = TRUE,
                placeholder = "select or type a group"
              )
            )
          ),
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::textInput(
              "name_common",
              "Common name*",
              placeholder = "e.g. Kittiwake"
            ),
            shiny::textInput(
              "name_scientific",
              "Scientific name",
              placeholder = "e.g. Rissa_tridactyla"
            ),
            bslib::input_switch(
              "crm_recommended",
              "Recommended for CRM use?",
              value = FALSE
            )
          ),

          shiny::tags$hr(),
          shiny::tags$h5("Method & scale"),
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::selectInput(
              "method",
              "Method*",
              choices = .mb_choices$method
            ),
            shiny::selectInput(
              "spatial_scale",
              "Spatial scale*",
              choices = .mb_choices$spatial_scale
            ),
            shiny::selectInput(
              "temporal_scale",
              "Temporal scale*",
              choices = .mb_choices$temporal_scale
            )
          ),
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::numericInput(
              "month",
              "Month",
              value = NA,
              min = 1,
              max = 12,
              step = 1
            ),
            shiny::selectizeInput(
              "season",
              "Season(s)",
              choices = .mb_choices$season,
              multiple = TRUE,
              options = list(
                create = TRUE,
                placeholder = "select or type a season"
              )
            ),
            shiny::numericInput(
              "year",
              "Year",
              value = NA,
              min = 1900,
              max = 2100,
              step = 1
            )
          ),

          shiny::tags$hr(),
          shiny::tags$h5("Location"),
          bslib::layout_columns(
            col_widths = c(3, 3, 3, 3),
            shiny::textInput("country", "Country", value = "UK"),
            shiny::textInput("region", "Region"),
            shiny::textInput("site", "Site"),
            shiny::textInput("sea_area", "Sea area (e.g. ICES division)")
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::numericInput("lon", "Longitude", value = NA, step = 0.001),
            shiny::numericInput("lat", "Latitude", value = NA, step = 0.001)
          ),

          shiny::tags$hr(),
          shiny::tags$h5("Source"),
          shiny::radioButtons(
            "input_type",
            "Input type*",
            choices = .mb_choices$input_type,
            inline = TRUE
          ),

          shiny::tags$hr(),
          shiny::tags$h5("Covariates"),
          shiny::actionButton(
            "add_covariate",
            "Add covariate",
            icon = shiny::icon("plus"),
            class = "btn-secondary"
          ),
          shiny::tags$br(),
          shiny::tags$br(),
          DT::DTOutput("covariates_table"),
          shiny::actionButton(
            "remove_covariate",
            "Remove selected covariate",
            class = "btn-outline-danger btn-sm mt-2"
          )
        )
      ),

      # ---- Actions / preview -------------------------------------------
      shiny::tagList(
        bslib::card(
          bslib::card_header("Build & download"),
          bslib::card_body(
            shiny::actionButton(
              "build",
              "Build metadata",
              icon = shiny::icon("hammer"),
              class = "btn-primary w-100"
            ),
            shiny::tags$br(),
            shiny::tags$br(),
            shiny::uiOutput("download_ui"),
            shiny::tags$br(),
            shiny::textOutput("build_status")
          )
        ),
        bslib::card(
          bslib::card_header("Preview"),
          bslib::card_body(
            shiny::verbatimTextOutput("preview")
          )
        )
      )
    )
  )
}

#' @noRd
run_metadata_builder_server <- function() {
  function(input, output, session) {
    covariates <- shiny::reactiveVal(list())
    metadata_built <- shiny::reactiveVal(NULL)

    # ---- Add a covariate via modal ------------------------------------
    shiny::observeEvent(input$add_covariate, {
      shiny::showModal(shiny::modalDialog(
        title = "Add covariate",
        shiny::textInput(
          "cov_key",
          "Covariate key*",
          placeholder = "e.g. wind_speed (as it appears in fhd_draws)"
        ),
        shiny::textInput(
          "cov_label",
          "Label*",
          placeholder = "e.g. Wind Speed"
        ),
        shiny::radioButtons(
          "cov_type",
          "Type*",
          choices = .mb_choices$cov_type,
          inline = TRUE
        ),
        shiny::conditionalPanel(
          "input.cov_type == 'factor' || input.cov_type == 'logical'",
          shiny::textInput(
            "cov_levels",
            "Levels (comma-separated)",
            placeholder = "e.g. low, medium, high"
          )
        ),
        shiny::textAreaInput(
          "cov_blurb",
          "Short description",
          placeholder = "Short description of the covariate"
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_covariate", "Add", class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$confirm_covariate, {
      shiny::req(input$cov_key, input$cov_label, input$cov_type)

      levels_val <- if (
        identical(input$cov_type, "numeric") ||
          !nzchar(input$cov_levels %||% "")
      ) {
        NULL
      } else {
        trimws(strsplit(input$cov_levels, ",")[[1]])
      }
      if (identical(input$cov_type, "logical")) {
        levels_val <- c(TRUE, FALSE)
      }

      new_cov <- list(list(
        label = input$cov_label,
        type = input$cov_type,
        levels = levels_val,
        blurb = if (nzchar(input$cov_blurb %||% "")) input$cov_blurb else NULL
      ))
      names(new_cov) <- input$cov_key

      current <- covariates()
      current[[input$cov_key]] <- new_cov[[1]]
      covariates(current)

      shiny::removeModal()
    })

    shiny::observeEvent(input$remove_covariate, {
      sel <- input$covariates_table_rows_selected
      current <- covariates()
      if (length(sel) == 0 || length(current) == 0) {
        shiny::showNotification(
          "Select a covariate row to remove first.",
          type = "warning"
        )
        return(invisible(NULL))
      }
      covariates(current[-sel])
    })

    output$covariates_table <- DT::renderDT({
      cov <- covariates()
      if (length(cov) == 0) {
        return(DT::datatable(
          data.frame(
            key = character(),
            label = character(),
            type = character(),
            levels = character(),
            blurb = character()
          ),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }
      df <- do.call(
        rbind,
        lapply(names(cov), function(k) {
          data.frame(
            key = k,
            label = cov[[k]]$label,
            type = cov[[k]]$type,
            levels = paste(cov[[k]]$levels, collapse = ", "),
            blurb = cov[[k]]$blurb %||% "",
            stringsAsFactors = FALSE
          )
        })
      )
      DT::datatable(
        df,
        rownames = FALSE,
        selection = "single",
        options = list(dom = "t", pageLength = 10)
      )
    })

    # ---- Build the metadata list --------------------------------------
    shiny::observeEvent(input$build, {
      missing_required <- c(
        if (!nzchar(input$fhd_id %||% "")) "Dataset ID",
        if (!nzchar(input$species_id %||% "")) "Species ID",
        if (!nzchar(input$name_common %||% "")) "Common name"
      )
      if (length(missing_required) > 0) {
        shiny::showNotification(
          paste(
            "Missing required field(s):",
            paste(missing_required, collapse = ", ")
          ),
          type = "error"
        )
        return(invisible(NULL))
      }

      lon_val <- input$lon %||% NA
      lat_val <- input$lat %||% NA
      has_coords <- !is.na(lon_val) && !is.na(lat_val)

      sf_obj <- NULL
      if (has_coords && requireNamespace("sf", quietly = TRUE)) {
        sf_obj <- sf::st_sfc(sf::st_point(c(lon_val, lat_val)), crs = 4326)
      } else if (has_coords) {
        shiny::showNotification(
          "Package 'sf' isn't installed; skipping 'sf_obj' (lon/lat still saved).",
          type = "warning"
        )
      }

      cov <- covariates()

      metadata <- list(
        fhd_id = input$fhd_id,
        method = input$method,
        spatial_scale = input$spatial_scale,
        temporal_scale = input$temporal_scale,
        month = {
          m <- input$month %||% NA
          if (is.na(m)) NA_integer_ else as.integer(m)
        },
        season = paste(input$season, collapse = ", "),
        year = {
          y <- input$year %||% NA
          if (is.na(y)) NA_integer_ else as.integer(y)
        },
        country = input$country,
        region = input$region,
        site = input$site,
        sea_area = input$sea_area,
        species_id = input$species_id,
        name_common = input$name_common,
        name_scientific = input$name_scientific,
        group = input$group,
        crm_recommended = isTRUE(input$crm_recommended),
        lon = lon_val,
        lat = lat_val,
        sf_obj = sf_obj,
        input_type = input$input_type,
        covariates = if (length(cov) == 0) NULL else cov
      )

      metadata_built(metadata)
      shiny::showNotification(
        "Metadata built. See preview and download below.",
        type = "message"
      )
    })

    output$build_status <- shiny::renderText({
      if (is.null(metadata_built())) "Not built yet." else "Ready to download."
    })

    output$preview <- shiny::renderPrint({
      shiny::req(metadata_built())
      utils::str(metadata_built())
    })

    output$download_ui <- shiny::renderUI({
      if (is.null(metadata_built())) {
        return(shiny::actionButton(
          "noop_download",
          "Download .rds",
          class = "w-100"
        ))
      }
      shiny::downloadButton(
        "download",
        "Download .rds",
        class = "btn-success w-100"
      )
    })

    shiny::observeEvent(input$noop_download, {
      shiny::showNotification("Click 'Build metadata' first.", type = "warning")
    })

    output$download <- shiny::downloadHandler(
      filename = function() {
        id <- metadata_built()$fhd_id
        paste0(if (nzchar(id %||% "")) id else "fhd_metadata", ".rds")
      },
      content = function(file) {
        saveRDS(metadata_built(), file)
      }
    )
  }
}

# `%||%` is used for default-if-empty fallbacks throughout this file; rlang
# is already a dependency of this package so we reuse its implementation.
#' @importFrom rlang %||%
NULL
