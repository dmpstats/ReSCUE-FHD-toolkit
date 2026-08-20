#' user_upload UI Function
#'
#' @description A shiny Module allowing users to manually upload a flight-height distribution.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_user_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Dataset Information Section
    bslib::card(
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(9, 3),
          tagList(
            tags$h6(tags$b("Upload Instructions")),
            p(
              "Upload your flight-height distribution datasets by completing the required fields below."
            ),
            tags$hr(),
            tags$small(
              tags$i(
                "Note: All uploaded data must be pre-prepared using the ReSCUETools R package, which is not yet publicly availab. Alternatively, you can populate the CSV schema provided on the right-hand side."
              )
            )
          ),
          # Right side: Square button, centered
          div(
            class = "d-flex align-items-center justify-content-center",
            style = "height: 100%; min-height: 150px;",
            actionButton(
              ns("download_schema"),
              "Download CSV Schema",
              class = "btn-success",
              icon = bsicons::bs_icon("download"),
              style = "width: 140px; height: 140px; border-radius: 8px; padding: 10px; line-height: 1.2;"
            )
          )
        )
      ),
      class = "card border-warning mb-3"
    ),
    bslib::layout_column_wrap(
      widths = c(1, 1),
      textInput(
        ns("fhd_id"),
        "Dataset ID*",
        placeholder = "Enter a unique dataset ID"
      ),
      textInput(
        ns("species"),
        "Species Name*",
        placeholder = "Enter species name"
      )
    ),

    # Data Collection Method and Season Section
    bslib::layout_column_wrap(
      widths = c(1, 1, 1),
      radioButtons(
        ns("method"),
        "Data Collection Method*",
        choices = c("Altimeter", "GPS", "LiDAR-DAS"),
        inline = FALSE
      ),
      radioButtons(
        ns("season"),
        "Season*",
        choices = c(
          "Both" = "nonbreeding, breeding",
          "Breeding" = "breeding",
          "Non-breeding" = "nonbreeding"
        ),
        inline = FALSE
      ),
      # File Upload Section
      bslib::card(
        "Note: User-uploaded files with additional covariates are not yet supported.",
        class = "card bg-warning"
      ),
      fileInput(
        ns("fhd_file"),
        "Upload Flight Height Distribution CSV*",
        accept = c(".csv", ".rds")
      )
    ),

    # Site Location Section
    bslib::layout_column_wrap(
      widths = c(1, 1, 1),
      numericInput(
        ns("lon"),
        "Site Longitude",
        value = NA,
        step = 0.001
      ),
      numericInput(
        ns("lat"),
        "Site Latitude",
        value = NA,
        step = 0.001
      ),
      textInput(
        ns("site_name"),
        "Site Name",
        placeholder = "Enter site name"
      )
    ),
    p("* Required fields"),
    # Some whitespace
    br(),

    # Action Buttons
    bslib::layout_columns(
      widths = c(1, 1, 1),
      min_height = "75px",

      actionButton(
        ns("clear_all_uploads"),
        "Clear All Uploads",
        class = "btn-warning",
        width = "100%"
      ),
      actionButton(
        ns("clear"),
        "Clear Fields",
        class = "btn-secondary",
        width = "100%"
      ),
      actionButton(
        ns("submit"),
        "Submit",
        class = "btn-primary",
        width = "100%"
      )
    ),

    # A white line to separate the form from the metadata table
    tags$hr(style = "border-top: 1px solid #fff;"),
    # And the metadata table
    DT::DTOutput(ns("metadata_table"))
  )
}

#' user_upload Server Functions
#'
#' @noRd
mod_user_upload_server <- function(id, clear_trigger = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Keep reactive lists of the metadata entries and draws
      metadata <- reactiveVal(list())
      draws <- reactiveVal(list())

      # On Submit, prepare a metadata entry with these fields
      observeEvent(
        input$submit,
        {
          if (
            length(input$fhd_id) == 0 ||
              length(input$species) == 0 ||
              length(input$method) == 0 ||
              length(input$season) == 0 ||
              is.null(input$fhd_file)
          ) {
            showNotification(
              "Please fill in all required fields and upload a file.",
              type = "error"
            )
            return(NULL)
          }

          # Validate required fields
          req(
            input$fhd_id,
            input$species,
            input$method,
            input$season,
            input$fhd_file
          )

          # Create a new metadata entry
          new_entry <- data.frame(
            fhd_id = input$fhd_id,
            species_id = input$species,
            method = input$method,
            season = input$season,
            file_path = input$fhd_file$datapath,
            lon = input$lon,
            lat = input$lat,
            site = input$site_name,
            stringsAsFactors = FALSE
          )

          # Read the uploaded file
          file_data <- if (grepl("\\.csv$", input$fhd_file$name)) {
            read.csv(input$fhd_file$datapath)
          } else if (grepl("\\.rds$", input$fhd_file$name)) {
            readRDS(input$fhd_file$datapath)
          } else {
            showNotification(
              "Unsupported file type. Please upload a CSV or RDS file.",
              type = "error"
            )
            return(NULL)
          }

          # Later, we'll implement checks for essential columns here

          # Update reactive lists with new entry
          current_metadata <- metadata()
          current_metadata[[input$fhd_id]] <- new_entry
          metadata(current_metadata)

          current_draws <- draws()
          current_draws[[input$fhd_id]] <- file_data
          draws(current_draws)

          # If successful, show a success notification
          showNotification(
            paste("Successfully uploaded dataset:", input$fhd_id),
            type = "message"
          )
        }
      )

      # Clear the fields when the clear button is pressed OR
      # when the modal is closed OR
      # when the user uploads a new dataset
      observeEvent(
        c(input$clear, input$submit),
        {
          updateTextInput(session, "fhd_id", value = "")
          updateTextInput(session, "species", value = "")
          updateRadioButtons(session, "method", selected = character(0))
          updateRadioButtons(session, "season", selected = character(0))
          updateNumericInput(session, "lon", value = NA)
          updateNumericInput(session, "lat", value = NA)
          updateTextInput(session, "site_name", value = "")
        }
      )

      # Render the metadata uploaded by the user
      output$metadata_table <- DT::renderDT({
        req(length(metadata()) > 0)
        DT::datatable(
          metadata() |>
            dplyr::bind_rows(),
          options = list(pageLength = 5, scrollX = TRUE),
          rownames = FALSE
        )
      })

      observeEvent(
        input$clear_all_uploads,
        {
          if (length(metadata()) == 0) {
            showNotification(
              "No uploaded datasets to clear.",
              type = "warning"
            )
            return(NULL)
          }

          # Clear the reactive lists
          metadata(list())
          draws(list())

          # Show a notification
          showNotification(
            "All uploaded datasets have been cleared.",
            type = "message"
          )
        }
      )

      # Clear on the reactive trigger
      observeEvent(
        clear_trigger(),
        {
          # Clear the reactive lists
          metadata(list())
          draws(list())
        }
      )

      return(
        list(
          metadata = metadata,
          draws = draws
        )
      )
    }
  )
}
