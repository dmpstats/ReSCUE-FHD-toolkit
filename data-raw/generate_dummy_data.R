# generate_dummy_data.R
# This file will generate a set of dummy FHDs according to our data schema
# (see fhd_schema_tentative.R).
#
# Outputs (written under `dir`):
#   draws/    - one .rds per entry; contains the long-format draws data frame
#   metadata/ - one .rds per entry; contains a flat named list (covariates
#               remain nested) suitable for row-binding across entries

library(tools)
library(sf)

generate_dummy_data <- function(n_datasets = 10, dir = "data-dummy") {
  draws_dir <- file.path(dir, "draws")
  metadata_dir <- file.path(dir, "metadata")

  for (d in c(draws_dir, metadata_dir)) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE)
    } else {
      rds_files <- list.files(d, pattern = "\\.rds$", full.names = TRUE)
      if (length(rds_files) > 0) file.remove(rds_files)
    }
  }

  for (i in seq_len(n_datasets)) {
    # ------------------------------------------------------------------
    # Sample species
    # ------------------------------------------------------------------
    species <- sample(
      c(
        "Puffin",
        "Guillemot",
        "Black Guillemot",
        "Great Northern Diver",
        "Shag",
        "Razorbill",
        "Kittiwake"
      ),
      1
    )
    species_id <- paste0("species_", gsub(" ", "_", tolower(species)))
    name_common <- tools::toTitleCase(species)
    name_scientific <- paste0(
      sample(c("Aves", "Alcidae", "Uria", "Cepphus", "Fratercula"), 1),
      "_",
      sample(c("alca", "guillemot", "cristata", "arctica", "arctica"), 1)
    )
    group <- sample(c("auk", "diver", "seaduck", "gull", "tern"), 1)
    crm_recommended <- sample(c(TRUE, FALSE), 1)

    # ------------------------------------------------------------------
    # Sample data-source descriptors
    # ------------------------------------------------------------------
    method <- sample(c("LiDAR-DAS", "GPS", "Altimeter"), 1)
    spatial_scale <- sample(c("site-specific", "regional", "national"), 1)
    temporal_scale <- sample(c("monthly", "seasonal", "annual"), 1)
    month <- sample(1:12, 1)
    year <- sample(2000:2024, 1)
    seasons <- sample(
      c("breeding", "nonbreeding"),
      sample(1:2, 1),
      replace = FALSE
    )
    region <- sample(
      c("UK", "Scotland", "England", "Wales", "Northern Ireland"),
      1
    )
    sea_area <- sample(c("IVa", "IVb", "IVc", "VIa", "VIIe"), 1)
    site <- paste(
      sample(LETTERS, 1),
      sample(c("Point", "Head", "Isle", "Bay"), 1)
    )
    lat <- runif(1, 49.5, 61.5)
    lon <- runif(1, -8.5, 2.5)
    sf_obj <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)

    # ------------------------------------------------------------------
    # Covariates (nested; kept outside flat structure by design)
    # ------------------------------------------------------------------
    cov_names <- sample(
      c(
        "wind_speed",
        "wind_direction",
        "temperature",
        "humidity",
        "precipitation"
      ),
      sample(c(0, 1, 1), 1)
    )

    # Use NULL (not list()) when there are no covariates: bind_rows treats
    # NULL list-column entries as NA and keeps the row, whereas list() causes
    # silent row-dropping when mixed with non-empty covariate lists.
    covariates <- if (length(cov_names) == 0) {
      NULL
    } else {
      setNames(
        lapply(cov_names, function(cov) {
          list(
            label = cov,
            type = "numeric",
            levels = NULL,
            blurb = paste("Simulated", cov, "covariate")
          )
        }),
        cov_names
      )
    }

    # ------------------------------------------------------------------
    # Unique entry ID
    # ------------------------------------------------------------------
    fhd_id <- paste0(
      species,
      "_",
      stringr::str_replace_all(region, " ", "_"),
      "_",
      stringr::str_replace_all(site, " ", "_")
    )

    # ------------------------------------------------------------------
    # Build draws data frame
    # ------------------------------------------------------------------
    n_iter <- sample(100:1000, 1)
    max_height <- sample(50:200, 1)

    grid_list <- list(
      draw_id = 1:n_iter,
      height = seq(0, max_height, length.out = 100) + 0.5
    )
    for (cov in cov_names) {
      cov_max <- sample(0:100, 1)
      grid_list[[cov]] <- seq(0, cov_max, by = 1)
    }

    draws <- do.call(
      expand.grid,
      c(grid_list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    )

    iter_pars <- data.frame(
      draw_id = 1:n_iter,
      meanlog = rnorm(n_iter, mean = log(max_height / 3), sd = 0.3),
      sdlog = runif(n_iter, 0.3, 0.8)
    )

    draws <- merge(draws, iter_pars, by = "draw_id")
    draws$probability <- with(
      draws,
      dlnorm(height, meanlog = meanlog, sdlog = sdlog)
    )
    draws$meanlog <- NULL
    draws$sdlog <- NULL

    # ------------------------------------------------------------------
    # Flat metadata list  (covariates remain nested)
    # ------------------------------------------------------------------
    metadata <- list(
      fhd_id = fhd_id,
      method = method,
      spatial_scale = spatial_scale,
      temporal_scale = temporal_scale,
      month = month,
      season = paste(seasons, collapse = ", "),
      year = year,
      country = "UK",
      region = region,
      site = site,
      sea_area = sea_area,
      species_id = species_id,
      name_common = name_common,
      name_scientific = name_scientific,
      group = group,
      crm_recommended = crm_recommended,
      lon = lon,
      lat = lat,
      sf_obj = sf_obj,
      input_type = "rescue-library",
      covariates = covariates # nested; excluded from flat row-binding
    )

    # ------------------------------------------------------------------
    # Save draws and metadata separately
    # ------------------------------------------------------------------
    saveRDS(draws, file = file.path(draws_dir, paste0(fhd_id, ".rds")))
    saveRDS(metadata, file = file.path(metadata_dir, paste0(fhd_id, ".rds")))
  }

  message(
    "Done. Wrote ",
    n_datasets,
    " entries to:\n",
    "  draws:    ",
    draws_dir,
    "\n",
    "  metadata: ",
    metadata_dir
  )
  invisible(NULL)
}

generate_dummy_data(n_datasets = 5, dir = "data-dummy")

# Try calling and binding the metadata
metadata_files <- list.files(
  "data-dummy/metadata",
  pattern = "\\.rds$",
  full.names = TRUE
)
metadata_list <- lapply(metadata_files, readRDS)
metadata_df <- dplyr::bind_rows(metadata_list)
metadata_df
