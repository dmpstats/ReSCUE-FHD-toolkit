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

# Call BDMPS regions
bd <- sf::st_read(
  "data-raw/BDMPS_regions/Final BDMPS regions long format.shp",
  quiet = TRUE
)

generate_dummy_data <- function(
  dir = "data-dummy",
  seed = 1234,
  bd = NULL
) {
  # If bd is not provided, load it from file
  if (is.null(bd)) {
    bd <- sf::st_read(
      "data-raw/BDMPS_regions/Final BDMPS regions long format.shp",
      quiet = TRUE
    )
  }

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

  # define list of (strictly-categorical) covariates with pre-defined levels (i.e. do not
  # vary over simulations)
  covar_list <- list(
    wind_speed = list(
      label = "Wind Speed",
      type = "factor",
      levels = c("low", "medium", "high"),
      blurb = "Simulated wind speed covariate"
    ),
    wind_direction = list(
      label = "Wind Direction",
      type = "factor",
      levels = c("north", "south", "east", "west"),
      blurb = "Simulated wind direction covariate"
    ),
    temperature = list(
      label = "Temperature",
      type = "factor",
      levels = c("hot", "mild", "freezing"),
      blurb = "Simulated temperature covariate"
    ),
    daytime = list(
      label = "Daytime",
      type = "logical",
      levels = c(TRUE, FALSE),
      blurb = "Simulated daytime covariate"
    ),
    precipitation = list(
      label = "Precipitation",
      type = "factor",
      levels = c("rain", "snow", "none"),
      blurb = "Simulated precipitation covariate"
    )
  )

  set.seed(seed)

  # Iterate over each row in bd (species/BDMPS region/geometry combination)
  for (i in seq_len(nrow(bd))) {
    # ------------------------------------------------------------------
    # Extract species and BDMPS region from bd
    # ------------------------------------------------------------------
    species <- bd$Species[i]
    bdmps_region <- bd$BDMPS.regi[i]
    bd_polygon <- sf::st_geometry(bd)[i]

    # Create species_id from the actual species name
    species_id <- paste0("species_", gsub(" ", "_", tolower(species)))
    name_common <- tools::toTitleCase(species)

    # Generate synthetic scientific name (plaintext, doesn't matter later)
    name_scientific <- paste0(
      sample(c("Aves", "Alcidae", "Uria", "Cepphus", "Fratercula"), 1),
      "_",
      sample(c("alca", "guillemot", "cristata", "arctica", "arctica"), 1)
    )
    group <- sample(c("auk", "diver", "seaduck", "gull", "tern"), 1)
    crm_recommended <- sample(c(TRUE, FALSE), 1)

    # ------------------------------------------------------------------
    # Sample data-source descriptors (plaintext, doesn't matter later)
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
    region <- bdmps_region
    sea_area <- sample(c("IVa", "IVb", "IVc", "VIa", "VIIe"), 1)
    site <- paste(
      sample(LETTERS, 1),
      sample(c("Point", "Head", "Isle", "Bay"), 1)
    )
    # Use polygon geometry from bd instead of random point
    sf_obj <- bd_polygon

    # ------------------------------------------------------------------
    # Covariates (nested; kept outside flat structure by design)
    # ------------------------------------------------------------------

    # sample which covariates to include from pre-defined list
    covar_idx <- sample(1:length(covar_list), sample(0:3, 1))

    # Use NULL (not list()) when there are no covariates: bind_rows treats
    # NULL list-column entries as NA and keeps the row, whereas list() causes
    # silent row-dropping when mixed with non-empty covariate lists.
    covariates <- if (length(covar_idx) == 0) {
      NULL
    } else {
      covar_list[covar_idx]
    }

    # ------------------------------------------------------------------
    # Unique entry ID (based on species and BDMPS region)
    # ------------------------------------------------------------------
    fhd_id <- paste0(
      gsub(" ", "_", tolower(species)),
      "_",
      stringr::str_replace_all(bdmps_region, " ", "_"),
      "_",
      i # add index to ensure uniqueness if needed
    )

    # ------------------------------------------------------------------
    # Build draws data frame
    # ------------------------------------------------------------------
    n_iter <- sample(100:500, 1)
    max_height <- sample(50:300, 1)

    grid_list <- list(
      draw_id = 1:n_iter,
      height = seq(0, max_height, length.out = 100) + 0.5
    )

    # for (cov in cov_names) {
    #   cov_max <- sample(0:100, 1)
    #   grid_list[[cov]] <- seq(0, cov_max, by = 1)
    # }

    for (cov in names(covariates)) {
      grid_list[[cov]] <- covariates[[cov]]$levels
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

    # Add a random meanlog shift per covariate level so that different levels
    # produce visually distinct flight-height distributions.
    total_shift <- numeric(nrow(draws))
    for (cov in names(covariates)) {
      lvls <- as.character(covariates[[cov]]$levels)
      level_shifts <- stats::setNames(
        rnorm(length(lvls), mean = 0, sd = 0.3),
        lvls
      )
      total_shift <- total_shift + level_shifts[as.character(draws[[cov]])]
    }

    draws$probability <- with(
      draws,
      dlnorm(height, meanlog = meanlog + total_shift, sdlog = sdlog)
    )
    draws$meanlog <- NULL
    draws$sdlog <- NULL

    # order by height, for each draw_it and covariates combination (if any)
    draws <- draws[
      do.call(order, draws[c(names(covariates), "draw_id", "height")]),
    ]

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
    nrow(bd),
    " entries to:\n",
    "  draws:    ",
    draws_dir,
    "\n",
    "  metadata: ",
    metadata_dir
  )
  invisible(NULL)
}

generate_dummy_data(dir = "data-dummy", bd = bd)

# Try calling and binding the metadata
metadata_files <- list.files(
  "data-dummy/metadata",
  pattern = "\\.rds$",
  full.names = TRUE
)

metadata_list <- lapply(metadata_files, readRDS)

# below is slightly hacky, but needed to ensure that covariates are always a list (even
# if NULL) so that row-binding works. there should be 1-row per fhd entry
metadata_tbl <- metadata_list |>
  # need to nest covariates list into a higher-level list so that list-columns are
  # preserved
  purrr::map(
    function(x) {
      #browser()
      x$covariates <- list(x$covariates %||% NULL)
      x
    }
  ) |>
  purrr::map(tibble::as_tibble_row) |>
  purrr::list_rbind()
