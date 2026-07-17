# generate_dummy_data.R
# This file will generate a set of dummy FHDs according to our data schema
# (see fhd_schema_tentative.R).

library(uuid)
library(tools)
library(sf)

generate_dummy_data <- function(n_datasets = 10, dir = "data-dummy") {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  } else {
    # Clear the directory
    rds_files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
    if (length(rds_files) > 0) {
      file.remove(rds_files)
    }
  }

  for (i in 1:n_datasets) {
    # We will generate a random FHD entry following the structure of the schema defined in fhd_schema_tentative.R.
    # First, we'll randomly sample a species:
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

    # Capitalise the name for name_common
    name_common <- tools::toTitleCase(species)

    # And generate 2x random words joined by _ for name_scientific
    name_scientific <- paste0(
      sample(c("Aves", "Alcidae", "Uria", "Cepphus", "Fratercula"), 1),
      "_",
      sample(c("alca", "guillemot", "cristata", "arctica", "arctica"), 1)
    )

    # Is this CRM recommended?
    crm_recommended <- sample(c(TRUE, FALSE), 1)

    # Choose the method of data collection:
    method <- sample(c("LiDAR-DAS", "GPS", "Altimeter"), 1)

    # Choose a spatial scale
    spatial_scale <- sample(c("site-specific", "regional", "national"), 1)

    # Choose a temporal scale
    temporal_scale <- sample(c("monthly", "seasonal", "annual"), 1)

    # Choose a month
    month <- sample(1:12, 1)
    # And a year between 2000 and 2024
    year <- sample(2000:2024, 1)

    # Choose season(s) - this might be breeding, nonbreeding, or both:
    seasons <- sample(
      c("breeding", "nonbreeding"),
      sample(1:2, 1),
      replace = FALSE
    )

    # Next, we'll randomly generate a point coordinate around the UK to use:
    lat <- runif(1, 49.5, 61.5)
    lon <- runif(1, -8.5, 2.5)
    sf_obj <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)

    # A few more dummy data-source descriptors
    region <- sample(
      c("UK", "Scotland", "England", "Wales", "Northern Ireland"),
      1
    )
    sea_area <- sample(c("IVa", "IVb", "IVc", "VIa", "VIIe"), 1)
    site <- paste(
      sample(LETTERS, 1),
      sample(c("Point", "Head", "Isle", "Bay"), 1),
      sep = " "
    )

    # Dummy taxonomic/functional group
    group <- sample(c("auk", "diver", "seaduck", "gull", "tern"), 1)

    # Choose a set of covariates to include in the FHD
    covariates <- sample(
      c(
        "wind_speed",
        "wind_direction",
        "temperature",
        "humidity",
        "precipitation"
      ),
      # We might have one or none
      sample(c(0, 1, 1), 1)
    )

    # And generate a unique ID for this FHD:
    fhd_uuid <- paste0(
      species,
      "_",
      stringr::str_replace_all(region, " ", "_"),
      "_",
      stringr::str_replace_all(site, " ", "_"),
      ".rds"
    )

    # Now, we'll randomly generate a number of draws/iterations for the FHD, somewhere between 100 and 1000:
    n_iter <- sample(100:1000, 1)

    # And choose a maximum height for the FHD, somewhere between 50 and 200 meters:
    max_height <- sample(50:200, 1)

    # ------------------------------------------------------------------
    # Build the long-format grid: one row per draw x height, plus one
    # column per selected covariate. Each covariate gets its own random
    # max_value (0-100), and takes integer increments from 0 to that max.
    # ------------------------------------------------------------------

    grid_list <- list(
      obs_iter = 1:n_iter,
      height = seq(0, max_height, length.out = 100) + 0.5
    )

    covariate_max_values <- list()
    for (cov in covariates) {
      cov_max <- sample(0:100, 1)
      covariate_max_values[[cov]] <- cov_max
      grid_list[[cov]] <- seq(0, cov_max, by = 1)
    }

    dummy_fhd <- do.call(
      expand.grid,
      c(grid_list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    )

    # Simulate a flight-height "density" for each draw: draw random
    # log-normal parameters per iteration, then evaluate at each height.
    iter_pars <- data.frame(
      obs_iter = 1:n_iter,
      meanlog = rnorm(n_iter, mean = log(max_height / 3), sd = 0.3),
      sdlog = runif(n_iter, 0.3, 0.8)
    )

    dummy_fhd <- merge(dummy_fhd, iter_pars, by = "obs_iter")
    dummy_fhd$probability <- with(
      dummy_fhd,
      dlnorm(height, meanlog = meanlog, sdlog = sdlog)
    )
    dummy_fhd$meanlog <- NULL
    dummy_fhd$sdlog <- NULL

    # ------------------------------------------------------------------
    # Compile the generated data into the fhd_schema structure
    # ------------------------------------------------------------------

    # fhd$draws: height, probability/proportion_time, draw_id (+ covariates)
    draws <- dummy_fhd
    names(draws)[names(draws) == "obs_iter"] <- "draw_id"

    # # fhd$summaries: summarise probability across draws, for every
    # # combination of height (and any covariates)
    # group_cols <- c("height", covariates)
    # summaries <- do.call(
    #   rbind,
    #   lapply(
    #     split(draws, draws[group_cols], drop = TRUE),
    #     function(x) {
    #       data.frame(
    #         x[1, group_cols, drop = FALSE],
    #         probability = mean(x$probability),
    #         mean = mean(x$probability),
    #         median = median(x$probability),
    #         lower_95 = unname(quantile(x$probability, 0.025)),
    #         upper_95 = unname(quantile(x$probability, 0.975)),
    #         row.names = NULL
    #       )
    #     }
    #   )
    # )
    # rownames(summaries) <- NULL

    fhd_entry <- list(
      fhd_id = fhd_uuid,
      fhd = list(
        draws = draws,
        covariates = covariates
        # summaries = summaries
      ),
      metadata = list(
        fhd_id = fhd_uuid,
        data_source = list(
          data_type = method,
          spatial_scale = spatial_scale,
          temporal_scale = temporal_scale,
          sf_obj = sf_obj,
          month = month,
          season = paste(seasons, collapse = ", "),
          year = year,
          country = "UK",
          region = region,
          site = site,
          sea_area = sea_area
        ),
        species = list(
          id = species_id,
          name_common = name_common,
          name_scientific = name_scientific,
          group = group
        ),
        crm_recommended = crm_recommended
      ),
      # Underpinning model components - left as placeholders here since no
      # model is actually fitted as part of this dummy-data generator.
      model = list(
        pars_summaries = NULL,
        fitted_model = NULL,
        newdata = NULL
      ),
      shiny = list(
        id = fhd_uuid,
        input_type = "rescue-library"
      )
    )

    saveRDS(
      fhd_entry,
      file = file.path(
        dir,
        paste0(fhd_uuid, ".rds")
      )
    )
  }

  invisible(NULL)
}

generate_dummy_data(n_datasets = 10, dir = "data-dummy")
