# generate_dummy_data.R
# This file will generate a set of dummy FHDs according to our data schema.
generate_dummy_data <- function(n_datasets = 10, dir = "data-dummy") {
  for (i in 1:n_datasets) {

    # We will generate a random FHD entry following the structure of the schema defined in fhd_schema_tentative.R.
    # First, we'll randomly sample a species:
    species <- sample(
      c("Puffin", "Guillemot", "Black Guillemot", "Great Northern Diver", "Shag", "Razorbill", "Kittiwake"),
      1
    )
    species_id <- paste0("species_", gsub(" ", "_", tolower(species)))

    # And generate a unique ID for this FHD:
    fhd_uuid <- paste0("fhd_", uuid::UUIDgenerate())

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

    # Choose a month
    month <- sample(1:12, 1)
    # And a year between 2000 and 2024
    year <- sample(2000:2024, 1)

    # Choose a year between 2000 and 

    # Choose seaons - this might be breeding, nonbreeding, or both:
    seasons <- sample(c("breeding", "nonbreeding"), 2, replace = TRUE) |> unique()

    # Next, we'll randomly generate a point coordinate around the UK to use:
    lat <- runif(1, 49.5, 61.5)
    lon <- runif(1, -8.5, 2.5)

    # Choose a set of covariates to include in the FHD
    covariates <- sample(
      c("wind_speed", "wind_direction", "temperature", "humidity", "precipitation"),
      sample(1:5, 1)
    )

    # Now, we'll randomly generate a number of iterations for the FHD, somewhere between 100 and 10000:
    n_iter <- sample(100:10000, 1)

  }
}