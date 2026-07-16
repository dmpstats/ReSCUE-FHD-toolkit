# ----------------------------------------------------------------------
# Tentative schema for the FHD data coming from ReSCUE
# ----------------------------------------------------------------------

library(data.tree)

if (1) {
  fhd_schema <- Node$new("FHD_entry")

  fhd_schema$AddChild("fhd", class = "<list>")

  fhd_schema$fhd$AddChild(
    "draws",
    class = "<data.frame>",
    descr = "Posterior/booststrap samples of FHD",
    values = "Columns: 'height', 'probability/proportion_time', 'draw_id'"
  )

  fhd_schema$fhd$AddChild(
    "summaries",
    class = "<data.frame>",
    descr = "Estimated FHD summary",
    values = "Columns: 'height', 'probability', 'mean', 'median', 'lower_95', 'upper_95', etc."
  )

  fhd_schema$AddChild("metadata", class = "<list>")

  fhd_schema$metadata$AddChild(
    "data_source",
    class = "<list>",
    descr = "Data source and survey information"
  )

  fhd_schema$metadata$data_source$AddChild(
    "data_type",
    class = "<character>",
    descr = "Measuring/sensor system",
    values = "'LiDAR-DAS', 'GPS', 'Altimeter'"
  )

  fhd_schema$metadata$data_source$AddChild(
    "spatial_scale",
    class = "<character>",
    descr = "Spatial scale of the data",
    values = "'site-specific', 'regional', 'national'"
  )

  fhd_schema$metadata$data_source$AddChild(
    "temporal_scale",
    class = "<character>",
    descr = "Temporal scale of the data",
    values = "'monthly', 'seasonal', 'annual'"
  )

  fhd_schema$metadata$data_source$AddChild(
    "sf_obj",
    class = "<sfc>",
    descr = "Geometry of the survey site, region, etc."
  )

  fhd_schema$metadata$data_source$AddChild(
    "month",
    class = "<integer>",
    descr = "For monthly scale",
    values = "1 – 12"
  )

  fhd_schema$metadata$data_source$AddChild(
    "season",
    class = "<character>",
    descr = "Season or life-history stage",
    values = "'breeding', 'spring', etc.",
  )

  fhd_schema$metadata$data_source$AddChild(
    "year",
    class = "<integer>"
  )

  fhd_schema$metadata$data_source$AddChild(
    "country",
    class = "<character>"
  )

  fhd_schema$metadata$data_source$AddChild(
    "region",
    class = "<character>",
    descr = "Relevant administrative or ecological region",
    values = "'UK', 'Scotland', 'England', 'North Sea', 'Baltic Sea', etc."
  )

  fhd_schema$metadata$data_source$AddChild(
    "site",
    class = "<character>",
    descr = "Specific survey site or colony"
  )

  fhd_schema$metadata$data_source$AddChild(
    "sea_area",
    class = "<character>",
    descr = "Relevant sea areas (e.g. ICES Divs)"
  )

  fhd_schema$metadata$AddChild(
    "species",
    class = "<list>",
    descr = "Species-level information"
  )

  fhd_schema$metadata$species$AddChild(
    "id",
    class = "<character>",
    descr = "Unique species ID"
  )

  fhd_schema$metadata$species$AddChild(
    "name_common",
    class = "<character>",
    descr = "Common species name"
  )

  fhd_schema$metadata$species$AddChild(
    "name_scientific",
    class = "<character>",
    descr = "Binomial scientific name"
  )

  fhd_schema$metadata$species$AddChild(
    "group",
    class = "<character>",
    descr = "Taxonomic or functional group"
  )

  fhd_schema$metadata$AddChild(
    "crm_recommended",
    class = "<logical>",
    descr = "Recommended for use in CRMS?"
  )

  fhd_schema$AddChild(
    "model (?)",
    class = "<list>",
    descr = "Underpinning model components"
  )

  fhd_schema$`model (?)`$AddChild(
    "pars_summaries (?)",
    class = "<data.frame>",
    descr = "Summary of parameters' posterior predictions",
  )

  fhd_schema$`model (?)`$AddChild(
    "fitted_model (?)",
    class = "<model_object>"
  )

  fhd_schema$`model (?)`$AddChild(
    "newdata (?)",
    class = "<data.frame>",
    descr = "Data to generate predictions"
  )

  fhd_schema$AddChild(
    "shiny",
    class = "<list>",
    descr = "Information for Shiny internal use"
  )

  fhd_schema$shiny$AddChild(
    "id",
    class = "<character>",
    descr = "unique ID for the FHD entry"
  )

  fhd_schema$shiny$AddChild(
    "input_type",
    class = "<character>",
    descr = "Data library(ies) or user-uploaded data",
    values = "one of: 'rescue-library', 'user-upload'"
  )
}


print(fhd_schema, "class", "descr", "values")
