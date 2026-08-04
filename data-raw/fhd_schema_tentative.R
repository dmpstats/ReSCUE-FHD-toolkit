# ----------------------------------------------------------------------
# Tentative schema for the FHD data coming from ReSCUE
# ----------------------------------------------------------------------

library(data.tree)

if (1) {
  fhd_schema <- Node$new("FHD_entry")

  fhd_schema$AddChild(
    "fhd_id",
    class = "<character>",
    descr = "Unique ID for the FHD entry"
  )

  fhd_schema$AddChild(
    "fhd_draws",
    class = "<data.frame>",
    descr = "Posterior/booststrap samples of FHD",
    values = "Columns: 'height', 'probability', 'draw_id', 'cov1', 'cov2', ..."
  )

  fhd_schema$AddChild("metadata", class = "<list>")

  fhd_schema$metadata$AddChild(
    "method",
    class = "<character>",
    descr = "Method used to estimate FHD",
    values = "'LiDAR-DAS', 'GPS', 'Altimeter'"
  )

  fhd_schema$metadata$AddChild(
    "spatial_scale",
    class = "<character>",
    descr = "Spatial scale of the data and model",
    values = "'site-specific', 'regional', 'national'"
  )

  fhd_schema$metadata$AddChild(
    "temporal_scale",
    class = "<character>",
    descr = "Temporal scale of the data and model",
    values = "'monthly', 'seasonal', 'annual'"
  )

  fhd_schema$metadata$AddChild(
    "covariates",
    class = "<tibble>",
    descr = "Covariates in the model"
  )

  # fhd_schema$metadata$covariates$AddChild(
  #   "label",
  #   class = "<char-column>",
  #   descr = "Covariate's name, as in `fwd_draws`"
  # )

  # fhd_schema$metadata$covariates$AddChild(
  #   "type",
  #   class = "<char-column>",
  #   descr = "type of covariate",
  #   values = "'numeric', 'factor', 'logical'"
  # )
  # fhd_schema$metadata$covariates$AddChild(
  #   "levels",
  #   class = "<list-column>",
  #   descr = "categories of the covariate (if factor)",
  #   values = "c('level_1', 'level_2', 'level_3')"
  # )

  # fhd_schema$metadata$covariates$AddChild(
  #   "blurb",
  #   class = "<char-column>",
  #   descr = "Short description"
  # )

  fhd_schema$metadata$AddChild(
    "covariates",
    class = "<list>",
    descr = "Covariates in the model"
  )

  fhd_schema$metadata$covariates$AddChild(
    "cov1",
    class = "<list>"
  )

  fhd_schema$metadata$covariates$cov1$AddChild(
    "label",
    class = "<character>",
    descr = "Name of covariate, as in `fwd_draws`"
  )

  fhd_schema$metadata$covariates$cov1$AddChild(
    "type",
    class = "<character>",
    descr = "type of covariate",
    values = "'numeric', 'factor', 'logical'"
  )

  fhd_schema$metadata$covariates$cov1$AddChild(
    "levels",
    class = "<character>",
    descr = "categories of the covariate (if factor)"
  )

  fhd_schema$metadata$covariates$cov1$AddChild(
    "blurb",
    class = "<character>",
    descr = "Short description"
  )

  fhd_schema$metadata$covariates$AddChild(
    "cov2",
    class = "<list>"
  )

  fhd_schema$metadata$covariates$cov2$AddChild(
    "label",
    class = "<character>",
    descr = "Name of covariate, as in `fwd_draws`"
  )

  fhd_schema$metadata$covariates$cov2$AddChild(
    "type",
    class = "<character>",
    descr = "type of covariate",
    values = "'factor', 'logical'"
  )

  fhd_schema$metadata$covariates$cov2$AddChild(
    "levels",
    class = "<character>",
    descr = "categories of the covariate (if factor)",
    values = "'level_1', 'level_2', 'level_3'"
  )

  fhd_schema$metadata$covariates$cov2$AddChild(
    "blurb",
    class = "<character>",
    descr = "Short description"
  )

  fhd_schema$metadata$covariates$AddChild(
    " ..."
  )

  fhd_schema$metadata$AddChild(
    "month",
    class = "<integer>",
    descr = "For monthly scale",
    values = "1 – 12"
  )

  fhd_schema$metadata$AddChild(
    "season",
    class = "<character>",
    descr = "Season or life-history stage",
    values = "'breeding', 'spring', etc.",
  )

  fhd_schema$metadata$AddChild(
    "year",
    class = "<integer>"
  )

  fhd_schema$metadata$AddChild(
    "country",
    class = "<character>"
  )

  fhd_schema$metadata$AddChild(
    "region",
    class = "<character>",
    descr = "Relevant administrative or ecological region",
    values = "'UK', 'Scotland', 'England', 'North Sea', etc."
  )

  fhd_schema$metadata$AddChild(
    "site",
    class = "<character>",
    descr = "Specific survey site or colony"
  )

  fhd_schema$metadata$AddChild(
    "sea_area",
    class = "<character>",
    descr = "Relevant sea areas (e.g. ICES Divs)"
  )

  fhd_schema$metadata$AddChild(
    "species_id",
    class = "<character>",
    descr = "Unique species ID"
  )

  fhd_schema$metadata$AddChild(
    "name_common",
    class = "<character>",
    descr = "Common species name"
  )

  fhd_schema$metadata$AddChild(
    "name_scientific",
    class = "<character>",
    descr = "Binomial scientific name"
  )

  fhd_schema$metadata$AddChild(
    "group",
    class = "<character>",
    descr = "Species taxonomic or functional group"
  )

  fhd_schema$metadata$AddChild(
    "crm_recommended",
    class = "<logical>",
    descr = "Recommended for use in CRMs?"
  )

  fhd_schema$metadata$AddChild(
    "sf_obj",
    class = "<sfc>",
    descr = "Geometry of the survey site, region, etc."
  )

  fhd_schema$AddChild(
    "shiny",
    class = "<list>",
    descr = "Information for Shiny internal use"
  )

  fhd_schema$shiny$AddChild(
    "input_type",
    class = "<character>",
    descr = "Data library(ies) or user-uploaded data",
    values = "one of: 'rescue-library', 'user-upload'"
  )
}


print(fhd_schema, "class", "descr", "values")

data.tree::ToDataFrameTree(fhd_schema, "class", "descr", "values") |>
  saveRDS("data-raw/fhd_schema_tentative.rds")


# readRDS("data-dummy/metadata/Kittiwake_Wales_V_Bay.rds")
# readRDS("data-dummy/draws/Kittiwake_Wales_V_Bay.rds")
