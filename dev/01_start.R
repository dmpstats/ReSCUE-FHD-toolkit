# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application and set some default {golem} options
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
	pkg_name = "ReSCUEApp", # The name of the golem package containing the app (typically lowercase, no underscore or periods)
	pkg_title = "ReSCUE Flight Height Distribution Toolkit App", # What the Package Does (One Line, Title Case, No Period)
	pkg_description = "ReSCUEApp is a Shiny application that provides a user-friendly interface to the ReSCUETools R package, making seabird flight height data and outputs from the ReSCUE project accessible to offshore wind developers, environmental consultants, conservation bodies, NGOs, and academics. It integrates ReSCUETools functionality with UK flight height distribution data developed by the British Trust for Ornithology (BTO), serving as a reference point for collision risk modelling and other analyses. The application includes links to relevant documents and guidance on applying the data to existing assessment tools.", # What the package does (one paragraph).
	authors = c(
		person(
			given = "Bruno", # Your First Name
			family = "Caneco", # Your Last Name
			email = "bruno@dmpstats.co.uk", # Your email
			role = c("aut", "cre") # Your role (here author/creator)
		),
		person(
			given = "Callum", # Your First Name
			family = "Clarke", # Your Last Name
			email = "callum@dmpstats.co.uk", # Your email
			role = c("aut") # Your role (here author/creator)
		),
		person(
			given = "Grant", # Your First Name
			family = "Humphries", # Your Last Name
			email = "grwhumphries@blackbawks.net", # Your email
			role = c("aut") # Your role (here author/creator)
		),
		person(
			given = "Carl", # Your First Name
			family = "Donovan", # Your Last Name
			email = "carl@dmpstats.co.uk", # Your email
			role = c("ctb") # Your role (here author/creator)
		),
		person(
			given = "Tim", # Your First Name
			family = "Kasoar", # Your Last Name
			email = "TKAS@niras.com", # Your email
			role = c("ctb") # Your role (here author/creator)
		)
	),
	repo_url = NULL, # The URL of the GitHub repo (optional),
	pkg_version = "0.0.0.9000", # The version of the panckage containing the app
	set_options = TRUE # Set the global golem options
)
## Install the required dev dependencies ----
golem::install_dev_deps()

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license("Golem User") # You can set another license here
golem::use_readme_rmd(open = FALSE)
devtools::build_readme()
# Note that `contact` is required since usethis version 2.1.5
# If your {usethis} version is older, you can remove that param
usethis::use_code_of_conduct(contact = "Golem User")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon() # path = "path/to/ico". Can be an online file.
# golem::remove_favicon() # Uncomment to remove the default favicon

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

## Use git ----
usethis::use_git()
## Sets the remote associated with 'name' to 'url'
usethis::use_git_remote(
	name = "origin",
	url = "https://github.com/<OWNER>/<REPO>.git"
)

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
