# load_packages.R

# List of required packages
packages <- c(  "dplyr",
                "tidyr",
                "readxl",
                "stringr",
                "purrr",
                "ggplot2",
                "httr2",
                "janitor"
)

# Function to install and load required packages
check_and_load_packages <- function(pkg_list) {
  for (pkg in pkg_list) {
    if (!require(pkg, character.only = TRUE)) {
      message(sprintf("Installing '%s'...", pkg))
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
}

# Run the function
check_and_load_packages(packages)

# Options for `tigris` package
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
