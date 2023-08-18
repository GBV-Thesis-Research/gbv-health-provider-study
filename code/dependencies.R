packages <- c("redcapAPI", "REDCapR", "lintr", "styler", "tidyverse", "data.table", "dplyr", "psych", "gtsummary", "stringdist",
              "readxl")

new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)

# Load packages
invisible(lapply(packages, library, character.only = TRUE))
