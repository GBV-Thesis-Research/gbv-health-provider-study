packages <- c("redcapAPI", "REDCapR", "lintr", "styler")

new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)

# Load packages
invisible(lapply(packages, library, character.only = TRUE))
