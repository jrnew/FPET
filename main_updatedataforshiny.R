#======================================================================
# main_updatedataforshiny.R
# Details steps to update FPET with new UNPD data and model run results.
# Jin Rou New, 2015
#======================================================================
# Overview:
# Process updated data/Number-of-women-married-in union_15-49.csv file.
# Output new shiny/data.output_UNPD2015.rda file using global.R > subsetData().
# Copy new UNPD run folder to output folder, checking that it has data.global.rda etc.
# Copy result CSV files to tables folder.
# Edit UNPD run name in global.R > getRunnameUNPD() > run.name & name.
# Output output/{run.name.UNPD}/dataCPmodel_input.csv using FPET.
# Update about.R.
#======================================================================
# Process updated data/Number-of-women-married-in union_15-49.csv file.
ProcessMWRAForFPET <- function( # Process updated MWRA file for Shiny FPET app.
  MWRA.new.csv, ##<< MWRA CSV file path for old data.
  MWRA.old.csv ##<< MWRA CSV file path for old data.
) {
  library(dplyr)
  mwra.new <- read.csv(MWRA.new.csv, stringsAsFactors = FALSE)
  mwra.old <- read.csv(MWRA.old.csv, stringsAsFactors = FALSE)
  if (!is.logical(all.equal(names(mwra.new), names(mwra.old)))) {
    cat("Mismatch in column names!\n")
    cat(paste0("Column names in MWRA.new.csv: ", paste(names(mwra.new), collapse = ", "), "\n"))
    cat(paste0("Column names in MWRA.old.csv: ", paste(names(mwra.old), collapse = ", "), "\n"))
  }
  # Fix column names of mwra.new
  names(mwra.new) <- gsub("MW_1549_", "X", names(mwra.new))
  names(mwra.new)[names(mwra.new) == "LocID"] <- "ISO.code"
  names(mwra.new)[names(mwra.new) == "Location"] <- "Country"  
  if (nrow(mwra.new) != nrow(mwra.old)) {
    cat(paste0("MWRA.new.csv has ", nrow(mwra.new), " countries while MWRA.old.csv has ",
               nrow(mwra.old), " countries!\n"))
    if (length(setdiff(mwra.new$Country, mwra.old$Country)) != 0)
      cat(paste0("Countries found in MWRA.new.csv but not in MWRA.old.csv: ", 
                 paste(setdiff(mwra.new$Country, mwra.old$Country), collapse = ", "), "\n"))
    if (length(setdiff(mwra.old$Country, mwra.new$Country)) != 0)
      cat(paste0("Countries found in MWRA.old.csv but not in MWRA.new.csv: ", 
                 paste(setdiff(mwra.old$Country, mwra.new$Country), collapse = ", "), "\n"))
  }
  # Get Country.letter.code for mwra.new
  if (!("Country.letter.code" %in% names(mwra.new))) {
    Country.letter.code <- left_join(mwra.new, mwra.old[, names(mwra.old) %in% 
                                                          c("Country.letter.code", "ISO.code")])$Country.letter.code
    if (any(is.na(Country.letter.code)))
      cat(paste0("Country.letter.code needs to be manually inputed for: ", 
                 paste(mwra.new$Country[is.na(Country.letter.code)], collapse = ", "), "\n"))
    mwra.new <- data.frame(Country.letter.code, mwra.new)
  }
  # Check MWRA differences between mwra.new and mwra.old for countries in both files
  countries <- intersect(mwra.new$Country, mwra.old$Country)
  years <- names(mwra.new.check)[grepl("X", names(mwra.new.check))]
  mwra.old <- mwra.old[order(mwra.old$Country), ][mwra.old$Country %in% countries, ]
  mwra.new.check <- mwra.new[order(mwra.new$Country), ][mwra.new$Country %in% countries, ]
  for (year in years) {
    countries.with.differences <- mwra.new.check$Country[mwra.new.check[, year] != mwra.old[, year]]
    if (length(countries.with.differences) > 0)
      cat(paste0("Countries with differences in MWRA between old and new files in year ",
                 gsub("X", "", year), ": ",
                 paste(countries.with.differences, collapse = ", "), "\n"))
  }
  names(mwra.new) <- gsub("\\.", " ", names(mwra.new))
  write.csv(mwra.new, file = MWRA.new.csv, row.names = FALSE)
  cat(paste0("Processed MWRA.new.csv file written out to ", MWRA.new.csv))
}

ProcessMWRAForFPET(MWRA.new.csv = "data/Number-of-women-married-in union_15-49.csv", 
                   MWRA.old.csv = "data/Number-of-women-married-in union_15-49_UNPD2014.csv")
#======================================================================
# Output new shiny/data.output_UNPD2015.rda file using global.R > subsetData().
source("global.R")
data <- read.csv("data/dataCPmodel.csv", stringsAsFactors = FALSE)
data.output <- subsetData(data)
save(data.output, file = "shiny/data.output_UNPD.rda")
#======================================================================
# Output output/{run.name.UNPD}/dataCPmodel_input.csv using FPET.
# Add periods in place of space for column names.
run.name <- "Run20150408"
data <- read.csv("data/dataCPmodel.csv", stringsAsFactors = FALSE)
dataCPmodel <- data
names(data)
write.csv(dataCPmodel, file = file.path("output", run.name, "dataCPmodel_input.csv"),
          row.names = FALSE)
