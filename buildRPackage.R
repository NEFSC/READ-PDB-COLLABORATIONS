# R package build script: MUST be run in cloned copy of repository.

library(tidyverse)

##### ID R scripts with function documentation or alternatively create buildFiles vector with the names of ONLY the files to be added/updated in the R package here #####
# Find all .R scripts
files <- list.files(path = here::here(), pattern = ".R", recursive = TRUE, full.names = TRUE)

# Find all scripts with some Roxygen documentation (assumes that documented scripts have ONLY functions, if not true, could be a problem)
documented <- rep(NA, length(files))
for(ifile in 1:length(files)){
  documented[ifile] <- TRUE %in% grepl("#'", readLines(files[ifile])) # All files must end with an empty line for this to work
}

buildFiles <- files[documented == TRUE] # !!!!! May remove any specific problematic files or files that do not need to be updated here !!!!!!
buildFiles <- buildFiles[-which(buildFiles == here::here("buildRPackage.R"))] # Don't try to turn this script into an R function
buildFiles <- buildFiles[-which(grepl(pattern = ".Rmd", buildFiles))] # Don't process .Rmd files
buildFiles <- buildFiles[-which(buildFiles %in% list.files(here::here("R"), pattern = ".R", full.names = TRUE))] # Don't reprocess R files, these will be overwritten with any changes. Will get a warning if all files already copied to R folder

# Only build files that are already tracked by git (avoids adding half-baked functions created by whomever runs this script)
# tracked_files_short <- read.table(text = base::system(command = "git ls-tree --full-tree --name-only -r main", intern = TRUE)) 
tracked_files_short <- base::system(command = "git ls-tree --full-tree --name-only -r main", intern = TRUE)
tracked_files <- paste0(here::here(), "/", tracked_files_short)

buildFiles <- buildFiles[which(buildFiles %in% tracked_files == TRUE)] # only build git tracked files


##### Copy documented functions into R folder #####
file.copy(buildFiles, here::here("R"), overwrite = TRUE) # If documentation removed, empty file with no content will be retained in R folder

##### Add loaded functions to DESCRIPTION file #####
packageFiles <- list.files(path = here::here("R"))

packages <- NULL
for(ifile in 1:length(packageFiles)){
  readFile <- readLines(here::here("R", packageFiles[ifile]))
  packages <- c(packages, readFile[grepl("library", readFile)==TRUE])
}

dependencies <- packages %>% str_split_i(., "[(]", i = 2) %>% str_split_i(., "[)]", i = 1) %>% unique()

# Update description Imports
description <- readLines(here::here("DESCRIPTION"))
description[which(grepl("Imports:", description, fixed = TRUE) == TRUE)] <- paste0("Imports: ", paste(dependencies, collapse = ", "))

writeLines(description, here::here("DESCRIPTION"))

##### Comment out library lines in R function scripts
libraryLines <- NULL
for(ifile in 1:length(packageFiles)){
  readFile <- readLines(here::here("R", packageFiles[ifile]))
  libraryLines[[ifile]] <- which(grepl("library",readFile)==TRUE) 
  
  # Comment out library lines
  readFile[libraryLines[[ifile]]] <- paste0("# ", readFile[libraryLines[[ifile]]])
  writeLines(readFile, here::here("R", packageFiles[ifile]))
}

##### Add export statement if not already in documentation !!! doesn't add export statements if multiple functions in a file and one already has export statement 
export <- NULL
for(ifile in 1:length(packageFiles)){
  readFile <- readLines(here::here("R", packageFiles[ifile]))
  export[[ifile]] <- TRUE %in% grepl("export",readFile)
  
  # If no Roxygen export for the function then add
  if(export[[ifile]] == FALSE){
    if(TRUE %in% (diff(which(grepl("#'", readFile) == TRUE)) > 3)){ # If there are more than 4 lines between documentation lines then assume multiple functions in a file and export all
      docLines <- which(grepl("#'", readFile) == TRUE) # ID lines in file with roxygen documentation
      docBreaks <- which(diff(which(grepl("#'", readFile) == TRUE)) > 3)  # ID where breaks in documentation exist (this is last line of documentation before next break)
      
      revisedFile <- readFile[1:docLines[docBreaks[1]]]
      for(ibreak in 1:length(docBreaks)){
        if(ibreak == length(docBreaks)){ # If final break add additional export after last function and append lines through end of file
          revisedFile <- c(revisedFile, "#' @export", readFile[(docLines[docBreaks[ibreak]]+1):max(docLines)], 
                           "#' @export", readFile[(max(docLines)+1):length(readFile)])
          
          # revisedFile <- c(revisedFile, "#' @export", readFile[(docBreaks[ibreak]+1):length(readFile)])
        } else{ # Otherwise append lines through next documentation block
          revisedFile <- c(revisedFile, "#' @export", readFile[(docLines[docBreaks[ibreak]]+1):docLines[docBreaks[ibreak+1]]])
          # revisedFile <- c(revisedFile, "#' @export", readFile[(ibreak+1):docBreaks[ibreak+1]])
        }
        
      }
      
    } else{ # Otherwise assume one function and add @export at end of documentation
      lineBreak <- max(which(grepl("#'", readFile) == TRUE))  # This is the last row of documentation
      revisedFile <- c(readFile[1:lineBreak], "#' @export", readFile[(lineBreak+1):length(readFile)])
    }
    writeLines(revisedFile, here::here("R", packageFiles[ifile]))
    
  }
}

##### Document R package #####
devtools::document()

# NOTE: Functions that have roxygen documentation but 1) are not tracked by git or 2) were excluded from the buildFiles vector will trigger an error that they are "listed as exports, but not present in namespace"

##### Confirm package loads as expected - after this the following need to be updated on GitHub: R, man, DESCRIPTION, NAMESPACE files #####
# install.packages("pdbCollaborations")
library(pdbCollaborations)
?pdbCollaborations::makeReportData
?pdbCollaborations::create.brp.text
  
# To resolve warnings/errors check the following:
  # Do all functions have a roxygen2 @title
  # Do all .R scripts have an empty line at the end of the file, if not this will cause readLines() to throw an error
  # By default this script will export all functions with at least a title documented with roxygen2
  # Documentation build doesn't like special characters like % in documentation list items and will return the following error: @return has mismatched braces or quotes.

##### Final step #####
# Must commit changes to the following files/folders and push to GitHub so package available for installation via pak or devtools::install_github()
  # DESCRIPTION
  # man
  # NAMESPACE
  # R
  # LICENSE - only update if a change to the license was made which is unlikely
