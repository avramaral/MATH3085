setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library("knitr")

output_files <- paste("_sol.R")
for (i in 5:9) {
  input_file  <- paste("lab0", i, "/lab0", i, ".qmd", sep = "")
  output_file <- paste("lab0", i, "/lab0", i, "_sol.R", sep = "")
  purl(input_file, output = output_file, documentation = 0)
  
  # Remove header
  clean_code <- readLines(output_file)
  clean_code <- clean_code[-(1:1)]
  writeLines(clean_code, output_file)
}

