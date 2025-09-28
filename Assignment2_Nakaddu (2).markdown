
title: "Assignment 2: Mining Data from Database Management Systems"author: "Nakaddu Charity (S2B38/027, B30496)"date: "2025-09-28"output:  pdf_document:    toc: true    number_sections: truedev: cairo_pdf

Assignment 2: Mining Data from Database Management Systems
Author: Nakaddu Charity (S2B38/027, B30496)
Objective
This RMarkdown documents the mining of data from an in-directory DBMS (DuckDB), using 10 digital development datasets for Uganda sourced from the World Bank Data Repository (https://data360.worldbank.org).
# Global RMarkdown options: show code, suppress messages/warnings
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, collapse = TRUE)
# Install and load required packages
# Author: Nakaddu Charity (S2B38/027, B30496)
if (!require(fs)) install.packages("fs")
if (!require(DBI)) install.packages("DBI")
if (!require(duckdb)) install.packages("duckdb")
if (!require(readr)) install.packages("readr")
if (!require(dplyr)) install.packages("dplyr")
if (!require(summarytools)) install.packages("summarytools")
if (!require(WDI)) install.packages("WDI")
if (!require(knitr)) install.packages("knitr")
suppressPackageStartupMessages({
  library(fs)           # File system operations
  library(DBI)          # Database interface
  library(duckdb)       # In-directory DBMS
  library(readr)        # Read/write CSVs
  library(dplyr)        # Data manipulation
  library(summarytools) # Descriptive statistics
  library(WDI)          # World Bank data interface
  library(knitr)        # Table formatting
})

Execution
1. Create Directory "Assignment2" [10 MARKS]
# Create the local directory named "Assignment2"
# Author: Nakaddu Charity (S2B38/027, B30496)
if (!fs::dir_exists("Assignment2")) fs::dir_create("Assignment2")
# Explanation: fs::dir_create() ensures the required directory exists, meeting Task 1

2. Add 10 Datasets to Directory [10 MARKS]
# Define 10 digital-related World Bank indicators
# Author: Nakaddu Charity (S2B38/027, B30496)
indicators <- list(
  internet_users = "IT.NET.USER.ZS",    # Internet users (% of population)
  mobile_subs    = "IT.CEL.SETS.P2",    # Mobile subscriptions (per 100 people)
  fixed_broad    = "IT.NET.BBND.P2",    # Fixed broadband subscriptions (per 100)
  secure_servers = "IT.NET.SECR.P6",    # Secure servers (per million people)
  mobile_broad   = "IT.CEL.BBND.P2",    # Mobile broadband subscriptions
  fixed_phone    = "IT.TEL.FIX.P2",     # Fixed telephone subscriptions
  ict_exports    = "TX.VAL.ICTG.ZS.UN", # ICT goods exports (% of total)
  ict_imports    = "TM.VAL.ICTG.ZS.UN", # ICT goods imports (% of total)
  hi_tech_exp    = "TX.VAL.TECH.MF.ZS", # High-tech exports (% of manufactured)
  tech_rd        = "SP.POP.TECH.RD.P6"  # Technicians in R&D (per million people)
)
# Download data for Uganda (UGA, 2000-2023) and save as CSVs
for (name in names(indicators)) {
  code <- indicators[[name]]
  df <- tryCatch(
    WDI(country = "UGA", indicator = code, start = 2000, end = 2023),
    error = function(e) {
      cat("Error downloading:", name, "\n")
      data.frame()
    }
  )
  path <- file.path("Assignment2", paste0(name, ".csv"))
  readr::write_csv(df, path)
  cat("Saved:", path, "with", nrow(df), "rows\n")
}
# Explanation: WDI downloads 10 digital indicators for Uganda; CSVs saved in Assignment2; tryCatch handles errors

3. Write Code Illustrating All 10 Datasets are Present [10 MARKS]
# List files to verify all 10 datasets are saved
# Author: Nakaddu Charity (S2B38/027, B30496)
data_files <- list.files("Assignment2", pattern = "\\.csv$")
cat("Total files found:", length(data_files), "\n")
knitr::kable(data.frame(Datasets = data_files), caption = "Files in 'Assignment2' Directory")
# Explanation: list.files() confirms 10 datasets as CSVs, meeting Task 3

4. Mine the Datasets for Missing Data [10 MARKS]
# Connect to DuckDB in Assignment2 directory
# Author: Nakaddu Charity (S2B38/027, B30496)
con <- dbConnect(duckdb::duckdb(), dbdir = "Assignment2/assignment2.duckdb")
csv_files <- list.files("Assignment2", pattern = "\\.csv$", full.names = TRUE)
# Import CSVs as tables into DuckDB
for (file in csv_files) {
  table_name <- tools::file_path_sans_ext(basename(file))
  tryCatch({
    query <- sprintf("CREATE OR REPLACE TABLE %s AS SELECT * FROM read_csv_auto('%s')", table_name, file)
    dbExecute(con, query)
    cat("Imported:", table_name, "\n")
  }, error = function(e) {
    cat("Failed to import:", table_name, "\n")
  })
}
# Check missing values across all tables
tables <- dbListTables(con)
missing_summary <- data.frame(Dataset = character(), Total_Missing_Values = integer())
for (table in tables) {
  df <- dbReadTable(con, table)
  total_missing <- sum(is.na(df))
  missing_summary <- rbind(missing_summary, data.frame(Dataset = table, Total_Missing_Values = total_missing))
}
knitr::kable(missing_summary, caption = "Summary of Total Missing Values (NA) per Dataset")
# Explanation: DuckDB imports datasets; sum(is.na()) calculates missing values for Task 4

5. Generate Descriptive Statistics of One Variable [30 MARKS]
# List indicators for numeric columns
# Author: Nakaddu Charity (S2B38/027, B30496)
indicator_columns <- list(
  internet_users = "IT.NET.USER.ZS",
  mobile_subs    = "IT.CEL.SETS.P2",
  fixed_broad    = "IT.NET.BBND.P2",
  secure_servers = "IT.NET.SECR.P6",
  mobile_broad   = "IT.CEL.BBND.P2",
  fixed_phone    = "IT.TEL.FIX.P2",
  ict_exports    = "TX.VAL.ICTG.ZS.UN",
  ict_imports    = "TM.VAL.ICTG.ZS.UN",
  hi_tech_exp    = "TX.VAL.TECH.MF.ZS",
  tech_rd        = "SP.POP.TECH.RD.P6"
)
stats_summary <- data.frame(Dataset = character(), N = integer(), Mean = numeric(), Median = numeric(), SD = numeric(), Min = numeric(), Max = numeric())
for (table in names(indicator_columns)) {
  if (table %in% dbListTables(con)) {
    col <- indicator_columns[[table]]
    df <- dbReadTable(con, table)
    if (col %in% names(df)) {
      values <- suppressWarnings(as.numeric(df[[col]]))
      values <- values[!is.na(values)]
      if (length(values) > 1) {
        stats <- descr(values, stats = c("mean", "median", "min", "max", "sd"), round.digits = 3, silent = TRUE)
        stats_summary <- rbind(stats_summary, data.frame(
          Dataset = table,
          N = stats$n[1],
          Mean = stats$mean[1],
          Median = stats$median[1],
          SD = stats$sd[1],
          Min = stats$min[1],
          Max = stats$max[1]
        ))
      } else {
        cat("\nNo valid numeric data in", col, "for", table, "\n")
      }
    } else {
      cat("\nColumn", col, "not found in", table, "\n")
    }
  } else {
    cat("\nTable", table, "not found in database\n")
  }
}
knitr::kable(stats_summary, caption = "Descriptive Statistics for Digital Indicator Variables (2000-2023)")
# Explanation: descr() computes mean, median, SD, min, max for numeric variables; results tabulated for Task 5

6. Save RMarkdown [30 MARKS]
This document is saved as "Assignment2_Nakaddu.Rmd" and knitted to PDF.
Author: Nakaddu Charity (S2B38/027, B30496)
Explanation: The RMarkdown format ensures reproducible analysis, presenting code and results for grading. Data sourced from the World Bank Data Repository (https://data360.worldbank.org), with no plagiarism.
# Close database connection
# Author: Nakaddu Charity (S2B38/027, B30496)
dbDisconnect(con)
# Explanation: Closes DuckDB connection to release file lock
