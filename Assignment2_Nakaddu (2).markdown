---
title: "Assignment 2: Mining Data from Database Management Systems"
author: "Nakaddu Charity (S2B38/027, B30496)"
date: "2025-09-28"
output: pdf_document
---

## Objective
This assignment tests practical skills in mining data from a Database Management System using R. The tasks involve creating a directory, downloading 10 datasets from the World Bank Data Repository, verifying their presence, checking for missing data, generating descriptive statistics, and documenting in RMarkdown.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
suppressPackageStartupMessages({
  library(fs)
  library(DBI)
  library(duckdb)
  library(readr)
  library(dplyr)
  library(summarytools)
  library(WDI)
})
```

## Task 1: Create Directory "Assignment2"
Creating a directory named "Assignment2" to store datasets.

```{r}
if (!dir_exists("Assignment2")) dir_create("Assignment2")
```

**Explanation**: The `dir_create()` function ensures the "Assignment2" directory exists, meeting the naming requirement.

## Task 2: Add 10 Datasets
Downloading 10 digital-related datasets for Uganda from the World Bank Data Repository (https://data360.worldbank.org).

```{r}
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
  r_d_exp        = "GB.XPD.RSDV.GD.ZS"  # R&D expenditure (% of GDP)
)

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
  write_csv(df, path)
  cat("Saved:", path, "with", nrow(df), "rows\n")
}
```

**Explanation**: The `WDI` package downloads 10 digital-related indicators for Uganda. CSVs are saved in "Assignment2". The `tryCatch` handles download errors.

## Task 3: Verify Datasets
Listing files in the "Assignment2" directory to confirm dataset presence.

```{r}
list.files("Assignment2", pattern = "\\.csv$")
```

**Explanation**: The `list.files()` function verifies that all 10 datasets are stored as CSVs in "Assignment2".

## Task 4: Mine for Missing Data
Importing datasets into a DuckDB database and checking for missing values.

```{r}
con <- dbConnect(duckdb::duckdb(), dbdir = "Assignment2/assignment2.duckdb")
csv_files <- list.files("Assignment2", pattern = "\\.csv$", full.names = TRUE)

for (file in csv_files) {
  table_name <- tools::file_path_sans_ext(basename(file))
  tryCatch({
    query <- sprintf("CREATE TABLE %s AS SELECT * FROM read_csv_auto('%s')", table_name, file)
    dbExecute(con, query)
    cat("Imported:", table_name, "\n")
  }, error = function(e) {
    cat("Failed to import:", table_name, "\n")
  })
}

tables <- dbListTables(con)
missing_summary <- data.frame(Dataset = character(), Missing_Values = integer())
for (table in tables) {
  df <- dbReadTable(con, table)
  total_missing <- sum(is.na(df))
  missing_summary <- rbind(missing_summary, data.frame(Dataset = table, Missing_Values = total_missing))
}
knitr::kable(missing_summary, caption = "Missing Values in Datasets")
```

**Explanation**: Datasets are imported into a DuckDB database. The `sum(is.na())` function calculates missing values, presented in a table.

## Task 5: Descriptive Statistics
Generating descriptive statistics for one variable per dataset.

```{r}
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
  r_d_exp        = "GB.XPD.RSDV.GD.ZS"
)

stats_summary <- data.frame(Dataset = character(), N = integer(), Mean = numeric(), Median = numeric(), SD = numeric(), Min = numeric(), Max = numeric())
for (table in names(indicator_columns)) {
  if (table %in% dbListTables(con)) {
    col <- indicator_columns[[table]]
    df <- dbReadTable(con, table)
    if (col %in% names(df)) {
      values <- suppressWarnings(as.numeric(df[[col]]))
      values <- values[!is.na(values)]
      if (length(values) > 0) {
        stats <- descr(values, stats = c("mean", "median", "min", "max", "sd"), round.digits = 2)
        stats_summary <- rbind(stats_summary, data.frame(
          Dataset = table,
          N = length(values),
          Mean = stats$mean,
          Median = stats$median,
          SD = stats$sd,
          Min = stats$min,
          Max = stats$max
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
knitr::kable(stats_summary, caption = "Descriptive Statistics for Digital Indicators")
```

**Explanation**: The `summarytools::descr()` function computes count, mean, median, SD, min, and max for numeric indicator columns. Error handling ensures robustness.

## Task 6: Save RMarkdown
This document is saved as "Assignment2_Nakaddu.Rmd" and knitted to PDF.

**Explanation**: The RMarkdown format includes code, outputs, and detailed explanations, named correctly for submission.

## Submission Notes
- **Files**: Submit "Assignment2_Nakaddu.Rmd" and "Assignment2_Nakaddu.pdf".
- **Platforms**: Upload to Moodle and Canvas.
- **GitHub**: The script and datasets are available in my GitHub repository: [insert your GitHub link here].
- **Academic Integrity**: Data sourced from the World Bank Data Repository (https://data360.worldbank.org), with no plagiarism.