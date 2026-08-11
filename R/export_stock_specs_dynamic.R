#' @title export_stock_specs_dynamic
#' @description Hardened Dynamic StockEff Specification Extractor - scrapes stockEff tables for current stock specifications
#'
#' @param dbconn An active database connection object. If NULL, prompts the user for credentials.
#' @param module Character. One of "CF", "OB", or "SV".
#' @param itis Character. The species ITIS code.
#' @param stock Character. Stock abbreviation.
#' @param sa_year Numeric/Integer. Stock assessment year.
#' @param assessment Character. Assessment abbreviation.
#' @param schema Character. Target schema ("STOCKEFF" or "STOCKEFF_PRE_PROD"). Default is "STOCKEFF".
#' @param sex Character. Required for "SV" module parameters. Default is "NONE".
#' @param out_dir Character. Directory path to save the Excel file. Defaults to working directory.
#' @param max_rows Numeric. Safety threshold to prevent RAM crashes (default 100,000).
#' @param blacklist Character vector. Tables ending in '_S' to explicitly exclude from extraction.
#' @param whitelist Character vector. Tables ending in '_S' from other modules/global structures to explicitly include.
#' @param STGonly Logical. Set to TRUE to see results from staging form inputs for your stock (default FALSE)
#'
#' @return An xlsx file with current stock specifications.
#'
#' @importFrom dplyr rename_with
#' @importFrom stringr str_pad
#' @importFrom cli cli_alert_info cli_alert_danger cli_alert_warning cli_alert_success
#' @importFrom glue glue
#' @importFrom ROracle Oracle dbConnect dbGetQuery dbDisconnect
#' @importFrom writexl write_xlsx
#' @importFrom rstudioapi askForPassword
#' @importFrom DBI dbGetQuery
#'
#' @examples
#' \dontrun{
#' # For Commercial landings (Using existing connection)
#' export_stock_specs_dynamic(dbconn
#'                            ,module = "CF"
#'                            ,itis = "172873"
#'                            ,stock = "UNIT"
#'                            ,sa_year = 2024
#'                            ,assessment = "MT2"
#'                            ,out_dir = paste0(getwd(),"/ConfirmSpecs/")
#' )
#' 
#' # Dynamic login mode (If connection is NULL)
#' export_stock_specs_dynamic(dbconn = NULL
#'                            ,module = "CF"
#'                            ,itis = "172873"
#'                            ,stock = "UNIT"
#'                            ,sa_year = 2024
#'                            ,assessment = "MT2"
#'                            ,out_dir = paste0(getwd(),"/ConfirmSpecs/")
#' )
#' }
#' @export
export_stock_specs_dynamic <- function(dbconn = NULL, module, itis, stock, sa_year, assessment, 
                                       schema = "STOCKEFF", sex = "NONE", out_dir = "./",
                                       max_rows = 100000,
                                       blacklist = c("I_CF_MONTHS_S", "I_OB_MONTHS_S", "I_CF_PROJECTED_SPECS_S",
                                                     "I_OB_PROJECTED_SPECS_S", "I_SV_PROJECTED_SPECS_S"),
                                       whitelist = c("I_ASSESSMENT_NOTES_S"),
                                       STGonly = FALSE) {
  
  if (!module %in% c("CF", "OB", "SV")) {
    stop("Invalid module code. Choose 'CF', 'OB', or 'SV'.")
  }
  
  # --- Dynamic Connection Management ---
  auto_disconnect <- FALSE
  if (is.null(dbconn)) {
    cli::cli_alert_info("Connection not provided. Authenticating with Oracle...")
    dbconn <- tryCatch({
      ROracle::dbConnect(
        ROracle::Oracle(), 
        user = rstudioapi::askForPassword("Oracle Username"), 
        password = rstudioapi::askForPassword("Oracle Password"), 
        dbname = "NEFSC_pw_oraprod"
      )
    }, error = function(e) {
      cli::cli_alert_danger("Database connection failed: {e$message}")
      return(NULL)
    })
    if (is.null(dbconn)) stop("An active database connection is required to proceed.")
    auto_disconnect <- TRUE
  }
  
  # Validate connection health
  is_functional <- tryCatch({
    # Send a lightweight query to verify Oracle availability
    ROracle::dbGetQuery(dbconn, "SELECT 1 FROM DUAL")
    TRUE
  }, error = function(e) FALSE)
  
  if (!is_functional) {
    stop("Oracle connection is non-functional or has dropped. Please verify.")
  }
  
  # --- Apply Staging-Specific Refinements ---
  if (STGonly) {
    schema <- "STOCKEFF_PRE_PROD"
    cli::cli_alert_info("Isolating staging tables ({.code STG_I_{module}}) in schema {.val {schema}}...")
  } else {
    cli::cli_alert_info("Dynamically discovering and safely extracting {.val {module}} specifications from {.val {schema}}...")
  }
  
  # ============================================================================
  # Module-Independent Common Query
  # ============================================================================
  stock_assessment <- ROracle::dbGetQuery(
    dbconn, 
    glue::glue("SELECT * FROM {schema}.i_stock_assessment_j 
                WHERE species_itis = {itis} 
                  AND stock_abbrev = '{stock}' 
                  AND sa_year = {sa_year} 
                  AND assessment_abbrev = '{assessment}' 
                  AND module_code = '{module}'")
  )
  
  # Safeguard list initialization: write placeholder instead of empty df to protect spreadsheet write
  sheet_list <- list()
  if (nrow(stock_assessment) > 0) {
    sheet_list[["Assessment_Info"]] <- stock_assessment
  } else {
    sheet_list[["Assessment_Info"]] <- data.frame(Status = "No active metadata found in i_stock_assessment_j")
  }
  
  # ============================================================================
  # Dynamic Table and Column Discovery
  # ============================================================================
  if (STGonly) {
    table_filter_sql <- glue::glue("table_name LIKE 'STG\\_I\\_{module}\\_%' ESCAPE '\\'")
  } else {
    whitelist_sql <- if (length(whitelist) > 0) {
      paste0("OR table_name IN (", paste0("'", whitelist, "'", collapse = ", "), ")")
    } else {
      ""
    }
    table_filter_sql <- glue::glue("
      (table_name LIKE 'I_{module}\\_%' ESCAPE '\\' AND table_name LIKE '%\\_S' ESCAPE '\\')
      {whitelist_sql}
    ")
  }
  
  meta_query <- glue::glue("
    SELECT table_name, column_name 
    FROM all_tab_columns 
    WHERE owner = UPPER('{schema}') 
      AND ({table_filter_sql})
    ORDER BY table_name, column_id"
  )
  
  metadata <- ROracle::dbGetQuery(dbconn, meta_query)
  
  if (nrow(metadata) == 0) {
    cli::cli_alert_danger("No matching tables found for schema {.val {schema}} and module {.val {module}} (STGonly = {STGonly}).")
    if (auto_disconnect) ROracle::dbDisconnect(dbconn)
    return(invisible(NULL))
  }
  
  # Split columns into a list grouped by table name
  tables_to_query <- split(metadata$COLUMN_NAME, metadata$TABLE_NAME)
  
  # ============================================================================
  # Guarded Dynamic Query Loop
  # ============================================================================
  for (table_name in names(tables_to_query)) {
    
    # Check against the customizable blacklist (skipped if STGonly to prevent accidental drops)
    if (!STGonly && table_name %in% blacklist) {
      cli::cli_alert_warning("Skipping blacklisted specification table: {.val {table_name}}")
      next
    }
    
    cols <- tables_to_query[[table_name]]
    where_clauses <- c()
    
    if ("SPECIES_ITIS" %in% cols) {
      where_clauses <- c(where_clauses, glue::glue("species_itis = {itis}"))
    }
    if ("STOCK_ABBREV" %in% cols) {
      where_clauses <- c(where_clauses, glue::glue("stock_abbrev = '{stock}'"))
    }
    if ("SA_YEAR" %in% cols) {
      where_clauses <- c(where_clauses, glue::glue("sa_year = {sa_year}"))
    }
    if ("ASSESSMENT_ABBREV" %in% cols) {
      where_clauses <- c(where_clauses, glue::glue("assessment_abbrev = '{assessment}'"))
    }
    
    # Enforce sex matching strictly for SV module tables, bypass for global shared notes
    if ("SEX_TYPE" %in% cols && module == "SV" && table_name != "I_ASSESSMENT_NOTES_S") {
      where_clauses <- c(where_clauses, glue::glue("sex_type = '{sex}'"))
    }
    
    # Custom conditional logic for notes cross-module visibility
    if (table_name == "I_ASSESSMENT_NOTES_S" && "MODULE_CODE" %in% cols) {
      where_clauses <- c(where_clauses, glue::glue("(module_code = '{module}' OR module_code = 'GN')"))
    }
    
    where_str <- ""
    if (length(where_clauses) > 0) {
      where_str <- paste0("WHERE ", paste(where_clauses, collapse = " AND "))
    }
    
    # --- Guardrail: Quick Row Count Check Before Fetching Data ---
    count_query <- glue::glue("SELECT COUNT(1) AS row_count FROM {schema}.{table_name} {where_str}")
    total_rows <- tryCatch({
      res_count <- ROracle::dbGetQuery(dbconn, count_query) |> dplyr::rename_with(tolower)
      res_count$row_count[1]
    }, error = function(e) Inf)
    
    if (is.na(total_rows) || is.null(total_rows)) total_rows <- 0
    
    # If the rows exceed safety constraints, halt extraction or log a defensive cutoff
    if (total_rows > max_rows) {
      cli::cli_alert_danger("Table {.val {table_name}} bypassed safe memory limits ({total_rows} rows). Skipping sheet extraction.")
      next
    }
    
    # Assemble the final data query
    target_query <- glue::glue("SELECT * FROM {schema}.{table_name} {where_str}")
    
    tryCatch({
      df_data <- ROracle::dbGetQuery(dbconn, target_query)
      
      # Strip prefixes cleanly so sheet names fit comfortably under Excel's 31-character ceiling
      clean_sheet_pattern <- glue::glue("^STG_I_{module}_STOCK_|^STG_I_{module}_|^I_{module}_STOCK_|^I_{module}_")
      clean_sheet_name <- gsub(clean_sheet_pattern, "", table_name)
      short_sheet_name <- substr(clean_sheet_name, 1, 31)
      
      if (table_name == "I_CF_STOCK_CAA_BLOCKS_S" && "NESPP4" %in% cols && nrow(df_data) > 0) {
        df_data$NESPP4 <- stringr::str_pad(df_data$NESPP4, width = 4, pad = "0")
      }
      
      # SAFE SHEET WRITING: If table exists but has 0 records, write a placeholder df. 
      if (nrow(df_data) > 0) {
        sheet_list[[short_sheet_name]] <- df_data
      } else {
        sheet_list[[short_sheet_name]] <- data.frame(Status = "No records configured in database for this stock criteria.")
      }
      
    }, error = function(e) {
      cli::cli_alert_warning("Failed to query dynamic table {.val {table_name}}: {e$message}")
    })
  }
  
  # ============================================================================
  # Workbook Compilation and File Generation
  # ============================================================================
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  clean_dir <- gsub("/$", "", out_dir)
  
  # Add _STG suffix to file name if running in staging isolation mode
  suffix <- if(STGonly) "_STG_Specs" else "_Specs_Dynamic"
  file_name <- glue::glue("{clean_dir}/{itis}_{stock}_{sa_year}_{assessment}_{module}{suffix}.xlsx")
  
  # writexl alignment
  writexl::write_xlsx(sheet_list, path = file_name)
  cli::cli_alert_success("Successfully written dynamic spreadsheet to: {.file {file_name}}")
  
  if (auto_disconnect) {
    cli::cli_alert_info("Closing temporary connection...")
    ROracle::dbDisconnect(dbconn)
  }
  
  return(invisible(NULL))
}
