#!/usr/bin/env Rscript

# Michael Wagner
# Division of Biomedical Informatics
# Cincinnati Children's Hospital Medical Center
# September, 2022
# based on code by 

# Jason Homsy, M.D., Ph.D. and Marko Boskovski, M.D.
# Seidman Lab, Dept. of Genetics, Harvard Medical School
# 03/10/2016

# Revisions:
# 4/14/2016: Changed all column headers in tables to upper case.
# """"""""": Adjusted class of PATID columns to be consistent.
# 09/02/2022: Many modifiations, sanity checks, simplifications, fixes and debug/logging information added
# 08/20/2024: Added command line arguments, help message and put code into functions

doc <- "
      Usage:
        entrypoint.R [-h | --help] [--data <data.file>] [--cases <case.file>] [--zip-output-tables]

         
      Options:
        -h --help             Show available parameters.
        --data <data.file>    Specify input data file.
        --cases <case.file>   Specify list of cases file.
        --zip-output-tables   Zip output files if specified
      "
opt <- docopt::docopt(doc)


sink("output.log",split=TRUE)


# Access the parsed arguments
data.file <- opt[["--data"]]
case.file <- opt[["--cases"]]
ZIP_OUTPUT_TABLES <- opt[["--zip-output-tables"]]

# Define global variables
Columns_Remove <- toupper(c("MEDRECN", "PATFNAME", "PATID", "PATLNAME", "PATMNAME", "PATPOSTALCODE", "PATREGION", 
                            "BIRTHCIT", "BIRTHSTA", "HOSPNAME", "HOSPNPI", "HOSPID", "HOSPZIP", "HOSPSTAT", 
                            "SURGEON", "SURGEONID", "SURGNPI", "TIN", "ASSTSURGEON", "ASSTSURGNPI", "ASSTSURGEONID", 
                            "HICNUMBER", "PATMINIT", "PATCOUNTRY", "MATNAMEKNOWN", "MATSSNKNOWN", "MATLNAME", 
                            "MATFNAME", "MATMINIT", "MATMNAME", "MATSSN", "PARTICID", "VENDORID", "CNSLTATTND", 
                            "CNSLTATTNDID", "ATTENDSURG", "SURGEON", "SURGEONID", "SURGNPI", "ASSTSURGEON", 
                            "ASSTSURGEONID", "ASSTSURGNPI", "RESIDENT", "RESIDENTID", "HOSPZIP", "HOSPNPI", 
                            "REFCARD", "REFPHYS", "HANDOFFANESTH", "HANDOFFSURG", "HANDOFFPHYSSTAFF", 
                            "HANDOFFNURSING", "PRIMANESNAME", "PRIMANESNPI", "SECANES", "SECANESNAME", 
                            "CRNA", "CRNANAME", "NONCVPHYS", "FELRES"))

# Function to read in data files
read_data_files <- function(data.file, case.file) {
  require(dplyr)
  if (!file.exists(data.file))
    stop("Data file '", data.file, "' not found. Please check your command line argument.")
  if (!file.exists(case.file))
    stop("Case file '", case.file, "' not found. Please check your command line argument.")
  
  STS <- readLines(data.file)

  Case.Master <- read.delim(case.file, stringsAsFactors=F, colClasses = "character")
  names(Case.Master) <- toupper(names(Case.Master))
  
  check_cols <- c("MEDRECN", "PCGC.BLINDED.ID")
  if (!(sum(check_cols %in% names(Case.Master)) == 2 & ncol(Case.Master) == 2)) {
    stop("list.of.cases file has either incorrect header or incorrect column number.")
  }
  
  no_pcgc_ids <- dim(unique(select(Case.Master,PCGC.BLINDED.ID)))[1]
  no_mrns <- dim(unique(select(Case.Master,MEDRECN)))[1]
  if (no_pcgc_ids != no_mrns) warning(c("Number of unique MRNs (", no_mrns, ") different from number of unique PCGC IDs (", no_pcgc_ids, ")"))
  message(c("Found ", no_pcgc_ids, " PCGC participant(s) and ", no_mrns, " unique MRN(s) in ", case.file, ".\n"))
  
  return(list(STS = STS, Case.Master = Case.Master))
}

# Function to process and filter tables
process_and_filter_tables <- function(STS) {
  STS <- STS[sapply(STS, nchar) > 0]
  
  start <- grep("^[*][*][*]", STS)
  mark <- vector('integer', length(STS))
  mark[start] <- 1
  mark <- cumsum(mark)
  
  df <- lapply(split(STS, mark), function(.data){
    if (length(.data) == 1) {
      .input = data.frame()
      attr(.input, 'name') <- gsub("[*]", "", .data[1])
      return(.input)
    }
    .input <- read.table(textConnection(.data), skip=1, header=TRUE, sep="|", quote="\"", stringsAsFactors=FALSE)
    attr(.input, 'name') <- gsub("[*]", "", .data[1])
    names(.input) <- toupper(names(.input))
    .input
  })
  names(df) <- sapply(df, attr, 'name')
  
  return(df)
}

# Function to clean up and validate tables
validate_tables <- function(df) {
  tables <- names(df)
  cat(c("\nSuccessfully read the following tables from ", data.file, ":"))
  
  expected_tables <- c("Demographics", "Operations", "NCAbnormality", "NCAA", "Syndromes", "ChromAbnormalities", 
                       "PreopFactors", "Diagnosis", "Procedures", "Complications")


  df <- lapply(df, function(.table) {  
    cat(c("\nTable ", attr(.table, "name"), " with ", dim(.table)[1], " rows and ", dim(.table)[2], " columns."))
    if (dim(.table)[1] == 0) {
      cat(c("\nTable ", attr(.table, "name"), " has no data."))
      return(NULL)
    }
    if (!(("PATID" %in% names(.table)) || ("OPERATIONID" %in% names(.table)))) {
      stop(paste0(c("Table ", attr(.table, "name"), " has neither PATID nor OPERATIONID as a column.")))
    }
    .table
  })
  
  missing <- setdiff(expected_tables, tables)
  extra <- setdiff(tables, expected_tables)
  
  if (length(missing) > 0) {
    cat(c("\nMissing table(s): ", paste(missing, collapse=", ")))
  }
  
  if (length(extra) > 0) {
    cat(c("\nExtra table(s): ", paste(extra, collapse=", ")))
  }
  
  
  nulls <- sapply(df, is.null)
  if (length(names(df[nulls])) > 0) {
    cat(paste0("\nPurging ", paste(names(df[nulls]), collapse = ", "), " due to lack of header or rows."))
    df <- df[!nulls]
  }
  
  return(df)
}

# Function to add PCGC identifiers
add_identifiers <- function(df, Case.Master) {
  require(dplyr); require(gtools)
  df <- lapply(df, function(.table) {
    if ("PATID" %in% names(.table)) {
      .table$PATID <- as.character(.table$PATID)
    }
    if ("MEDRECN" %in% names(.table)) {
      .table$MEDRECN <- as.character(.table$MEDRECN)
    }
    return(.table)
  })
  
  Case.Master <- left_join(Case.Master, select(df$Demographics, PATID, MEDRECN), by="MEDRECN")
  
  cat(c("\nFound ", dim(unique(select(Case.Master,PATID)))[1], " PCGC participant(s) in Demographics table.\n"))
  
  #Add OPERATIONID to Case.Master
  Case.Master <- left_join(Case.Master,select(df$Operations, PATID, OPERATIONID),
                           by="PATID")
  
  
  #For any duplicated columns after running toupper, uniquefy 
  df <- lapply(df, function(.table) {
    .names <- names(.table)
    for (x in .names[duplicated(.names)]) {
      .names[.names %in% x] <- paste(.names[.names %in% x], 1:length(.names[.names %in% x]), sep=".")
    }
    names(.table) <- .names
    return(.table)
  })
  
  #Use Case.Master to add PCGC.BLINDED.ID to all tables
  df <- lapply(df, function(.table) {
    if (invalid(.table)) return(.table)
    if ("OPERATIONID" %in% names(.table)) {
      .table <- suppressMessages(left_join(.table, unique(select(Case.Master, PCGC.BLINDED.ID, PATID, OPERATIONID), by="OPERATIONID")))
    } else {
      .table <- suppressMessages(left_join(.table, unique(select(Case.Master, PCGC.BLINDED.ID, PATID), by="PATID")))
    }
    return(.table)
  })
  
  return(list(df = df, Case.Master = Case.Master))
}

# Function to filter and clean tables
filter_and_clean_tables <- function(df, Case.Master) {
  cat('\nAfter filtering rows:')
  df <- lapply(df, function(.table){
    if (invalid(.table)) return(.table)
    
    .table <- .table %>% filter(PCGC.BLINDED.ID %in% Case.Master$PCGC.BLINDED.ID)
    cat(c("\nTable ", attr(.table, "name"), " with ", dim(.table)[1], " rows and ", dim(.table)[2], " columns."))
    
    col_idx <- grep("PCGC.BLINDED.ID", names(.table))
    .table <- suppressMessages(.table[, c(col_idx, (1:ncol(.table))[-col_idx])])
    
    .table <- .table[, !(names(.table) %in% Columns_Remove)]
    .table
  })
  
  return(df)
}

# Function to write output tables
write_output_tables <- function(df,ZIP_OUTPUT_TABLES = FALSE) {
  OUTPUT_FILE_vec <- NULL
  cat("\n")
  for (.table_name in names(df)) {
    .table <- df[[.table_name]]
    if (invalid(.table)) {
      cat(c("\nSkipping ", .table_name, " - no data"))
      next
    }
    OUTPUT_FILE <- paste(.table_name, "txt", sep=".")
    cat(c("\nWriting ",nrow(.table)," records to ",OUTPUT_FILE))
    OUTPUT_FILE_vec <- append(OUTPUT_FILE_vec, OUTPUT_FILE)
    suppressWarnings(write.table(.table, OUTPUT_FILE, sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE))
  }
  
  #zip output tables (for UNIX systems)
  if (ZIP_OUTPUT_TABLES) {
    ZIP_COMMAND <- paste("zip -rm STS_tables.zip", paste(OUTPUT_FILE_vec, collapse=" "))
    system(ZIP_COMMAND, ignore.stdout=TRUE)
    cat("\nZipped up all files into STS_tables.zip!")
  }
  return(OUTPUT_FILE_vec)
}


# Main function to orchestrate the process
main <- function(data.file, case.file, ZIP_OUTPUT_TABLES = FALSE) {
  data <- read_data_files(data.file, case.file)
  df <- process_and_filter_tables(data$STS)
  df <- validate_tables(df)
  deid_output <- add_identifiers(df, data$Case.Master)
  df <- deid_output$df
  Case.Master <- deid_output$Case.Master
  df <- filter_and_clean_tables(df, Case.Master)
  
  OUTPUT_FILE_vec <- write_output_tables(df, ZIP_OUTPUT_TABLES)

}


# Call the main function with your specific arguments
# Replace 'data.file' and 'case.file' with your actual file paths
main(data.file, case.file, ZIP_OUTPUT_TABLES = ZIP_OUTPUT_TABLES)


sink()