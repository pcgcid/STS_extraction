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

#define function for displaying a help message with the --help command line argument
get_help <- function(){
  cat("
    USAGE:
      Rscript filter-STS.R  \
      --data STS.datafile.txt \
      --cases list.of.cases.txt \
      
    OPTIONS:
      --zip-output-tables
      --help (prints this help message and exits)
    
    WHERE:
      *STS.datafile.txt is a flat text file of the site's STS data
      containing concatenated tables, each table separated by
      by a table name of the format:
        ***Table-name

      *list.of.cases.txt is tab delimited text file of 
          MEDRECN PCGC.BLINDED.ID
      with the above as headers for the two columns.

      If using the '--zip-output-tables' option, then the individual
      tab delimted tables will be zipped together. This option is best used on a Mac or
      UNIX-based system. I'm not sure how well this will work on a Windows-based system.

    OUTPUT:
          STS_tables.zip: If using the '--zip-output-tables' option, this is a zipped archive of files where 
          each file is a single output table, as a tab-delimited text file, where the name of the file 
          corresponds to the name of the table. If not using this option, then each file will appear in the
          output directory.

    R PACKAGES REQUIRED:
      1. dplyr, minimum version 1.0.9
      2. gtools, minimum version 3.9.0
      ")
  stop(call. = FALSE)
}

# TODO: put code segments into functions

##################
# set default column names to be removed before writing output files
# PLEASE ADD TO LIST IF NON-STANDARD PHI COLUMN NAMES WERE USED
##################
Columns_Remove <- toupper(c("MEDRECN", 
                            "PATFNAME", "PATID", "PATLNAME", "PATMNAME",
                            "PATPOSTALCODE", "PATREGION", "BIRTHCIT", "BIRTHSTA", "HOSPNAME",
                            "HOSPNPI", "HOSPID", "HOSPZIP", "HOSPSTAT", "SURGEON", 
                            "SURGEONID", "SURGNPI", "TIN",
                            "ASSTSURGEON", "ASSTSURGNPI", "ASSTSURGEONID",
                            "HICNUMBER", "PATMINIT", "PATCOUNTRY", 
                            "MATNAMEKNOWN", "MATSSNKNOWN", "MATLNAME", "MATFNAME", "MATMINIT", 
                            "MATMNAME", "MATSSN", "PARTICID", "VENDORID",
                            "CNSLTATTND", "CNSLTATTNDID", "ATTENDSURG", "SURGEON", 
                            "SURGEONID", "SURGNPI", "ASSTSURGEON",
                            "ASSTSURGEONID", "ASSTSURGNPI", "RESIDENT", "RESIDENTID", 
                            "HOSPZIP", "HOSPNPI", "REFCARD",
                            "REFPHYS", "HANDOFFANESTH", "HANDOFFSURG", "HANDOFFPHYSSTAFF", 
                            "HANDOFFNURSING", "PRIMANESNAME",
                            "PRIMANESNPI", "SECANES", "SECANESNAME", "CRNA", "CRNANAME", 
                            "NONCVPHYS", "FELRES"))
#ZIP_OUTPUT_TABLES=TRUE
#######################

############################
#get command-line arguments:
############################
args <- commandArgs(trailingOnly = TRUE)
shift=FALSE
for (i in seq_along(args)) {
  if (shift) {shift=FALSE; next}
  switch(args[i],
         "--data"={data.file <- args[i+1]; shift=TRUE},
         "--cases"={case.file <- args[i+1]; shift=TRUE},
         "--zip-output-tables"={ZIP_OUTPUT_TABLES=TRUE},
         "--help"={get_help()},
{break}
  )
}

#data.file <- "STS_dummy_data.txt"
#case.file <- "MRN_PCGC.txt"

###############################
#check installed packages and minimum versions
###############################
use <- function(package, version=0, ...) {
  package <- as.character(substitute(package))
  if (!require(package, ..., character.only=TRUE, quietly=TRUE))
    stop("Package '", package, 
         "' required, but not available. Please install.")
  library(package, ..., character.only=TRUE)
  pver <- packageVersion(package)
  if (compareVersion(as.character(pver), as.character(version)) < 0)
    stop("Version ", version, " of '", package, 
         "' required, but only ", pver, " is available")
  invisible(pver)
}

message("Checking Installed Packages...")
suppressMessages(use(dplyr, '1.0.9')) ## check how to auto-install as package dependency?
suppressMessages(use(gtools, '3.9.0'))


#################################
#Read in data
#################################
#message("Reading in Data...")

#read in STS data 
if (!file.exists(data.file))
  stop("Data file '", data.file,
       "' not found. Please check your command line argument.")
if (!file.exists(case.file))
  stop("Case file '", case.file,
       "' not found. Please check your command line argument")

con <- file(data.file, "r")
STS <- readLines(con)
closeAllConnections()

# read in case list, check column headers
Case.Master <- read.delim(case.file, stringsAsFactors=F, colClasses = "character")
names(Case.Master) <- toupper(names(Case.Master)) #standardize to avoid trouble
# basic sanity check - need those columns to be there
check_cols <- c("MEDRECN", "PCGC.BLINDED.ID")
if (!(sum(check_cols %in% names(Case.Master)) == 2 & ncol(Case.Master) == 2)) {
  stop("list.of.cases file has either incorrect header or incorrect column number.")
}
#message("Read ", nrow(Case.Master), "PCGC IDs from case file ", case.file)

no_pcgc_ids <- dim(unique(select(Case.Master,PCGC.BLINDED.ID)))[1]
no_mrns <- dim(unique(select(Case.Master,MEDRECN)))[1]
if (no_pcgc_ids != no_mrns) warning(c("Number of unique MRNs (", no_mrns, ") different from number of unique PCGC IDs (", no_pcgc_ids, ")"))

message(c("Found ", no_pcgc_ids, " PCGC participant(s) and ", no_mrns, " unique MRN(s) in ", case.file, ".\n"))

#Remove blank lines
#message("Processing and Filtering tables...")
STS <- STS[sapply(STS, nchar) > 0]

#split tables into data frames
start <- grep("^[*][*][*]", STS)
mark <- vector('integer', length(STS))
mark[start] <- 1
mark <- cumsum(mark)

df <- lapply(split(STS, mark), function(.data){
    if (length(.data) == 1) {
      .input = data.frame()
      attr(.input, 'name') <- gsub("[*]", "", .data[1])  # save the name 
      return(.input)
      }
    .input <- read.table(textConnection(.data), skip=1, header=TRUE, 
                         sep="|", quote="\"", stringsAsFactors=FALSE)
    attr(.input, 'name') <- gsub("[*]", "", .data[1])  # save the name 
    #message(c("Read "), attr(.input, 'name'))
    names(.input) <- toupper(names(.input))
    .input
  })
names(df) <- sapply(df, attr, 'name')

message(c("\nSuccessfully read the following tables from ", data.file, ":"))

#message(cat(names(df),sep="\n"))

# report on number of rows and columns in every data frame / table
df <- lapply(df, function(.table) {
  message(c("Table ", attr(.table, "name"), " with ", dim(.table)[1], " rows and ", dim(.table)[2], " columns."))
  
  if (dim(.table)[1] == 0) {
    message(c("Table ", attr(.table, "name"), " has no data."))
    return(NULL)
  }
  ## check for existence of PATID and OPERATIONID in every frame....
  if (!(("PATID" %in% names(.table)) || ("OPERATIONID" %in% names(.table) )))
  {
    stop(paste0(c("Table ", attr(.table, "name"), " has neither PATID nor OPERATIONID as a column.")))
  }
  .table
})

#Remove nulls (tables with no header and no rows)
nulls <- sapply(df, is.null)
if (length(names(df[nulls])) > 0)
{
  message(paste0("\nPurging ", paste(names(df[nulls]), collapse = ", "), " due to lack of header or rows."))
  df <- df[!nulls]
}

#Add PCGC identifiers.
#First, adjust class of PATID across all tables so that they are consistent.
#PATID_CLASS_CHARACTER <- any(sapply(df, function(.table) is.character(.table$PATID)))
#if (PATID_CLASS_CHARACTER) {
df <- lapply(df, function(.table) {
  if ("PATID" %in% names(.table)) {
    .table$PATID <- as.character(.table$PATID)
  }
  if ("MEDRECN" %in% names(.table)) {
    .table$MEDRECN <- as.character(.table$MEDRECN)
  }
  #names(.table) <- toupper(names(.table)) # obsolete
  return(.table)
})
#}

#Add PATID to Case.Master
Case.Master <- left_join(Case.Master,
                         select(df$Demographics, PATID, MEDRECN),
                         by="MEDRECN")

message(c("Found ", dim(unique(select(Case.Master,PATID)))[1], " PCGC participant(s) in Demographics table.\n"))

#Add OPERATIONID to Case.Master
Case.Master <- left_join(Case.Master,
                         select(df$Operations, PATID, OPERATIONID),
                         by="PATID")


#For any duplicated columns after running toupper, uniquefy # WHY???? USE CASE?
df <- lapply(df, function(.table) {
  .names <- names(.table)
  for (x in .names[duplicated(.names)]) {
    .names[.names %in% x] <- paste(.names[.names %in% x], 1:length(.names[.names %in% x]), sep=".")
  }
  names(.table) <- .names
  return(.table)
})

#Use Case.Master to add PCGC.BLINDED.ID to all tables
df <- lapply(df, function(.table){
    if (invalid(.table)) 
      {
        message()
        return(.table)
      }
    if ("OPERATIONID" %in% names(.table)) {
      .table <- suppressMessages(left_join(.table, unique(select(Case.Master, 
                                                          PCGC.BLINDED.ID, PATID, OPERATIONID), 
                                                          # by=c("PATID", "OPERATIONID"))))
                                                          by=c("OPERATIONID"))))
    } else {
      .table <- suppressMessages(left_join(.table, unique(select(Case.Master, 
                                                                 PCGC.BLINDED.ID, PATID), 
                                                          by="PATID")))
    }
    return(.table)
  })

#Filter tables
#Filter on desired PCGC.BLINDED.ID, move PCGC.BLINDED.ID to begining of table, remove unwanted columns
message(c("\nAfter filtering rows:"))
df <- lapply(df, function(.table){
  if (invalid(.table)) return(.table)
  
  .table <- .table %>% filter(PCGC.BLINDED.ID %in% Case.Master$PCGC.BLINDED.ID)
  message(c("Table ", attr(.table, "name"), " with ", dim(.table)[1], " rows and ", dim(.table)[2], " columns."))
  
  col_idx <- grep("PCGC.BLINDED.ID", names(.table))
  .table <- suppressMessages(.table[, c(col_idx, (1:ncol(.table))[-col_idx])])

  # remove unwanted columns with PHI
  #.table <- suppressMessages(.table %>% select(-one_of(Columns_Remove))) # how to avoid warnings??
  
  .table <- .table[, !(names(.table) %in% Columns_Remove)]
   .table
})

#Write one table per file, tab delimited
OUTPUT_FILE_vec <- NULL
message("\n")
for (.table_name in names(df)) {

  .table <- df[[.table_name]]
  if (invalid(.table)) {
    message(c("Skipping ", .table_name, " - no data"))
    next
  }
  OUTPUT_FILE <- paste(.table_name, "txt", sep=".")
    
  message(c("Writing ",nrow(.table)," records to ",OUTPUT_FILE))
  OUTPUT_FILE_vec <- append(OUTPUT_FILE_vec, OUTPUT_FILE)
  suppressWarnings(write.table(.table, 
                               OUTPUT_FILE, 
                               sep="\t", quote=FALSE, row.names=FALSE, 
                               col.names=TRUE))
}

if(!"ZIP_OUTPUT_TABLES" %in% ls()) {ZIP_OUTPUT_TABLES=FALSE}

# sys.info will give information about Windows vs. Mac/Linux - don't do this for Windows
if (ZIP_OUTPUT_TABLES) { # assumes UNIX system
  ZIP_COMMAND <- paste("zip -rm STS_tables.zip", paste(OUTPUT_FILE_vec, collapse=" "))
  system(ZIP_COMMAND, ignore.stdout=TRUE) 
  message("\nZipped up all files into STS_tables.zip!")
}
message("Done.")
