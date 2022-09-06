#!/usr/bin/env Rscript

#Jason Homsy, M.D., Ph.D. and Marko Boskovski, M.D.
#Seidman Lab, Dept. of Genetics, Harvard Medical School
#03/10/2016
#Copywrite 2016, All rights reserved.

# Heavily modified and adapted by Michael Wagner, Cincinnati Children's Hospital
# September, 2022

#Revisions:
#4/14/2016: Changed all column headers in tables to upper case.
#""""""""": Adjusted class of PATID columns to be consistent.
#09/02/2022: Many modifiations, fixes and debug/logging information added

#define function for displaying a help message with the --help command line argument
get_help <- function(){
  cat("
    USAGE:
      filter-STS.R  \
      --data STS.datafile.txt \
      --cases list.of.cases.txt \
      
    OPTIONS:
      --zip-output-tables
      --help (prints this help message and exits)
    
    WHERE:
      *STS.datafile.txt is a flat text file of the STS data
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
      At least 5 files:
      1. STS_tables.rda: R data object (list) of all tables, filtered for PCGC.BLINDED.ID
      2. STS_WIDE_TABLE.rda: R data object (dataframe) of a single wide-format table, transposed to
          one line per subject per operation.
      3. STS_tables_concat.txt: A flat text files of all output tables ('|' delimited)
          concatenated, and identified with '***name' in a manner similar to the input datafile.
      4. STS_WIDE_TABLE.txt: same as (2) but output as a tab-delimited text file.
      5. STS_tables.zip: If using the '--zip-output-tables' option, this is a zipped archive of files where 
          each file is a single output table, as a tab-delimited text file, where the name of the file 
          corresponds to the name of the table. If not using this option, then each file will appear in the
          output directory (more messy).

    R PACKAGES REQUIRED:
      1. dplyr, minimum version 0.4.2
      2. gtools, minimum version 3.5.0
      ")
  stop(call. = FALSE)
}

# put code segments into functions
# push into GitHub
# add format validation, what if columns are missing

#set defaults
Columns_Remove <- toupper(c("MEDRECN", 
                            "PATFNAME", "PATID", "PATLNAME", "PATMNAME",
                            "PATPOSTALCODE", "PATREGION", "BIRTHCIT", "BIRTHSTA", "HOSPNAME",
                            "HOSPNPI", "HOSPID", "HOSPZIP", "HOSPSTAT", "SURGEON", 
                            "SURGEONID", "SURGNPI", "TIN",
                            "ASSTSURGEON", "ASSTSURGNPI", "ASSTSURGEONID",
                            "HICNUMBER", "PATMINIT", "PATCOUNTRY", 
                            "MATNAMEKNOWN", "MATSSNKNOWN", "MATLNAME", "MATFNAME", "MATMINIT", 
                            "MATMNAME", "MATSSN", "PARTICID", "VENDORID",
                            "CNSLTATTND", "CNSLTATTNDID", "ATTENDSURG", "SURGEON", "SURGEONID", "SURGNPI", "ASSTSURGEON",
                            "ASSTSURGEONID", "ASSTSURGNPI", "RESIDENT", "RESIDENTID", "HOSPZIP", "HOSPNPI", "REFCARD",
                            "REFPHYS", "HANDOFFANESTH", "HANDOFFSURG", "HANDOFFPHYSSTAFF", "HANDOFFNURSING", "PRIMANESNAME",
                            "PRIMANESNPI", "SECANES", "SECANESNAME", "CRNA", "CRNANAME", "NONCVPHYS", "FELRES"))
ZIP_OUTPUT_TABLES=TRUE

#get command-line arguments:
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

data.file <- "~/OneDrive/Grants/Active/B2B/STS/50010con.dat.txt"
case.file <- "~/OneDrive/Grants/Active/B2B/STS/cases.txt"

#check installed packages and minimum versions
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
suppressMessages(use(dplyr, '0.4.2')) ## update, also check how to auto-install
suppressMessages(use(gtools, '3.5.0'))
#suppressMessages(use(rowr, '1.1.2')) - doesn't exist any more.

#Read in data
message("Reading in Data...")
if (!file.exists(data.file))
  stop("Data file '", data.file,
       "' not found.")
if (!file.exists(case.file))
  stop("Case file '", case.file,
       "' not found.")

con <- file(data.file, "r")
STS <- readLines(con)
closeAllConnections()

# read in case list, check column headers
Case.Master <- read.delim(case.file, stringsAsFactors=F, colClasses = "character")
names(Case.Master) <- toupper(names(Case.Master)) #standardize to avoid trouble
check_cols <- c("MEDRECN", "PCGC.BLINDED.ID")
if (!(sum(check_cols %in% names(Case.Master)) == 2 & ncol(Case.Master) == 2)) {
  stop("list.of.cases file has either incorrect header or incorrect column number.")
}
message("Read ", nrow(Case.Master), " IDs from case file ", case.file)

#Remove blank lines
message("Processing and Filtering tables...")
STS <- STS[sapply(STS, nchar) > 0]

## check for existence of PATID and OPERATIONID in every frame....

# report on number of rows and columns in every frame

#split tables
start <- grep("^[*][*][*]", STS)
mark <- vector('integer', length(STS))
mark[start] <- 1
mark <- cumsum(mark)

df <- lapply(split(STS, mark), function(.data){
    if (length(.data) == 1) {return(NULL)}
    .input <- read.table(textConnection(.data), skip=1, header=TRUE, 
                         sep="|", quote="\"", stringsAsFactors=FALSE)
    attr(.input, 'name') <- gsub("[*]", "", .data[1])  # save the name 
    #message(c("Read "), attr(.input, 'name'))
    .input
    #output name of table that was just read as debugging information?
  })
names(df) <- sapply(df, attr, 'name')

message(c("Successfully read the following tables from ", data.file, ":\n"))

#message(cat(names(df),sep="\n"))

df <- lapply(df, function(.table) {
  message(c("Table ", attr(.table, "name"), " with ", dim(.table)[1], " rows and ", dim(.table)[2], " columns."))
})

#Remove nulls (tables with no header and no rows)
nulls <- sapply(df, is.null)
if (length(nulls))
{
  #say something about the tables that are about to be purged!
}
df <- df[!nulls]

#Add PCGC identifiers.
#First, adjust class of PATID across all tables so that they are consistent.
PATID_CLASS_CHARACTER <- any(sapply(df, function(.table) is.character(.table$PATID)))
if (PATID_CLASS_CHARACTER) {
  df <- lapply(df, function(.table) {
    if ("PATID" %in% names(.table)) {
      .table$PATID <- as.character(.table$PATID)
    }
    names(.table) <- toupper(names(.table))
    message(names(.table),sep="")
    return(.table)
  })
}

#names(df$Demographics) <- toupper(names(df$Demographics))
#names(df$Operations) <- toupper(names(df$Operations))
#unclear if we need to check other tables for sensitive data.....

#Add PATID to Case.Master
Case.Master <- left_join(Case.Master,
                         select(df$Demographics, PATID, MEDRECN),
                         by="MEDRECN")
#Add OPERATIONID to Case.Master
Case.Master <- left_join(Case.Master,
                         select(df$Operations, PATID, OPERATIONID),
                         by="PATID")

#Change all column headers in df to upper case. - DEPRECATED
#df <- lapply(df, function(.table) {
#  names(.table) <- toupper(names(.table))
#  return(.table)
#  })

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
df <- lapply(df, function(.table){
  if (invalid(.table)) return(.table)
  .table <- .table %>% filter(PCGC.BLINDED.ID %in% Case.Master$PCGC.BLINDED.ID)
  col_idx <- grep("PCGC.BLINDED.ID", names(.table))
  .table <- .table[, c(col_idx, (1:ncol(.table))[-col_idx])]
  #.table <- suppressMessages(.table %>% select(-one_of(Columns_Remove))) # how to avoid warnings??
  .table <- .table[,!(names(df$Operations) %in% Columns_Remove)]
   .table
})

#df$Demographics <- df$Demographics[,!(names(df$Demographics) %in% Columns_Remove)]
#df$Operations <- df$Operations[,!(names(df$Operations) %in% Columns_Remove)]


#DO WE NEED THIS????
#df is the final list of tables.
#to make a wide table, need to split the tables into 2 groups.
#GROUP_1 is grouped by PCGC.BLINDED.ID
GROUP_1 <- c("Non-Cardiac Abnormality", "NCAA", "Syndromes", "ChromAbnormalities")
#GROUP_2 is grouped by PCGC.BLINDED.ID and OPERATIONID
GROUP_2 <- c("PreopFactors", "Diagnosis", "Procedures", "Complications", "PreopMeds", "IntraopPharm", "ICUPharm", "AAdvEvents")

#need to "transpose" the tables in these groups so that there is one line per patient per operation
#the number of columns for each variable must be the maximum number of variables per patient, so this is not a real transpose

#define function for transposing the smallest possible unit
TRANSPOSE_TABLE <- function(.table, .max, .table_name) {
  #The column DESCR_TXT is in most tables, need to uniqueify
  if ("DESCR_TXT" %in% names(.table))
    names(.table)[names(.table) == "DESCR_TXT"] <- paste("DESCR_TXT", .table_name, sep=".")
  #same is true of "PRIMARY_CD"
  if ("PRIMARY_CD" %in% names(.table))
    names(.table)[names(.table) == "PRIMARY_CD"] <- paste("PRIMARY_CD", .table_name, sep=".")
  COLUMNS <- names(.table)
  i <- 1
  for (.column in COLUMNS) {
    NEXT_BLOCK <- lapply(.table[, .column], function(x) x)
    NEXT_BLOCK <- append(NEXT_BLOCK, rep(NA, .max - length(NEXT_BLOCK)))
    NEXT_BLOCK <- as.data.frame(do.call(cbind, NEXT_BLOCK), stringsAsFactors=FALSE)
    names(NEXT_BLOCK) <- if (.max > 1) {paste(.column, 1:.max, sep=".")} else {.column}
    if (i == 1) {.output <- NEXT_BLOCK; i <- i + 1} else {.output <- cbind(.output, NEXT_BLOCK)}
  }
  return(.output)
}

#Transpose the tables for GROUP_1
TRANSPOSED_TABLES_GROUP_1 <- vector(mode="list")
for (.group in GROUP_1) {
  if (invalid(df[[.group]])) next
  FULL_TABLE <- df[[.group]]
  SET_MAX <- FULL_TABLE %>% 
    group_by(PCGC.BLINDED.ID) %>% 
    summarise(COUNTS=n()) %>% 
    select(COUNTS) %>% 
    unlist %>% 
    max
  SUBJECTS <- unique(FULL_TABLE$PCGC.BLINDED.ID)
  TABLE_BUILD <- NULL
  for (.subject in SUBJECTS) {
    SUBSET_TABLE <- FULL_TABLE %>% filter(PCGC.BLINDED.ID %in% .subject)
    SUBSET_TABLE <- SUBSET_TABLE %>% select(-PCGC.BLINDED.ID)
    SUBSET_TABLE_TRANSPOSED <- as.data.frame(TRANSPOSE_TABLE(SUBSET_TABLE, SET_MAX, .group), 
                                             stringsAsFactors=FALSE)
    SUBSET_TABLE_TRANSPOSED$PCGC.BLINDED.ID <- .subject
    TABLE_BUILD <- rbind(TABLE_BUILD, SUBSET_TABLE_TRANSPOSED)
  }
  TRANSPOSED_TABLES_GROUP_1 <- append(TRANSPOSED_TABLES_GROUP_1, list(TABLE_BUILD))
  names(TRANSPOSED_TABLES_GROUP_1)[max(1, length(names(TRANSPOSED_TABLES_GROUP_1)))] <- .group
}

#Transpose the tables for GROUP_2
TRANSPOSED_TABLES_GROUP_2 <- vector(mode="list")
for (.group in GROUP_2) {
  if (invalid(df[[.group]])) next
  FULL_TABLE <- df[[.group]]
  SET_MAX <- FULL_TABLE %>% 
    group_by(PCGC.BLINDED.ID, OPERATIONID) %>% 
    summarise(COUNTS=n()) %>%
    as.data.frame %>%
    select(COUNTS) %>% 
    unlist %>% 
    max
  SUBJECTS <- unique(FULL_TABLE$PCGC.BLINDED.ID)
  OPERATIONS <- unique(FULL_TABLE$OPERATIONID)
  TABLE_BUILD <- NULL
  for (.subject in SUBJECTS) {
    for (.operation in OPERATIONS) {
      SUBSET_TABLE <- FULL_TABLE %>% filter(PCGC.BLINDED.ID %in% .subject,
                                            OPERATIONID %in% .operation)
      SUBSET_TABLE <- SUBSET_TABLE %>% select(-PCGC.BLINDED.ID, -OPERATIONID)
      SUBSET_TABLE_TRANSPOSED <- as.data.frame(TRANSPOSE_TABLE(SUBSET_TABLE, SET_MAX, .group), 
                                               stringsAsFactors=FALSE)
      SUBSET_TABLE_TRANSPOSED$PCGC.BLINDED.ID <- .subject
      SUBSET_TABLE_TRANSPOSED$OPERATIONID <- .operation
      TABLE_BUILD <- rbind(TABLE_BUILD, SUBSET_TABLE_TRANSPOSED)
    }
  }
  TRANSPOSED_TABLES_GROUP_2 <- append(TRANSPOSED_TABLES_GROUP_2, list(TABLE_BUILD))
  names(TRANSPOSED_TABLES_GROUP_2)[max(1, length(names(TRANSPOSED_TABLES_GROUP_2)))] <- .group
}

#Find which tables did not get transposed so the columns can simply be added as NAs
NAMES_TRANSPOSED <- c(names(TRANSPOSED_TABLES_GROUP_1), names(TRANSPOSED_TABLES_GROUP_2))
NAMES_NOT_TRANSPOSED <- c(GROUP_1, GROUP_2)
NAMES_NOT_TRANSPOSED <- NAMES_NOT_TRANSPOSED[!NAMES_NOT_TRANSPOSED %in% NAMES_TRANSPOSED]
#which has columns to add?
NOT_TRANSPOSED <- sapply(NAMES_NOT_TRANSPOSED, function(.name){nrow(df[[.name]])})
NOT_TRANSPOSED <- NOT_TRANSPOSED[!sapply(NOT_TRANSPOSED, is.null)]
NAMES_NOT_TRANSPOSED <- names(NOT_TRANSPOSED)

#Start joining tables to make a wide table
message("Joining tables...")
#Join Demographics with Group_1
WIDE_TABLE <- df$Demographics
for (.table_join in TRANSPOSED_TABLES_GROUP_1) {
  WIDE_TABLE <- left_join(WIDE_TABLE, 
                          .table_join,
                          by="PCGC.BLINDED.ID")
}
#add operations
WIDE_TABLE <- left_join(WIDE_TABLE,
                        df$Operations,
                        by="PCGC.BLINDED.ID") #will expand lines of WIDE_TABLE for each proband by # of operations
#make OPERATIONID the second column
col_idx <- grep("OPERATIONID", names(WIDE_TABLE))
WIDE_TABLE <- WIDE_TABLE[, c(1, col_idx, (2:ncol(WIDE_TABLE))[-col_idx])]
#Join in Group_2
for (.table_join in TRANSPOSED_TABLES_GROUP_2) {
  WIDE_TABLE <- left_join(WIDE_TABLE, 
                          .table_join,
                          by=c("PCGC.BLINDED.ID", "OPERATIONID"))
}
#add empty columns for the tables with headers but no data, just for record keeping
for (.empty_table_name in NAMES_NOT_TRANSPOSED) {
  .df_empty <- df[[.empty_table_name]]
  #remove any potential confounding columns from the empty tables
  remove_columns <- c("OPERATIONID", "PATID")
  names(.df_empty)[names(.df_empty) == "DESCR_TXT"] <- paste("DESCR_TXT", .empty_table_name, sep=".")
  .df_empty <- .df_empty %>% select(-one_of(remove_columns))
  WIDE_TABLE <- rowr::cbind.fill(WIDE_TABLE, .df_empty)
}

#Write output files
message("Writing output...")
#Write R data objects
STS_tables <- df
save(STS_tables, file="STS_tables.rda")

STS_WIDE_TABLE <- WIDE_TABLE
save(STS_WIDE_TABLE, file="STS_WIDE_TABLE.rda")

#Write flat text files
#All tables concatenated, "|" delimited, in a similar manner as the input
sink("STS_tables_concat.txt") #to overwrite any existing file
sink()
for (.table_name in names(df)) {
  .table <- df[[.table_name]]
  sink(file="STS_tables_concat.txt", append=TRUE)
  cat(paste("***", .table_name, sep=""), "\n")
  sink()
  suppressWarnings(write.table(.table, 
                               "STS_tables_concat.txt", 
                               sep="|", quote=FALSE, row.names=FALSE, 
                               col.names=TRUE, append=TRUE))
}

#Write wide table at text file, tab delimited
write.table(WIDE_TABLE, "STS_WIDE_TABLE.txt", 
            sep="\t", quote=FALSE, row.names=FALSE, 
            col.names=TRUE)

#Write one table per file, tab delimited
OUTPUT_FILE_vec <- NULL
for (.table_name in names(df)) {

  
  .table <- df[[.table_name]]
  if (invalid(.table)) next
    OUTPUT_FILE <- paste(.table_name, "txt", sep=".")
    
    message(c("Writing ",nrow(.table)," records to ",OUTPUT_FILE))
    OUTPUT_FILE_vec <- append(OUTPUT_FILE_vec, OUTPUT_FILE)
  suppressWarnings(write.table(.table, 
                               OUTPUT_FILE, 
                               sep="\t", quote=FALSE, row.names=FALSE, 
                               col.names=TRUE))
}

# sys.info will give information about Windows vs. Mac/Linux
if (ZIP_OUTPUT_TABLES) { # assumes UNIX system
  ZIP_COMMAND <- paste("zip -rm STS_tables.zip", paste(OUTPUT_FILE_vec, collapse=" "))
  system(ZIP_COMMAND, ignore.stdout=TRUE) 
}
