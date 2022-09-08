## STS_extraction
 
### A Package to anonymize/recode STS (Society for Thoracic Surgeons) data

Michael Wagner
Division of Biomedical Informatics
Cincinnati Children's Hospital Medical Center
September, 2022
based on code by 

Jason Homsy, M.D., Ph.D. and Marko Boskovski, M.D.
Seidman Lab, Dept. of Genetics, Harvard Medical School

This R script can be used to create a subset of an STS data submission and remove unwanted columns (e.g., those with PHI or other sensitive data). The basic syntax is as follows:


> Rscript filter-STS.R  --data STS.datafile.txt --cases list.of.cases.txt [--zip-output-tables -help]

WHERE

  STS.datafile.txt: a flat '|'-separated text file of the site's STS data
      containing concatenated tables, each table separated by
      by a table name of the format:
        ***Table-name
      This corresponds to the standard file format sent to STS by sites.

  list.of.cases.txt: a flat, tab-delimited file with exactly two columns, 
      MEDRECN PCGC.BLINDED.ID

  --zip-output-tables: optional flag to have tab delimited output files 
      zipped into a file called STS_tables.zip (Mac or Linux only)



### Instructions:

Run script. Install packages if missing. Run again. Pray. Examine output (check for PHI in output and do other sanity checks.. Be happy if it works.


