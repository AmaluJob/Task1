#Load necessary libraries
library(dplyr)
library(tidyr)
 
#Define input file paths
gene_info_file <- "C:/Users/Ashis/OneDrive/Desktop/Tasks (1)/Tasks (1)/Homo_sapiens.gene_info.gz"
gmt_file <- "C:/Users/Ashis/OneDrive/Desktop/Tasks (1)/Tasks (1)/h.all.v2023.1.Hs.symbols.gmt"
 
#Read the gene_info file using read.table with gzfile
gene_info <- read.table(gzfile(gene_info_file), header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE, colClasses = "character", fill = TRUE)
 
#Extract relevant columns (GeneID, Symbol, Synonyms)
gene_info <- gene_info %>% select(V2, V3, V5) %>%
   mutate(Synonyms = strsplit(V5, "\\|")) %>%
   unnest(Synonyms)
 
#Create a mapping of Symbol to GeneID including synonyms
symbol_to_geneid <- gene_info %>%
   group_by(Synonyms) %>%
   summarize(GeneID = first(V2)) %>%
   as.list()
 
#Read the GMT file and replace gene symbols with Entrez IDs
new_gmt_lines <- character(0)

gmt_lines <- readLines(gmt_file)
for (line in gmt_lines) {
   parts <- strsplit(line, "\t")[[1]]
   pathway_name <- parts[1]
   pathway_description <- parts[2]
   genes <- parts[-c(1, 2)]
   
   #Replace symbols with Entrez IDs
   entrez_ids <- sapply(genes, function(symbol) {
     if (symbol %in% names(symbol_to_geneid)) {
       return(symbol_to_geneid[[symbol]])
     } else {
       return(symbol)
     }
   })
  
   #Create a new line with Entrez IDs
   new_line <- paste(pathway_name, pathway_description, entrez_ids, sep = "\t")
   new_gmt_lines <- c(new_gmt_lines, new_line)
}

#Write the new GMT file
output_file <- "output.gmt"
writeLines(new_gmt_lines, con = output_file)

#Print success message
cat("GMT file with Entrez IDs has been created:", output_file, "\n")