library(tidyverse)
library(lubridate)
library(pdftools)
library(janitor)
library(gridExtra)

# # pull latest SEP into table from pdf
latest_sep_date <-  format(today(), "%Y%m%d")
text <-  paste0("https://www.federalreserve.gov/monetarypolicy/files/fomcprojtabl", latest_sep_date, ".pdf")

download.file(text, "latest_sep_pdf.pdf", mode = "wb")

raw_pdf <- pdf_text("latest_sep_pdf.pdf")

 raw_sep <- 
   raw_pdf[2] %>%
   str_split("\n", simplify= TRUE) 
 
sep_table <- 
   raw_sep[11:24] %>% 
     # str_squish() %>% 
     str_replace("Core PCE inflation4", "Core PCE inflation")


#make each row of characters that I need into a numbers
numbers_list = list()
for (i in 1:length(sep_table)) {
   
   numbers <- sep_table[[i]] %>% str_extract("[:digit:]+.*")
   numbers_df <- data.frame(numbers) 

   numbers_list <- bind_rows(numbers_list, numbers_df)
}

#cleaning data from pdf
clean_sep <-
   numbers_list %>% 
   #creating fake column names to make it easier to extract   
   separate(numbers, c("A","B","C","D","E","F","G","H","I","J"), sep="\\s") %>% 
   filter(!is.na(A)) %>% 
   select(A, B, C, D, E) %>% 
   row_to_names(row_number = 1) %>% 
   rename("Longer run" = Longer) %>% 
   mutate()

final_sep_data <-
   bind_cols(clean_sep, c("Real GDP Growth", paste0("    ", format(today()-90, "%B"), " projections"), 
                          "Unemployment rate",  paste0("    ", format(today()-90, "%B"), " projections"),
                          "PCE inflation",  paste0("    ", format(today()-90, "%B"), " projections"), 
                          #naming this something different so that can clean up the longer run numbers later
                          "Core PCE inflation",  paste0("    ", format(today()-90, "%B"), " projections1"), 
                          "Fed funds rate",  paste0("    ", format(today()-90, "%B"), " projections"))) %>% 
   rename("Summary of Economic Projections" = `...6`) %>% 
   select(`Summary of Economic Projections`, everything()) %>% 
      #clearning up longer run numbers for core PCE inflation because those aren't available
   mutate(`Longer run` = ifelse(`Summary of Economic Projections` == "Core PCE inflation", "-", `Longer run`),
          `Longer run` = ifelse( `Summary of Economic Projections`==  paste0("    ", format(today()-90, "%B"), " projections1"), "-", `Longer run`)) %>% 
   #format all data labels correctly
    mutate(`Summary of Economic Projections` = 
              ifelse(`Summary of Economic Projections` == paste0("    ", format(today()-90, "%B"), " projections1"), 
                     paste0("    ", format(today()-90, "%B"), " projections"), `Summary of Economic Projections`))
   
write_csv(final_sep_data, "latest_sep_data.csv")
 

 
 
 