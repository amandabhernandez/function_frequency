### Author: AHz
### Date: 2023-01-14
### Written in: R version 4.2.1
### Purpose: figure out which functions I use most often 

###############################################################################
# 0. LOAD PACKAGES AND WRITE FUNCTIONS   ######################################
###############################################################################

library(tidyverse)

str_ignore_file <- function(dat, file_str) {
  dat %>% 
    filter(!str_detect(string = file_path, pattern = file_str))
}

###############################################################################
# 1. READ IN R SCRIPTS    ######################################
###############################################################################

R_files <- data.frame(file_path = list.files(path="../", pattern="\\.R$", all.files=FALSE,
           full.names=TRUE, recursive = TRUE))

# ignore files that are duplicative or I didn't write (also shiny apps)
myR_files <- R_files %>% 
  str_ignore_file("archive") %>% 
  str_ignore_file("app\\.R") %>% 
  str_ignore_file("ui\\.R") %>% 
  str_ignore_file("server\\.R") %>% 
  str_ignore_file("Data-Explorer-master") %>% 
  str_ignore_file("step-by-step-shiny-master") %>% 
  str_ignore_file("Jahred code review") %>% 
  str_ignore_file("ID529data") %>% 
  str_ignore_file("scraps")
  

my_code <- list()

for(i in 1:length(unique(myR_files$file_path))){
  
  script_name <- myR_files$file_path[i]
  my_code[[script_name]] <- readLines(myR_files$file_path[i])

}

all_my_code <- map(.x = my_code, .f = paste, collapse = "") %>% 
  map(data.frame) %>% 
  bind_rows(.id = "file_path") %>% 
  rename(code = `.x..i..`)

#how many r scripts? 
length(unique(all_my_code$file_path))

###############################################################################
# 2. PULL OUT TEXT DATA     ######################################
###############################################################################


functions_in_code <- all_my_code %>% 
  mutate(functions = str_extract_all(code, pattern = "\\w{2,}(?=\\()")) %>% 
  unnest_longer(col = "functions")


function_frequency <- functions_in_code %>% 
  group_by(functions) %>% 
  count() %>% 
  arrange(desc(n))

