### Author: AHz
### Date: 2023-01-14
### Written in: R version 4.2.1
### Purpose: figure out which functions I use most often 

################################################################################
#  0. README  ###############################################################
################################################################################

# The following script reads all .R files within a user-provided set of folders 
# as plain text. Functions and packages are extracted from that text and counted
# for frequency of use!
# 
# To reproduce: update the file path (my_path) and the ignore_files vector in 
# section 2 


###############################################################################
# 1. SET UP/DEPENDENCIES  ######################################
###############################################################################

pacman::p_load(tidyverse, here, lubridate)
theme_set(theme_classic())

# set your working directory to the location of this file (if not using RProj)
# source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(source_file_loc)


###############################################################################
# 2. SET YOUR FILE PATH HERE!!! ######################################
###############################################################################

#_# SET YOUR FILE PATH HERE (this should be the upper level folder on your 
#_# computer where you want to look for any file that ends with .R)

my_path <- "../"

R_files <- data.frame(file_path = 
                        list.files(path= my_path, pattern="\\.R$", 
                                   all.files=FALSE, full.names=TRUE, 
                                   recursive = TRUE))


#_# CHANGE THIS VECTOR (include all string patterns you want to exclude -- mine 
#_# are all files that are duplicative (rip) or I didn't write, also shiny apps)

ignore_files <- c("archive", "app\\.R", "ui\\.R", "server\\.R",
                  "Data-Explorer-master", "step-by-step-shiny-master", 
                  "Jahred code review", "ID529data", "scraps", "R source/paper",
                  "ID529/lectures",
                  "epicalc_v3.R", "UCMR3/Data/PFAS point source data/R")



###############################################################################
# 3. PULL IN R SCRIPTS ######################################
###############################################################################

# should be good to run 

myR_files <- R_files %>% 
  filter(!str_detect(string = file_path, pattern = paste0(ignore_files, collapse = "|")))
  
my_code <- list()
script_mod_time <- list()

for(i in 1:length(unique(myR_files$file_path))){
  
  script_name <- myR_files$file_path[i]
  my_code[[script_name]] <- readLines(myR_files$file_path[i])
  
  script_mod_time[[script_name]] <- file.info(script_name)$mtime
  
}

all_my_code <- map(.x = my_code, .f = paste, collapse = " ") %>% 
  map2(., script_mod_time, data.frame) %>% 
  bind_rows(.id = "file_path") %>% 
  rename(code = `.x..i..`,
         mod_time = `.y..i..`) %>% 
  mutate(year_modified = year(mod_time))


###############################################################################
# 3. PULL OUT TEXT DATA     ######################################
###############################################################################


functions_in_code <- all_my_code %>% 
  mutate(functions = str_extract_all(code, 
                                     pattern = "\\w{2,}(?:\\.|_*)\\w*(?=\\()")) %>% 
  unnest_longer(col = "functions")

packages_in_code <- all_my_code %>%
  mutate(packages = str_extract_all(code,
                                    pattern = "(?<=library\\(|p_load\\()\\w*|\\w*(?=\\:\\:)")) %>%
  unnest_longer(col = "packages")

#get a list of packages loaded into the code
packages_in_code %>% 
  filter(str_detect(packages, ".+")) %>% 
  group_by(packages) %>%
  count() %>% 
  arrange(desc(n)) %>% 
  write_csv(here("packages_list.csv"))

cat(paste0("congrats! you wrote ", length(unique(all_my_code$file_path)), 
           " R scripts that used ", length(unique(functions_in_code$functions)), 
           " unique functions from ", length(unique(packages_in_code$packages)),
           " different packages! you're a staR!!!"))

cat(paste0("in 2023, you wrote ", length(unique(all_my_code$file_path[which(all_my_code$year_modified == "2023")])), 
           " R scripts that used ", length(unique(functions_in_code$functions[which(functions_in_code$year_modified == "2023")])), 
           " unique functions from ", length(unique(packages_in_code$packages[which(packages_in_code$year_modified == "2023")])),
           " different packages! wow!"))

###############################################################################
# 5. CREATE FUNCTION USE FREQUENCY TABLE   ####################
###############################################################################

function_frequency <- 
  functions_in_code %>% 
  group_by(functions) %>% 
  count(name = "freq") %>% 
  arrange(desc(freq))



packages <- c("dplyr", "base", "stringr", "tidyr", "readr", "purrr",  "stats", "utils", "ggplot2")
packages_list <- list()
packages_df <- data.frame(package = character(), functions = character())
for(i in unique(packages)){
    packages_list[[i]] <- ls(paste0("package:", i))
    packages_df <- bind_rows(packages_df, data.frame(package = paste0(i, "::"),
                                   functions = packages_list[[i]]))
  }





function_packages <- function_frequency %>% 
  left_join(packages_df %>% 
              filter(!(package == "stats::" & functions == "filter")) %>% 
              filter(!(package == "tidyr::" & functions == "contains")), 
            by = "functions") %>% 
  mutate(package = case_when(
    functions == "as_flextable" ~ "flextable::",
    functions %in%  c("multi_reclass", "density_plot", 
                      "format_mod_output", "clean_mod_terms") ~ "custom function",
    functions == "p_load" ~ "pacman::",
    functions == "tidy" ~ "broom::",
    functions == "getActiveDocumentContext" ~ "rstudioapi::",
    functions %in% c("clean_names", "tabyl", 
                     "adorn_totals", "adorn_percentages") ~ "janitor::", 
    functions %in% c("all_continuous", "tbl_regression",
                     "tbl_summary") ~ "gtsummary::",
    functions %in% c("ymd_hms", "ymd", "hour", "minute", 
                     "second", "day", "month", "year", 
                     "interval", "as.duration", "as.period",
                     "mdy_hms", "now"
    ) ~ "lubridate::",
    functions %in% c("read_xlsx") ~ "readxl::", 
    TRUE ~ package)) %>% 
  filter(!(functions == "any_of" & package == "tidyr::"))

###############################################################################
# 6. WORD CLOUD (and a bar graph for christian)   ####################
###############################################################################

my_palette <- c("#66C5CC","#F6CF71","#F89C74","#DCB0F2",
                "#87C55F","#9EB9F3","#FE88B1", "#C9DB74", 
                "#8BE0A4","#B497E7","#D3B484","#B3B3B3")
                         
plot_functions <- function_packages[1:50, ]

ggplot(plot_functions, aes(x = reorder(functions, -freq), y = freq, 
                           fill = reorder(package, -freq))) + 
  geom_col() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  scale_fill_manual(values = my_palette, "package") + 
  theme_bw() + 
  theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  labs(x = "function", y = "# of times used") + 
  ggtitle("My top 50 most used R functions")

ggsave(paste0("My_top_50_most_used_functions_", Sys.Date(),".png"), width = 12, height = 8)


### WORD CLOUD 
plot_functions_wc <- function_packages %>% 
  rownames_to_column(var = "rank") %>% 
  mutate(rank = as.numeric(rank),
         color = case_when(
           package == "dplyr::" ~ "#66C5CC",
           package == "ggplot2::" ~ "#F6CF71",
           package == "base::" ~ "#F89C74",
           package == "readr::" ~ "#DCB0F2",
           package == "stringr::" ~ "#87C55F",
           package == "purrr::" ~ "#9EB9F3", 
           package == "tidyr::" ~ "#FE88B1",
           package == "pacman::" ~ "#C9DB74",
           package == "broom::" ~ "#8BE0A4",
           package == "stats::" ~ "#B497E7",
           TRUE ~ "#B3B3B3"
         )) %>% 
  dplyr::relocate(rank, .after = color) %>% 
  filter(freq > 1)

functions_wordcloud <- wordcloud2::wordcloud2(plot_functions_wc,
                                              size = 1.3, color = plot_functions_wc$color)

htmlwidgets::saveWidget(functions_wordcloud,"my_function_wordcloud.html",selfcontained = F)
webshot::webshot("my_function_wordcloud.html","my_function_wordcloud.png", vwidth = 800, vheight = 800, delay = 10)



