### Author: AHz
### Date: 2024-12-20
### Written in: R version 4.2.2
### Purpose: year in code


###############################################################################
# 1. LOAD PACKAGES  ######################################
###############################################################################

pacman::p_load(tidyverse, here, lubridate)


###############################################################################
# 2. SET FILE PATH  ######################################
###############################################################################


my_path <- "../"

R_files <- data.frame(file_path = 
                        list.files(path= my_path, pattern="\\.R$", 
                                   all.files=FALSE, full.names=TRUE, 
                                   recursive = TRUE))



ignore_files <- c("archive", 
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
         mod_time = `.y..i..`)
  


###############################################################################
# 4. FILTER TO 2024 ######################################
###############################################################################

scripts24 <- all_my_code %>% 
  filter(mod_time %within% interval(start = "2024-01-01", end = "2024-12-31")) %>% 
  filter(!str_detect(file_path, "app\\.R|ui\\.R|server\\.R"))

scripts24_w_shiny <- all_my_code %>% 
  filter(mod_time %within% interval(start = "2024-01-01", end = "2024-12-31"))


###############################################################################
# 5. wRapped stats ######################################
###############################################################################

#how many apps?

scripts24_w_shiny %>% 
  filter(str_detect(file_path, "app.R|ui.R"))




###############################################################################
# 3. PULL OUT TEXT DATA     ######################################
###############################################################################


functions_in_code <- scripts24 %>% 
  mutate(functions = str_extract_all(code, 
                                     pattern = "\\w{2,}(?:\\.|_*)\\w*(?=\\()")) %>% 
  unnest_longer(col = "functions")

packages_in_code <- scripts24 %>%
  mutate(packages = str_extract_all(code,
                                    pattern = "(?<=library\\(|p_load\\()\\w*|\\w*(?=\\:\\:)")) %>%
  unnest_longer(col = "packages")

packages_in_code %>% 
  filter(str_detect(packages, ".+")) %>% 
  group_by(packages) %>%
  count() %>% 
  arrange(desc(n)) %>% 
  write_csv(here("packages_list.csv"))

cat(paste0("congrats! you wrote ", length(unique(scripts24$file_path)), 
           " R scripts that used ", length(unique(functions_in_code$functions)), 
           " unique functions from ", length(unique(packages_in_code$packages)),
           " different packages! you're a staR!!!"))

pipes_in_code <- scripts24 %>% 
  mutate(pipes = str_extract_all(code, 
                                     pattern = "%>%")) %>% 
  unnest_longer(col = "pipes") %>% 
  group_by(file_path) %>% 
  count() %>% 
  group_by(.) %>% 
  summarize(sum(n))

###############################################################################
# 5. CREATE FUNCTION USE FREQUENCY TABLE   ####################
###############################################################################

function_frequency <- 
  functions_in_code %>% 
  group_by(functions) %>% 
  count(name = "freq") %>% 
  arrange(desc(freq))



packages <- c("dplyr", "base", "stringr", "tidyr", "readr", "purrr",  
              "stats", "utils", "ggplot2", "tmap", "forcats")
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
                "#8BE0A4","#B497E7","#D3B484","#D3D3D3")

my_wrapped_palette <- c("#8C2b59", "#8dd3ee", "#9EB9F3", "#d36d99")

plot_functions <- function_packages[1:15, ]

ggplot(plot_functions, aes(x = reorder(functions, -freq), y = freq, 
                           fill = reorder(package, -freq))) + 
  geom_col() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  scale_fill_manual(values = my_palette, "package") + 
  #scale_fill_viridis_d() + 
  theme_bw() + 
  theme(plot.background = element_blank(),
        panel.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 18), 
        axis.title = element_text(size = 25),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.position = "bottom",
        legend.box.background = element_rect(fill='transparent', linewidth = 0),
        legend.background = element_rect(fill='transparent')
) + 
  labs(x = "function", y = "# of times used")

ggsave(paste0("2024 wRapped/My_top_15_most_used_functions_", Sys.Date(),".png"), width = 15, height = 10)


## top packages
function_packages %>% 
  group_by(package) %>% 
  summarize(freq = sum(freq)) %>% 
  arrange(desc(freq))

### WORD CLOUD 
plot_functions_wc <- function_packages %>% 
  rownames_to_column(var = "rank") %>% 
  mutate(rank = as.numeric(rank),
         color = case_when(
           package == "dplyr::" ~ "#8C2b59",
           package == "ggplot2::" ~ "#499ebf",
           package == "base::" ~ "#d36d99",
           package == "readr::" ~ "#DCB0F2",
           package == "stringr::" ~ "#bf8fa1",
           package == "purrr::" ~ "#9EB9F3", 
           package == "tidyr::" ~ "#FE88B1",
           package == "pacman::" ~ "#ffdeff",
           package == "broom::" ~ "#8dd3ee",
           package == "stats::" ~ "#B497E7",
           TRUE ~ "#B3B3B3"
         )
         # color = case_when(
         #   rank > 300 ~ "#fc4e2a", 
         #   rank <= 300 & rank > 200 ~ "#fd8d3c",
         #   rank <= 200 & rank > 100 ~ "#feb24c",
         #   rank <= 100 & rank > 50 ~ "#fed976",
         #   rank <= 50 & rank > 10 ~ "#FFB5A2",
         #   rank <= 10 & rank > 1 ~ "#e88370", 
         #   rank == 1 ~ "#ce597d")
  ) %>% 
  dplyr::relocate(rank, .after = color) %>% 
  filter(freq > 1)

functions_wordcloud <- wordcloud2::wordcloud2(plot_functions_wc, gridSize = 3, size = 1.25, ellipticity = 2,
                                              color = plot_functions_wc$color)

functions_wordcloud


# wordcloud2::letterCloud(plot_functions_wc, word = "R", 
#                        size = 1.3, color = plot_functions_wc$color)

htmlwidgets::saveWidget(functions_wordcloud,"2023 wRapped/my_function_wordcloud.html",selfcontained = F, 
                        knitrOptions = )
webshot::webshot("2023 wRapped/my_function_wordcloud.html","2023 wRapped/my_function_wordcloud.png", 
                 vwidth = 1080, vheight = 1920, delay = 10)

