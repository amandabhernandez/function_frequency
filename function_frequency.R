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
# 1. LOAD PACKAGES  ######################################
###############################################################################

library(tidyverse)
library(here)

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
                  "epicalc_v3.R", "UCMR3/Data/PFAS point source data/R")


###############################################################################
# 3. PULL IN R SCRIPTS ######################################
###############################################################################

# should be good to run 

myR_files <- R_files %>% 
  filter(!str_detect(string = file_path, pattern = paste0(ignore_files, collapse = "|")))
  
my_code <- list()

for(i in 1:length(unique(myR_files$file_path))){
  
  script_name <- myR_files$file_path[i]
  my_code[[script_name]] <- readLines(myR_files$file_path[i])

}

all_my_code <- map(.x = my_code, .f = paste, collapse = " ") %>% 
  map(data.frame) %>% 
  bind_rows(.id = "file_path") %>% 
  rename(code = `.x..i..`)


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



cat(paste0("congrats! you wrote ", length(unique(all_my_code$file_path)), 
           " R scripts that used ", length(unique(functions_in_code$functions)), 
           " unique functions from ", length(unique(packages_in_code$packages)),
           " different packages! you're a staR!!!"))

###############################################################################
# 5. CREATE FUNCTION USE FREQUENCY TABLE   ####################
###############################################################################

function_frequency <- 
  functions_in_code %>% 
  group_by(functions) %>% 
  count(name = "freq") %>% 
  arrange(desc(freq))


###############################################################################
# 6. WORD CLOUD (and a bar graph for christian)   ####################
###############################################################################

plot_functions <- function_frequency[1:50, ]

ggplot(plot_functions, aes(x = reorder(functions, -freq), y = freq), fill = "#FFB5A2") + 
  geom_col() + 
  scale_y_continuous(expand = c(0,0)) + 
  theme_bw() + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  labs(x = "function", y = "# of times used") + 
  ggtitle("My top 50 most used R functions")

ggsave("My top 50 most used functions.png", width = 10, height = 8)


### WORD CLOUD 
plot_functions_wc <- function_frequency %>% 
  rownames_to_column(var = "rank") %>% 
  mutate(rank = as.numeric(rank),
         color = case_when(
           rank > 300 ~ "#fc4e2a", 
           rank <= 300 & rank > 200 ~ "#fd8d3c",
           rank <= 200 & rank > 100 ~ "#feb24c",
           rank <= 100 & rank > 50 ~ "#fed976",
           rank <= 50 & rank > 10 ~ "#FFB5A2",
           rank <= 10 & rank > 1 ~ "#e88370", 
           rank == 1 ~ "#ce597d")) %>% 
  dplyr::relocate(rank, .after = color) %>% 
  filter(freq > 1)

functions_wordcloud <- wordcloud2::wordcloud2(plot_functions_wc,
                                              size = 1.3, color = plot_functions_wc$color)

htmlwidgets::saveWidget(functions_wordcloud,"my_function_wordcloud.html",selfcontained = F)
webshot::webshot("my_function_wordcloud.html","my_function_wordcloud.png",vwidth = 800, vheight = 800, delay = 10)



###############################################################################
# 7. REGRESSION MODEL (aka christian testa work zone) ####################
###############################################################################

# regression model --------------------------------------------------------

function_frequency_w_rank <- function_frequency %>% 
  ungroup() %>% 
  mutate(rank = 1:nrow(.)) %>% 
  mutate(y = freq, x = rank)

y <- function_frequency_w_rank$y
x <- function_frequency_w_rank$x

model <- nls(y ~ 1 + b*a^x, 
    start = list(a = mean((y-1)^(1/x), na.rm=T), b = 10), control = list(maxiter=1000))

plot(y~x)
lines(fitted(model) ~ x, col = 'red')


