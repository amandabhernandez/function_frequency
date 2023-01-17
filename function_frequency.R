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

# SET YOUR FILE PATH HERE (this should be the upper level folder on your computer
# where you want to look for any file that ends with .R)
my_path <- "../"

R_files <- data.frame(file_path = 
                        list.files(path= my_path, pattern="\\.R$", 
                                   all.files=FALSE, full.names=TRUE, 
                                   recursive = TRUE))

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
  str_ignore_file("scraps") %>%
  str_ignore_file("R source/paper") %>% 
  str_ignore_file("epicalc_v3.R") %>% 
  str_ignore_file("UCMR3/Data/PFAS point source data/R")
  

my_code <- list()

for(i in 1:length(unique(myR_files$file_path))){
  
  script_name <- myR_files$file_path[i]
  my_code[[script_name]] <- readLines(myR_files$file_path[i])

}

all_my_code <- map(.x = my_code, .f = paste, collapse = " ") %>% 
  map(data.frame) %>% 
  bind_rows(.id = "file_path") %>% 
  rename(code = `.x..i..`)

#how many r scripts? 
length(unique(all_my_code$file_path))

###############################################################################
# 2. PULL OUT TEXT DATA     ######################################
###############################################################################


functions_in_code <- all_my_code %>% 
  mutate(functions = str_extract_all(code, 
                                     pattern = "\\w{2,}(?:\\.|_*)\\w*(?=\\()")) %>% 
  unnest_longer(col = "functions")

packages_in_code <- all_my_code %>% 
  mutate(packages = str_extract_all(code, 
                                    pattern = "(?<=library\\(|p_load\\()\\w*|\\w*(?=\\:\\:)")) %>% 
  unnest_longer(col = "packages")


function_frequency <- 
  functions_in_code %>% 
  group_by(functions) %>% 
  count(name = "freq") %>% 
  arrange(desc(freq))


write_csv(function_frequency, "function_frequency.csv")

## Get functions
packages_list <- list()
for(i in unique(packages_in_code$packages)){
  packages_list[[i]] <- ls(paste0("package:", i))
}



###############################################################################
# 3. WORD CLOUD (and a bar graph for christian)   ####################
###############################################################################

plot_functions <- function_frequency[1:50, ] %>% 
  rownames_to_column("rank") %>% 
  mutate(rank = as.numeric(rank), 
         color = case_when(
           #rank >= 400 ~ "#bd0026",
           #rank <= 400 & rank > 300 ~ "#fc4e2a",
           rank > 300 ~ "#fc4e2a", 
           rank <= 300 & rank > 200 ~ "#fd8d3c",
           rank <= 200 & rank > 100 ~ "#feb24c",
           rank <= 50 & rank > 25 ~ "#fed976",
           rank <= 25 & rank > 10 ~ "#FFB5A2",
           rank <= 10 & rank > 1 ~ "#e88370", 
           rank == 1 ~ "#ce597d"))

ggplot(plot_functions, aes(x = reorder(functions, -freq), y = freq, fill = color)) + 
  geom_col() + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = c("#ce597d", "#e88370", "#FFB5A2", "#fed976")) + 
  theme_bw() + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  labs(x = "function", y = "# of times used") + 
  ggtitle("Amanda's top 50 most used R functions")

ggsave("Amanda's top 50 most used functions.png", width = 10, height = 8)

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

# back to word cloud ------------------------------------------------------



### WORD CLOUD 
plot_functions_wc <- function_frequency %>% 
  rownames_to_column(var = "rank") %>% 
  mutate(rank = as.numeric(rank),
         color = case_when(
           #rank >= 400 ~ "#bd0026",
           #rank <= 400 & rank > 300 ~ "#fc4e2a",
           rank > 300 ~ "#fc4e2a", 
           rank <= 300 & rank > 200 ~ "#fd8d3c",
           rank <= 200 & rank > 100 ~ "#feb24c",
           rank <= 100 & rank > 50 ~ "#fed976",
           rank <= 50 & rank > 10 ~ "#FFB5A2",
           rank <= 10 & rank > 1 ~ "#e88370", 
           rank == 1 ~ "#ce597d")) %>% 
  dplyr::relocate(rank, .after = color) %>% 
  filter(freq > 1)

figPath <- here::here("hex.png")

functions_wordcloud <- wordcloud2::wordcloud2(plot_functions_wc,
                                              size = 2, color = plot_functions_wc$color)

# functions_wordcloud$sizingPolicy$browser$padding <- 0


htmlwidgets::saveWidget(functions_wordcloud,"function_frequency.html",selfcontained = F)
webshot::webshot("function_frequency.html","function_frequency.png",vwidth = 800, vheight = 800, delay = 20)

