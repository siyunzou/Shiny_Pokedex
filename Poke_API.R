# API Call https://nguyeneva.github.io/2020-02-06-rwrapper/

library(jsonlite) 
library(httr) 
library(tidyverse)
library(stringr)
library(dplyr)
library(urltools)
library(re2)
library(devtools)
devtools::install_github("nguyeneva/data534_project/pokeWrapper")

library(pokeWrapper)
pokeframe <- initializeDataFrame()

df <- url_parse(pokeframe$speciesURL)
re.split(r"(/[0-9]+/)",pokeframe$speciedURL)

# add a column for url id 
pokeframe_df <- pokeframe %>%
  mutate(url_id = strsplit(trimws(speciesURL, whitespace = "/"), "/", fixed = TRUE)) %>%
  rename(path = url_id) %>% 
  unnest_wider(path, names_sep = "") %>%
  select(-path1, -path2, -path3, -path4, -path5, -path6) %>%
  rename(url_id = path7)

# set url_id to have leading 0s for ones that need it
pokeframe_df$url_id <- sprintf("%03d", as.numeric(pokeframe_df$url_id))
