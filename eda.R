library(tidyverse)
library(dplyr)

pokeframe_df <- read_csv("pokemon_df.csv")

pokeframe_df <- pokeframe_df %>% arrange(id, pokemon)

pokemon_list <- as.list(pokeframe_df$pokemon)



