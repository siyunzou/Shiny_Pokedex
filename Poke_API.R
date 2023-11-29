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

write_csv(pokeframe_df, "pokeframe_df.csv")

#---------------------API FUNCTION (My own pull)-----------------------------------
# building an pokemon API function to pull data
poke_api <- function(path){
  out <- tryCatch(
    {
      url <- modify_url("https://pokeapi.co", path=paste("/api/v2",path, sep=""))
      response <- GET(url, timeout(10))
      if (http_error(response)){
        if (round(status_code(response)/100,0)==5 | path==500){ # NOTE: all path==500 checks are for testing purposes only (allows simulation of inability to reach API)
          if (path == 500){
            # If 500 was passed in set the response to 500 so that the while loop can be tested
            response = 500
          }
          # If the error status code is in the 500 range attempt to call the API up to 5 more times, with a timed delay between calls
          delayTime <- 1
          while (round(status_code(response)/100,0)==5 & delayTime<=16){
            Sys.sleep(delayTime) # Delay time between calls
            response <- GET(url, timeout(10)) # Attempting to reach the API again
            # Exponential backoff of request time +/- randomly selected value between 0+5% of current delay time
            delayTime = delayTime*2+runif(1, -delayTime*0.05, delayTime*0.05)
            # For Testing when the API can't be reached
          }
          if (round(status_code(response)/100,0)==5 | path==500){
            stop("Unable to reach API", call. = FALSE)
          }
        } else if (round(status_code(response)/100,0)==4){
          stop("Invalid Input Path", call. = FALSE)
        }
      }
      return(response)
    },
    error=function(cond){
      print(gsub("\n","",gettext(cond)))
    })
}

# testing pull from /pokemon/
res <- poke_api('/pokemon/5')

df = fromJSON(rawToChar(res$content))

# pulling pokemon ability from json
ability <- df$abilities[,1, drop = FALSE] %>%
  reframe(abilities = paste(ability$name, collapse= ","))
# pulling pokemon name from json
name <- df$name
# pulling pokemon height from json
height <- df$height
# pulling pokemon weight from json
weight <- df$weight
# pulling pokemon id from json
id <- df$id
# pulling pokemon type from json
type <- df$types[,2,drop = FALSE] %>%
  reframe(type = paste(type$name, collapse= ","))
# pulling pokemon top 5 moves from json
moves <- head(df$moves[,1,drop=FALSE],5) %>%
  reframe(moves = paste(move$name, collapse= ","))
# putting all the data into a dataframe
data <- data.frame(id, name, height, weight, type, ability, moves)

# for loop to grab data from each API pull on /pokemon/

pokemon_df <- data.frame()
ids <- 1:151
for (i in ids){
  res <- poke_api(paste0('/pokemon/', i))
  df = fromJSON(rawToChar(res$content))
  
  ability <- df$abilities[,1, drop = FALSE] %>%
    reframe(abilities = paste(ability$name, collapse= ","))
  
  pokemon <- df$name
  height <- df$height
  weight <- df$weight
  id <- df$id
  
  type <- df$types[,2,drop = FALSE] %>%
    reframe(type = paste(type$name, collapse= ","))

  moves <- head(df$moves[,1,drop=FALSE],5) %>%
    reframe(moves = paste(move$name, collapse= ","))
  
  data <- data.frame(id, pokemon, height, weight, ability, type, moves)
  pokemon_df <- rbind(pokemon_df, data)
}

# set id to have leading 0s for ones that need it
pokemon_df$id <- sprintf("%03d", as.numeric(pokemon_df$id))

write_csv(pokemon_df, "pokemon_df.csv")

#-------------------------R Wrapper-----------------------------------
# install.packages("devtools")
# install.packages("roxygen2")
# install.packages("usethis")
# install.packages("curl")
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("attempt")
# install.packages("purrr")
# devtools::install_github("r-lib/desc")


