# downloading png from webpage
download.file("https://assets.pokemon.com/assets/cms2/img/pokedex/full/001.png",destfile="images/myimage.png")

# set function to download png 
get_png <- function(x){
  url <- paste0("https://assets.pokemon.com/assets/cms2/img/pokedex/full/",x,".png")
}

# loop to save each png 
for (i in pokeframe_df$url_id){
  download.file(get_png(i), destfile = paste0("images/",i,".png"))
}

