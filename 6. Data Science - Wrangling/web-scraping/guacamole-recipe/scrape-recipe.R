library(tidyverse)
library(rvest)

url <- 'https://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609'
h <- read_html(url)
key <- h %>% html_nodes('span.o-RecipeInfo__a-Headline') %>% html_text()
value <- h %>% html_nodes(".o-RecipeInfo__a-Description") %>% html_text()

guacamole_recipe <- data.frame(key, value)
guacamole_recipe