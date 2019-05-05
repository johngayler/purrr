library(tidyverse)

# How many starships has luke been in? Solve for one element first  and then build out the recipt from there
luke <- people[[1]]

length(luke$starships)


map(people, ~ length(.x$starships))


# Find the name of each character's home world
load("data/planet_lookup.rda")

planet_lookup[[1]]
planet_lookup[1]

planet_lookup[luke$homeworld]


 map(people, #Object I'm iterating over
    ~ planet_lookup[.x$homeworld]) # What am I doing? subsetting planet_lookup based on people's homeworld var

 # Name each of our characters
 people <- people %>% 
   set_names(map_chr(people, "name"))

 # How many starships has each character been in?
 map_int(people, ~ length(.x$starships)) %>% hist()
 
 # What colour is each character's hair?
 
 map_chr(people, ~ .x[["hair_color"]])
 
  # Is the character male?
 map_lgl(people, ~ .x[["gender"]] == "male")
 
 # Get character vec with
 map_chr(people, ~ .x[["gender"]])
 
 # How heavy is each character?
  map(people, ~ .x[["mass"]]) %>% as.integer()

 map_chr(people, ~ .x[["mass"]]) %>% readr::parse_number(na = "unknown")
 
 # Extract skin colour for each person
 map(people, 5)
 map(people, ~ .x[[5]])
 map(people, ~ .x$skin_color)
 map(people, ~ .x[["skin_color"]])
 
 # Get the number of starships for each character
char_starships <- map(people, ~ .x$starships)
map_chr(char_starships, length) 
 
# Which film has the most characters?
map(films, ~ length(.x$characters))

# Charlotte's solution

films[[1]]
map(films, "characters")
map(films, ~ length(.x$characters))

chars <- map(films, "characters") %>% 
  map(length) %>% 
  set_names(map_chr(films, "title")) %>% 
  # Convert to tibble
  as_tibble() %>%  
  gather(key = "film", value = "characters") %>% 
  arrange(-characters)
  
# Create the planet lookup vector from earlier

planets_vec <- planets %>% 
  set_names(map_chr(planets, "url")) %>% 
  map_chr(~ .x$name)

# Which species has the m ost possible eye colours?

eye_colours <- map(species, "eye_colors") %>% 
  set_names(map_chr(species, "name")) %>% 
  as_tibble() %>% 
  gather(key = "species", value = "eye_colours") %>% 
  mutate(count = str_count(eye_colours, ",") + 1) %>% 
  arrange(count)


map_chr(species, "eye_colors") %>%
  set_names(map_chr(species, "name")) %>% 
  strsplit(", ") %>%
  map_int(length) %>% sort()


# Convert lists into tibble easily
people_tbl <- tibble(
  name = map_chr(people, "name"),
  films = map(people, "films"),
  height = people %>% map_chr("height") %>% 
    readr::parse_number(na = "unknown"),
  species = people %>% map_chr("species", .null = NA_character_)
)

map(people, ~.x$films) %>% 
  map(length)

# Add in the number of films that each character has featured in
people_tbl <- people_tbl %>% 
  mutate(n_films = map(people, ~.x$films) %>% 
           map_int(length)) %>% 
  
  mutate(film_numbers = films %>% map_chr(.x$episode_id))
  arrange(-n_films)


# Add in the number of each episode (eg. star wars 1) that each character stared in and then collapse those into a single string.

  

  
  
  
# 1:35 mins
  

