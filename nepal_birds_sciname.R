library(tidyverse); library(taxize)

nepal_birds.w <- read.csv("nepal_birds.csv") %>% mutate(
  Frequency = apply(Frequency %>% matrix(), 1, FUN = function(x) {
    strsplit(x, "%")[[1]][1]
  })
) %>% mutate(Frequency = as.numeric(Frequency)/100) %>% 
  pivot_wider(names_from = "Region", values_from = "Frequency") %>%
  mutate(
    Dolakha = ifelse(is.na(Dolakha), 0, Dolakha)
  , Gandaki = ifelse(is.na(Gandaki), 0, Gandaki)
  , Chitawan = ifelse(is.na(Chitawan), 0, Chitawan)
  ) %>% mutate(
    sciname = "", .after = Species
  ) %>% mutate(
    mean_abund = (Dolakha + Gandaki + Chitawan) / 3
  )

for (i in 2:nrow(nepal_birds.w)) {
  
  test_name <- nepal_birds.w[i, 1] %>% unlist()
  get_name  <- comm2sci(test_name)[1]
  the_name  <- get_name %>% unlist() %>% unname()
  
  if(length(the_name) > 0) {
    nepal_birds.w[i, 2] <- get_name %>% unlist() %>% unname()
  }
  
}

nepal_birds.w.c <- nepal_birds.w %>% 
  mutate(
    genus   = apply(sciname %>% matrix(), 1, FUN = function(x) {strsplit(x, " ")[[1]][1]})
  , species = apply(sciname %>% matrix(), 1, FUN = function(x) {strsplit(x, " ")[[1]][2]})
  , .after = sciname
  ) %>% arrange(desc(mean_abund))

write.csv(nepal_birds.w.c, "nepal_birds_wide.csv")


