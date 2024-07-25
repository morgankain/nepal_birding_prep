library(tidyverse)
library(readxl)
library(plotly)
library(auk)
birds <- read_excel("nepal_birds_wide.xlsx")

## To look up ebird url
#all_tax <- auk::ebird_taxonomy
#birds %>% left_join(., all_tax %>% dplyr::select(common_name, species_code) %>% 
#            rename(Name = common_name)) %>%
#  mutate(url = paste("https://ebird.org/species/", species_code, sep = ""), .after = 1) %>%
#  write.csv("birds_with_url.csv")

## ggplot theme
theme_set(theme_bw())
suppressWarnings(
  theme_update(
    axis.text.x = element_text(size = 16)
    , axis.text.y = element_text(size = 16)
    , axis.title.x = element_text(size = 16)
    , axis.title.y = element_text(size = 16)
    , legend.title = element_text(size = 12)
    , panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
    , strip.background = element_blank()
    , panel.margin = unit(0, "lines")
    , legend.key.size = unit(.55, "cm")
    , legend.key = element_rect(fill = "white")
    , panel.margin.y = unit(0.5, "lines")
    , panel.border = element_rect(colour = "black", fill = NA, size = 1)
    , strip.text.x = element_text(size = 16, colour = "black", face = "bold"))
)

shape_commonality <- birds %>% group_by(Shape) %>% summarize(prop = mean(Mean)) %>% 
  arrange(desc(prop)) %>% mutate(Shape = as.factor(Shape))

birds.l <- birds %>% pivot_longer(c(Dolakha, Gandaki, Chitawan, Mean), names_to = "Location", values_to = "Proportion") %>%
  mutate(Shape = factor(Shape, levels = rev(shape_commonality$Shape))) %>%
  filter(Location != "Mean")

gg.1 <- birds.l %>% {
  ggplot(., aes(Proportion, Shape, customdata = url)) +
    geom_point(aes(colour = Location, group = Name)) +
    scale_colour_brewer(palette = "Dark2") +
    theme(axis.text.y = element_text(size = 10)) +
    facet_wrap(~Location, scales = "free")
}

gg.1b <- ggplotly(gg.1)

gg.1c <- htmlwidgets::onRender(gg.1b, "
     function(el, x) {
     el.on('plotly_click', function(d) {
     var url = d.points[0].customdata;
     //url
     window.open(url);
     });
     }
     ")

gg.1c

htmlwidgets::saveWidget(gg.1c, "nepal_birds.html")
