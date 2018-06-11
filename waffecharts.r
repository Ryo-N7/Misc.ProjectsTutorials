# waffle charts and infographics

#install.packages("waffle")
library(waffle)
library(extrafont)

font_import()

loadfonts(device = "win")

waffle(c(50, 30, 15, 5), rows = 5, title = "wafflewafflewaffle om nom nom")
# default colors: Set2 (Color brewer)

waffle(c(50, 30, 15, 5), rows = 5, 
       use_glyph = "child", glyph_size = 3.5, 
       title = "Not just the men but the women and children too!")


parts <- c(One=80, Two=30, Three=20, Four=10)
waffle(parts, rows=8, use_glyph="shield")

chart <- waffle(parts, rows=8)
print(chart)

waffle(c(no = 80, use_glyph = "car", ))


parts <- c(`Basic\nGrills` = (50-10-5), `Business\nSuits` = 10, `Hipster\nLosers` = 5)

waffle(parts, rows = 5, glyph_size = 6, colors = c("#1696d2", "#fdbf11", "#000000"), legend_pos = "bottom", use_glyph = "coffee") +
  labs(title = "Number of Coffees taken in DC-1B Starbucks in Day",
       subtitle = "1 Square == 1000 Cawwhhfees",
       caption = "Fake Starbucks Marketing Department") +
  theme(text = element_text(family = "Trebuchet MS"))


parts <- c(`Strikers` = 7, `Mittelfeldspieler` = 15, `Hipster\nLosers` = 13, `Keepers` = 3)

waffle(parts, rows = 4, glyph_size = 6, colors = c("#1696d2", "#fdbf11", "#000000", "#8B0000"), 
       legend_pos = "bottom", use_glyph = "soccer-ball-o") +
  labs(title = "Positional make-up of Glue-Sniffers FC",
       subtitle = "1 Square == 1 Squad Player",
       caption = "Glue-Sniffers FC Marketing Department") +
  theme(text = element_text(family = "Trebuchet MS"))
# better to do a bar plot for this eh





























































































