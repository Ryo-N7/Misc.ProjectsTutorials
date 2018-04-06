library(fs)
library(ggalt) # devtools::install_github("hrbrmstr/ggalt")
library(igraph) 
library(ggraph) # devtools::install_github("thomasp85/igraph")
library(hrbrthemes) # devtools::install_github("hrbrmstr/hrbrthemes")
library(tidyverse)



installed.packages() %>%
  as_data_frame() %>%
  mutate(pkg_dir = sprintf("%s/%s", LibPath, Package)) %>%
  select(pkg_dir) %>%
  mutate(pkg_dir_size = map_dbl(pkg_dir, ~{
    fs::dir_info(.x, all=TRUE, recursive=TRUE) %>%
      summarise(tot_dir_size = sum(size)) %>% 
      pull(tot_dir_size)
  })) %>% 
  summarise(
    total_size_of_all_installed_packages=ggalt::Gb(sum(pkg_dir_size))
  ) %>% 
  unlist()

installed.packages() %>%
  as_data_frame() %>%
  mutate(pkg_dir = sprintf("%s/%s", LibPath, Package)) %>%
  mutate(dir_info = map(pkg_dir, fs::dir_info, all=TRUE, recursive=TRUE)) %>% 
  mutate(dir_size = map_dbl(dir_info, ~sum(.x$size))) -> xdf

select(xdf, Package, dir_size) %>% 
  mutate(grp = "ROOT") %>% 
  add_row(grp = "ROOT", Package="ROOT", dir_size=0) %>% 
  select(grp, Package, dir_size) %>% 
  arrange(desc(dir_size)) -> gdf

select(gdf, -grp) %>% 
  mutate(lab = sprintf("%s\n(%s)", Package, ggalt::Mb(dir_size))) %>% 
  mutate(lab = ifelse(dir_size > 1500000, lab, "")) -> vdf

g <- graph_from_data_frame(gdf, vertices=vdf)

ggraph(g, "treemap", weight=dir_size) +
  geom_node_tile(fill="lightslategray", size=0.25) +
  geom_text(
    aes(x, y, label=lab, size=dir_size), 
    color="#cccccc", family=font_ps, lineheight=0.875
  ) +
  scale_x_reverse(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_size_continuous(trans="sqrt", range = c(0.5, 8)) +
  ggraph::theme_graph(base_family = font_ps) +
  theme(legend.position="none")








