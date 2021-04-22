# Load packages
library(dplyr)
library(readr) 
library(tidyr)
library(sf) 
library(ggplot2)
library(viridis)
library(igraph) 
library(spdep)
library(tidygraph)
library(ggraph)

msoa_sf <- sf::st_read("data/maps/Middle_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.shp")

msoa <- readr::read_csv('data/msoa_cases.csv', 
                col_types = cols()) %>% 
  select(where(~!all(is.na(.)))) %>% 
  pivot_longer(dplyr::starts_with("wk_"), 
               names_to = "week",
               values_to = "cases", 
               names_prefix = "wk_") %>% 
  mutate(week = as.integer(week)) %>% 
  mutate(across(c(latest_7_days, cases), 
                .fns = ~ifelse(is.na(.), 0, .)))

msoa$cases[msoa$cases == -99] <- 0

la <- msoa %>% 
  group_by(lad19_nm, week) %>% 
  summarise(cases = sum(cases))

la %>% 
  arrange(desc(cases)) %>% 
  head(100) %>% 
  View

# Map
msoa_data_wk41 <- msoa %>% 
  filter(lad19_nm == "Liverpool", 
         week == 41) 

# Covid cases are clustered in municipalities

pdf("CovidMap.pdf", 10, 10)
msoa_sf %>% 
  inner_join(msoa_data_wk41 %>% distinct(msoa11_cd, cases), by = c("MSOA11CD" = "msoa11_cd")) %>% 
  ggplot()+
  geom_sf(aes(geometry = geometry, fill = cases)) +
  scale_fill_viridis_c(option = 'turbo') + 
  theme_void() +
  labs(fill = "Cases") + 
  theme(legend.position = "left", 
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 18),
        legend.key.size = unit(2, 'cm'), 
        legend.key.height = unit(2, 'cm'), 
        legend.key.width = unit(2, 'cm'))
dev.off()

# Network 

# Extract Neighbours from our geo data
liverpool_sf <- msoa_sf %>% 
  inner_join(msoa %>% filter(lad19_nm == "Liverpool", week == 41) , 
             by = c("MSOA11CD" = "msoa11_cd"))

liverpool_msoa_neighbours <- spdep::poly2nb(liverpool_sf)

adj_mat <- spdep::nb2mat(liverpool_msoa_neighbours, style = "B")
rownames(adj_mat) <- liverpool_sf$msoa11_hclnm
colnames(adj_mat) <- liverpool_sf$msoa11_hclnm

liverpool_network <- igraph::graph_from_adjacency_matrix(adj_mat, mode = "undirected") %>% 
  tidygraph::as_tbl_graph()

# Converting LONG LAT coordinates
coords <- liverpool_sf %>% 
  as.data.frame() %>% 
  select(LONG_, LAT) %>% 
  sp::SpatialPoints(proj4string = CRS("EPSG:4326")) %>% # LAT LONG code
  as.data.frame() %>% 
  bind_cols(msoa11_hclnm = liverpool_sf$msoa11_hclnm) 

liverpool_network <- liverpool_network %>% 
  activate("nodes") %>% 
  left_join(coords, by = c("name" = "msoa11_hclnm"))

liverpool_network %>% 
  as_tibble() %>% 
  head

# Final Graph

pdf("NetworkMap.pdf", 10, 10)
ggraph(liverpool_network, layout = "manual", x = LONG_, y = LAT)  +
  geom_sf(data = liverpool_sf, fill = "turquoise1") +
  theme_void() +
  geom_edge_link(colour = 'white', width = 3) + 
  geom_node_point(size = degree(liverpool_network)*1.5, colour = 'black')
dev.off()

