library(units)
library(tidyverse)
library(sf)
library(readxl)

# 1.1

USAboundaries::us_states

conus = USAboundaries::us_states() %>%
  filter(!state_name %in% c("Puerto Rico",
                            "Alaska",
                            "Hawaii"))

counties = USAboundaries::us_counties() %>%
  st_as_sf(counties) %>%
  filter(!state_name %in% c("Puerto Rico",
                            "Alaska",
                            "Hawaii")) %>%
  st_transform(counties, crs = 5070)


# 1.2
county_centroid = st_centroid(counties) %>%
  st_union()

# 1.3
# voronoi tessellation
vor_tess = st_voronoi(county_centroid) %>%
  st_cast() %>%
  st_as_sf() %>%
  mutate(id=1:n())

# triangulated tessellation
tri_tess = st_triangulate(county_centroid) %>%
  st_cast() %>%
  st_as_sf() %>%
  mutate(id=1:n())

# gridded coverage
gridded_counties = st_make_grid(county_centroid, n = 70, square = T) %>%
  st_cast() %>%
  st_as_sf() %>%
  mutate(id=1:n())

# hexagonal coverage
hex_counties = st_make_grid(county_centroid, n=70, square = F) %>%
  st_cast() %>%
  st_as_sf() %>%
  mutate(id=1:n())

# 1.4
vor_tess = st_intersection(vor_tess, st_union(counties))
tri_tess = st_intersection(tri_tess, st_union(counties))
gridded_counties = st_intersection(gridded_counties, st_union(counties))
hex_counties = st_intersection(hex_counties, st_union(counties))


# 1.5
counties_simp = rmapshaper::ms_simplify(counties, keep = .01)
plot(counties_simp$geometry)
mapview::npts(counties)
mapview::npts(counties_simp)

tri_tess = st_intersection(tri_tess, counties_simp)

vor_tess = st_intersection(vor_tess, counties_simp)


# 1.6
plot_tess = function(sf_obj, title){
  ggplot()+
    geom_sf(data = sf_obj, fill = "white", col = "navy", size = .2)+
    theme_void() +
    labs(title = title, caption = paste("This tessellation has:", nrow(sf_obj), "features." ))
  }

# 1.7
plot_tess(vor_tess, "Voronoi Tessellation")
plot_tess(tri_tess, "Triangulated Tessellation")
plot_tess(gridded_counties, "Gridded Coverage")
plot_tess(hex_counties, "Hexagonal Coverage")





# Question 2
# 2.1

tess_sum = function(sf_obj, text){

  area = st_area(sf_obj) %>%
  units::set_units("km^2") %>%
  units::drop_units()

data.frame(num_features = nrow(sf_obj),
           mean_area = mean(area),
           sd_area = sd(area),
           total_area = sum(area)
)


}

# 2.2

vor_sum_tess = tess_sum(vor_tess, "Voronoi")
tri_sum_tess = tess_sum(tri_tess, "Triangulation")
hex_sum = tess_sum(hex_counties, "Hexagon")
grid_sum = tess_sum(gridded_counties, "Grid")

# 2.3

summary_tess = bind_rows(
  tess_sum(counties_simp, "Original Counties"),
  tess_sum(tri_tess, "Triangulation"),
  tess_sum(vor_tess, "Voronoi"),
  tess_sum(hex_counties, "Hexagon"),
  tess_sum(gridded_counties, "Grid")
)

# 2.4
knitr::kable(summary_tess,
            col.names = c("Number of Features", "Mean Area", "Standard Deviation of Features", "Total Area"))


# Question 3
# 3.1
dams <- read_excel("NID2019_U.xlsx")

dams_sf = dams %>%
  filter(!is.na(LONGITUDE), !is.na(LATITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4326) %>%
  st_transform(5070)


# 3.2

pip_func = function(points, polygon, id){
  st_join(polygon, points) %>%
    st_drop_geometry() %>%
    count(get('id')) %>%
    setNames(c(id, "n")) %>%
    left_join(polygon, by = id) %>%
    st_as_sf()

}
pip_func_county_simp = function(points, polygon, id){
  st_join(polygon, points) %>%
    st_drop_geometry() %>%
    count(get('geoid')) %>%
    setNames(c(id, "n")) %>%
    left_join(polygon, by = id) %>%
    st_as_sf()

}

# 3.3
vor_pip = pip_func(dams_sf, vor_tess, "id")
tri_pip = pip_func(dams_sf, tri_tess, "id")
gridded_pip = pip_func(dams_sf, gridded_counties, "id")
hex_pip = pip_func(dams_sf, hex_counties, "id")
counties_simp_pip = pip_func_county_simp(dams_sf, counties_simp, "geoid")


# 3.4
plot_pip = function(sf_obj, title){
  ggplot()+
    geom_sf(data = sf_obj, aes(fill = log(n)))+
    scale_fill_viridis_c()+
    theme_void() +
    labs(title = title, caption = paste("This tessellation has:", sum(sf_obj$n), "dams." ))
}

plot_pip(vor_pip, "Voronoi")
plot_pip(tri_pip, "triangulation")
plot_pip(gridded_pip, "gridded")
plot_pip(hex_pip, "hexagonal")
plot_pip(counties_simp_pip, "original")




# 4.1
dam_freq = strsplit(dams$PURPOSES, split = "") %>%
     unlist() %>%
     table() %>%
     as.data.frame() %>%
     setNames(c("abbr", "count"))








plot(dam_freq$count[3:8])
ggplot(dam_freq, aes(x=abbr))+
  geom_bar(data = dam_freq)
