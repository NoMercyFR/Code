library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
###############################""
# --- 1. Load data ---
data <- read.csv("C:/Users/cleme/Documents/Master_Allemagne/Topic_Economic_History/Dataset_code/data_set_2010_allvar.csv")

# --- 2. Load shapefile (ONE st_read only!) ---
gdl_shapes <- st_read("C:/Users/cleme/Documents/Master_Allemagne/Topic_Economic_History/Dataset_code/GDL Shapefiles V6.6/GDL Shapefiles V6.6 large.shp")
        
# Option C — most robust: filter on the gdlcode prefix (always present)


# --- 3. INNER join → keeps ONLY your regions (huge speed gain) ---
map_data <- gdl_shapes %>%
  inner_join(data, by = c("gdlcode" = "GDLCODE")) %>%
  filter(!grepl("^MDG", gdlcode))


cat("Regions matched:", nrow(map_data), "\n")   # should be > 0

# --- 4. Simplify geometry for fast rendering ---
map_data <- st_make_valid(map_data)
map_data <- st_simplify(map_data, dTolerance = 0.01, preserveTopology = TRUE)


unique(map_data$gdlcode[grepl("MDG", map_data$gdlcode)])
# --- 5. Create 5 quantile bins ---
map_data <- map_data %>%
  mutate(fullsci_q = cut(
    fullsci,
    breaks = quantile(fullsci, probs = seq(0, 1, 0.2), na.rm = TRUE),
    include.lowest = TRUE,
    labels = c("Q1 (highest)", "Q2", "Q3", "Q4", "Q5 (lowest)")
  ))

# --- 6. Africa background ---
africa <- ne_countries(continent = "Africa", returnclass = "sf") %>%
  filter(admin != "Madagascar")
map_data <- st_transform(map_data, st_crs(africa))

# --- 7. Plot (assign + print explicitly) ---
p <- ggplot() +
  geom_sf(data = africa, fill = "grey95", color = "grey70", linewidth = 0.2) +
  geom_sf(data = map_data, aes(fill = fullsci_q), color = "black", linewidth = 0.05) +
  scale_fill_brewer(palette = "YlOrRd",direction = -1, name = "Corruption Index\n(quintiles)", na.value ="blue") +
  coord_sf(xlim = c(-20, 52), ylim = c(-36, 38)) +
  labs(
    title = "Corruption Index by Subnational Region",
    subtitle = "Africa", caption = "Source: Global Data Lab"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

print(p)   # <-- forces the plot to display
###
print(2)
