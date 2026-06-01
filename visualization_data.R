library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(spatialreg)
library(spdep)

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
  mutate(gvi_q = cut(
    gvi,
    breaks = quantile(gvi, probs = seq(0, 1, 0.2), na.rm = TRUE),
    include.lowest = TRUE,
    labels = c("Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)")
  ))

# --- 6. Africa background ---
africa <- ne_countries(continent = "Africa", returnclass = "sf") %>%
  filter(admin != "Madagascar")
map_data <- st_transform(map_data, st_crs(africa))

# --- 7. Plot (assign + print explicitly) ---
p <- ggplot() +
  geom_sf(data = africa, fill = "grey95", color = "grey70", linewidth = 0.2) +
  geom_sf(data = map_data, aes(fill = gvi_q), color = "black", linewidth = 0.05) +
  scale_fill_brewer(palette = "YlOrRd", name = "Vulnerability Index\n(quintiles)", na.value ="blue") +
  coord_sf(xlim = c(-20, 52), ylim = c(-36, 38)) +
  labs(
    title = "Vulnerability Index",
    subtitle = "Africa", caption = "Source: Global Data Lab"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

print(p)   # <-- forces the plot to display
###


############Econometric part###########

map_without_na <- map_data %>%
  filter(!is.na(edyr25),
         !is.na(fullsci),
         !is.na(regpopm),
         !is.na(hhsize),
         !is.na(iwi),
         !is.na(gvi))

map_without_na <- st_make_valid(map_without_na)
cat("Regions after dropping NA:", nrow(map_without_na), "\n")

# --- Project to metres (mandatory for length computation) ---
map_proj <- st_transform(map_without_na,
                         crs = "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25")
map_proj <- st_make_valid(map_proj)
n        <- nrow(map_proj)

# --- Queen contiguity (tells us which pairs share a border) ---
nb_queen <- poly2nb(map_proj, queen = TRUE, snap = 1e-4)

# --- Perimeter of each region in metres ---
map_proj$perimeter_m <- as.numeric(st_perimeter(map_proj))

# --- Shared border length matrix ---
W_raw <- matrix(0, n, n)

for (i in seq_len(n)) {
  nbrs <- nb_queen[[i]]
  if (length(nbrs) == 0 || nbrs[1] == 0L) next
  for (j in nbrs) {
    inter <- tryCatch(
      st_intersection(map_proj[i, "geometry"], map_proj[j, "geometry"]),
      error = function(e) NULL
    )
    if (is.null(inter) || nrow(inter) == 0) next
    if (any(grepl("LINE|COLLECTION", as.character(st_geometry_type(inter))))) {
      W_raw[i, j] <- as.numeric(st_length(inter))
    }
  }
}

# --- Divide by own perimeter: w_ij = border_ij / P_i ---
W_perim <- W_raw / map_proj$perimeter_m

# --- Convert to listw ---
W_perim_listw <- mat2listw(W_perim, style = "W", zero.policy = TRUE)





#Moran and Geary Index on raw data

#In both tests, there are a a high autocorrelation
moran_index <- moran.test(map_without_na$edyr25, listw = W_perim_listw, zero.policy = TRUE)
geary_index <- geary.test(map_without_na$edyr25, listw = W_perim_listw, zero.policy = TRUE)

print(moran_index)
print(geary_index)

#Regression with OLS 

ols<- lm(edyr25 ~ fullsci + regpopm + hhsize + iwi, data = map_without_na)
print(ols)

moran.test(residuals(ols), listw = W_perim_listw, zero.policy = TRUE)

lm.RStests(ols, listw = W_perim_listw, test = "all")

sem_model <- errorsarlm(edyr25 ~ fullsci + regpopm + hhsize + iwi,
                        data = map_without_na,
                        listw = W_perim_listw,
                        zero.policy = TRUE)

summary(sem_model)
