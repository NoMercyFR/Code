library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(spatialreg)
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

library(spdep)

map_without_na <- map_data %>%
  filter(!is.na(edyr25),
         !is.na(fullsci),
         !is.na(regpopm),
         !is.na(hhsize),
         !is.na(iwi),
         !is.na(gvi))


map_without_na <- st_make_valid(map_without_na) #problem with package sf

cat("Regions after dropping NA in edyr25:", nrow(map_without_na), "\n")

#Queen contiguity means that 2 polygone are considered neighbor either if they share a border or a corner

nb_queen <- poly2nb(map_without_na, queen = TRUE)

#Then I standardized the row , equal weight between neighbor even if a country shares a larger border 

W_queen <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)

#Moran and Geary Index on raw data

#In both tests, there are a a high autocorrelation
moran_index <- moran.test(map_without_na$edyr25, listw = W_queen, zero.policy = TRUE)
geary_index <- geary.test(map_without_na$edyr25, listw = W_queen, zero.policy = TRUE)

print(moran_index)
print(geary_index)

#Regression with OLS 

ols<- lm(edyr25 ~ fullsci + regpopm + hhsize + iwi, data = map_without_na)
print(ols)

moran.test(residuals(ols), listw = W_queen, zero.policy = TRUE)


####Specification model with LM test to choose between Spatial autoregressive model(SAR) and Spatial Error Model(SEM)


# LM battery → tells you SAR vs SEM
lm.RStests(ols, listw = W_queen, test = "all")

#The conclusion is weird but I am going to implement SEM

sem_model <- errorsarlm(edyr25 ~ fullsci + regpopm + hhsize + iwi,
                       data = map_without_na,
                       listw = W_queen,
                       zero.policy = TRUE)

summary(sem_model)
print(2)
########################
