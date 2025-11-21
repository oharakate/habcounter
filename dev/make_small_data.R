## Make a smaller version of some data

library(sf)
library(dplyr)


# Make a small version of habmos, preserving combinations of variables
# of interest
habmos <- st_read("data/HABMOS_SCOTLAND.shp")
View(habmos)

# Plan to keep 1 record for every distinct combination of these variables:
attribute_cols <- c("HABITAT_CO","HABITAT_SOU", "SYMBOL")

test_data_unique <- habmos %>%
  # Use distinct() and apply it across the selected columns
  distinct(across(all_of(attribute_cols)),
           .keep_all = TRUE)
View(test_data_unique)

st_write(test_data_unique, "test_data_unique_large.shp", append = FALSE)


# Another option  - just a random sample:
habmos <- st_read("data/HABMOS_SCOTLAND.shp")
sample_size <- 50

# Get the indices (row numbers) for a random sample
random_indices <- sample(
  x = 1:nrow(habmos),
  size = sample_size,
  replace = FALSE
)

# Subset the sf object using the indices
test_data <- habmos[random_indices, ]
View(test_data)

# 4. Save the new shapefile (optional)
st_write(test_data, "data/test_data.shp")



# Make a simplified aoi
aoi <- st_read("data/f2oaoi.shp")
head(aoi)

# Simplify the geometry
aoi_simple <- aoi %>%
  # Apply simplification to the geometry column
  st_simplify(preserveTopology = TRUE, dTolerance = 5000)
plot(aoi_simple)


st_write(aoi_simple, "data/test_aoi.shp", append = FALSE)
