# === 1. Install required packages (only once) ===
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")

# === 2. Load libraries ===
library(dplyr)
library(tidyr)

# === 3. Define file path (update if needed) ===
file_path <- "/Users/alex/Desktop/DATA VIZ/Project/Data cleanning/combined_houses.csv"

# Check if file exists
if (!file.exists(file_path)) {
  stop("File not found! Please check the path: ", file_path)
}

# === 4. Read the CSV file (no header, 30 columns) ===
data <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE)

# === 5. Name the 30 columns (3 properties × 10 fields) ===
colnames(data) <- paste0("V", 1:30)

# Define standard column names for one property
var_names <- c("ID", "price", "condition", "district", "floor", "address", "rooms", "city", "area", "link")

# === 6. Split each set of 10 columns into one property block ===
property1 <- data[1:10] %>% setNames(var_names)
property2 <- data[11:20] %>% setNames(var_names)
property3 <- data[21:30] %>% setNames(var_names)

# Combine into one long data frame (one row per apartment)
data_long <- bind_rows(property1, property2, property3)

# === 7. Clean the data ===
data_clean <- data_long %>%
  select(-ID, -link) %>%  # Remove ID and link
  mutate(
    price = as.numeric(price),
    area = as.numeric(area),
    rooms = as.numeric(rooms),
    floor = as.numeric(floor)
  ) %>%
  drop_na(price, area) %>%  # Remove rows with invalid price or area
  mutate(
    across(where(is.character), ~ trimws(.))  # Trim whitespace
  )

# === 8. Save directly to Desktop ===
output_path <- "/Users/alex/Desktop/cleaned_houses.csv"
write.csv(data_clean, output_path, row.names = FALSE)

# === 9. Confirm success ===
message("✅ Success! Cleaned data saved to: ", output_path)
head(data_clean)

