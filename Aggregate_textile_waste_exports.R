# 1. Script documentation
# Author: Bauke Post
# Date created: 19/11/2024
# Description: merges UN COMTRADE data for Netherlands' 6039 exports 2019-2024

# 2. Set working directory
setwd("~/Project Support/Schrijf Advies aan AIV (TOA)/Economische Analyse van Textielstromen")

# 3. Load packages
library(dplyr)
library(forcats)
library(ggplot2)
library(readxl)
library(plotly)


# Define constants
hs_6309 <- "630900"
hs_6310 <- c("631010", "631090")
all_hs_codes <- c("630900", hs_6310)

# 5. Define functions
# Reads UN COMTRADE data downloaded as .xlsx from WITS into a dataframe
read_WITS_file <- function(file) 
{
  read_excel(file, sheet = "By-HS6Product")
}

# Aggregates export data
aggregate_6310 <- function(data)
{
  data %>%
    group_by(exportbestemming, year, hs_code) %>%
    summarize(
      volume_ton = sum(volume_ton),
      total_trade_value_1000usd = sum(trade_value_1000usd)) %>%
    ungroup()
}

# 6. Main execution
# Get list of 6309, 631010, 631090 datasets for 2019-2023
files <- list.files(path = "Datasets", 
                    pattern = "*.xlsx",
                    full.names = TRUE)

list_export_data <- lapply(files, read_WITS_file)

# Join all datasets by binding rows
export_data <- bind_rows(list_export_data)

# Remove superfluous list
rm(list_export_data)

# Subset colums to select only relevant variables
export_data <- select(export_data, 
                               "hs_code" = "ProductCode", 
                               "year" = "Year",
                               "exportbestemming" = "Partner", 
                               "trade_value_1000usd" = "Trade Value 1000USD",
                               "quantity_kg" = "Quantity")

# Convert strings to numeric (year, trade value, quantity)
export_data <- mutate(export_data,
                                year = as.numeric(year),
                                trade_value_1000usd = as.numeric(trade_value_1000usd),
                                quantity_kg = as.numeric(quantity_kg))

# Convert quantity to tonnes for readability
export_data <- export_data %>%
                          mutate(volume_ton = quantity_kg / 1000) %>%
                          select(-"quantity_kg")


# Split hs code into digits 1-4 ("hs_code") and 5-6 ("hs_extension") to create
# common value for 631010/90
export_data <- export_data %>%
  mutate(hs_extension = substr(hs_code, 
                        nchar(hs_code)-1, 
                        nchar(hs_code)),
         hs_code = substr(hs_code, 1, nchar(hs_code)-2),
         .after = hs_code)

# Calculate average value for exports in 2023, aggregating 6310's subcategories
export_data_2023 <- export_data %>%
  filter(year == 2023 & exportbestemming != "World") %>%
  aggregate_6310() %>%
  select(-"year") %>%
  mutate(gemiddelde_waarde_usd = (total_trade_value_1000usd / volume_ton) * 1000)

# Assign factor ordering to countries based on total exports 
export_data_2023 <- export_data_2023 %>%
  group_by(exportbestemming) %>%
  mutate(totaal_volume_ton = sum(volume_ton)) %>%
  ungroup() %>%
  mutate(exportbestemming = fct_reorder(exportbestemming, desc(totaal_volume_ton))) 

# Subset 2023 exports based on total exports
export_data_2023 <- export_data_2023 %>%
  filter(as.integer(exportbestemming) < 30) %>%
  mutate(exportbestemming = fct_reorder(exportbestemming, totaal_volume_ton))

# Create bar chart
bar_chart <- ggplot(export_data_2023, aes(fill = hs_code,
                             x = volume_ton,
                             y = exportbestemming,
                             text = sprintf("%s-export naar %s in 2023 \nVolume (in ton): %.0f \nGemiddelde waarde per ton (USD): %.0f \nTotale export naar %s (in ton): %.0f",
                                             hs_code, exportbestemming, volume_ton, gemiddelde_waarde_usd, exportbestemming, totaal_volume_ton))) + 
  geom_bar(position="stack", stat = "identity") +
  guides(fill=guide_legend(title="HS Classificatie")) +
  labs(title = "Top 30 exportbestemmingen gebruikt textiel, Nederland (2023)",
       subtitle="Gebruikte kleding en textiel (6309) en Vodden en lompen (6310)",
       caption = "Bron: UN COMTRADE", 
       x = "Volume (in ton)", 
       y = "Exportbestemming")

# Exporteer chart naar html
bar_chart_plotly <- ggplotly(bar_chart, tooltip = "text")
htmlwidgets::saveWidget(as_widget(bar_chart_plotly), "bestemmingen_2023_volume.html")

  


# Sort by most valuable (average value/tonne exported)
ordered_value_data_2023 <- export_data %>%
  filter(year == 2023 & exportbestemming != "World") %>% 
  group_by(exportbestemming, hs_code) %>%
  summarise(volume_ton = sum(volume_ton),
            trade_value_usd = trade_value_1000usd * 1000,
            waarde_per_ton_usd = trade_value_usd / volume_ton) %>%
  ungroup() %>%
  View()
  mutate(average_value_per_tonne_usd = (trade_value_1000usd / volume_ton) * 1000)
  
  
  
  