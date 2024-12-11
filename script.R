##### UPDATE DATA INPUTS #####

library(readr)
library(dplyr)
library(janitor)
library(ggplot2)

# Parameters

prod_URL <- "https://www.fao.org/fishery/static/Data/GlobalProduction_2024.1.0.zip" # URL of the FAO production data
asfis_URL <- "https://www.fao.org/fishery/static/ASFIS/ASFIS_sp.zip" # URL of the ASFIS database of species names
asfis_filename <- "ASFIS_sp_2024.csv"
number_species <- 200 # number of top species to create charts for
capture_color = "#377eb8"
aquaculture_color = "#4daf4a"

# Load functions

source("functions.R")

# Get production data from FAO's server

temp <- tempfile()
download.file(prod_URL, temp)
data <- read_csv(unz(temp, "Global_production_quantity.csv")) %>%
  clean_names()
countries <- read_csv(unz(temp, "CL_FI_COUNTRY_GROUPS.csv"), na = "") %>% # adding na = "" so that Namibia's ISO2 code isn't interpreted as a missing value
  clean_names() %>%
  select(un_code, iso2_code, name_en, continent_group_en, geo_region_group_en)
species <- read_csv(unz(temp, "CL_FI_SPECIES_GROUPS.csv")) %>%
  clean_names() %>%
  select(x3a_code, name_en, scientific_name, isscaap_group_en, yearbook_group_en)
water <- read_csv(unz(temp, "CL_FI_WATERAREA_GROUPS.csv")) %>%
  clean_names() %>%
  select(code, name_en, inland_marine_group_en)
source <- read_csv(unz(temp, "CL_FI_PRODUCTION_SOURCE_DET.csv")) %>%
  clean_names() %>%
  select(code, name_en)
unlink(temp)

# Get ASFIS data from FAO's server

temp <- tempfile()
download.file(asfis_URL, temp)
species_multilingual <- read_csv(unz(temp, asfis_filename)) %>%
  clean_names() %>%
  rename(english_name_asfis = english_name) %>%
  select(alpha3_code, english_name_asfis, french_name, spanish_name, arabic_name, chinese_name, russian_name)
unlink(temp)

# Join tables, restructure and save data

prod_raw <- data %>%
  left_join(countries, by = c("country_un_code" = "un_code"), keep = FALSE) %>%
  rename(un_code = country_un_code, country = name_en) %>%
  left_join(species, by = c("species_alpha_3_code" = "x3a_code"), keep = FALSE) %>%
  rename(`3alpha_code` = species_alpha_3_code, english_name = name_en) %>%
  left_join(species_multilingual, by = c("3alpha_code" = "alpha3_code"), keep = FALSE) %>% # add multilingual species names
  left_join(water, by = c("area_code" = "code"), keep = FALSE) %>%
  rename(fishing_area_code = area_code, fishing_area_name = name_en) %>%
  left_join(source, by = c("production_source_det_code" = "code"), keep = FALSE) %>%
  rename(production_source_detailed_name = name_en) %>%
  mutate(production_source_name = case_when(
    production_source_detailed_name == "Aquaculture production (freshwater)" ~ "Aquaculture production", 
    production_source_detailed_name == "Aquaculture production (brackishwater)" ~ "Aquaculture production", 
    production_source_detailed_name == "Aquaculture production (marine)" ~ "Aquaculture production",
    production_source_detailed_name == "Capture production" ~ "Capture production",
    production_source_detailed_name == "Aquaculture production" ~ "Aquaculture production")
  ) %>%
  mutate(measure = case_when(
    measure == "Q_tlw" ~ "Tonnes - live weight", 
    measure == "Q_no_1" ~ "Number")
  ) %>%
  rename(unit = measure,	year = period,	flag = status) %>%
  select(un_code, iso2_code, country, continent_group_en, geo_region_group_en, `3alpha_code`, english_name, scientific_name, french_name, spanish_name, arabic_name, chinese_name, russian_name, isscaap_group_en, yearbook_group_en, fishing_area_code, fishing_area_name, inland_marine_group_en, production_source_detailed_name, production_source_name, unit, year, value, flag) %>%
  group_by_at(vars(-value)) %>%
  summarise(value = sum(value)) %>%
  ungroup()

# Create aquaculture and capture plots

for (i in unique(prod_raw$unit)) {
 
  for (j in unique(prod_raw$production_source_name)) {
    
    top_species <- get_top_species(unit_selection = i, prod_source_selection = j, species_name_language = "english_name")
    
    for (k in top_species[!is.na(top_species)]) {
      
      max_value <- get_max_value(unit_selection = i, prod_source_selection = j, species_name_language = "english_name", selected_species = k)
      
      plot_data <- get_plot_data(unit_selection = i, prod_source_selection = j, species_name_language = "english_name", selected_species = k)
      
      image <- generate_chart(data = plot_data)
      
      ggsave(file = paste0("./", i,"/", j ,"/", match(k, top_species), "_", tolower(k), "_", tolower(j), "_", ifelse(max_value > 10^6, "million ", "thousand "), ifelse(k == "Tonnes - live weight", "tonnes ", "individuals "), min(plot_data$year), "-", max(plot_data$year), ".svg"), plot = image, width = 10, height = 6)
      
      cat(
        paste0("Global ", tolower(j), " of ", k, " (''", unique(plot_data$scientific_name),"'')"," in ", ifelse(max_value > 10^6, "millions of ", "thousands of "), ifelse(k == "Tonnes - live weight", "tonnes from ", "individuals from "), min(plot_data$year), " to ", max(plot_data$year), ", as reported by the [[Food and Agriculture Organization|FAO]]<ref>{{Cite web |title=Fisheries and Aquaculture - Global Production |url=https://www.fao.org/fishery/en/collection/global_production?lang=en |access-date=", Sys.Date(), "|website=Food and Agriculture Organization of the United Nations (FAO)}}</ref>"),
        file = paste0("./", i,"/", j ,"/", match(k, top_species), " ", tolower(k), ".txt")
      )
      
    }
    
  }
   
}

# Create total production plots

top_species <- get_top_species(unit_selection = "Tonnes - live weight", 
                           prod_source_selection = c("Capture production", "Aquaculture production"), 
                           species_name_language = "english_name")

for (k in top_species[!is.na(top_species)]) {
  
  max_value <- get_max_value(unit_selection = "Tonnes - live weight", 
            prod_source_selection = c("Capture production", "Aquaculture production"), 
            species_name_language = "english_name", 
            selected_species = k)
  
  plot_data <- get_plot_data(unit_selection = "Tonnes - live weight", 
                prod_source_selection = c("Capture production", "Aquaculture production"), 
                species_name_language = "english_name", 
                selected_species = k)
  
  image <- generate_chart(data = plot_data)
  
  ggsave(file = paste0("./", "Total production" ,"/", match(k, top_species), "_", tolower(k), "_", tolower("Total production"), "_", ifelse(max_value > 10^6, "million tonnes", "thousand tonnes"), "_", min(plot_data$year), "-", max(plot_data$year), ".svg"), plot = image, width = 10, height = 6)
  
  cat(
    paste0("Capture (blue) and aquaculture (green) production of ", k, " (''", unique(plot_data$scientific_name),"'')"," in ", ifelse(max_value > 10^6, "million tonnes from ", "thousand tonnes from "), min(plot_data$year), " to ", max(plot_data$year), ", as reported by the [[Food and Agriculture Organization|FAO]]. Source: [https://www.fao.org/fishery/en/fishstat FAO]."),
    file = paste0("./", "Total production" ,"/", match(k, top_species), " ", tolower(k), ".txt")
  )
  
}