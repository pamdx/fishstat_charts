##### UPDATE DATA INPUTS #####

library(readr)
library(dplyr)
library(janitor)
library(ggplot2)

# PARAMETERS

prod_URL <- "https://www.fao.org/fishery/static/Data/GlobalProduction_2024.1.0.zip" # URL of the FAO production data
asfis_URL <- "https://www.fao.org/fishery/static/ASFIS/ASFIS_sp.zip" # URL of the ASFIS database of species names
number_species <- 200 # number of top species to create charts for
capture_color = "#377eb8"
aquaculture_color = "#4daf4a"

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
species_multilingual <- read_csv(unz(temp, "ASFIS_sp_2023.txt")) %>%
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

for (i in unique(prod_raw$production_source_name)) {
 
  top_species <- prod_raw %>%
    filter(unit == "Tonnes - live weight", year == max(year), production_source_name == i) %>%
    group_by(english_name) %>%
    summarize(value = sum(value)) %>%
    arrange(desc(value)) %>%
    head(number_species) %>%
    pull(english_name)
  
  for (j in top_species[!is.na(top_species)]) {
    
    max_value <- prod_raw %>%
      filter(unit == "Tonnes - live weight", production_source_name == i, english_name == j) %>%
      group_by(english_name, year) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      summarize(value = max(value)) %>%
      pull()
    
    plot_data <- prod_raw %>%
      filter(unit == "Tonnes - live weight", production_source_name == i, english_name == j) %>%
      group_by(english_name, scientific_name, year) %>%
      summarize(value = sum(value)) %>%
      {if(max_value > 10^6) mutate(., value = value/10^6) else mutate(., value = value/10^3)}
    
    image <- ggplot(data = plot_data, aes(x = year, y = value)) +
      geom_col(fill = ifelse(i == "Capture production", capture_color, aquaculture_color)) +
      scale_x_continuous(breaks = seq(min(plot_data$year), max(plot_data$year), 5), expand = c(0, 0.2)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_bw() +
      theme(
        text = element_text(size = 17),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()
      )
    
    ggsave(file = paste0("./", i ,"/", match(j, top_species), "_", tolower(j), "_", tolower(i), "_", ifelse(max_value > 10^6, "million tonnes, ", "thousand tonnes, "), min(plot_data$year), "-", max(plot_data$year), ".svg"), plot = image, width = 10, height = 6)
    
    cat(
      paste0("Global ", tolower(i), " of ", j, " (''", unique(plot_data$scientific_name),"'')"," in ", ifelse(max_value > 10^6, "million tonnes from ", "thousand tonnes from "), min(plot_data$year), " to ", max(plot_data$year), ", as reported by the [[Food and Agriculture Organization|FAO]]. Source: [https://www.fao.org/fishery/en/fishstat FAO]."),
      file = paste0("./", i ,"/", match(j, top_species), " ", tolower(j), ".txt")
    )
    
  }
   
}

# Create total production plots

top_species <- prod_raw %>%
  filter(unit == "Tonnes - live weight", year == max(year)) %>%
  group_by(english_name) %>%
  summarize(value = sum(value)) %>%
  arrange(desc(value)) %>%
  head(number_species) %>%
  pull(english_name)

for (k in top_species[!is.na(top_species)]) {
  
  max_value <- prod_raw %>%
    filter(unit == "Tonnes - live weight", english_name == k) %>%
    group_by(english_name, year) %>%
    summarize(value = sum(value)) %>%
    ungroup() %>%
    summarize(value = max(value)) %>%
    pull()
  
  plot_data <- prod_raw %>%
    filter(unit == "Tonnes - live weight", english_name == k) %>%
    group_by(english_name, scientific_name, production_source_name, year) %>%
    summarize(value = sum(value)) %>%
    {if(max_value > 10^6) mutate(., value = value/10^6) else mutate(., value = value/10^3)}
  
  image <- ggplot(data = plot_data, aes(x = year, y = value)) +
    geom_bar(fill = production_source_name, position = "stack", stat = "identity") +
    scale_x_continuous(breaks = seq(min(plot_data$year), max(plot_data$year), 5), expand = c(0, 0.2)) +
    scale_y_continuous(expand = c(0, 0)) + 
    scale_fill_manual(breaks = c("Capture production", "Aquaculture production"), values=c(capture_color, aquaculture_color)) +
    theme_bw() +
    theme(
      text = element_text(size = 17),
      panel.border = element_blank(),
      panel.background = element_blank(),
      panel.grid.major.x = element_blank(), 
      panel.grid.minor.x = element_blank(), 
      axis.title.x = element_blank(), 
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )
  
  ggsave(file = paste0("./", "Total production" ,"/", match(k, top_species), "_", tolower(k), "_", tolower("Total production"), "_", ifelse(max_value > 10^6, "million tonnes", "thousand tonnes"), "_", min(plot_data$year), "-", max(plot_data$year), ".svg"), plot = image, width = 10, height = 6)
  
  cat(
    paste0("Capture (blue) and aquaculture (green) production of ", k, " (''", unique(plot_data$scientific_name),"'')"," in ", ifelse(max_value > 10^6, "million tonnes from ", "thousand tonnes from "), min(plot_data$year), " to ", max(plot_data$year), ", as reported by the [[Food and Agriculture Organization|FAO]]. Source: [https://www.fao.org/fishery/en/fishstat FAO]."),
    file = paste0("./", "Total production" ,"/", match(k, top_species), " ", tolower(k), ".txt")
  )
  
}