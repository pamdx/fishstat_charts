get_top_species <- function(unit_selection, prod_source_selection, species_name_language){
  
  prod_raw %>%
    filter(unit == unit_selection, year == max(year), production_source_name %in% prod_source_selection) %>%
    group_by(!! sym(species_name_language)) %>%
    summarize(value = sum(value)) %>%
    arrange(desc(value)) %>%
    head(number_species) %>%
    pull(!! sym(species_name_language))
  
}

get_max_value <- function(unit_selection, prod_source_selection, species_name_language, selected_species){
  
  prod_raw %>%
    filter(unit == unit_selection, production_source_name %in% prod_source_selection, !! sym(species_name_language) == selected_species) %>%
    group_by(!! sym(species_name_language), year) %>%
    summarize(value = sum(value)) %>%
    ungroup() %>%
    summarize(value = max(value)) %>%
    pull()
  
}

get_plot_data <- function(unit_selection, prod_source_selection, species_name_language, selected_species){
  
  prod_raw %>%
  filter(unit == unit_selection, production_source_name %in% prod_source_selection, !! sym(species_name_language) == selected_species) %>%
  group_by(!! sym(species_name_language), scientific_name, production_source_name, year) %>%
  summarize(value = sum(value)) %>%
    {if(max_value > 10^6) mutate(., value = value/10^6) else mutate(., value = value/10^3)}
  
}

generate_chart <- function(data){
  
  ggplot(data = data, aes(fill = production_source_name, x = year, y = value)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_x_continuous(breaks = seq(min(data$year), max(data$year), 10), expand = c(0, 0.2)) +
    scale_y_continuous(expand = c(0, 0)) + 
    scale_fill_manual(breaks = c("Capture production", "Aquaculture production"), values = c(capture_color, aquaculture_color)) +
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
      axis.text.x = element_text(vjust = -1),
      legend.position = "none"
    )
  
}