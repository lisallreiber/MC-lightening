gen_sc_table_simple <- function(data = fake_survey,
                                sc_var = "Q1_SC_fav_lang") {
  
  sc_table_simple <- data %>% janitor::tabyl(!!sym({{sc_var}})) 
  return(sc_table_simple)
}

gen_sc_table_fancy <- function(data = fake_survey,
                               sc_var = "Q1_SC_fav_lang", 
                               sc_label = "Favourite Language"
) {
  
  sc_table_fancy <- data %>% 
    drop_na(!!sym({{sc_var}})) %>% 
    count("{ sc_label }" := !!sym({{sc_var}}), name = "Frequency", sort = TRUE) %>% 
    mutate("Proportion" = round(Frequency / sum(Frequency) * 100, 1),
           "Prop in Percent" = paste0(Proportion, "%"))
  
  return(sc_table_fancy)
}

gen_sc_table <- function(data = fake_survey,
                         sc_var = "Q1_SC_fav_lang", 
                         sc_label = "Favourite Language",
                         type = c("simple", "fancy")
) {
  
  if (type == "simple") {
    
    sc_table <- data %>% 
      gen_sc_table_simple(sc_var = sc_var)
    return(sc_table)
    
  } else if (type == "fancy") {
    
    sc_table <- data %>% 
      gen_sc_table_fancy(sc_var = sc_var,
                         sc_label = sc_label)
    return(sc_table)
    
  }
  
}