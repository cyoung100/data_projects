server <- function(input, output) {
  
  ## TAB 1
  output$waiting_times_plot <- renderPlot({
    
    wait %>%
      filter(hb %in% input$hb_code_input) %>%
      filter(year < 2023) %>% 
      group_by(year_quarter, winter) %>%
      summarise(wait_count = sum(number_over12hours_episode, na.rm = TRUE)) %>%
      mutate(year_quarter = factor(year_quarter), winter = factor(winter)) %>%
      ggplot(aes(x = year_quarter, y = wait_count, fill = winter)) +
      geom_bar(stat = "identity") +
      labs(title = "Waiting Times Greater than 12 hours",
           x = "",
           y = "Number of Episodes\n",
           fill = "") +
      scale_y_continuous(labels = scales::label_number(
        scale_cut = scales::cut_short_scale())) +
      theme_minimal(base_size = default_font_base_size) +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$stays_by_sex <- renderPlot({
    
    age_sex %>%
      select(year_quarter, hb, sex, stays) %>%
      filter(hb %in% input$hb_code_input) %>% 
      group_by(year_quarter, sex) %>%
      summarise(stays_count = sum(stays)) %>%
      ungroup() %>%
      ggplot(aes(x = year_quarter, y = stays_count,
                 colour = sex, group = sex)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::label_number(
                           scale_cut = scales::cut_short_scale())) +
      theme_minimal(base_size = default_font_base_size) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom") +
      labs(title = "Stays by Sex",
           x = "",
           y = "Number of Stays\n",
           colour = "") 
  })
  
  output$stays_by_simd <- renderPlot({
    
    deprivation %>% 
      select(simd, hb, year_quarter, stays) %>%
      filter(hb %in% input$hb_code_input) %>% 
      group_by(simd, year_quarter) %>% 
      summarise(total_stays = sum(stays)) %>%
      ungroup() %>% 
      ggplot(aes(x = year_quarter, y = total_stays,
                 colour = factor(simd), group = simd)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::label_number(
                           scale_cut = scales::cut_short_scale())) +
      scale_colour_discrete(name = "SIMD (1 = most, 5 = least)") +
      theme_minimal(base_size = default_font_base_size) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom") +
      labs(
        title = "Stays by SIMD",
        subtitle = "Scottish Index of Multiple Deprivation",
        x = "",
        y = "Total Number of Stays in Days\n")
  })
  
  output$bed_occupancy_percent <- renderPlot({
    
    bed_occupancy %>% 
      filter(hb %in% input$hb_code_input) %>%
      filter(specialty_name == "All Specialties") %>% 
      group_by(year_quarter) %>% 
      summarise(percentage_occupancy = 
                  (sum(total_occupied_beddays) / sum(all_staffed_beddays)) * 100,
                specialty_name = specialty_name, season = season) %>% 
      distinct() %>% 
      ggplot(aes(x = year_quarter, y = percentage_occupancy,
                 fill = season)) +
      geom_col() +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal(base_size = default_font_base_size) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      labs(title = "Percentage Bed Occupancy",
           x = "",
           y = "Occupancy (%)\n",
           fill = "") + 
      scale_fill_manual(values = c("winter" = "#f8766d",
                                    "summer" = "#00bfc4"))
      
  })
  
  ## TAB 2 - demographics
  
  output$deprivation_title <- renderText({
    
    if(input$demog_input == "Sex") {
      title <- "Effect of COVID-19 on Different Sexes"
    }
    
    if(input$demog_input == "Age") {
      title <- "Effect of COVID-19 on Different Age Categories"
    }
    
    if(input$demog_input == "SIMD") {
      title <- "Effect of COVID-19 in Deprived Areas"
    }
    
    title
  })
  
  output$deprivation_text <- renderText({
    if(input$demog_input == "Sex") {
      text <- "Females significantly outnumber Males in stays, by around 50,000
      per quarter. There is no significant change in this statistic post-COVID.
      Nor is there a significant pattern in the length of stays, separated by sex."
    }
    
    if(input$demog_input == "Age") {
      text <- "Generally, younger people have fewer stays in hospital than
      older people. On average, older people have longer stays than younger age
      groups."
    }
    
    if(input$demog_input == "SIMD") {
      text <- "The Scottish Index of Multiple Deprivation is a relative
      measure of deprivation across Scotland. SIMD looks at the extent to which
      an area is deprived across seven domains: income, employment, education,
      health, access to services, crime and housing. In this data set, the
      ranking is split into quintiles, ranging from most deprived (ranked 1) to
      least deprived (ranked 5). \n
      Patients from the most deprived areas experience a greater number of stays;
      patients from the least deprived areas experience least stays. The reason
      for this is connecting to the domains on which SIMD is ranked. All
      quintiles experience similar trends through the pandemic."
    }
    
    text
  })
  
  
  output$stays_plot <- renderPlot({
  
    if(input$demog_input == "Sex") {
      plot <- age_sex %>%
        select(year_quarter, hb, admission_type, sex, stays) %>%
        filter(hb %in% input$hb_code_input_demog,
               admission_type == input$admission_type_input) %>% 
        group_by(year_quarter, sex) %>%
        summarise(stays_count = sum(stays)) %>%
        ungroup() %>%
        ggplot(aes(x = year_quarter, y = stays_count, colour = sex, group = sex)) +
        geom_line() +
        geom_point() +
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::label_number(
                             scale_cut = scales::cut_short_scale())) +
        theme_minimal(default_font_base_size) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom") +
        labs(title = "Number of Stays in Hospital",
             x = "",
             y = "Total Number of Stays\n",
             colour = "")
    }
    
    if(input$demog_input == "Age") {
      plot <- age_sex %>% 
        select(year_quarter, hb, admission_type, age_range, stays) %>% 
        filter(hb %in% input$hb_code_input_demog,
               admission_type == input$admission_type_input) %>% 
        group_by(year_quarter, age_range) %>% 
        summarise(stays_count =  sum(stays)) %>% 
        ungroup() %>% 
        ggplot(aes(x = year_quarter, y = stays_count, colour = age_range, group = age_range)) +
        geom_line() +
        geom_point() +
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::label_number(
                             scale_cut = scales::cut_short_scale())) +
        theme_minimal(default_font_base_size) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom") +
        labs(title = "Number of Stays in Hospital",
             x = "",
             y = "Total Number of Stays\n",
             colour = "Age")
    }
    
    if(input$demog_input == "SIMD") {
      plot <- deprivation %>% 
        select(year_quarter, hb, admission_type, stays, simd) %>%
        filter(hb %in% input$hb_code_input_demog,
               admission_type == input$admission_type_input) %>% 
        group_by(simd, year_quarter) %>% 
        summarise(total_stays = sum(stays)) %>%
        ungroup() %>% 
        ggplot(aes(x = year_quarter, y = total_stays,
                   colour = factor(simd), group = simd)) +
        geom_line() +
        geom_point() +
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::label_number(
                             scale_cut = scales::cut_short_scale())) +
        theme_minimal(default_font_base_size) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom") +
        labs(
          title = "Number of Stays in Hospital",
          x = "",
          y = "Total Number of Stays\n",
          colour = "SIMD")

    }
  
    plot
    
  })
  
  
  output$stay_length_plot <- renderPlot({
    
    if(input$demog_input == "Sex") {
      stay_length <- age_sex %>% 
        select(year_quarter, hb, admission_type, sex, average_length_of_stay) %>% 
        filter(hb %in% input$hb_code_input_demog,
               admission_type == input$admission_type_input) %>% 
        group_by(year_quarter, sex) %>% 
        summarise(mean_stay = mean(average_length_of_stay, na.rm = TRUE)) %>% 
        ungroup() %>%
        ggplot(aes(x = year_quarter, y = mean_stay,
                   group = sex, colour = sex)) +
        geom_line() +
        geom_point() +
        scale_y_continuous(limits = c(0, NA)) +
        theme_minimal(base_size = default_font_base_size) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom") +
        labs(title = "Length of Stay",
             x = "",
             y = "Average Number of Days\n",
             colour = "")
    }
    
    if(input$demog_input == "Age") {
      stay_length <- age_sex %>% 
        select(year_quarter, hb, admission_type, age_range, average_length_of_stay) %>% 
        filter(hb %in% input$hb_code_input_demog,
               admission_type == input$admission_type_input) %>% 
        group_by(year_quarter, age_range) %>% 
        summarise(mean_stay =  mean(average_length_of_stay, na.rm = TRUE)) %>% 
        ungroup() %>% 
        ggplot(aes(x = year_quarter, y = mean_stay,
                   colour = age_range, group = age_range)) +
        geom_line() +
        geom_point() +
        theme_minimal(base_size = default_font_base_size) +
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom") +
        labs(title = "Length of Stay",
             x = "",
             y = "Average Number of Days\n",
             colour = "Age")
    }
    
    if(input$demog_input == "SIMD") {
      stay_length <- deprivation %>% 
        select(year_quarter, hb, admission_type, simd, average_length_of_stay) %>% 
        filter(hb %in% input$hb_code_input_demog,
               admission_type == input$admission_type_input) %>% 
        group_by(year_quarter, simd) %>% 
        summarise(mean_stay = mean(average_length_of_stay, na.rm = TRUE)) %>% 
        ungroup() %>%
        ggplot(aes(x = year_quarter, y = mean_stay, group = simd, colour = as.factor(simd))) +
        geom_line() +
        geom_point() +
        scale_y_continuous(limits = c(0, NA)) +
        theme_minimal(base_size = default_font_base_size) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom") +
        labs(title = "Length of Stay",
             x = "",
             y = "Average Number of Days\n",
             colour = "SIMD")
    }
    
    if(input$admission_type_input == "All Day cases") {
        stay_length <- ""
    }
    
    stay_length
    
    })
  
  

  
  ## TAB 3 - geographic
  
  output$wait_plot <- renderPlot({
    
    p <- wait %>%
      filter(health_board %in% input$health_board) %>%
      group_by(year_quarter, winter) %>%
      summarise(wait_count = sum(get(input$wait_duration), na.rm = TRUE)) %>%
      mutate(year_quarter = factor(year_quarter), winter = factor(winter)) %>%
      ggplot(aes(x = year_quarter, y = wait_count, fill = winter)) +
      geom_bar(stat = "identity") +
      labs(y = "Episodes", x = "",
           title = "Waiting Times",
           fill = "") +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::label_number(
                           scale_cut = scales::cut_short_scale())) +
      theme_minimal(base_size = default_font_base_size) +
      theme(legend.position = "bottom")+
      scale_fill_discrete(labels = c("Winter", "Non-Winter")) +
      coord_flip()
    
    p
  })
  
  output$wait_map <- renderPlot({
    
    filtered_map <- waiting_map %>% 
      filter(year %in% input$selected_year,
             health_board %in% input$health_board)
    
    m <- ggplot(filtered_map, aes_string(fill = input$wait_duration)) +
      geom_sf(colour = "black") +
      theme_minimal(base_size = default_font_base_size) +
      theme(panel.grid.major = element_blank(),
            axis.text = element_blank(),
            legend.position = "bottom",
            legend.key.width = unit(2, "cm")) +  
      labs(
        fill = "Episodes",
        title = "Seasonal comparison of waiting times") +
      viridis::scale_fill_viridis(option = "magma", direction = -1) +
      facet_wrap(~winter) +
      guides(size = 10)
    
    print(m)
  
  })
  
  ## TAB 4 - beds
  
  output$bed_occupancy_total <- renderPlot({
    
    bed_occupancy %>% 
      filter(hb %in% input$hb_code_input_beds) %>%
      filter(specialty_name == input$specialty_name) %>% 
      group_by(year_quarter, specialty_name) %>% 
      summarise(total_occupied_beddays = sum(total_occupied_beddays),
                all_staffed_beddays = sum(all_staffed_beddays)
      ) %>% 
      ggplot(aes(x = year_quarter, group = specialty_name)) +
      geom_line(aes(y = total_occupied_beddays, colour = "Occupied Beddays")) +
      geom_point(aes(y = total_occupied_beddays, colour = "Occupied Beddays")) +
      geom_line(aes(y = all_staffed_beddays, colour = "Staffed Beddays")) +
      geom_point(aes(y = all_staffed_beddays, colour = "Staffed Beddays")) +
      scale_y_continuous(labels = scales::label_number(
                           scale_cut = scales::cut_short_scale())) +
      
      theme_minimal(base_size = default_font_base_size) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Bed Occupancy",
           x = "\nQuarter ending",
           y = "Number of Beddays\n") + 
      scale_colour_manual(name = "Beddays", 
                          labels = c("Occupied", "Staffed"),
                          values = c("Staffed Beddays" = "seagreen",
                                     "Occupied Beddays" = "red"))
    
  })
  
}