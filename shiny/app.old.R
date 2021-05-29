{
  library(shiny)
  library(shinydashboard)
  
  library(tidyverse)
  library(zoo)
  library(tidyquant)
  library(ggwordcloud)
  library(ggforce)
  library(cowplot)
  library(tidytext)
  library(scales)
  library(reactable)
  library(boot)
  
 
  tidy_topics_all <- list.files(path = "data/", pattern = "tidy_topics_\\d", full.names = T, include.dirs=T, all.files =T) %>% 
    as_tibble() %>% 
    mutate(posts = map(value, read_csv)) %>% 
    mutate(level = str_extract(value, "\\d")) %>% 
    select(posts, level)
  
  tidy_topic_docs_all <- list.files(path = "data/", pattern = "tidy_topics_docs", full.names = T, include.dirs=T, all.files =T) %>% 
    as_tibble() %>% 
    mutate(posts = map(value, read_csv)) %>% 
    mutate(level = str_extract(value, "\\d")) %>% 
    select(posts, level)
  
  topic_from_word <- function(my_word, my_level){
    tidy_topics_all %>%
      filter(level == my_level) %>% 
      unnest(posts) %>% 
      filter(word == my_word) %>% 
      pull(topic) %>% 
      return()
  }
  
  all_subs <- tidy_topic_docs_all %>%
    filter(level == max(level)) %>% 
    unnest(posts) %>% 
    pull(Sub) %>% 
    unique()
  
  
  plot_sub_graph_density <- function(track_subs, my_word, round, my_level){
    
    # # Comment out
    # track_subs <- c("Adelaide", "Sydney", "Melbourne", "Liverpool", "London")
    # my_word <- "mental"
    # my_level <- 0
    #-------------------------
    
    # Get topic from input word
    my_topic <- topic_from_word(my_word, my_level)
    
    # # Unused Code
    # # Plot: Proportion containing mention of word
    # tidy_topic_docs_all %>% 
    #   filter(level == my_level) %>% 
    #   unnest(posts) %>% 
    #   select(topic, Sub, Date, p) %>% 
    #   dplyr::filter(topic == my_topic) %>% 
    #   mutate(Sub = ifelse(Sub %in% track_subs, Sub, "Other")) %>% 
    #   mutate(Date = as_date(Date))%>% 
    #   mutate(Year = Date %>% year %>% as.character) %>% 
    #   mutate(Month = Date %>% month %>% as.character) %>% 
    #   group_by(Year, Month) %>%
    #   mutate(Date = ymd(str_replace(Date,"2019", "2020"))) %>% 
    #   mutate(Date = floor_date(Date, unit = "month")) %>% 
    #   mutate(About = p > 0) %>% 
    #   group_by(Sub, Month, Year, Date) %>% 
    #   summarise(p = mean(About)) %>% 
    #   ggplot() +
    #   theme_minimal() + 
    #   geom_bar(aes(x = Date, y = p, fill = Year), stat = "identity", position = "dodge") + 
    #   scale_x_date(date_breaks = "months", date_labels = "%b") +
    #   coord_x_date(xlim = c(as.Date("2020-01-01"),as.Date("2020-12-01")), expand = T) +
    #   facet_wrap(.~Sub, ncol = 3, scales = "free_x")
    
    # # Plot: Loess
    # span = 1
    # tidy_topic_docs_all %>% 
    #   filter(level == my_level) %>% 
    #   unnest(posts) %>% 
    #   select(topic, Sub, Date, p) %>% 
    #   dplyr::filter(topic == my_topic) %>% 
    #   mutate(Sub = ifelse(Sub %in% track_subs, Sub, "Other")) %>% 
    #   mutate(Date = as_date(Date))%>% 
    #   mutate(Year = Date %>% year %>% as.character) %>% 
    #   mutate(Month = Date %>% month %>% as.character) %>% 
    #   group_by(Year, Month) %>%
    #   mutate(Date = ymd(str_replace(Date,"2019", "2020"))) %>% 
    #   ggplot() +
    #   theme_minimal() + 
    #   geom_smooth(aes(x = Date, y = p, color = Year), method = "loess", span = span) + 
    #   scale_x_date(date_breaks = "months", date_labels = "%b") +
    #   coord_x_date(xlim = c(as.Date("2020-01-01"),as.Date("2020-12-31")), expand = T) +
    #   facet_wrap(.~Sub, ncol = 3, scales = "free_x"

    # # Plot: Violin
    # tidy_topic_docs_all %>% 
    #   filter(level == my_level) %>% 
    #   unnest(posts) %>% 
    #   select(topic, Sub, Date, p) %>% 
    #   dplyr::filter(topic == my_topic) %>% 
    #   mutate(Sub = ifelse(Sub %in% track_subs, Sub, "Other")) %>% 
    #   mutate(Date = as_date(Date))%>% 
    #   mutate(Year = Date %>% year %>% as.character) %>% 
    #   mutate(Month = Date %>% month %>% as.character) %>% 
    #   group_by(Year, Month) %>%
    #   mutate(Date = ymd(str_replace(Date,"2019", "2020"))) %>% 
    #   mutate(Date = floor_date(Date, unit = "month")) %>%
    #   ungroup() %>% 
    #   ggplot() +
    #   theme_minimal() + 
    #   geom_violin(aes(x = Date, group = interaction(Year,Date), y = p, fill = Year), alpha = 0.5) + 
    #   scale_x_date(date_breaks = "months", date_labels = "%b") +
    #   coord_x_date(xlim = c(as.Date("2020-01-01"),as.Date("2020-12-01")), expand = T) +
    #   facet_wrap(.~Sub, ncol = 3, scales = "free_x") + 
    #   labs(x = "Month", y = "Density")
      
    # Plot: Bar
    plot_dens <- tidy_topic_docs_all %>% 
      filter(level == my_level) %>% 
      unnest(posts) %>% 
      select(topic, Sub, Date, p) %>% 
      dplyr::filter(topic == my_topic) %>% 
      mutate(Sub = ifelse(Sub %in% track_subs, Sub, "Other")) %>% 
      mutate(Date = as_date(Date))%>% 
      mutate(Year = Date %>% year %>% as.character) %>% 
      mutate(Month = Date %>% month %>% as.character) %>% 
      group_by(Year, Month) %>%
      mutate(Date = ymd(str_replace(Date,"2019", "2020"))) %>% 
      mutate(Date = floor_date(Date, unit = round)) %>% 
      group_by(Sub, Year, Date) %>% 
      summarise(mean_p = mean(p, na.rm = TRUE), error = qnorm(0.975, 0,1)*sqrt(mean(p)*(1-mean(p))/(n())), n = n()) %>% 
      mutate(lwr = max(0,mean_p - error), upr = mean_p + error) %>% 
      ggplot() +
      theme_minimal() + 
      geom_bar(aes(x = Date, y = mean_p, fill = Year), stat = "identity", position = "identity", alpha = 0.5) + 
      #geom_errorbar(aes(x = Date, y = mean_p, ymin=lwr, ymax=upr,color = Year), position = "identity", alpha = 0.5) +
      scale_x_date(date_breaks = "months", date_labels = "%b") +
      coord_x_date(xlim = c(as.Date("2020-01-01"),as.Date("2020-12-01")), expand = T) +
      facet_wrap(.~Sub, ncol = 3, scales = "free_x") + 
      labs(x = "Month", y = "Density") + 
    	labs(title = paste("Topic density each ", round," for '",my_word,"' topic", sep = ""))
    
      return(plot_dens)
  } 
  
  plot_sub_graph_n_posts <- function(track_subs, my_level, round){
    
    # Plot posts per time
    plot_posts <- tidy_topic_docs_all %>% 
      filter(level == my_level) %>% 
      unnest(posts) %>% 
      select(Sub, Date, count) %>%
      mutate(Sub = ifelse(Sub %in% track_subs, Sub, "Other")) %>% 
      mutate(Date = as_date(Date))%>% 
      mutate(Year = Date %>% year %>% as.character) %>% 
      mutate(Month = Date %>% month %>% as.character) %>% 
      mutate(Date = ymd(str_replace(Date,"2019", "2020"))) %>% 
      mutate(Date = floor_date(Date, unit = round)) %>% 
      group_by(Sub, Date, Year) %>% 
      summarise(count = sum(count)) %>% 
      ggplot() + 
      theme_minimal() +
      geom_bar(aes(x = Date, y = count, fill = Year), stat = "identity", position = "identity", alpha = 0.5) + 
      scale_x_date(date_breaks = "months", date_labels = "%b") +
      coord_x_date(xlim = c(as.Date("2020-01-01"),as.Date("2020-12-01")), expand = T) +
      facet_wrap(.~Sub, ncol = 3, scales = "free_x") + 
      labs(x = "Month", y = "Number of Posts") + 
    	labs(title = paste("Number of posts per ", round, sep = ""))
    
    return(plot_posts)
  }
  
  plot_sub_graph_wc <- function(my_word, my_level){
    
    my_topic <- topic_from_word(my_word, my_level)
    
    plot_wc <- tidy_topics_all %>%
      filter(level == my_level) %>% 
      unnest(posts) %>% 
      filter(topic == my_topic) %>% 
      select(topic, p, word) %>% 
      mutate(p2 = ifelse(word == my_word, Inf, p),
             is_my_word = ifelse(word == my_word, 1, 0)) %>% 
      top_n(150,p2) %>% 
      mutate(p = p/max(p)) %>% 
      ungroup() %>% 
      ggplot(
        aes(
          label = word, size = p,
          color = is_my_word
        )
      ) +
      geom_text_wordcloud_area(rm_outside = T,grid_size = 20) +
      scale_size_area(max_size = 20) +
      theme_minimal() +
      labs(title = paste("Topic: ", my_topic))
    
    return(plot_wc)
  }
  
  plot_sub_graph_wc_from_topic <- function(my_topic){
    
    plot_wc <- tidy_topics_all %>%
      filter(level == 0) %>% 
      unnest(posts) %>% 
      filter(topic == my_topic) %>% 
      select(topic, p, word) %>% 
      top_n(50,p) %>% 
      mutate(p = p/max(p)) %>% 
      ungroup() %>% 
      ggplot(
        aes(
          label = word, size = p	      
        )
      ) +
      geom_text_wordcloud_area(rm_outside = T) +
      #scale_size_area(max_size = ) +
      theme_minimal() +
      labs(title = paste("Topic: ", my_topic))
    
    return(plot_wc)
  }
  
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(
      	"Controls", tabName = "dashboard", icon = icon("dashboard"))
    ),
    textInput("my_word", "Focus word:", value =  "mental", width = "80%"),
    sliderInput("my_level", "Topic Level (lower gives more specific topics):", value = 0, min = 0, max = 2, step = 1, width = "80%"),
    selectInput("round", "Bar Width", choices = c("week", "month", "bimonth", "halfyear", "year"),
                multiple = FALSE, selected = "week", width = "80%"),
    checkboxGroupInput("track_subs", 
                       "Subs to include:",
                       all_subs,
                       width = "80%",
    									 selected = all_subs)
  )
  
  box_wc <- box(
    side = "right", height = "auto", width = "auto",
    plotOutput("plot_wc", height = "1000px")
  )
  
  box_empty <- box(
    side = "right", height = "auto", width = "auto"
  )
  
  box_dens <- box(
    side = "right", height = "auto",width = "auto",
    plotOutput("plot_dens", height = "1000px")
  )
  
  box_n_posts <- box(
    side = "right", height = "auto", width = "auto",
    plotOutput("plot_n_posts", height = "1000px")
  )

  body <- dashboardBody(
    fluidRow(
      box_wc
    ),
    fluidRow(
      box_dens
    ),
    fluidRow(
    	box_n_posts
    )
  )
  
  ui <- dashboardPage(
    dashboardHeader(title = "Prostate Cancer"),
    sidebar,
    body,
    skin = "purple"
  )
  
  server <- function(input, output) {
    
    output$menuitem <- renderMenu({
      menuItem("Menu item", icon = icon("calendar"))
    })
    
    ### Plot: Topic density
    output$plot_dens <- renderPlot({
      plot_sub_graph_density(
        track_subs = input$track_subs, 
        my_word = input$my_word,
        round = input$round,
        my_level = input$my_level
      )
    },res = 100)
    
    ### Plot: Number of posts per unit time
    output$plot_n_posts <- renderPlot({
      plot_sub_graph_n_posts(
        track_subs = input$track_subs, 
        round = input$round,
        my_level = input$my_level
      )
    },res = 100)
    
    ### Plot: Topic wordcloud
    output$plot_wc <- renderPlot({
      plot_sub_graph_wc(
        my_word = input$my_word,
        my_level = input$my_level
      )
    },res = 100)
    
    output$plot_wc_1 <- renderPlot({
      plot_sub_graph_wc_from_topic(
        my_topic = input$my_topic_1		  
      )
    },res = 100)
    
    output$plot_wc_2 <- renderPlot({
      plot_sub_graph_wc_from_topic(
        my_topic = input$my_topic_2	  
      )
    },res = 100)

  } 
  shinyApp(ui, server)
}

