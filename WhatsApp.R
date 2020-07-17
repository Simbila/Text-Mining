# WhatsApp
    # Author: Wilbert J. Simbila
    # July, 2020


    # create working directory -----------------------------------------------
    setwd("~")
    dir.create("WhatsApp", showWarnings = FALSE)
    setwd("./WhatsApp")
    
    # Install required packages
    # install.packages("rwhatsapp", "tidyverse", "tidymodels", "scales", "lubridate", "wordcloud", "tidytext'", dependencies = TRUE)
    suppressPackageStartupMessages({
      library(rwhatsapp)
      library(tidyverse)
      library(tidymodels)
      library(scales)
      library(lubridate)
      library(wordcloud)
      library(tidytext)
    })
    
    
    # load and read data -------------------------------------------------------
    chat <- rwa_read("nkanwachat.txt")
    whatschat <- chat %>% 
      filter(!is.na(author)) %>% 
      filter(!str_detect(author, pattern = "Family")) %>% 
      separate(time, into = c("Date", "Time"), sep = " ") %>% 
      select(-Time) %>% 
      mutate(Date = ymd(Date)) %>%
      count(author) %>% 
      mutate(proportional = n/sum(n), 
             author = fct_reorder(author, proportional)) %>% 
      view()
      
      # Bar plot --------------------------------------------------------------
      ggplot(data = whatschat, aes(author, proportional)) +
      geom_col(fill = "dodgerblue") +
      geom_text(aes(label = n), hjust = -0.5, size = 2.5) +
      coord_flip() + 
      scale_y_continuous(name = "Participation Percentage", breaks = breaks_width(0.05), 
                         labels = label_percent()) +
        scale_x_discrete(name = "Participants") +
      labs(title = "NKANWA FAMILY WHATSAPP GROUP MEMBER'S PARTICIPATIONS", subtitle = "Date from 24 May - 16 July, 2020", caption = "simbillah@gmail.com") +
      theme_light()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
        theme(plot.subtitle = element_text(hjust = 0.5))
      
      
      # Wordcloud --------------------------------------------------------------
      set.seed(123)
      wordcloud(words = whatschat$author, freq = whatschat$n, min.freq = 1,
                max.words = 43, random.order = FALSE, rot.per = 0.2,
                colors = brewer.pal(8, "Dark2"))
      
    #
    stop_words <- stop_words %>% 
      rename(words = word) 
    
    # Chat frequency for individual participant -------------------------------- 
      chat_df <- chat %>% 
        filter(author == "Clement Nkanwa") %>% 
        #mutate(text = as.character(text)) %>% 
        unnest_tokens(input = text, output = words) %>% 
        anti_join(stop_words) %>% 
        count(words,sort = TRUE) %>% 
        filter(!str_detect(words, pattern = "omitted"), 
               !str_detect(words, pattern = "http"),
               !str_detect(words, pattern = "â"),
               !str_detect(words, pattern = "image"),
               !str_detect(words, pattern = "[0-9]"),
               n > 5) %>% 
        mutate(proportional = n/sum(n),
               words = fct_reorder(words, proportional)) %>% 
      view()
      
      # Plot Bar Chart for for individual participant Whatsapp Chat -----------------------------------
      ggplot(data = chat_df, aes(words, proportional)) +
        geom_col(fill = "dodgerblue") +
        geom_text(aes(label = n), hjust = -0.5, size = 2.5) +
        coord_flip() + 
        scale_y_continuous(name = "Participation Percentage", 
                           labels = label_percent()) +
        scale_x_discrete(name = "Participants") +
        labs(title = "CLEMENT LUKALA MOST WORDS USED CHATTING IN TEAM MAPPING WAHTSAPP GROUP", subtitle = "Date from 24 May - 13 July, 2020", caption = "simbillah@gmail.com") +
        theme_light()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
        theme(plot.subtitle = element_text(hjust = 0.5))
      
      
    # Word cloud group participation ------------------------------------
      set.seed(123)
      wordcloud(words = chat_df$words, freq = chat_df$n, min.freq = 1,
                max.words = 3000, random.order = FALSE, rot.per = 0.2,
                colors = brewer.pal(8, "Dark2"))
      
    
