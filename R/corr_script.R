


df_cor <- read_excel("./data/workbook_r.xlsx", sheet = 3)



### Strength

df_plot_s <- df_cor %>% 
  
  pivot_longer(cols = shoting:fast_moveing,
               names_to = "task",
               values_to = "corr") %>% 
  dplyr::select(-c(exercise, test)) %>% 
  filter(mode != "Power") %>% 
  filter(mode != "Strength_end") %>% 
  unite("author", author, year, sep = " ") %>% 
  mutate(taskname = factor(task, levels= c("shoting", "3.2kmrun", "3.2loaded", "2milerun", "2mileLoadedRun", "jump_performance", 
                                              "lift_lower_repeat", "load_carry", "loaded_march", "loaded_casualty","total_load", "crawl", 
                                              "dig", "fast_moveing"),
                              
                              labels= c("Marksmanship", "3.2 km run", "3.2 km Loaded Run", "2 mile run", "2 mile Loaded Run", "Jump skills",
                                        "Lift-Lower repeat", "Load carry", "Loaded march", "Casualty drag","Total load", "Crawl", "Dig", "Move fast"))) %>% 
filter(!taskname %in% c("2 mile run", "2 mile Loaded Run")) %>%

  filter(mode == "Strength")

  

plotstr <- ggplot(df_plot_s, aes(corr, author, color = author)) + 
  labs(x = "correlation with Strength", y= "Author/Year") +
  
  #geom_vline(xintercept = 1, color = "gray40", lty = 2) +
  #geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0, size = 0.3) + 
  
  geom_point(shape = 24, size= 1.8, fill = group.study.color[3]) +
  
  #  annotate("rect", xmin = 1.05, xmax = 1.8, ymin = 0.8, ymax = 1.2,  alpha = .2) +
  
  # facet_wrap(  comparison ~ .) +
  
  #scale_color_manual(values = c("gray50", "gray10")) +
  scale_x_continuous(limits = c(-1, 1), 
                     expand = c(0, 0), 
                     breaks = c(-0.7, -0.5, -0.4, -0.2, 0, 0.2, 0.4, 0.5, 0.7), 
                   labels = c(-0.7, "",  -0.4, "",  0, "", 0.4, "", 0.7)) +
  
geom_vline(xintercept = 0.4, linetype="dashed", color = "red", size=0.2) +
  geom_vline(xintercept = -0.4, linetype="dashed", color = "blue", size=0.2) +  
  
  
coord_flip() +

  facet_grid(~taskname) +
    
  dissertation_theme() +
  theme(legend.position = "none",
    strip.background = element_rect(color = "white", fill = "white"), 
        strip.text = element_text(size = 10, angle = 90),
        #axis.title.y = element_blank()  , 
        #axis.line.y = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 10),
        #axis.text.x = element_text(angle=45, v=0.5, size=2), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank())
       
  


saveRDS(plotstr, file="./derived/corr_STR.RDS")




### Endurance

df_plot_e <- df_cor %>% 
  
  pivot_longer(cols = shoting:fast_moveing,
               names_to = "task",
               values_to = "corr") %>% 
  dplyr::select(-c(exercise, test)) %>% 
  filter(mode != "Power") %>% 
  filter(mode != "Strength_end") %>% 
  unite("author", author, year, sep = " ") %>% 
  mutate(taskname = factor(task, levels= c("shoting", "3.2kmrun", "3.2loaded", "2milerun", "2mileLoadedRun", "jump_performance", 
                                           "lift_lower_repeat", "load_carry", "loaded_march", "loaded_casualty","total_load", "crawl", 
                                           "dig", "fast_moveing"),
                           
                           labels= c("Marksmanship", "3.2 km run", "3.2 km Loaded Run", "2 mile run", "2 mile Loaded Run", "Jump skills",
                                     "Lift-Lower repeat", "Load carry", "Loaded march", "Casualty drag","Total load", "Crawl", "Dig", "Move fast"))) %>% 
  filter(!taskname %in% c("2 mile run", "2 mile Loaded Run")) %>%
  
  filter(mode == "Aerobic")

plotend <- ggplot(df_plot_e, aes(corr, author, color = author)) + 
  labs(x = "correlation with Endurance", y= "Author/Year") +
  
  #geom_vline(xintercept = 1, color = "gray40", lty = 2) +
  #geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0, size = 0.3) + 
  
  geom_point(shape = 24, size = 1.8, fill = group.study.color[3]) +
  
  #  annotate("rect", xmin = 1.05, xmax = 1.8, ymin = 0.8, ymax = 1.2,  alpha = .2) +
  
  # facet_wrap(  comparison ~ .) +
  
  #scale_color_manual(values = c("gray50", "gray10")) +
  scale_x_continuous(limits = c(-1, 1), 
                     expand = c(0, 0), 
                     breaks = c(-0.7, -0.5, -0.4, -0.2, 0, 0.2, 0.4, 0.5, 0.7), 
                     labels = c(-0.7, "",  -0.4, "",  0, "", 0.4, "", 0.7)) +
  
  geom_vline(xintercept = 0.4, linetype="dashed", color = "red", size=0.2) +
  geom_vline(xintercept = -0.4, linetype="dashed", color = "blue", size=0.2) +  
  
  
  coord_flip() +
  
  facet_grid(~taskname) +
  
  dissertation_theme() +
  theme(legend.position = "none",
        strip.background = element_rect(color = "white", fill = "white"), 
        strip.text = element_text(size = 10, angle = 90),
        #axis.title.y = element_blank()  , 
        #axis.line.y = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 10),
        #axis.text.x = element_text(angle=45, v=0.5, size=2), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank())


saveRDS(plotend, file="./derived/corr_END.RDS")
