



deploy_df <- readxl::read_excel("./data/workbook_r.xlsx", sheet=2) %>% 
  
  unite("author", author, year, sep=" ", na.rm =TRUE, remove=FALSE) %>% 
  filter(author %in% c("sharp 2008", "lester 2010", "rintamaki 2012", "warr 2013")) %>% 
  pivot_longer(cols = end_pre_per:frequency_days_wk,
               names_to = "phys_time",
               values_to = "value") %>% 
  dplyr::select(author, phys_time, value) %>% 
  filter(phys_time %in% c("end_pre_per", "end_dur_per", "str_pre_per", "str_dur_per")) %>% 
  mutate(cate = ifelse(phys_time == "end_pre_per", "END",
                       ifelse(phys_time == "end_dur_per", "END", "STR")))
  
  
  

  
  #mutate(author = fct_reorder(author, desc(delayed_measurements)))
  



## plot duration ##########

 ggplot(deploy_df, aes(value, phys_time, color = author)) + 
  labs(x = "Phys", y= "Author/Year") +
  
  #geom_vline(xintercept = 1, color = "gray40", lty = 2) +
  #geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0, size = 0.3) + 
  
  geom_bar(stat="identity") +
  #geom_text(aes(label=country), hjust=0.9, angle = 0) +
  
  #geom_point(shape = 24, fill = group.study.color[3]) +
  
  #  annotate("rect", xmin = 1.05, xmax = 1.8, ymin = 0.8, ymax = 1.2,  alpha = .2) +
  
   facet_grid(cate~author) +
  
  #scale_color_manual(values = c("gray50", "gray10")) +
  #scale_x_continuous(limits = c(0.8, 2), 
  #                   expand = c(0, 0), 
  #                   breaks = c(0.8, 1, 1.2, 1.4, 1.6, 1.8, 2), 
#                   labels = c("",  1, "",   1.4, "", 1.8, "")) +


coord_flip() +

dissertation_theme() +
  theme(strip.background = element_rect(color = "white", fill = "white"), 
        strip.text = element_text(size = 7),
        #axis.title.y = element_blank()  , 
        #axis.line.y = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle=0, v=0.5, size=10), 
        axis.title.x = element_text(angle=0, v=0.5, size=10), 
        legend.position = "none")
