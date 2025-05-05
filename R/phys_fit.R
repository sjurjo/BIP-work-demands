


deploy_df <- readxl::read_excel("./data/workbook_r.xlsx", sheet=2) %>% 
  
  unite("author", author, year, sep=" ", na.rm =TRUE, remove=FALSE) %>% 
  
  #mutate(author = fct_reorder(author, desc(delayed_measurements)))
  
  arrange(desc(deployed))
  
  
## plot duration ##########
  
dur <- ggplot(deploy_df, aes(deployed, author, color = author)) + 
  labs(x = "Deployment duration", y= "Author/Year") +
  
  #geom_vline(xintercept = 1, color = "gray40", lty = 2) +
  #geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0, size = 0.3) + 
  
  geom_bar(stat="identity",colour="black", ) +
  geom_text(aes(label=country), colour="white", hjust=1.4, angle = 0) +
  
  
  
  
  #geom_point(shape = 24, fill = group.study.color[3]) +
  
  # annotate("rect", xmin = 1.05, xmax = 1.8, ymin = 0.8, ymax = 1.2,  alpha = .2) +
  
  # facet_wrap(  comparison ~ .) +
  
  #scale_color_manual(values = c("gray50", "gray10")) +
  #scale_x_continuous(limits = c(0.8, 2), 
  #                   expand = c(0, 0), 
  #                   breaks = c(0.8, 1, 1.2, 1.4, 1.6, 1.8, 2), 
#                   labels = c("",  1, "",   1.4, "", 1.8, "")) +

#coord_flip() +
  
  dissertation_theme() +
  theme(strip.background = element_rect(color = "white", fill = "white"), 
        strip.text = element_text(size = 7),
        #axis.title.y = element_blank()  , 
        #axis.line.y = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle=0, v=0.5, size=10), 
        axis.title.x = element_text(angle=0, v=0.5, size=10), 
        legend.position = "none")



saveRDS(dur, file="./derived/duration.RDS")


## body comp ##########


body <- deploy_df %>% 
  pivot_longer(cols = body_mass_ch:fat_mass_ch,
                           names_to = "comp",
                           values_to = "change") %>% 
  dplyr::select(-c(comment, country, bmd)) %>% 
  mutate(bodyname = factor(comp, levels= c("body_mass_ch", "muscle_mass_ch", "fat_mass_ch"),
                           labels=c("Body mass", "Muscle mass", 
                                           "Fat mass"))) %>% 
  

 ggplot(aes(change, author, color = author)) + 
   
  labs(x = "%-change", y= "Author/Year") +
  
  #geom_vline(xintercept = 1, color = "gray40", lty = 2) +
  #geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0, size = 0.3) + 
  
  #geom_bar(stat="identity") +
  #geom_text(aes(label=country), hjust=0.9, angle = 0) +
  
  geom_point(shape = 24, fill = group.study.color[3]) +
  
  #  annotate("rect", xmin = 1.05, xmax = 1.8, ymin = 0.8, ymax = 1.2,  alpha = .2) +
  
   facet_wrap(  bodyname ~ .) +
  
  geom_vline(xintercept = 0.0, linetype="dashed", color = "black", size=0.2) +
  
  #scale_color_manual(values = c("gray50", "gray10")) +
  #scale_x_continuous(limits = c(0.8, 2), 
  #                   expand = c(0, 0), 
  #                   breaks = c(0.8, 1, 1.2, 1.4, 1.6, 1.8, 2), 
#                   labels = c("",  1, "",   1.4, "", 1.8, "")) +

#coord_flip() +

dissertation_theme() +
  theme(strip.background = element_rect(color = "white", fill = "white"), 
        strip.text = element_text(size = 12),
        #axis.title.y = element_blank()  , 
        #axis.line.y = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle=0, v=0.5, size=10), 
        axis.title.x = element_text(angle=0, v=0.5, size=10), 
        legend.position = "none")


saveRDS(body, file="./derived/comp.RDS")


## physical performance ###############

phys <- deploy_df %>% 
  pivot_longer(cols = Aerobic:Power,
               names_to = "phys",
               values_to = "change") %>% 
  dplyr::select(-c(comment, country, bmd)) %>% 
  
  
  
  ggplot(aes(change, author, color = author)) + 
  
  labs(x = "%-change", y= "Author/Year") +
  
  #geom_vline(xintercept = 1, color = "gray40", lty = 2) +
  #geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0, size = 0.3) + 
  
  #geom_bar(stat="identity") +
  #geom_text(aes(label=country), hjust=0.9, angle = 0) +
  
  geom_point(shape = 24, fill = group.study.color[3]) +
  
  #  annotate("rect", xmin = 1.05, xmax = 1.8, ymin = 0.8, ymax = 1.2,  alpha = .2) +
  
  facet_wrap(  phys ~ .) +
  
  geom_vline(xintercept = 0.0, linetype="dashed", color = "black", size=0.2) +
  
  #scale_color_manual(values = c("gray50", "gray10")) +
  #scale_x_continuous(limits = c(0.8, 2), 
  #                   expand = c(0, 0), 
  #                   breaks = c(0.8, 1, 1.2, 1.4, 1.6, 1.8, 2), 
  #                   labels = c("",  1, "",   1.4, "", 1.8, "")) +
  
  #coord_flip() +
  
  dissertation_theme() +
  theme(strip.background = element_rect(color = "white", fill = "white"), 
        strip.text = element_text(size = 12),
        #axis.title.y = element_blank()  , 
        #axis.line.y = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle=0, v=0.5, size=10), 
        axis.title.x = element_text(angle=0, v=0.5, size=10), 
        legend.position = "none")

  
saveRDS(phys, file="./derived/physical.RDS")



### delay post measurements

wb <- readxl::read_excel("./data/workbook_r.xlsx", sheet=2) %>% 
  
  unite("author", author, year, sep=" ", na.rm =TRUE, remove=FALSE) %>% 
  
  #mutate(author = fct_reorder(author, desc(delayed_measurements)))
  
  arrange(desc(delayed_measurements))


wbplot <- ggplot(wb, aes(delayed_measurements, author, color = author)) + 
  labs(x = "Days post-deployment measurement", y= "Author/Year") +
  
  #geom_vline(xintercept = 1, color = "gray40", lty = 2) +
  #geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0, size = 0.3) + 
  

  geom_bar(stat="identity",colour="black", ) +
  
  #geom_point(shape = 24, fill = group.study.color[3]) +
  
  #  annotate("rect", xmin = 1.05, xmax = 1.8, ymin = 0.8, ymax = 1.2,  alpha = .2) +
  
  # facet_wrap(  comparison ~ .) +
  
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
        axis.text.x = element_text(angle=45, v=0.6, size=10), 
        axis.title.y = element_text(size=10), 
        legend.position = "none")


saveRDS(wbplot, file="./derived/delayed.RDS")

