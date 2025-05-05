

#### Figure theme/color/settings for dissertation  ####

library(ggplot2)
library(cowplot)
library(ggtext)



label.size <- 12
line.size <- 0.3

# Within figures texts
text.size <- 2.5


## Color themes 

# Study/Groups colors 

group.study.color <- c("#b2182b", #1 Dark red
                       "#ef8a62", #2 Orange
                       "#fddbc7", #3 Pink ish
                       "#d1e5f0", #4 Light blue
                       "#67a9cf", #5 Blue
                       "#2166ac", #6 Navy blue
                       "#9ebcda", #7 Light purple
                       "#8856a7", #8 Dark purple
                       "#e0ecf4", #9 Intermediate purple
                       "#ffeda0", #10 Yellow ish
                       "#009E73") #11 Dark green
# Alternative "purple"
# group.study.color <- c("#762a83", # Purple, dark
#   "#af8dc3", # Purple light
#   "#e7d4e8", #
#   "#d9f0d3", #
#   "#7fbf7b", #
#   "#1b7837") #





dissertation_theme <- function() {
  theme_bw() +
    theme(axis.title = element_text(size = 7),
          axis.text = element_text(size = 7, color = "black"),
          axis.line = element_line(color = "black", size = line.size),
          axis.ticks = element_line(color = "black", size = line.size),
          legend.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank())
  
}



