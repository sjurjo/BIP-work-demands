
library(tidyverse)
library(dplyr)
library(readxl)
library(latex2exp)
library(rmarkdown)


library(emmeans)
library(lme4)
library(brms)
library(nlme)
library(glmmTMB)
library(MASS)
library(feather)
library(ggsignif)


### Plotting, tables etc.
library(cowplot)
library(knitr)
library(kableExtra)
library(ggtext)
library(latex2exp)
library(ggrepel)



#remotes::install_github('yihui/tinytex')

#tinytex::install_tinytex()


#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("latex2exp")
#install.packages("rmarkdown")
#
#
#install.packages("emmeans")
#install.packages("lme4")
#install.packages("brms")
#install.packages("nlme")
#install.packages("glmmTMB")
#install.packages("MASS")
#install.packages("feather")



### Plotting, tables etc.
#install.packages("cowplot")
#install.packages("knitr")
#install.packages("kableExtra")
#install.packages("ggtext")
#install.packages("latex2exp")
#install.packages("ggrepel")


library(devtools)
#install_github("dhammarstrom/publR", build_vignettes = TRUE)
