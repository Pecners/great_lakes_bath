#################################
# This file extracts the legend #
# created in R/load_data.R.     #
#################################

library(cowplot)
library(ggpubr)

my_legend <- get_legend(e)

as_ggplot(my_legend)

ggsave("legend.png", bg = "transparent")
