# Sam used a neat platform tidydatatutor https://tidydatatutor.com to visualize transformations


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                        STEP 1: LOAD PACKAGES & DATA                      ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........................load packages.........................

library(tidyverse)

#..........................read in data..........................

sweaters <- read_csv("ugly_sweaters.csv") %>% 
  # only take respondents who have a sweater
  filter(hs_tf == "Yes")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                        STEP 2: WRANGLE/PROCESS DATA                      ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#.....................wrangle sweater colors.....................

tidy_colors <- sweaters %>% 
  separate_rows(colors, sep = ", ") %>%  # 1) tidy colors (1 color per row)
  separate_rows(colors, sep = " and ") %>% 
  group_by(sweater) %>%  # 2) group by sweater ID
  summarize(num_colors = length(sweater))  # 3) calculate how many colors each sweater has on it

#..................wrangle sweater descriptions..................

tidy_descriptions <- sweaters %>% 
  separate_rows(image_desc, sep = " ")  %>%  # 1) tidy descriptions (1 word per row)
  group_by(sweater) %>%  # 2) group by sweater ID
  summarize(num_words = length(sweater))  # 3) calculate how many colors each sweater has on it

#........................join data frames.........................

tidy_counts <- full_join(tidy_colors, tidy_descriptions)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                           STEP 3: VISUALIZE DATA                         ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#....................create basic scatterplot....................

simple_plot <- ggplot(tidy_counts, aes(x = num_colors, y = num_words)) +
  geom_jitter()

#..............create slightly fancier scatterplot...............

fancy_plot <- ggplot(tidy_counts, aes(x = num_colors, y = num_words)) +
  geom_jitter() +
  scale_x_continuous() +
  geom_smooth() +
  labs() +
  theme_classic()

fancy_plot
