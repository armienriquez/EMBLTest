# PLOTTING
plt <- ggplot(data = surveys_complete,
       mapping = aes(x = weight, y = hindfoot_length))
       + geom_point()

plt
str(plt)

#Scatter plot
plt + geom_point()

# Put a title
plt + 
    geom_point() +
    ggtitle("My first plot")

# 1. define ggplot object
# plt <- ggplot(data = <data.frame>, mapping =<aesthetics>)
# x, y, color, shape, etc. aesthetics

# 2. add geometry layer(s)
# geometry functions have predictable names
# geom_{point, line, bar, histogram, violin, hex, ...}

install.packages("hexbin")
library(hexbin)

ggplot(data = surveys_complete, 
       mapping = aes(x = weight, y = hindfoot_length)) +
      geom_hex()

# Alpha is transparency
ggplot(data = surveys_complete, 
       mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1)

# Color the points
ggplot(data = surveys_complete, 
       mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, color = "blue")

# Color points by species ID, more information
ggplot(data = surveys_complete, 
       mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.25, aes(color = species_id))


# Globally define the color for each species rather than by point
ggplot(
  data = surveys_complete,
  mapping = aes(
    x = weight,
    y = hindfoot_length,
    color = species_id
  )
) +
  geom_point(alpha = 0.25)

# Challenge: scatterplot weight vs species_id color by plot_type

ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight,
    color = plot_type
  )
) +
  geom_point(alpha = 0.25)

# More meaningful representation as a box plot
ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight,
    #color = plot_type
  )
) +
  geom_boxplot()

# Jitter added a little value for each x coordinate colored in salmon
# Does not draw the outliers
ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight,
    #color = plot_type
  )
) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.25, color = "salmon") 

# What comes first is the back layer and what comes last is the front layer
ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight,
    #color = plot_type
  )
) +
  geom_jitter(alpha = 0.25, color = "salmon") +
  geom_boxplot(outlier.shape = NA, fill = NA) # makes the box transparent

# Challenge: produce a violin plot of weight by species_id
ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight))+
  geom_violin(color="blue")+
  scale_y_log10() +
  ylab("Weight (log10)")
# Change the scale to make the plot more meaningful and change the y axis title 

# Challenge: Make a boxplot + jittered scatterplot of 
# hindfoot_length by species_id. Boxplot should be in front of the 
# dots and filled with white.

ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = hindfoot_length
    )
) +
  geom_jitter(alpha = 0.25, color = "firebrick") +
  geom_boxplot(outlier.shape = NA, fill = "white") 


ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = hindfoot_length
  )
) +
  geom_jitter(alpha = 0.25, aes(color = plot_id)) +
  geom_boxplot(outlier.shape = NA, fill = "white")
#Since plot_id is a numeric vector, this is interpreted as a color gradient

# This colors the plot_id with discrete colors
ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = hindfoot_length
  )
) +
  geom_jitter(aes(color = factor(plot_id))) +
  geom_boxplot(outlier.shape = NA, fill = "white")

# How to define colors
# "red", "green"
# rgb (red = .3, green = .3, blue = .3)
# hexadecimal #dedede

yearly_count <- surveys_complete %>% 
  count(year, genus)
View(yearly_count)

# Line plot of yearly count of each genus with legend
ggplot(data = yearly_count,
       mapping = aes(
         x = year, 
         y=n, 
         color = genus)) + 
      geom_line()

# This is another way to write it using pipe
yearly_count %>% 
  ggplot(mapping = aes(x = year, y = n, color = genus)) +
  geom_line()

# This is a grey scale plot with different shapes of points for each genus
ggplot(data = yearly_count,
       mapping = aes(
         x = year, 
         y=n, 
         shape = genus, 
         group = genus)) + 
  geom_line() +
  geom_point()


yearly_count_graph <- surveys_complete %>% 
  count(year, genus) %>% 
  ggplot(mapping = aes(x = year, y=n, color = genus)) +
  geom_line()

# One plot per genus 
ggplot(
  data = yearly_count,
  mapping = aes(
    x = year,
    y = n)) + 
  geom_line() + 
  facet_wrap(facets = vars(genus))

# Showing both sexes for each genus
plt <- surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot(
    mapping = aes(
      x = year,
      y=n,
      color=sex)) +
  geom_line() + 
  facet_wrap(facets = vars(genus)) + 
  xlab("Year of observation") +
  ylab("Number of individuals")+
  ggtitle("Observed genera over time")+
  theme_bw(base_size = 10) +
  theme(
    legend.position = "bottom",
    aspect.ratio = 1,
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid = element_blank())
plt

# Cheat sheet here https://ggplot2.tidyverse.org/reference/
# You can also annotate. We'll just look it up haha
# Check ggpubr package where you can do pair-wise comparisons like anova 
# https://rpkgs.datanovia.com/ggpubr/

ggsave(filename = "data/plot.pdf",
       plot = plt,
       width = 20,
       height = 20)

surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot(
    mapping = aes(
      x = year,
      y=n,
      color=sex)) +
  geom_line() + 
  facet_grid(
    cols = vars(genus))

# With all the fancy formatting
plt <- surveys_complete %>%
  count(year, genus, sex) %>%
  ggplot(
    mapping = aes(
      x=year,
      y=n,
      color = sex)) +
  geom_line() +
  facet_wrap(facet= vars(genus),
             scales = "free"
  ) +
  scale_color_manual(values = c("tomato", "dodgerblue"),
                     labels = c("female", "male"),
                     name = "Sex") +
  xlab("Years of observation") +
  ylab("Number of individuals") +
  ggtitle("Observed genera over time") +
  theme_bw(base_size = 8) +
  theme(
    legend.position = "bottom", # "none"
    aspect.ratio = 1,
    axis.text.x = element_text(angle = 45,
                               hjust = 1),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    strip.background =  element_blank()
  )
plt

# Know more about the difference between the two kinds of facets
# https://ggplot2.tidyverse.org/articles/faq-faceting.html