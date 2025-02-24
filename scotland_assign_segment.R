
library(tidyverse)

#### Read in data ####

cluster_data <- read_csv("data/Scotland_OA_2dim_170225.csv")

#### Asssign segments ####

get_segments <- function(x, y, n_segments, center_pct = 1 / n_segments) {
  segment_width = 360 / n_segments
  offset = segment_width / 2  # This is the key change
  
  # Get angles (0-360), now with offset
  angles <- -atan2(y, x) * (180 / pi) + 90 - offset  # Subtracted offset here
  angles <- angles %% 360
  
  # Get fan segments (1 to n_segments)
  segments <- ceiling(angles / segment_width)
  
  # Add center segment (n_segments + 1) for closest points
  distances <- sqrt(x^2 + y^2)
  segments[distances <= quantile(distances, center_pct)] <- n_segments + 1
  
  return(segments)
}

#Perform calculation
d_cluster_segments <- cluster_data |> 
  mutate(segment = get_segments(low_diversity, security, 12),
         segment_name = sprintf("segment_%02d", segment))  

#Write to file
d_cluster_segments |>
  select(oa21cd = output_area, low_diversity, security, segment, segment_name) |>
  write_csv("data/scotland_oa_segmentation.csv")


##Check % of responses in each segment
d_cluster_segments |> 
  count(segment_name) |> 
  mutate(pct = n / sum(n) * 100) |> 
  #mutate(segment_name = factor(segment_name, levels = as_factor(d_segment_names$segment_name))) |>
  arrange(segment_name) |> 
  print(n = Inf)

#### Plot the blob ####

##Create labels for plot 
d_cluster_labels <- d_cluster_segments |>
  group_by(segment) |>
  summarise(across(c(low_diversity, security), mean))

#Get neat colourscheme
segment_pal <- read_yaml("data/colourscheme.yml")

#Actually ploy ## ask where theme_fd comes from 
ggplot(d_cluster_segments, aes(x = low_diversity, 
                                    y = security, color = segment_name)) + 
  geom_point() +
  coord_fixed() + # to keep aspect ratio 1:1
  scale_colour_manual(values = segment_pal) +
  theme_fd() + 
  theme(legend.position = "none")  +
  geom_text(data = d_cluster_labels, aes(label = segment), colour = "black")

#write to file
ggsave("data/segmentation_scatterplot.png", width = 2000,height = 2000, units = "px")