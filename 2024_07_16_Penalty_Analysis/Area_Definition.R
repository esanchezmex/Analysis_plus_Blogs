# POST CLUSTERING ANALYSIS

library(tidyverse)

all_shots_clustered <- read.csv("~/Desktop/Goalie Analysis/all_shots_clustered.csv")

pens_clustered <- ggplot(data = all_shots_clustered) +
  geom_segment(x = 36, y = 0, xend = 36, yend = 2.75, color = "black", size = 2) +
  geom_segment(x = 036, y = 2.75, xend = 44, yend = 2.75, color = "black", size = 2) +
  geom_segment(x = 44, y = 0, xend = 44, yend = 2.75, color = "black", size = 2) +
  coord_cartesian(xlim = c(35, 45), ylim = c(0, 2.8)) +
  # geom_line(aes(x = shot.end_location.y, y = shot.end_location.z, color = factor(Cluster))) +
  geom_point(aes(x = shot.end_location.y, y = shot.end_location.z, color = factor(Cluster)), size = 3) +
  theme_classic() +
  theme(
    plot.title = element_text(vjust = -1, hjust = 0.5, size = 25),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.title = element_text()
  ) +
  labs(
    title = "Penalties Clustered",
    color = "Cluster:"
  )

ggsave("pens_clustered.png", pens_clustered)


# DEFINE LINES FOR A GOOD PENALTY

cluster_0 <- ggplot(data = all_shots_clustered %>% filter(Cluster == 0)) +
  geom_segment(x = 36, y = 0, xend = 36, yend = 2.75, color = "black", size = 2) +
  geom_segment(x = 36, y = 2.75, xend = 44, yend = 2.75, color = "black", size = 2) +
  geom_segment(x = 44, y = 0, xend = 44, yend = 2.75, color = "black", size = 2) +
  coord_cartesian(xlim = c(35, 45), ylim = c(0, 2.8)) +
  # geom_line(aes(x = shot.end_location.y, y = shot.end_location.z, color = factor(Cluster))) +
  geom_point(aes(x = shot.end_location.y, y = shot.end_location.z, color = factor(shot.outcome.name)), size = 3) +
  geom_segment(x = 42.75, y = 0, xend = 42.75, yend = 1.45, linetype = "dashed") +
  theme_classic() +
  theme(
    plot.title = element_text(vjust = -1, hjust = 0.5, size = 25),
    plot.subtitle = element_text(vjust = -3, hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  labs(
    title = "Cluster 0",
    subtitle = "Shot end locations greater than x = 42 have a significantly higher chance of going in"
  )
ggsave("cluster_0.png", cluster_0)


cluster_1 <- ggplot(data = all_shots_clustered %>% filter(Cluster == 1)) +
  geom_segment(x = 36, y = 0, xend = 36, yend = 2.75, color = "black", size = 2) +
  geom_segment(x = 36, y = 2.75, xend = 44, yend = 2.75, color = "black", size = 2) +
  geom_segment(x = 44, y = 0, xend = 44, yend = 2.75, color = "black", size = 2) +
  coord_cartesian(xlim = c(35, 45), ylim = c(0, 2.8)) +
  # geom_line(aes(x = shot.end_location.y, y = shot.end_location.z, color = factor(Cluster))) +
  geom_point(aes(x = shot.end_location.y, y = shot.end_location.z, color = factor(shot.outcome.name)), size = 3) +
  geom_segment(x = 37, y = 0, xend = 37, yend = 1.45, linetype = "dashed") +
  theme_classic() +
  theme(
    plot.title = element_text(vjust = -1, hjust = 0.5, size = 25),
    plot.subtitle = element_text(vjust = -3, hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  labs(
    title = "Cluster 1",
    subtitle = "Shot end locations less than x = 37 have a significantly higher chance of going in"
  )

ggsave("cluster_1.png", cluster_1)



clusters_2_3 <- ggplot(data = all_shots_clustered %>% filter(Cluster %in% c(2, 3))) +
  geom_segment(x = 36, y = 0, xend = 36, yend = 2.75, color = "black", size = 2) +
  geom_segment(x = 36, y = 2.75, xend = 44, yend = 2.75, color = "black", size = 2) +
  geom_segment(x = 44, y = 0, xend = 44, yend = 2.75, color = "black", size = 2) +
  coord_cartesian(xlim = c(35, 45), ylim = c(0, 2.8)) +
  # geom_line(aes(x = shot.end_location.y, y = shot.end_location.z, color = factor(Cluster))) +
  geom_point(aes(x = shot.end_location.y, y = shot.end_location.z, color = factor(shot.outcome.name)), size = 3) +
  geom_segment(x = 36, y = 1.35, xend = 44, yend = 1.35, linetype = "dashed") +
  theme_classic() +
  theme(
    plot.title = element_text(vjust = -1, hjust = 0.5, size = 25),
    plot.subtitle = element_text(vjust = -3, hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  labs(
    title = "Clusters 2 & 3",
    subtitle = "Shot end locations greater than y = 1.35 have a significantly higher chance of going in"
  )

ggsave("clusters_2_3.png", clusters_2_3)

# DEFINITION OF AREA
# xmin = 37
# xmax = 42.75
# ymin = 1.35
# ymax = 2.7 (crossbar)

good_pen_area <- ggplot(data = all_shots_clustered) +
  geom_segment(x = 36, y = 0, xend = 36, yend = 2.75, color = "black", size = 2) +
  geom_segment(x = 36, y = 2.75, xend = 44, yend = 2.75, color = "black", size = 2) +
  geom_segment(x = 44, y = 0, xend = 44, yend = 2.75, color = "black", size = 2) +
  coord_cartesian(xlim = c(35, 45), ylim = c(0, 2.8)) +
  # geom_line(aes(x = shot.end_location.y, y = shot.end_location.z, color = factor(Cluster))) +
  geom_point(aes(x = shot.end_location.y, y = shot.end_location.z, color = factor(shot.outcome.name)), size = 3) +
  geom_segment(x = 37, y = 1.35, xend = 42.75, yend = 1.35, linetype = "dashed") +
  geom_segment(x = 37, y = 0, xend = 37, yend = 1.35, linetype = "dashed") +
  geom_segment(x = 42.75, y = 0, xend = 42.75, yend = 1.35, linetype = "dashed") +
  theme_classic() +
  theme(
    plot.title = element_text(vjust = -1, hjust = 0.5, size = 25),
    plot.subtitle = element_text(vjust = -3, hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  ) +
  labs(
    title = "Good vs Bad Penalty Spots",
    subtitle = "We can classify a 'Good Penalty' as a penalty that lands outside the dashed bounds"
  )

ggsave("good_pen_area.png", good_pen_area)



