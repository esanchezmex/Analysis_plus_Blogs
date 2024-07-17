### DATA COLLECTION
###### Collect data on all penalties on target for mens senior international comps

library(StatsBombR)
library(tidyverse)

comps <- FreeCompetitions()
matches <- FreeMatches(comps)
selected_matches <- matches %>% 
  filter(
    competition.competition_name %in% c("African Cup of Nations",
                                        "FIFA World Cup",
                                        "UEFA Euro")
  ) %>% 
  filter(
    !(season.season_id %in% c(51, 54, 269, 270, 272))
  ) %>% 
filter(
  !(season.season_name == "1990")
)

all_events <- free_allevents(MatchesDF = selected_matches, Parallel = T)
all_events <- allclean(all_events)

pens <- 
  all_events %>% 
  filter(shot.type.name == "Penalty") %>% 
  select(shot.end_location.y, shot.end_location.z, shot.outcome.name) %>% 
  filter(shot.outcome.name %in% c("Saved", "Goal"))


all_pens_plot = ggplot(data = pens %>% filter(!shot.end_location.y < 36)) +
  geom_segment(x = 36, y = 0, xend = 36, yend = 2.75, color = "black", linewidth = 2) +
  geom_segment(x = 36, y = 2.75, xend = 44, yend = 2.75, color = "black", linewidth = 2) +
  geom_segment(x = 44, y = 0, xend = 44, yend = 2.75, color = "black", linewidth = 2) +
  coord_cartesian(xlim = c(35, 45), ylim = c(0, 2.8)) +
  geom_point(aes(x = shot.end_location.y, y = shot.end_location.z, color = shot.outcome.name)) +
  scale_color_brewer(palette = "Set1") +
  theme_classic() +
  theme(
    plot.title = element_text(vjust = 0.1, hjust = 0.5, size = 20),
    plot.subtitle = element_text(vjust = 0.1, hjust = 0.5, size = 12),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(title = "Penalties on Target",
       subtitle = "FIFA World Cup (2018, 2022)\nUEFA Euro (2020, 2024)\nAFCON (2023)")

ggsave("all_pens.png", all_pens_plot)

pens %>% filter(!shot.end_location.y < 36) %>% write.csv("penalties.csv")
