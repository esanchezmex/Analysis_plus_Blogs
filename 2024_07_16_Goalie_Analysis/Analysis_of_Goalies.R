# GOALIE ANALYSIS

library(tidyverse)
library(StatsBombR)
library(gt)

all_shots_clustered <- read.csv("~/Desktop/Goalie Analysis/all_shots_clustered.csv")

all_shots_clustered %>% 
  group_by(Cluster) %>% 
  summarise(save_pct = (sum(shot.outcome.name == "Saved") / n())*100) %>% 
  arrange(save_pct) %>% 
  gt() %>% 
  fmt_number(
    2
    ) %>% 
  cols_label(
    save_pct ~ "Save %"
    ) %>% 
  data_color(
    columns = save_pct,
    palette = c('lightgreen', "grey")
    ) %>% 
  gtsave("cluster_save_pct.png")


# Function to create metric: Good Penalty (1 = yes, 0 = no)
good_penalty <- function(df){
  new_df <- df %>% 
    mutate(good_pen = case_when(
      shot.end_location.z >= 1.35 & shot.end_location.z < 2.7 ~ 1,
      
      shot.end_location.y <= 37 ~ 1,
      
      shot.end_location.y >= 42.75 ~ 1,
      
      # Everything else is not a good penalty area
      TRUE ~ 0
    ))
  
  return(new_df)
}
  
result <- good_penalty(all_shots_clustered)
metric_ver <- ggplot(data = result) +
  geom_segment(x = 36, y = 0, xend = 36, yend = 2.75, color = "black", size = 2) +
  geom_segment(x = 36, y = 2.75, xend = 44, yend = 2.75, color = "black", size = 2) +
  geom_segment(x = 44, y = 0, xend = 44, yend = 2.75, color = "black", size = 2) +
  coord_cartesian(xlim = c(35, 45), ylim = c(0, 2.8)) +
  geom_point(aes(x = shot.end_location.y, y = shot.end_location.z, color = factor(good_pen))) +
  geom_segment(x = 37, y = 0, xend = 37, yend = 1.35, linetype = "dashed") +
  geom_segment(x = 42.75, y = 0, xend = 42.75, yend = 1.35, linetype = "dashed") +
  geom_segment(x = 37, y = 1.35, xend = 42.75, yend = 1.35, linetype = "dashed") +
  theme_classic() +
  theme(
    plot.title = element_text(vjust = -1, hjust = 0.5, size = 25),
    plot.subtitle = element_text(vjust = -3, hjust = 0.5),
    axis.title = element_blank(),
    # axis.text = element_blank(),
    axis.line = element_blank(),
    # axis.ticks = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  labs(
    title = "Verification of Metric",
    color = "Good Metric"
  )
ggsave("verification_of_metric.png", metric_ver)




# APPLICATION TO GOALIES

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

keeper_data <- all_events %>% 
  filter(goalkeeper.type.name %in% c("Penalty Saved", "Penalty Conceded") |
           shot.type.name == "Penalty") %>%
  select(player.name, goalkeeper.type.name, 
         shot.outcome.name, shot.end_location.y, shot.end_location.z) %>% 
  mutate(group_id = cumsum(is.na(goalkeeper.type.name))) %>%
  group_by(group_id) %>%
  
  # Combine the shooter and keeper rows
  summarise(
    shooter.name = first(player.name),
    goalkeeper.name = last(player.name),
    goalkeeper.type.name = last(goalkeeper.type.name),
    shot.outcome.name = first(shot.outcome.name),
    shot.end_location.y = first(shot.end_location.y),
    shot.end_location.z = first(shot.end_location.z)
  ) %>%
  
  # Apply metric unction
  good_penalty()

keeper_data <- keeper_data %>% 
  filter(
    !goalkeeper.name %in% c("João Félix Sequeira", "Cristiano Ronaldo dos Santos Aveiro",
                            "Teboho Mokoena", "Temitayo Olufisayo Olaoluwa Aina",
                            "Zakhele Lerato Lepasa", "Achraf Hakimi Mouh",
                            "Moussa Niakhate", "Arthur Masuaku", "Mostafa Mohamed Ahmed Abdallah",
                            "Emilio Nsue López", "Ivane Carminio Francisco Urrubal",
                            "Cédric Bakambu", "Percy Tau", "Aurélien Djani Tchouaméni",
                            "Marcos Aoás Corrêa", "Harry Kane", "Enzo Fernandez",
                            "Pablo Sarabia García", "Marko Livaja", "Jorge Luiz Frello Filho",
                            "Marcus Rashford", "Daniel Olmo Carvajal", "Ruben Vargas",
                            "Sergio Busquets i Burgos", "Gerard Moreno Balaguero",
                            "Gareth Frank Bale", "Mário Figueira Fernandes", "Andrés Mateus Uribe Villa",
                            "Bryan Ruiz González", "Gylfi Þór Sigurðsson", "Hannes Þór Halldórsson",
                            "")
  ) %>% 
  filter(!shot.outcome.name %in% c("Post", "Off T"))

multi_keeper <- ggplot(data = keeper_data %>% 
         filter(
           goalkeeper.name %in% c("Alisson Ramsés Becker", 
                                  "Damián Emiliano Martínez",
                                  "Danijel Subašić", 
                                  "David de Gea Quintana",
                                  "David Ospina Ramírez", 
                                  "Dimitry Bertaud", 
                                  "Diogo Meireles Costa",
                                  "Dominik Livaković", 
                                  "Edouard Mendy", 
                                  "Gianluigi Donnarumma", 
                                  "Hugo Lloris",
                                  "Igor Akinfeev", 
                                  "Jordan Pickford", 
                                  "Kasper Schmeichel",
                                  "Lionel Mpasi-Nzau", 
                                  "Mohamed Abougaba", 
                                  "Ronwen Williams", 
                                  "Unai Simón Mendibil",
                                  "Yahia Fofana", 
                                  "Yann Sommer", 
                                  "Yassine Bounou")
         )) +
  geom_segment(x = 36, y = 0, xend = 36, yend = 2.75, color = "black", size = 2) +
  geom_segment(x = 36, y = 2.75, xend = 44, yend = 2.75, color = "black", size = 2) +
  geom_segment(x = 44, y = 0, xend = 44, yend = 2.75, color = "black", size = 2) +
  coord_cartesian(xlim = c(35, 45), ylim = c(0, 2.8)) +
  geom_point(aes(x = shot.end_location.y, y = shot.end_location.z, color = shot.outcome.name)) +
  geom_segment(x = 37, y = 0, xend = 37, yend = 1.35, linetype = "dashed") +
  geom_segment(x = 42.75, y = 0, xend = 42.75, yend = 1.35, linetype = "dashed") +
  geom_segment(x = 37, y = 1.35, xend = 42.75, yend = 1.35, linetype = "dashed") +
  facet_wrap(~goalkeeper.name) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "lightgrey"),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Which Keepers Save the Most Difficult Penalties?",
    color = "Outcome"
  )

ggsave("multi_keeper_plot.png", multi_keeper)



# OVERALL SAVE PCT
keeper_data %>% 
  filter(goalkeeper.name %in% c("Alisson Ramsés Becker", 
                                "Damián Emiliano Martínez",
                                "Danijel Subašić", 
                                "David de Gea Quintana",
                                "David Ospina Ramírez", 
                                "Dimitry Bertaud", 
                                "Diogo Meireles Costa",
                                "Dominik Livaković", 
                                "Edouard Mendy", 
                                "Gianluigi Donnarumma", 
                                "Hugo Lloris",
                                "Igor Akinfeev", 
                                "Jordan Pickford", 
                                "Kasper Schmeichel",
                                "Lionel Mpasi-Nzau", 
                                "Mohamed Abougaba", 
                                "Ronwen Williams", 
                                "Unai Simón Mendibil",
                                "Yahia Fofana", 
                                "Yann Sommer", 
                                "Yassine Bounou")) %>% 
  group_by(goalkeeper.name) %>% 
  summarise(save_pct = (sum(shot.outcome.name == "Saved") / n())) %>% 
  arrange(-save_pct) %>% 
  gt() %>% 
  tab_header(
    title = md("Highest **Overall** Save %"),
    subtitle = "(in current dataset)"
  ) %>% 
  fmt_percent(
    2
  ) %>% 
  cols_label(
    goalkeeper.name ~ "Goalkeeper Name",
    save_pct ~ "Overall Save %"
  ) %>% 
  data_color(
    columns = save_pct,
    palette = c('white', "lightgreen")
  ) %>% 
  tab_style(
    locations = cells_column_labels(),
    style = cell_borders(sides = c("top", "bottom"), weight = 1)
  ) %>% 
  gtsave("overall_saves.png")


# PCT GOOD PENS FACED
keeper_data %>% 
  filter(goalkeeper.name %in% c("Alisson Ramsés Becker", 
                                "Damián Emiliano Martínez",
                                "Danijel Subašić", 
                                "David de Gea Quintana",
                                "David Ospina Ramírez", 
                                "Dimitry Bertaud", 
                                "Diogo Meireles Costa",
                                "Dominik Livaković", 
                                "Edouard Mendy", 
                                "Gianluigi Donnarumma", 
                                "Hugo Lloris",
                                "Igor Akinfeev", 
                                "Jordan Pickford", 
                                "Kasper Schmeichel",
                                "Lionel Mpasi-Nzau", 
                                "Mohamed Abougaba", 
                                "Ronwen Williams", 
                                "Unai Simón Mendibil",
                                "Yahia Fofana", 
                                "Yann Sommer", 
                                "Yassine Bounou")) %>% 
  group_by(goalkeeper.name) %>% 
  summarise(good_pens_faced = (sum(good_pen == 1) / n())) %>% 
  arrange(-good_pens_faced) %>% 
  gt() %>% 
  tab_header(
    title = md("Highest % of Good Penalties **Faced**"),
    subtitle = "(in current dataset)"
  ) %>% 
  fmt_percent(
    2
  ) %>% 
  cols_label(
    goalkeeper.name ~ "Goalkeeper Name",
    good_pens_faced ~ "% Good Pens Faced"
  ) %>% 
  data_color(
    columns = good_pens_faced,
    palette = c('white', "lightgreen")
  ) %>% 
  tab_style(
    locations = cells_column_labels(),
    style = cell_borders(sides = c("top", "bottom"), weight = 1)
  ) %>% 
  gtsave("overall_faced.png")



# PCT GOOD PENS SAVED
keeper_data %>% 
  filter(goalkeeper.name %in% c("Alisson Ramsés Becker", 
                                "Damián Emiliano Martínez",
                                "Danijel Subašić", 
                                "David de Gea Quintana",
                                "David Ospina Ramírez", 
                                "Dimitry Bertaud", 
                                "Diogo Meireles Costa",
                                "Dominik Livaković", 
                                "Edouard Mendy", 
                                "Gianluigi Donnarumma", 
                                "Hugo Lloris",
                                "Igor Akinfeev", 
                                "Jordan Pickford", 
                                "Kasper Schmeichel",
                                "Lionel Mpasi-Nzau", 
                                "Mohamed Abougaba", 
                                "Ronwen Williams", 
                                "Unai Simón Mendibil",
                                "Yahia Fofana", 
                                "Yann Sommer", 
                                "Yassine Bounou")) %>% 
  filter(good_pen == 1) %>% 
  group_by(goalkeeper.name) %>% 
  summarise(good_pens_save = (sum(shot.outcome.name == "Saved") / n())) %>% 
  arrange(-good_pens_save) %>% 
  gt() %>% 
  tab_header(
    title = md("Highest % of Good Penalties **Saved**"),
    subtitle = "(in current dataset)"
  ) %>% 
  fmt_percent(
    2
  ) %>% 
  cols_label(
    goalkeeper.name ~ "Goalkeeper Name",
    good_pens_save ~ md("% Good Pens **Saved**")
  ) %>% 
  data_color(
    columns = good_pens_save,
    palette = c('white', "lightgreen")
  ) %>% 
  tab_style(
    locations = cells_column_labels(),
    style = cell_borders(sides = c("top", "bottom"), weight = 1)
  ) %>% 
  gtsave("good_pens_saved.png")
  




