library(ggplot2)
library(ggimage)
library(gt)
library(gtExtras)
library(worldfootballR)
library(dplyr)

##############################################################
##       NOTE: DATA ACCURATE UP TO JANUARY 30, 2024         ##
##############################################################



liverpool_standard <- fb_big5_advanced_season_stats(season_end_year = 2024,
                                                    stat_type = "standard",
                                                    team_or_player = "player")
liverpool_standard <- liverpool_standard %>% filter(Squad == "Liverpool") %>% as_tibble()
liv_shooting <- fb_big5_advanced_season_stats(season_end_year = 2024,
                                              stat_type = "shooting",
                                              team_or_player = "player")
liv_shooting <- liv_shooting %>% filter(Squad == "Liverpool") %>% as_tibble()
liv_pass <- fb_big5_advanced_season_stats(season_end_year = 2024,
                                          stat_type = "passing",
                                          team_or_player = "player")
liv_pass <- liv_pass %>% filter(Squad == "Liverpool") %>% as_tibble()
liv <- inner_join(liverpool_standard, liv_shooting, by = "Player")
liv <- inner_join(liv, liv_pass, by = "Player")


# TABLE FOR TOP ASSISTER IN TEAM
liv_relevant <- liv %>% 
  select(Squad, Player, Min_Playing, Gls, Gls_Per, Ast.x, xG_Expected.x, xG_Per, 
         Sh_Standard, SoT_Standard, KP, Final_Third, PrgP)
liv_relevant <- map_team_logos(liv_relevant)
liv_relevant <- liv_relevant %>% select(squad_image, everything())

liv_ast <- liv_relevant %>% 
  select(squad_image, Squad, Player, Ast.x, KP, Final_Third, PrgP)
liv_ast_tb <- liv_ast %>% arrange(-Ast.x, -KP) %>% my_gt_theme_dark(map = "yes", n = 10) %>% 
  cols_label(
    Squad = md("**Club**"),
    Player = md("**Player**"),
    Ast.x = md("**Assists**"),
    KP = md("**Key Passes**"),
    Final_Third = md("**Passes Into Final Third**"),
    PrgP = md("**Progressive Passes**")
  ) %>% 
  tab_header(
    title = md("**Assists & Creativity**"), # md = Markdown
    subtitle = md("Premier League 2024")
  ) %>% 
  # FORMAT TEXT IN CELLS
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_body()
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = 1)
  ) %>% 
  tab_style(
    style = cell_fill(color = "white"),
    locations = cells_body()
  ) %>% 
  tab_style(
    style = cell_fill(color = "#C8102E"),
    locations = cells_body(rows = 1, columns = 1:2)
  ) %>%
  tab_style(
    style = cell_text(color = "white"), # White text color
    locations = cells_body(rows = 1)
  ) %>%
  # EDIT CELL BORDERS
  tab_style(
    style = cell_borders(sides = c("bottom", "top"), color = "black"),
    locations = cells_body()
  ) %>% 
  tab_style(
    style = cell_borders(sides = c("bottom"), color = "black"),
    locations = cells_column_labels()
  ) %>% 
  tab_style(
    style = cell_borders(sides = c("bottom"), color = "black", weight = px(2)),
    locations = list(cells_title(groups = "subtitle"),
                     cells_body())
  ) %>%
  tab_style(
    style = cell_borders(sides = c("top"), color = "black", weight = px(2)),
    locations = cells_title(groups = "title")
  ) %>% 
  fmt_number(
    columns = everything(),
    decimals = 0
  ) %>% 
  data_color(
    columns = everything(),
    method = "numeric",
    palette = c("white", "#C8102E"),
    reverse = F
  )  %>% 
  tab_options(
    column_labels.font.size = 15,
    table.background.color = "white",
  ) %>% 
  tab_footnote(
    footnote = md("Table by: **Esteban Sanchez**"),
    placement = "left"
  ) %>% 
  cols_width(
    Squad ~ px(50),
    Player ~ px(150),
    Ast.x ~ px(75),
    KP ~ px(75),
    Final_Third ~ px(75),
    PrgP ~ px(80),
  )



# SCATTER PLOT FOR ALL PREM TEAMS
prog <- prem_players %>% 
  group_by(Squad) %>% 
  summarise(progP = sum(PrgP_Progression, na.rm = T),
            progC = sum(PrgC_Progression, na.rm = T))
prog <- map_team_logos(prog)

min_y = min(prog$progC)
max_y = max(prog$progC)
min_x = min(prog$progP)
max_x = max(prog$progP)

ggplot(prog, aes(x = progP, y = progC)) +
  geom_image(aes(image = squad_image), size = 0.2) +
  geom_jitter(size = 0.2, width = 0.5, height = 0.5) +
  geom_vline(xintercept = 800, linetype = "solid", color = "darkgrey") +
  geom_hline(yintercept = 400, linetype = "solid", color = "darkgrey") +
  xlim(min_x - 10, max_x + 10) + 
  ylim(min_y - 10, max_y + 10) +
  theme(legend.background = element_rect(colour = "#e5e5e5", fill = "#e5e5e5"),
        legend.position = 'none',
        plot.background = element_rect(colour = "#e5e5e5", fill = "#e5e5e5"),
        plot.margin = margin(20,50,30,5),
        panel.background = element_rect(colour = "#e5e5e5", fill = "#e5e5e5"),
        panel.grid = element_line(linetype = "dotted", color = "#003566", size = 0.25),
        plot.title = element_text(hjust = 0, color = "#003566", size = 22.5, family = "AppleGothic", margin = margin(20,10,5,10)),
        plot.subtitle = element_text(hjust = 0, color = "#003566", size = 12, family = "AppleGothic", margin = margin(0,0,15,0)),
        axis.text.y = element_text(color = "#003566", size = 12, family = "AppleGothic"),
        axis.text.x = element_text(color = "#003566", size = 12, family = "AppleGothic", margin = margin(b = 5, unit = "pt")),
        axis.title.y = element_text(color = "#003566", size =12, family = "AppleGothic", margin = margin(20,20,20,20)),
        axis.title.x = element_text(color = "#003566", size =12, family = "AppleGothic", margin = margin(t = 8, unit = "pt")),
        axis.line = element_line(color= "#003566", size = 0.25),
        plot.caption = element_text(color = "#003566", face = "italic", family = "AppleGothic", size = 7)) +
  labs(title = "Klopp's men are proficient at moving up the field",
       subtitle = "Premier League teams plotted by Progressive Passes and Carries",
       x = "Progressive Passes",
       y = "Progressive Carries",
       caption = "Data from: Fbref\nGraph by: Esteban Sanchez")
  



# SCATTER PLOT HIGHLITHING LIVERPOOL PLAYERS
ind_prog <- 
  prem_players %>% 
  select(Squad, Player, PrgP_Progression, PrgC_Progression) %>% 
  mutate(
    is_liv = case_when(
      Squad == "Liverpool" ~ "yes",
      Squad != "Liverpool" ~ "no"
    )
  ) %>% 
  filter(PrgP_Progression > 30 & PrgC_Progression > 15)


ggplot(ind_prog, aes(x = PrgP_Progression, y = PrgC_Progression, color = is_liv)) +
  geom_point(size = 3) +
  xlim(min(ind_prog$PrgP_Progression), max(ind_prog$PrgP_Progression)) +
  ylim(min(ind_prog$PrgC_Progression), max(ind_prog$PrgC_Progression)) +
  scale_color_manual(
    values = c("yes" = "#C8102E", "no" = "darkgrey"),
    name = "Liverpool",
    breaks = "yes"
    ) +
  theme(legend.background = element_rect(colour = "#e5e5e5", fill = "#e5e5e5"),
        legend.position = 'right',
        legend.box.background = element_rect(color = "#e5e5e5", fill = "#e5e5e5"),
        legend.key = element_blank(),
        legend.title.align = 4,  
        legend.title = element_text(hjust = 0.5),
        plot.background = element_rect(colour = "#e5e5e5", fill = "#e5e5e5"),
        plot.margin = margin(20,20,30,5),
        panel.background = element_rect(colour = "#e5e5e5", fill = "#e5e5e5"),
        panel.grid = element_line(linetype = "dotted", color = "#003566", size = 0.25),
        plot.title = element_text(hjust = 0, color = "#003566", size = 22.5, family = "AppleGothic", margin = margin(20,10,5,10)),
        plot.subtitle = element_text(hjust = 0, color = "#003566", size = 12, family = "AppleGothic", margin = margin(0,0,15,0)),
        axis.text.y = element_text(color = "#003566", size = 12, family = "AppleGothic"),
        axis.text.x = element_text(color = "#003566", size = 12, family = "AppleGothic", margin = margin(b = 5, unit = "pt")),
        axis.title.y = element_text(color = "#003566", size =12, family = "AppleGothic", margin = margin(20,20,20,20)),
        axis.title.x = element_text(color = "#003566", size =12, family = "AppleGothic", margin = margin(t = 8, unit = "pt")),
        axis.line = element_line(color= "#003566", size = 0.25),
        plot.caption = element_text(color = "#003566", face = "italic", family = "AppleGothic", size = 7)) +
  guides(color = guide_legend(title = "Liverpool", label = FALSE)) +
  labs(title = "Well Complimented Klopp Signings",
       subtitle = "Salah and Big Dom compliment academy product Trent perfectly",
       x = "Progressive Passes",
       y = "Progressive Carries",
       caption = "Data from: Fbref\nGraph by: Esteban Sanchez") +
  geom_text(aes(label = ifelse(Player == "Mohamed Salah" |
                                 grepl("Dominik", Player),
                               as.character(Player),'')), 
            size = 3, hjust = -0.1, vjust = 0) +
  geom_text(
    aes(label = ifelse(grepl("Trent", Player),
                       as.character(Player),'')), 
    size = 3, hjust = 0, vjust = 2
  )











