extrafont::loadfonts(device="win")
setwd("2018/wk22-NFLStats/")

library(tidyverse)
library(ggrepel)
library(lubridate)
nfl.data <- read.csv("nfl_2010-2017.csv")

glimpse(nfl.data)

# Top Rushers in the NFL between 2000 and 2017

top_rushers <- nfl.data %>% 
    group_by(name, game_year) %>% 
    summarise(totalRushYds = sum(rush_yds, na.rm=TRUE),
              totalRushAttempts = sum(rush_att, na.rm=TRUE)) %>% 
    arrange(desc(totalRushYds)) %>% 
    ungroup() %>% 
    slice(1:10)
print(top_rushers)

# Where the great ones fit in the grand scheme of the NFL 2000-2017

best.runners <- unique(top_rushers$name)
main.colour = "#43a3ca"
length(best.runners)

summarised.runner.data <- nfl.data %>% 
    group_by(name, game_year) %>% 
    summarise(totalRushYds = sum(rush_yds, na.rm=T),
              totalRushAttempts = sum(rush_att, na.rm=T)) %>% 
    filter(totalRushAttempts > 50) %>%
    ungroup() %>% 
    mutate(top_runner = ifelse(name %in% best.runners, "highlight", "noHighlight"))

(best_season_plot <- summarised.runner.data %>% 
        ggplot(aes(x = game_year, y = totalRushYds, size = totalRushAttempts)) +
        geom_point(aes(alpha = as.factor(top_runner)), color = main.colour, position = position_jitter(width=0.1)) + 
        geom_smooth(method="loess", se=F, color="grey4", linetype="dashed", size=1, alpha=0.7) + 
        geom_path(
            data = filter(summarised.runner.data, top_runner == "highlight"),
            aes(group = name), color = main.colour, size = 0.75
        ) + 
        annotate("text",
                 x = 2014, y = 500, color = "grey4",
                 label = "Overall Trend", size = 8, fontface ="bold",
                 family="Calibri",
                 alpha = 0.6
        ) + 
        geom_text_repel(
            data = filter(summarised.runner.data, top_runner == "highlight") %>% 
                group_by(name) %>% 
                arrange(desc(totalRushYds)) %>% 
                slice(1),
            aes(label = name),
            size = 6,
            nudge_y = + 300,
            nudge_x = 0.5,
            segment.size = 0.5,
            segment.colour = "black",
            direction = "x",
            color = "#bc145a",
            fontface = "bold",
            family = "Calibri"
        ) +
        scale_x_continuous(breaks = seq(2000,2017,2)) + 
        scale_y_continuous(position = "right", limits=c(0,2400)) + 
        scale_alpha_discrete(range=c(0.9,0.2), guide = "none") + 
        labs(x = "Season",
             y = "Total Rush Yards",
             title = "Every rushing yardage total 2000 - 2017 (min 50 attempts)",
             subtitle = "Emphasis on rushers with the top 15 best rushing seasons in that timespan",
             caption = "Data: #TidyTuesday 2018 week 22") + 
        theme_minimal() + 
        theme(
            text = element_text(family = "Calibri"),
            legend.position = c(0.5,0.05),
            legend.direction = "horizontal",
            axis.text = element_text(size = rel(1)),
            title = element_text(size = rel(2), face="bold"),
            legend.text = element_text(size=rel(.9)),
            legend.title = element_text(size=rel(.75)),
            axis.title = element_text(size = rel(1.5))
        )
)

ggsave("best_season_runner_plot.png", best_season_plot,
       height=8, width = 12, units = "in", dpi ="retina")


nfl.data %>% 
    filter(name == "Chris Johnson" & game_year == 2009) %>% 
    summarise(totalyds = sum(rush_yds),
              totalatts = sum(rush_att))