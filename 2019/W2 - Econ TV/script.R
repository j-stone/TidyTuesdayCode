##### Tidy Tuesday #2 2019 - TV Ratings

# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-01-08

setwd("2019/W2 - Econ TV/")
library(tidyverse)
library(lubridate)
library(ggrepel)
main.color = "#43a3ca"
extrafont::loadfonts(device = "win")
df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")



############################################################################################
# https://twitter.com/thomas_mock/status/1083002040119451655

# Wanted to breakdown this plot @thomas_mock to understand a few of the intricacies with 
# the visualisation as an example of a more programmatically intensive visualisation for
# my 'understanding data' class where we only have one session on data viz therefore stick 
# to the basics of ggplot.

# Extended it to an NFL dataset from a previous tidytuesday and posted about it here
# https://jstone.netlify.com/post/wrangling-and-data-visualisation-extra-content/

top_tv_shows <- c("The Sopranos", 
                  #"Dexter", 
                  "The X-Files",
                  "The Wire", 
                  #"Breaking Bad", 
                  "Game of Thrones",
                  "Lost"#, 
                  #"Prison Break", 
                  #"Person of Interest"#,
                  #"Suits"
                  )


drama_df <- df %>% 
  mutate(
    top_shows = case_when(
      title %in% top_tv_shows ~ "highlight",
      TRUE ~ NA_character_
    ),
    year = year(date),
    title = case_when(
      title == "The X-Files" & date < ymd(20050101) ~ "The X-Files (OG)",
      TRUE ~ title
    )
  )


top_shows_plot <- drama_df %>% 
    # Initialise plot, only including shows with a drama tag for genre
    filter(str_detect(genres, "Drama")) %>% 
    # default mapping with time on x axis and average rating on y-axis 
    ggplot(aes(x = date, y = av_rating, size = share)) + 
    # add points to the coordinate system where x and y meet.
    # alpha parameter sets some transparency to the points, color should be obvious
    geom_point(alpha = 0.2, color = main.color) + 
    # Next an overall linear line-of-best-fit is added
    geom_smooth(method="lm", se=FALSE, color="grey4", linetype="dashed", size=1, alpha=0.7) +
    # Earlier we added a top_shows variable where the value is highlight for those shows we defined as a top show.
    # By filtering by that in the data argument we apply the geom_path layer only to those shows.
    geom_path(
      data = filter(drama_df, top_shows == "highlight"),
      # group = title necessary to get separate lines for separate titles.
      aes(group = title), color = main.color, size = 0.5
    ) + 
    # Add an annotation for the overall line-of-best-fit
    # Points to note here are the use of the ymd() function from the lubridate package 
    # to define where on the x-axis to place the annotation.
    # Also setting the font with the family parameter can be a sticking point 
    # if you select a family not recognised by your R session.
    annotate("text",
             x = ymd(20120101), y = 8.2, color = "grey4",
             label = "TV Drama Trend", size = 8, fontface = "bold",
             family = "Calibri",
             alpha = 0.6
    ) + 
    # Here, the data specification is built to get the first season for each title which 
    # will then be used as a starting location for the text to be displayed.
    geom_text_repel(
      data = filter(drama_df, top_shows == "highlight") %>% 
        group_by(title) %>% 
        slice(1),
      aes(label = title),
      size = 4,
      nudge_y = .6,
      segment.size = 0.5,
      segment.colour = "grey",
      direction = "x",
      color = "#42a2ca",
      fontface = "bold",
      family = "Calibri"
    ) + 
    # re-drawing the points only for the top shows without an alpha setting so they stand out.
    geom_point(
      data = filter(drama_df, top_shows == "highlight"),
      color = main.color
    ) + 
    # edit the x-axis, labels at every 2-year interval
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") + 
    # edit the y-axis start and end points, there are very few shows with a rating 
    # under 5.5 and it's going to greatly increase the quality of the visual for the 
    # bulk of the data to constrain the y-axis 
    scale_y_continuous(limits = c(5.5, 10.5), position = "right") + 
    # adding labels - self explanatory, make sure your plots are easy to understand
    labs(
      x = "",
      y = "Avg IMDb Rating\n",
      title = "TV dramas",
      subtitle = "Average IMDb user ratings, by show and season",
      caption = "Data: IMDb"
    ) +
    theme_minimal() + 
    # Tweaking the overall appearance
    theme(
      #      text = element_text(family = "Titillium Web"),
      legend.position = c(0.5, 0.1),
      legend.direction = "horizontal",
      legend.background = element_blank(),
      axis.text = element_text(size  = rel(1)),
      title = element_text(size = rel(2), face = "bold"),
      legend.text = element_text(size = rel(.9)),
      legend.title = element_text(size = rel(.75)),
      axis.title = element_text(size = rel(1.5))
    )

ggsave("top_tv_shows.png", top_shows_plot, 
       height = 8, width = 12, units = "in", dpi = "retina")
