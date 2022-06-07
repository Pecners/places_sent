library(tidyverse)
library(tidytext)
library(geomtextpath)
library(cowplot)


d <- read_file("data/text.txt")

lex <- "bing"
sd <- bind_rows(tibble(line = 1, text = " "),
                tibble(line = 1, text = d))

tmp <- sd %>%
  unnest_tokens(word, text) %>%
  left_join(get_sentiments(lex)) %>%
  mutate(ind = row_number(),
         score = case_when(sentiment == "positive" ~ 1,
                           sentiment == "negative" ~ -1,
                           TRUE ~ 0),
         run = cumsum(score))


df <- sd %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments(lex)) %>%
  mutate(ind = row_number(),
         score = case_when(sentiment == "positive" ~ 1,
                           sentiment == "negative" ~ -1,
                           TRUE ~ 0),
         run = cumsum(score))

df <- bind_rows(tibble(run = 0), df)

s <- spline(df$run, n = 10 * nrow(df))


df1 <- tibble(
  x = s$x,
  y = s$y
)

movey <- 15
blue <- "#1CC1E1"
pink <- "#E5A9AF"

jpeg::

df1 %>%
  mutate(y = y + movey) %>%
  ggplot(aes(x, y)) +
  geom_area(fill = "#B9D77F", color = "black") +
  annotate(geom = "text", x = df1$x[which(df1$y == max(df1$y))],
           y = max(df1$y) + movey + 1, label = "You've topped\nall the rest!",
           family = "Doctor Soos Light", lineheight = .9, vjust = 0) +
  annotate(geom = "text", x = df1$x[which(df1$y == min(df1$y))] + 1,
           y = 4, label = "You're in\nthe Waiting Place",
           family = "Doctor Soos Light", lineheight = .9, vjust = 1) +
  annotate(geom = "text", x = 56,
           y = movey + 4, label = "You're famous as\nfamous can be!",
           hjust = .5, vjust = 0, 
           family = "Doctor Soos Light", lineheight = .9) +
  annotate(geom = "text", x = 71,
           y = movey - 5, label = "You're\nmixed up",
           hjust = .5, vjust = 1, 
           family = "Doctor Soos Light", lineheight = .9) +
  annotate(geom = "text", x = 77.5,
           y = movey + 6, label = "You're off to\ngreat places!",
           hjust = 1, vjust = 1,
           family = "Doctor Soos Light", lineheight = .9) +
  annotate(geom = "segment", x = 2, xend = 15, y = 1, yend = 1,
           arrow = arrow(length = unit(.1, "cm"), type = "closed"),
           size = .5) +
  annotate(geom = "text", x = 2, y = 2, vjust = .5, hjust = 0, size = 3,
           label = "Story progression", family = "Doctor Soos Light") +
  annotate(geom = "segment", x = 15, xend = 15, y = movey - 2, yend = movey + 2,
           color = blue, arrow = arrow(length = unit(.1, "cm"), ends = "both")) +
  annotate(geom = "label", x = 15, y = movey+3.25, label = "Good things\nhappening",
           lineheight = .9, family = "Doctor Soos Light", color = blue,
           size = 3, fill = alpha("white", .75)) +
  annotate(geom = "label", x = 15, y = movey-3.25, label = "Bad things\nhappening",
           lineheight = .9, family = "Doctor Soos Light", color = blue,
           size = 3, fill = alpha("white", .75)) +
  geom_texthline(yintercept = movey, color = blue, linetype = 2,
                 label = "Neutral", family = "Doctor Soos Light") +
  annotate(geom = "text", x = max(df1$x), y = 1, label = "Graphic by Spencer Schien (@MrPecners)",
           color = alpha("black", .5), family = "Doctor Soos Light",
           hjust = 1.05) +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), breaks = c()) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#F4EE84", color = "#F4EE84"),
        text = element_text(family = "Doctor Soos Light"),
        plot.title = element_text(size = 40, margin = margin(t = 10, b = 75))) +
  labs(title = "Story arc of Oh, the Places You'll Go!",
       x = "", y = "")

ggsave("plot.png", w = 9, h = 5, bg = "white")
