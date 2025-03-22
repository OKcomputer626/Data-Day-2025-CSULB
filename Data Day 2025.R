if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # Easily Install and Load the 'Tidyverse'
  showtext,
  tidytext,
  janitor,
  jsonlite,
  ggimage,
  glue,
  ggtext
)

# Add Google Font
font_add_google(name = "Open Sans", family = "Sans")
font_add_google(name = "Merriweather", family = "Merriweather")

font1 <- "Sans"
font2 <- "Merriweather"

# Add local font
font_add("Font Awesome 6 Brands", here::here("fonts/otfs/Font Awesome 6 Brands-Regular-400.otf"))

# Automatically enable the use of showtext for all plots
showtext_auto()

# Set DPI for high-resolution text rendering
showtext_opts(dpi = 300)

# Load the JSON file
billie_data <- fromJSON("data/Lyrics_BillieEilish.json")

View(billie_data)

songs_df <- billie_data$songs %>%
  clean_names() %>%
  as_tibble() %>%
  select(title, lyrics) %>%
  mutate(lyrics_clean = str_remove_all(lyrics, "\\[.*?\\]"),
         lyrics_clean = str_replace_all(lyrics_clean, "\\n", " "),
         lyrics_clean = str_squish(lyrics_clean)) %>%
  select(-lyrics)

lyrics_words <- songs_df %>%
  unnest_tokens(word, lyrics_clean) %>%
  anti_join(stop_words)

sentiments_df <- lyrics_words %>%
  inner_join(get_sentiments("bing")) %>% 
  count(title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

df <- sentiments_df %>%
  mutate(test = if_else(sentiment < 0, "negative", "positive")) %>%
  arrange(test, sentiment) %>%
  group_by(test) %>%
  mutate(row_num = row_number(),
         row_num = if_else(test == "negative", rev(row_num), row_num)) %>%
  ungroup()

# update order
df <- df %>%
  mutate(test = factor(test, levels = c("positive", "negative")))

# Generate a social media caption with custom colors and font styling
social <- andresutils::social_caption(font_family = font1, bg_color = "#eed68e") 

cap <- paste0(
  "#DataDay2025CSULB | **Source**: Genius | **Graphic**: ", social
)

st <- "The 10 most frequent words from Billie Eilish’s lyrics, based on her top 50 songs on Genius. The 23-year-old
Highland Park native has been shaping music since 2015, earning nine Grammy Awards, two Academy Awards, two Golden Globes,
and multiple MTV VMAs, while reaching over 103 million Spotify listeners."

# Generate a social media caption with custom colors and font styling
social <- andresutils::social_caption(font_family = font2, icon_color = "#0ff40f") 

cap <- paste0(
  st,
  "<br>#DataDay2025CSULB | **Source**: Genius<br><br>",
  social
)

top_10 <- lyrics_words %>%
  count(word, sort = TRUE) %>%
  slice(1:10)

top_10 %>%
  ggplot(aes(x = fct_reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "#0ff40f", width = 0.8) +
  geom_text(aes(y = 2.5, label = n), hjust = 0, family = font2, size = 3, fontface = "bold", color = "#000000") +
  geom_image(data = slice_head(top_10, n = 1), aes(x = 7, y = 85, image = "pics/billie eilish logo.png"), size = 0.6) +
  coord_flip() +
  labs(x = NULL, y = NULL, tag = cap) +
  scale_y_continuous(expand = expansion(0, 0)) +
  theme_minimal(base_size = 11, base_family = font2) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(hjust = 0, color = "#000000"),
    plot.margin = margin(5, 10, 5, 10),
    plot.background = element_rect(fill = "#ffffff", colour = "#ffffff"),
    plot.tag.position = c(0.97, 0.36),
    plot.tag = element_textbox_simple(
      color = "#000000",
      hjust = 1,
      halign = 1,
      lineheight = 1.5,
      family = font2,
      size = 7,
      maxwidth = 0.7
    )
  )
  
ggsave("Visualization/Billie Eilish top 10 words.png", width = 7, height = 5)

lyrics_words %>%
  filter(word == "love") %>%
  count(title, word, sort = TRUE)


df %>% 
  ggplot() +
  geom_text(aes(x = test, y = if_else(test == "positive", row_num, (-1 * row_num)), label = title), color = "#000000", size = 3.5, family = font2) +
  geom_hline(yintercept = 0, color = "#000000") +
  annotate("text", x = 1, y = -15, label = "BILLIE EILISH", family = font2, fontface = "bold", size = 11.5, color = "#000000", hjust = 0.5) +
  annotate("text", x = 1, y = -19, label = "Sentiment analysis of the 50 most popular songs from Genius,\narranged from the most positive (top left)\nto the most negative (bottom right)", family = font1, size = 2.7, color = "#000000", hjust = 0.5) +
  annotate("text", x = 1, y = -1, label = "Positive", family = font1, size = 4, color = "#000000", fontface = "bold", hjust = 0.5) +
  annotate("text", x = 2, y = 1, label = "Negative", family = font1, size = 4, color = "#000000", fontface = "bold", hjust = 0.5) +
  geom_image(data = slice_head(df, n = 1), aes(x = 1, y = -33, image = "pics/Billie Eilish.png"), size = 0.3, hjust = 0.5) +
  labs(caption = cap) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.caption.position = "plot",
    plot.caption = element_markdown(family = font1, hjust = 0.5, size = 6),
    plot.margin = margin(0.25, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(color = NA, fill = "#eed68e")
  )

ggsave("Visualization/Billie Eilish Positve and Negative songs.png", width = 6, height = 9)

