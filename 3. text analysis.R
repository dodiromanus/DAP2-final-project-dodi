library(tidytext)
library(textdata)
library(sentimentr)
library(lubridate)
library(patchwork)

# PART 6: TEXT ANALYSIS USING SCRAPPED SUBREDDIT DATA

# PART 6a: LOADING AND PREPARING THE DATASETS

# Define the file paths
coal_file_path <- file.path(main_path, "Data", "subreddit_germany_coal.csv")
coal_power_plant_file_path <- file.path(main_path, "Data", "subreddit_germany_coal_power_plant.csv")

# Load the CSV files
subreddit_germany_coal <- read.csv(coal_file_path, stringsAsFactors = FALSE)
subreddit_germany_coal_power_plant <- read.csv(coal_power_plant_file_path, stringsAsFactors = FALSE)


# Remove unnecessary columns and tidy up the dataframe
subreddit_coal <- subreddit_germany_coal |>
  mutate(body = ifelse(dataType == "post", title, body)) |>
  mutate(createdAt = as.Date(ymd_hms(createdAt))) |>
  select(body, createdAt, dataType, numberOfComments, numberOfreplies, upVoteRatio, upVotes, parsedId, postId)
  

subreddit_coal_pp <- subreddit_germany_coal_power_plant |>
  mutate(body = ifelse(dataType == "post", title, body)) |>
  mutate(createdAt = as.Date(ymd_hms(createdAt))) |>
  select(body, createdAt, dataType, numberOfComments, numberOfreplies, upVoteRatio, upVotes, parsedId, postId)







# PART 6b: VISUALIZING THE COAL RELATED DISCUSSION WITH MONTHLY X-AXIS

# Define the timeframe
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-31")


# Filter and aggregate data for monthly posts and comments
coal_posts_monthly <- subreddit_coal |>
  filter(dataType == "post", createdAt >= start_date, createdAt < end_date) |>
  mutate(Month = format(createdAt, "%Y-%m")) |>
  group_by(Month) |>
  summarise(
    numberOfPosts = n(),
    totalComments = sum(numberOfComments, na.rm = TRUE)
  )

coal_power_plant_posts_monthly <- subreddit_coal_pp |>
  filter(dataType == "post", createdAt >= start_date, createdAt < end_date) |>
  mutate(Month = format(createdAt, "%Y-%m")) |>
  group_by(Month) |>
  summarise(
    numberOfPosts = n(),
    totalComments = sum(numberOfComments, na.rm = TRUE)
  )

# Combine both datasets
combined_monthly_posts <- bind_rows(
  coal_posts_monthly |>
    mutate(source = "Coal"),
  coal_power_plant_posts_monthly |>
    mutate(source = "Coal Power Plant")
)


# Convert Month back to Date for proper plotting
combined_monthly_posts <- combined_monthly_posts |>
  mutate(Month = as.Date(paste0(Month, "-01")))


# Define important dates with y positions for annotation
important_dates <- data.frame(
  Date = as.Date(c("2022-02-24", "2022-03-08", "2022-06-19", "2022-08-01")),
  Event = c(
    "Feb 24, 2022: Russia invaded Ukraine",
    "Mar 8, 2022: EU cuts Russian gas dependency",
    "Jun 19, 2022: Germany reopens coal plants",
    "Aug 1, 2022: Coal plants resume operations"
  ),
  y_position = c(350, 250, 150, 50)  # Different heights for annotations
)

# Create the combined plot
combined_plot <- ggplot(combined_monthly_posts, aes(x = Month)) +
  # Bars for number of posts
  geom_col(aes(y = numberOfPosts, fill = source), position = "dodge", color = "black") +
  geom_text(
    aes(y = numberOfPosts, label = numberOfPosts, group = source),
    position = position_dodge(width = 30), vjust = -0.5, size = 3
  ) +
  # Bars for number of comments
  geom_col(aes(y = -totalComments, fill = source), position = "dodge", color = "black", alpha = 0.7) +
  geom_text(
    aes(y = -totalComments, label = totalComments, group = source),
    position = position_dodge(width = 30), vjust = 1.5, size = 3
  ) +
  # Important dates as vertical lines
  geom_segment(
    data = important_dates,
    aes(x = Date, xend = Date, y = 0, yend = y_position),
    color = "black",
    linetype = "dashed",
    size = 0.6
  ) +
  geom_text(
    data = important_dates,
    aes(
      x = Date,
      y = y_position,  # Use custom y positions for annotations
      label = Event
    ),
    inherit.aes = FALSE,
    size = 3.25,
    color = "black",
    angle = 0,
    hjust = 0,
    vjust = -0.5
  ) +
  # Y-axis scale and labels
  scale_y_continuous(
    name = "Number of Posts / Comments",
    breaks = seq(-max(combined_monthly_posts$totalComments, na.rm = TRUE), 
                 max(combined_monthly_posts$numberOfPosts, na.rm = TRUE), by = 200),
    labels = abs(seq(-max(combined_monthly_posts$totalComments, na.rm = TRUE), 
                     max(combined_monthly_posts$numberOfPosts, na.rm = TRUE), by = 200))
  ) +
  scale_x_date(
    name = "Month",
    date_breaks = "1 month",  # Breaks by month
    date_labels = "%b %Y",  # Format as "Jan 2022"
    limits = c(start_date, end_date)
  ) +
  scale_fill_manual(values = c("Coal" = "blue", "Coal Power Plant" = "red")) +
  labs(
    title = "Monthly Aggregated Traffic of Posts Discussing Coal and Coal Power Plants",
    subtitle = "Positive y axis represent number of posts; Negative y axis represent number of comments",
    caption = "Data source: Reddit /r/germany",
    fill = "Source"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Display the plot
print(combined_plot)

# Ensure "Images" folder exists in main_path
images_folder <- file.path(main_path, "Images")
if (!dir.exists(images_folder)) {
  dir.create(images_folder)
}

# Save the plot as a PNG file in the "Images" folder
ggsave(
  filename = file.path(images_folder, "3.1 Monthly_Coal_Discussion_Traffic.png"),
  plot = combined_plot,
  width = 16, height = 10, units = "in", dpi = 300
)







# PART 6c: CHECKING THE POST AND COMMENT SENTIMENT USING NRC, AFINN, AND BING SENTIMENT ANALYSIS

# Create another column so we wont replace my current content column
subreddit_coal$body2 = subreddit_coal$body
tokenize_subreddit_coal <- unnest_tokens(subreddit_coal, word_tokens, body2, token = "words")

subreddit_coal_pp$body2 = subreddit_coal_pp$body
tokenize_subreddit_coal_pp <- unnest_tokens(subreddit_coal_pp, word_tokens, body2, token = "words")

# Filter out the stop words
tokenize_subreddit_coal <- anti_join(tokenize_subreddit_coal, stop_words, by = c("word_tokens" = "word"))
tokenize_subreddit_coal_pp <- anti_join(tokenize_subreddit_coal_pp, stop_words, by = c("word_tokens" = "word"))


# Loading all the sentiments (NRC, AFINN, and BING)
sentiment_nrc <- get_sentiments("nrc") |>
  rename(nrc = sentiment)

sentiment_afinn <- get_sentiments("afinn") |>
  rename(afinn = value)

sentiment_bing <- get_sentiments("bing") |>
  rename(bing = sentiment)


# Evaluate each word sentiment using NRC, AFINN, and BING
tokenize_subreddit_coal <- tokenize_subreddit_coal |>
  left_join(sentiment_nrc, by = c("word_tokens" = "word")) |>
  left_join(sentiment_afinn, by = c("word_tokens" = "word")) |>
  left_join(sentiment_bing, by = c("word_tokens" = "word")) |>
  select(createdAt, word_tokens, nrc:bing)

tokenize_subreddit_coal_pp <- tokenize_subreddit_coal_pp |>
  left_join(sentiment_nrc, by = c("word_tokens" = "word")) |>
  left_join(sentiment_afinn, by = c("word_tokens" = "word")) |>
  left_join(sentiment_bing, by = c("word_tokens" = "word")) |>
  select(createdAt, word_tokens, nrc:bing)






# PART 6d: VISUALIZING THE COAL RELATED DISCUSSION NRC SENTIMENT OVER TIME

# Aggregate tokenize_subreddit_coal by month
tokenize_subreddit_coal_monthly <- tokenize_subreddit_coal |>
  filter(createdAt >= start_date & createdAt <= end_date) |>
  mutate(Month = as.Date(format(createdAt, "%Y-%m-01"))) |>
  group_by(Month, nrc) |>
  summarise(
    count = n(),
    .groups = "drop"
  )

# Separate positive-negative sentiments
pos_neg_data <- tokenize_subreddit_coal_monthly |>
  filter(nrc %in% c("positive", "negative"))


# All NRC sentiments
all_nrc_data <- tokenize_subreddit_coal_monthly |>
  filter(!is.na(nrc))


# Define positive and negative sentiment groups
positive_sentiments <- c("anticipation", "joy", "positive", "surprise", "trust")
negative_sentiments <- c("anger", "disgust", "fear", "negative", "sadness")

# Adjust top plot (positive-negative sentiments)
nrc_plot_pos_neg <- ggplot(pos_neg_data, aes(x = Month, y = count, fill = nrc)) +
  geom_col(show.legend = FALSE, position = position_dodge(width = 25)) +
  geom_text(aes(label = count), vjust = 0, size = 3, position = position_dodge(width = 25)) +
  labs(
    title = "NRC Sentiment Analysis (Positive-Negative and Complete) Over Time",
    subtitle = "Top: Positive-Negative Sentiment; Bottom: Complete NRC Sentiment",
    x = NULL,
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, face = "italic"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "1 month"
  ) +
  scale_fill_manual(values = c("positive" = "lightgreen", "negative" = "orange"))

# Adjust bottom plot (split positive and negative sentiments, no data labels)
nrc_plot_full <- ggplot(all_nrc_data, aes(x = Month, fill = nrc)) +
  geom_col(
    aes(y = ifelse(nrc %in% positive_sentiments, count, 0)),
    show.legend = TRUE, position = "stack"
  ) +
  geom_col(
    aes(y = ifelse(nrc %in% negative_sentiments, -count, 0)),
    show.legend = FALSE, position = "stack"
  ) +
  labs(
    x = "Month",
    y = "Count",
    fill = "Sentiment"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "1 month"
  ) +
  scale_y_continuous(
    name = "Count",
    breaks = seq(-2000, 2000, by = 500),
    labels = abs(seq(-2000, 2000, by = 500))
  ) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Set3")) +
  guides(fill = guide_legend(nrow = 1, title.position = "top"))

# Combine the two plots vertically
combined_plot <- nrc_plot_pos_neg / nrc_plot_full +
  plot_layout(heights = c(3, 5))

print(combined_plot)

# Save the plot

# Ensure "Images" folder exists in main_path
images_folder <- file.path(main_path, "Images")
if (!dir.exists(images_folder)) {
  dir.create(images_folder)
}
ggsave(
  filename = file.path(images_folder, "3.2 Tokenize_subreddit_coal_NRC_sentiment.png"),
  plot = combined_plot,
  width = 14, height = 8, dpi = 600
)






# PART 6e: VISUALIZING THE COAL POWER PLANT RELATED DISCUSSION NRC SENTIMENT OVER TIME

# Aggregate tokenize_subreddit_coal_pp by month
tokenize_subreddit_coal_pp_monthly <- tokenize_subreddit_coal_pp |>
  filter(createdAt >= start_date & createdAt <= end_date) |>
  mutate(Month = as.Date(format(createdAt, "%Y-%m-01"))) |>
  group_by(Month, nrc) |>
  summarise(
    count = n(),
    .groups = "drop"
  )

# Separate positive-negative sentiments
pos_neg_data_pp <- tokenize_subreddit_coal_pp_monthly |>
  filter(nrc %in% c("positive", "negative"))


# All NRC sentiments
all_nrc_data_pp <- tokenize_subreddit_coal_pp_monthly |>
  filter(!is.na(nrc))

# Adjust top plot (positive-negative sentiments) for tokenize_subreddit_coal_pp
nrc_plot_pos_neg_pp <- ggplot(pos_neg_data_pp, aes(x = Month, y = count, fill = nrc)) +
  geom_col(show.legend = FALSE, position = position_dodge(width = 25)) +
  geom_text(aes(label = count), vjust = 0, size = 3, position = position_dodge(width = 25)) +
  labs(
    title = "NRC Sentiment Analysis (Positive-Negative and Complete) Over Time for Coal Power Plants",
    subtitle = "Top: Positive-Negative Sentiment; Bottom: Complete NRC Sentiment",
    x = NULL,
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, face = "italic"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "1 month"
  ) +
  scale_fill_manual(values = c("positive" = "lightgreen", "negative" = "orange"))

# Adjust bottom plot (split positive and negative sentiments, no data labels) for tokenize_subreddit_coal_pp
nrc_plot_full_pp <- ggplot(all_nrc_data_pp, aes(x = Month, fill = nrc)) +
  geom_col(
    aes(y = ifelse(nrc %in% positive_sentiments, count, 0)),
    show.legend = TRUE, position = "stack"
  ) +
  geom_col(
    aes(y = ifelse(nrc %in% negative_sentiments, -count, 0)),
    show.legend = FALSE, position = "stack"
  ) +
  labs(
    x = "Month",
    y = "Count",
    fill = "Sentiment"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "1 month"
  ) +
  scale_y_continuous(
    name = "Count",
    breaks = seq(-2000, 2000, by = 500),
    labels = abs(seq(-2000, 2000, by = 500))
  ) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Set3")) +
  guides(fill = guide_legend(nrow = 1, title.position = "top"))

# Combine the two plots vertically
combined_plot_pp <- nrc_plot_pos_neg_pp / nrc_plot_full_pp +
  plot_layout(heights = c(3, 5))


print(combined_plot_pp)

# Save the plot for tokenize_subreddit_coal_pp

# Ensure "Images" folder exists in main_path
images_folder <- file.path(main_path, "Images")
if (!dir.exists(images_folder)) {
  dir.create(images_folder)
}
ggsave(
  filename = file.path(images_folder, "3.3 Tokenize_subreddit_coal_pp_NRC_sentiment.png"),
  plot = combined_plot_pp,
  width = 14, height = 8, dpi = 600
)






# PART 6f: VISUALIZING THE RELATED DISCUSSION FOR AFINN AND BING SENTIMENT OVERTIME

# AFINN Sentiment Aggregation
afinn_monthly <- tokenize_subreddit_coal |>
  filter(!is.na(afinn), createdAt >= start_date & createdAt <= end_date) |>
  group_by(Month = floor_date(createdAt, "month")) |>
  summarise(
    avg_afinn = mean(afinn, na.rm = TRUE),
    total_words = n(),
    .groups = "drop"
  )

# Convert Month back to Date for proper plotting
afinn_monthly <- afinn_monthly |>
  mutate(Month = as.Date(paste0(Month, "-01")))


# Define important dates for AFINN with y positions for annotation
important_dates_afinn <- data.frame(
  Date = as.Date(c("2022-02-24", "2022-03-08", "2022-06-19", "2022-08-01")),
  Event = c(
    "Feb 24, 2022: Russia invaded Ukraine",
    "Mar 8, 2022: EU cuts Russian gas dependency",
    "Jun 19, 2022: Germany reopens coal plants",
    "Aug 1, 2022: Coal plants resume operations"
  ),
  y_position = c(0.5, 0.1, -0.3, -0.7)  # Different heights for annotations
)


# AFINN Sentiment Plot
afinn_plot <- ggplot(afinn_monthly, aes(x = Month, y = avg_afinn)) +
  geom_line(size = 1.2, color = "blue") +
  geom_point(size = 2, color = "blue") +
  geom_text(aes(label = round(avg_afinn, 2)), vjust = -0.5, size = 3) +
  # Important dates as vertical lines
  geom_segment(
    data = important_dates_afinn,
    aes(x = Date, xend = Date, y = min(afinn_monthly$avg_afinn), yend = y_position),
    color = "black",
    linetype = "dashed",
    size = 0.6
  ) +
  geom_text(
    data = important_dates_afinn,
    aes(
      x = Date,
      y = y_position,  # Use custom y positions for annotations
      label = Event
    ),
    inherit.aes = FALSE,
    size = 3.25,
    color = "black",
    angle = 0,
    hjust = 0,
    vjust = -0.5
  ) +
  labs(
    title = "AFINN Sentiment Score Over Time",
    subtitle = "Monthly average AFINN sentiment scores for coal-related discussions",
    x = "Month",
    y = "Average Sentiment Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(
    limits = c(as.Date("2022-01-01"), as.Date("2022-12-31")),  # Ensure x-axis ends at December 2022
    date_labels = "%b %Y",
    date_breaks = "1 month"
  ) +
  scale_y_continuous(
    limits = c(min(afinn_monthly$avg_afinn, na.rm = TRUE) - 0.1, 
               max(afinn_monthly$avg_afinn, na.rm = TRUE) + 0.1),  # Adjust y-axis range
    expand = expansion(mult = 0.05)  # Add a small margin
  )

# Save the AFINN plot
print(afinn_plot)

images_folder <- file.path(main_path, "Images")
ggsave(
  filename = file.path(images_folder, "3.4 tokenize_subreddit_coal_afinn_sentiment.png"),
  plot = afinn_plot,
  width = 14, height = 8, dpi = 600
)



# BING Sentiment Aggregation
bing_monthly <- tokenize_subreddit_coal |>
  filter(!is.na(bing), createdAt >= start_date & createdAt <= end_date) |>
  group_by(Month = floor_date(createdAt, "month"), bing) |>
  summarise(count = n(), .groups = "drop")

bing_monthly <- bing_monthly |>
  mutate(Month = as.Date(paste0(Month, "-01")))


# Define important dates for BING with y positions for annotation
important_dates_bing <- data.frame(
  Date = as.Date(c("2022-02-24", "2022-03-08", "2022-06-19", "2022-08-01")),
  Event = c(
    "Feb 24, 2022: Russia invaded Ukraine",
    "Mar 8, 2022: EU cuts Russian gas dependency",
    "Jun 19, 2022: Germany reopens coal plants",
    "Aug 1, 2022: Coal plants resume operations"
  ),
  y_position = c(-450, -350, -250, -150)  # Different heights for annotations
)


# BING Sentiment Plot (Clustered Column with More Spacing)
bing_plot <- ggplot(bing_monthly, aes(x = Month, y = count, fill = bing)) +
  geom_col(position = position_dodge(width = 25), color = "black") +  # Increased dodge width
  geom_text(
    aes(label = count),
    position = position_dodge(width = 25),
    vjust = -0.5,
    size = 3
  ) +
  # Use the `important_dates_bing` for geom_segment
  geom_segment(
    data = important_dates_bing,
    aes(x = Date, xend = Date, y = 0, yend = y_position),
    inherit.aes = FALSE,  # Prevents inheriting aesthetics from the main plot
    color = "black",
    linetype = "dashed",
    size = 0.6
  ) +
  geom_text(
    data = important_dates_bing,
    aes(
      x = Date,
      y = y_position,  # Use custom y positions for annotations
      label = Event
    ),
    inherit.aes = FALSE,
    size = 3.25,
    color = "black",
    angle = 0,
    hjust = 0,
    vjust = -0.5
  ) +
  scale_fill_manual(values = c("positive" = "lightgreen", "negative" = "orange")) +
  labs(
    title = "BING Sentiment Analysis Over Time",
    subtitle = "Monthly BING sentiment scores for coal-related discussions",
    x = "Month",
    y = "Count",
    fill = "Sentiment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(
    limits = c(start_date, end_date),  # Ensure x-axis ends at December 2022
    date_labels = "%b %Y",
    date_breaks = "1 month"
  )

# Save the BING plot
print(bing_plot)

images_folder <- file.path(main_path, "Images")
ggsave(
  filename = file.path(images_folder, "3.5 tokenize_subreddit_coal_bing_sentiment.png"),
  plot = bing_plot,
  width = 14, height = 8, dpi = 600
)
