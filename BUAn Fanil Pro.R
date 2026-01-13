# ================================
# Load Data (Base R)
# ================================

spotify <- read.csv("spotify_tracks.csv")

# -------------------------------
# Research Question 1
# Danceability vs Popularity
# Method: Correlation
# -------------------------------

cor_dance_pop <- cor(
  spotify$danceability,
  spotify$popularity,
  use = "complete.obs"
)
cor_dance_pop

plot(
  spotify$danceability,
  spotify$popularity,
  pch = 16,
  cex = 0.5,
  main = "Danceability vs Popularity",
  xlab = "Danceability",
  ylab = "Popularity"
)
abline(lm(popularity ~ danceability, data = spotify), col = "blue")

# -------------------------------
# Research Question 2
# Valence vs Popularity
# Method: Multivariate Graph
# -------------------------------

plot(
  spotify$valence,
  spotify$popularity,
  pch = 16,
  cex = 0.5,
  main = "Valence vs Popularity",
  xlab = "Valence",
  ylab = "Popularity"
)
abline(lm(popularity ~ valence, data = spotify), col = "red")

# -------------------------------
# Research Question 3
# Artist consistency
# Method: Group Comparisons
# -------------------------------

artist_counts <- table(spotify$artist_name)
valid_artists <- names(artist_counts[artist_counts >= 5])

filtered <- spotify[spotify$artist_name %in% valid_artists, ]

artist_summary <- aggregate(
  cbind(popularity, energy, tempo, danceability) ~ artist_name,
  data = filtered,
  FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                      sd = sd(x, na.rm = TRUE))
)

artist_summary

# -------------------------------
# Research Question 4
# Distribution of Energy
# Method: Univariate Graph
# -------------------------------

hist(
  spotify$energy,
  breaks = 30,
  main = "Distribution of Energy",
  xlab = "Energy"
)

# -------------------------------
# Research Question 5
# Tempo by Popularity Quartile
# Method: Group Comparisons
# -------------------------------
quantiles <- unique(
  quantile(spotify$popularity, probs = seq(0, 1, 0.25), na.rm = TRUE)
)

pop_quartile <- cut(
  spotify$popularity,
  breaks = quantiles,
  include.lowest = TRUE
)

# -------------------------------
# Research Question 6
# Popularity Ranges
# Method: Frequency Tables
# -------------------------------

pop_group <- cut(
  spotify$popularity,
  breaks = c(0, 25, 50, 75, 100),
  labels = c("Low", "Medium", "High", "Very High"),
  include.lowest = TRUE
)

table(pop_group)

# -------------------------------
# Research Question 7
# Audio Feature Correlations
# Method: Correlations
# -------------------------------

features <- spotify[, c(
  "popularity",
  "danceability",
  "energy",
  "valence",
  "tempo",
  "loudness",
  "acousticness",
  "instrumentalness",
  "speechiness",
  "duration_ms"
)]


cor(features, use = "complete.obs")

# -------------------------------
# Research Question 8
# Duration vs Popularity (Past 5 Years)
# Method: Group Comparisons
# -------------------------------

max_year <- max(spotify$release_year, na.rm = TRUE)
recent <- spotify[spotify$release_year >= max_year - 5, ]

duration_min <- recent$duration_ms / 60000

duration_group <- cut(
  duration_min,
  breaks = c(0, 2.5, 3, 3.5, 4, 10),
  labels = c("Under 2.5", "2.5–3", "3–3.5", "3.5–4", "Over 4")
)

tapply(recent$popularity, duration_group, mean, na.rm = TRUE)
