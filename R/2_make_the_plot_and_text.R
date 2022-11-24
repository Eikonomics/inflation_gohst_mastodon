
## Step 1 Create the chart to be published on Mastodon
plot_price_index <-
  ggplot() +
  geom_line(
    data = product_data,
    aes(
      x = ym, y = new_index,
      group = product_id, color = Undirvísitala
    ),
    linewidth = 1.3
  ) +
  ylab("Vísitala") +
  xlab("Ár") +
  ggtitle(random_product_name,
    subtitle = paste0(
      "Verðþróun, ",
      min(product_data$ym),
      " til ",
      max(product_data$ym),
      " (",
      min(product_data$ym),
      " = 100)"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 32),
    plot.subtitle = element_text(hjust = 0.5, size = 20),
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(hjust = 0.5)
  )

filename <- tempfile(pattern = "file", fileext = ".png")
print(filename)

ggsave(
  plot = plot_price_index,
  file = filename,
  height = 9,
  width = 20, bg = "white"
)

Sys.sleep(5)

## Step 2. Make the text for the chart

# Find the last price datapoint in the time series of the category
last_price <-
  product_data$new_index[product_data$ym == max(product_data$ym) & product_data$product_id == random_product]

# Find the last price data point in the overall index time series
last_price_visi <-
  product_data$new_index[product_data$ym == max(product_data$ym) & product_data$product_id != random_product]

# Set up what the gohst says, depending on what the data shows
Brag <- "verr"
Brag2 <- "Verði ykkur að góðu."

if (last_price_visi < last_price) {
  Brag <- "betur"
  Brag2 <- " Sorrý."
}
if (last_price_visi == last_price) {
  Brag <- "jafn vel"
}

# Store text in a var
text <-
  paste("Ég hef staðið mig ", Brag, " í að hækka verð á ", "'",
    random_product_name, "'", " heldur en öðrum vörum almennt. ",
    Brag2,
    sep = ""
  )

print(text)
