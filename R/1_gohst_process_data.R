library(pxweb)
library(tidyverse)
library(lubridate)

# Get the data from Stats Icleand
inflation_data_hagst <-
  pxweb_get(
    url = "http://px.hagstofa.is//pxis/api/v1/is/Efnahagur/visitolur/1_vnv/2_undirvisitolur/VIS01304.px",
    query = list(
      "Mánuður" = c("*"),
      "Undirvísitala" = c("*")
    )
  )
# convert data to a data frame
gohst_data <-
  as.data.frame(inflation_data_hagst, column.name.type = "text", variable.value.type = "text")

# Fix month and generate a group id for each category of the CPI basket
gohst_data <-
  gohst_data %>%
  mutate(ym = ym(paste0(substr(Mánuður, 1, 4), substr(Mánuður, 6, 8)))) %>%
  group_by(Undirvísitala) %>%
  mutate(product_id = cur_group_id())

# Find the number of items in the basked, excluding the average of the basket
MaxId <-
  max(gohst_data$product_id[gohst_data$Undirvísitala != "Vísitala neysluverðs"])

# Randomly pick an item to analyse
random_product <-
  sample(1:MaxId, 1, replace = FALSE)

# Find the name of the categoyr and store it in a var
random_product_name <-
  gohst_data$Undirvísitala[gohst_data$product_id == random_product][1]

# filter the data for easy ploting later
# re-base the CPI, in order to compare price developement over the last 24 months.
product_data <-
  gohst_data %>%
  filter(Undirvísitala == "Vísitala neysluverðs" | product_id == random_product) %>%
  filter(ym > floor_date(ym(substr(Sys.Date(), 1, 7)) - 365 * 2)) %>%
  group_by(Undirvísitala) %>%
  mutate(new_index = 100 * `Vísitala neysluverðs` / `Vísitala neysluverðs`[1])
