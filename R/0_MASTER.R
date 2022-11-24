library(tidyverse)
library(rtoot)
library(pxweb)
library(ggplot2)

access_token <- Sys.getenv("MASTODON_TOKEN")
type <- "user"
instance <- "mstdn.party"

token <- structure(
    list(
        bearer = access_token,
        type = type,
        instance = instance
    ),
    class = "rtoot_bearer"
)

source("R/99_Eikonomics_theme_work_in_progress.R")
source("R/1_gohst_process_data.R")
source("R/2_make_the_plot_and_text.R")

post_toot(
    status = text,
    media = filename,
    alt_text = text,
    token = token
)
