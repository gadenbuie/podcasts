library(tidyverse)
library(tidyRSS)
library(xml2)
library(glue)

# Look up here: https://castos.com/tools/find-podcast-rss-feed/
rss_feed <-
  # "https://feeds.megaphone.fm/ESP3054801210" # Disney Frozen: Forces of Nature
  "https://feeds.chrt.fm/noodle-loaf"
  # "https://feeds.megaphone.fm/RGS9185485759" # Rebel Girls
  # "https://feeds.megaphone.fm/storypirates"
  # "https://feeds.megaphone.fm/super-great-kids-stories"

podcasts <-
  read_xml(rss_feed) |>
  xml_find_all("//channel//item") |>
  map_dfr(function(node) {
    list(
      podcast = node |> xml_root() |> xml_find_first("//channel/title") |> xml_text(),
      title = node |> xml_find_first(".//title") |> xml_text(),
      link = node |> xml_find_first(".//link") |> xml_text(),
      date = node |> xml_find_first(".//pubDate") |> xml_text() |> anytime::anydate(),
      season = node |> xml_find_first(".//itunes:season") |> xml_text() |> as.integer(),
      episode = node |> xml_find_first(".//itunes:episode") |> xml_text() |> as.integer(),
      mp3_url = node |> xml_find_first(".//enclosure") |> xml_attr("url"),
      description = node |> xml_find_first(".//content:encoded") |> xml_text()
    )
  })


podcast_download <- function(podcast, date, season, episode, mp3_url, ...) {
  podcast <- snakecase::to_snake_case(podcast)
  if (is.na(episode)) episode <- 99
  dir <- fs::path("podcasts", podcast, sprintf("s%02d", season))
  file <- sprintf("%s-%s-s%02de%02d.mp3", podcast, date, season, episode)
  file <- fs::path(dir, file)
  if (fs::file_exists(file)) return()
  fs::dir_create(dir, recurse = TRUE)
  cli::cli_progress_step("{.path {file}}")
  downloader::download(mp3_url, file, quiet = TRUE)
}

podcasts |>
  filter(date > "2022-01-01") |>
  tidyr::replace_na(list(season = 0)) |>
  pwalk(podcast_download)
