library(tidyverse)
library(rvest)

root_url <- "https://www.etsy.com/shop/LaserTree"

# root_html <- rvest::read_html(root_url)
#
# products <- root_html |>
#   html_elements(css = "div[data-listing-id]")
#
# imgs <- products |>
#   html_elements(css = "img")
#
# links <- products |>
#   html_elements(css = "a")
#
#
#
# expected_rows <- length(imgs)
#
# links1 <- imgs |>
#   html_attr("src")
#
# links2 <- imgs |>
#   html_attr("data-src")
#
# links <- c(links1, links2)
# links <- links[!is.na(links)]
#
# returned_rows <- length(links)
#
# expected_rows == returned_rows
#
# links


# for(i in 1:3) {
#   url <- str_c(root_url, "?page=", i, sep = "")
#   html <- read_html(url)
#
#   products <- html |>
#     html_elements(css = "div[data-listing-id]")
#
#   imgs <- products |>
#     html_elements(css = "img")
#
#   links <- products |>
#     html_elements(css = "a")
# }


get_product_links <- function(html) {
  html |>
    html_elements(css = "div[data-listing-id]") |>
    html_elements(css = "a")
}

get_product_details <- function() {

}

get_store_products <- function(root_url) {
  flag <- TRUE
  i <- 1
  all_links <- list()
  while(flag) {
    url <- str_c(root_url, "?page=", i, sep = "")
    html <- read_html(url)

    links <- try(
      get_product_links(html)
    )

    if (inherits(links, "error")) {
      flag <- FALSE
      next()
    } else {
      all_links[[i]] <- links
    }

    i <- i + 1
  }

  return(all_links)
}

test <- get_store_products(root_url)


