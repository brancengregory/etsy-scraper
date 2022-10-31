library(tidyverse)
library(rvest)

root_url <- "https://www.etsy.com/shop/LaserTree"


get_product_links <- function(html) {
  links <- html |>
    html_elements(css = "div[data-listing-id]") |>
    html_elements(css = "a") |>
    html_attr("href")

  return(links)
}


get_product_details <- function(links) {
  links |>
    map(read_html)

  data <- tibble()

  data$name <- links |>
    map(~html_element(., css = "h1[data-buy-box-listing-title]") |> html_text2()) |>
    unlist()

  data$price <- links |>
    map(~html_element(., xpath = '//*[@id="listing-page-cart"]/div[3]/div/div[1]/div[1]/div[1]/div/div[1]/p/span[2]') |> html_text2()) |>
    unlist()

  return(data)
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

    if (length(links) == 0L || inherits(links, "error")) {
      flag <- FALSE
      next()
    } else {
      all_links[[i]] <- links
    }

    i <- i + 1
    next()
  }

  all_links <- all_links |>
    flatten()

  product_details <- get_product_details(all_links)

  return(product_details)
}

test <- get_store_products(root_url)


