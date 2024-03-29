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
  links <- links |>
    map(read_html)

  name <- links |>
    map(~ html_element(., css = "h1[data-buy-box-listing-title]") |> html_text2()) |>
    unlist()

  price <- links |>
    map(~ html_element(., xpath = '//*[@id="listing-page-cart"]/div[3]/div/div[1]/div[1]/div[1]/div/div[1]/p') |>
      html_text2()) |>
    unlist()

  data <- tibble(
    name = name,
    price = price
  ) |>
    mutate(
      price = map_dbl(
        price,
        ~ str_match(
          .,
          "(?:Price: )?(?:\\$)?(.*)(?:\\+)"
        ) |>
          last() |>
          as.numeric()
      )
    )

  return(data)
}


get_store_products <- function(root_url) {
  flag <- TRUE
  i <- 1
  all_links <- list()
  while (flag) {
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

df <- test |>
  mutate(
    product_cake_topper = str_detect(
      name,
      "(?i)topper"
    ),
    product_ornament = str_detect(
      name,
      "(?i)ornament"
    ),
    product_book = str_detect(
      name,
      "(?i)book"
    ),
    product_upgrade = str_detect(
      name,
      "(?i)(rush|upgrade|extra)"
    )
  ) |>
  pivot_longer(
    cols = starts_with("product_"),
    names_to = "type",
    names_pattern = "product_(.*)"
  ) |>
  filter(value == T) |>
  select(-value)

# df |>
#   write_csv("data/products.csv")