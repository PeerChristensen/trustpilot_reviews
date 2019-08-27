

library(tidyverse)
library(trustpilotR)

nike <- get_reviews("https://www.trustpilot.com/review/www.nike.com",page_lim = 50)

write_csv(nike,"nike_reviews.csv")

wm <- get_reviews("https://www.trustpilot.com/review/www.walmart.com",page_lim = 50)

write_csv(wm,"walmart_reviews.csv")
