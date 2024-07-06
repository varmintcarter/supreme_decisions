# supreme_court: trump v united states

# libraries  
library(tidyverse)
library(pdftools)
library(tidytext)
library(skimr)
# data ----
text <- pdf_text("./Data Science/trump_v_united_states/23-939_e2pg.pdf") |> 
  read_lines() # a revelation! https://www.r-bloggers.com/2018/03/extracting-pdf-text-with-r-and-creating-tidy-data/
my_tibble <- enframe(text) |> 
  rename(linenumber = name,
         text = value)

df <- my_tibble |> 
  mutate(text = str_squish(text)) |> 
  mutate(row_len = str_length(text)) |> 
  filter(row_len != 0)
  # n = 309


## even page pattern contains the name of the case----
df <- my_tibble |> 
  mutate(text = str_squish(text)) |> 
  mutate(row_len = str_length(text)) |> 
  filter(row_len != 0) |> 
  # n = 309
  mutate(trump_v_united_states = str_detect(text,
                                            regex("TRUMP v. UNITED STATES"))) |> 
  # case name removes all rows with even page numbers
  filter(trump_v_united_states == FALSE) |>
  # n = 59
  select(-trump_v_united_states)

## row_numbers for the odd page pattern ----
section_page_num <- my_tibble |> 
  mutate(text = str_squish(text)) |> 
  mutate(row_len = str_length(text)) |> 
  filter(row_len != 0) |> 
  mutate(odd_page_num = str_detect(text, 
                                   regex("^(Cite|\\(Slip Opinion\\))"))) |> 
  filter(odd_page_num == TRUE) |> 
  mutate(odd_value = str_extract(text, "([:digit:]+)$")) |> 
  select(linenumber, odd_value)

