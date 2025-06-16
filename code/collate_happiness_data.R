# Load tidyverse
library(tidyverse)

# Define function to collate happiness CSV files
collate_happiness_data <- function(folder_path = "Data/Happy") {
    # List all CSV files in the folder
    files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

    # Read and combine them into one tibble
    combined_data <- files %>%
        set_names() %>%
        map_dfr(
            ~ suppressMessages(read_csv(.x, show_col_types = FALSE)) %>%
                mutate(year = str_extract(basename(.x), "\\d{4}")),
            .id = "source_file"
        ) %>%
        select(-source_file) %>%
        mutate(year = as.integer(year))

    return(combined_data)
}

# Use the function to load the data
happy_data <- collate_happiness_data()

# Optional: View the first few rows
glimpse(happy_data)
