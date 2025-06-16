geom_ladder_barplot <- function(datcolat, Title, Subtitle,
                                                 xaxis_size = 10,
                                                 xaxis_rows = 2){

    # This removes the ugly summarise warning...
    options(dplyr.summarise.inform=F)

    dfp <-
        datcolat %>%
        group_by(`Regional indicator`) %>%
        summarise_at( vars( c(`Ladder score`, starts_with("Explained"), `Dystopia + residual`)), ~mean(.)) %>%
        gather(Score, Value, -`Regional indicator`, -`Ladder score`) %>%
        mutate(Score = gsub("Explained by: ", "", Score))

    # Make SA data bindable:
    SA <-
        datcolat %>%
        filter(`Country name` == "South Africa") %>%
        select(c(`Regional indicator`, `Ladder score`, starts_with("Explained"), `Dystopia + residual`)) %>%
        mutate(`Regional indicator` = "South Africa") %>%
        gather(Score, Value, -`Regional indicator`, -`Ladder score`) %>%
        mutate(Score = gsub("Explained by: ", "", Score))

    # See this makes your life much easier:
    dfp <- bind_rows(dfp,SA)

    # Arrange the Score column for consistency as from highest avg to lowest:

    order1 <- c("South Africa",
                datcolat %>%
                    group_by(`Regional indicator`) %>%
                    summarise(Lad = mean(`Ladder score`)) %>%
                    arrange(Lad) %>%
                    pull(`Regional indicator`))

    order2 <- dfp %>%
        group_by(Score) %>%
        summarise(Avg = mean(Value)) %>%
        arrange(Avg) %>% pull(Score)

    g <-
        dfp %>%
        plot_orderset(., Column = "Regional indicator", Order = order1) %>%
        plot_orderset(., Column = "Score", Order = order2) %>%
        ggplot() +
        geom_bar(aes(`Regional indicator`, y = Value, fill = Score), stat = "identity", position = "stack") +
        theme_bw() +
        scale_fill_brewer(palette="Dark2") +
        scale_x_discrete(guide = guide_axis(n.dodge = xaxis_rows)) +
        labs(title = Title, subtitle = Subtitle, caption = "Data source: World Happiness Index", x = "", y = "Breakdown of Happiness") +
        theme(legend.position = "top", legend.title = element_blank()) +
        theme(plot.title = element_text(size = 14),
              plot.subtitle = element_text(size = 12))

    g


}
#
# theme(axis.text.x = element_text(angle = 90, size = xaxis_size, hjust = 1),
#                     legend.title = element_text(size = 10),
#                     plot.title = element_text(size = 14),
#                     plot.subtitle = element_text(size = 12))

# axis.text.x = element_text(angle = 90, size = 10)) +

















#create function for the barplot
#
#
# geom_ladder_barplot <- function(df, xaxis_size = 5, xaxis_rows = 2) {
#
#  # create the regions scores
# region_scores_nosa <- df %>%
#     filter(`Country name` != "South Africa") %>%
#     select(`Country name`, `Regional indicator`, `Ladder score`, starts_with("Explained by"), `Dystopia + residual`) %>% #  Keep only relevant columns
#     group_by(`Regional indicator`) %>%
#     summarise(across(starts_with("Explained by"), median, na.rm = T),
#                   `Dystopia + residual` = median(`Dystopia + residual`, na.rm = T),
#                   `Ladder score` = median(`Ladder score`, na.rm = T), .groups = "drop")
#     # create SA score
# SA_scores <- df %>%
#     filter(`Country name` == "South Africa") %>%
#     select(`Regional indicator`, starts_with("Explained by"),`Dystopia + residual`, `Ladder score`) %>%
#     summarise(across(everything(), ~ median(.x, na.rm = TRUE))) %>%
#     mutate(`Regional indicator` = "South Africa") %>%
#    # distinct()
#
# # combine and put in right order
# df_plot <- bind_rows(SA_scores, region_scores_nosa)
#
# df_plot <- df_plot %>%
#     mutate(`Regional indicator` = fct_reorder(`Regional indicator`, `Ladder score`, .desc = TRUE)) %>%
#     mutate(`Regional indicator` = fct_relevel(`Regional indicator`, "South Africa"))
#
#
# # make the data tidier
# tidy_df <- df_plot %>%
#     pivot_longer(cols = starts_with("Explained by") | matches("Dystopia \\+ residual"),
#                  names_to = "Component", values_to = "Value")
#
#
#         # create the ggplot
# # Step 7: Plot
# g <- ggplot(tidy_df) +
#     geom_bar(aes(x = `Regional indicator`, y = Value, fill = Component), stat = "identity") +
#     theme_bw() +
#     scale_x_discrete(guide = guide_axis(n.dodge = xaxis_rows)) +
#     theme(axis.text.x = element_text(angle = 90, size = xaxis_size, hjust = 1),
#           legend.title = element_blank(),
#           plot.title = element_text(size = 14),
#           panel.grid.major.y = element_line(color = "grey80"),
#           plot.subtitle = element_text(size = 12)) +
#     labs(title = "Ladder Score Breakdown by Region",
#          subtitle = "With South Africa shown first",
#          x = "", y = "Ladder Score",
#          caption = "Data source: World Happiness Index") +
#          guides(fill = guide_legend(title = "Component"))
#
# return(g)
#
# }
#
#

# geom_ladder_barplot <- function(df, xaxis_size = 5, xaxis_rows = 2) {
#
#     # Region summaries (excluding SA)
#     region_scores_nosa <- df %>%
#         filter(`Country name` != "South Africa") %>%
#         select(`Country name`, `Regional indicator`, `Ladder score`,
#                starts_with("Explained by"), `Dystopia + residual`) %>%
#         group_by(`Regional indicator`) %>%
#         summarise(across(starts_with("Explained by"), median, na.rm = TRUE),
#                   `Dystopia + residual` = median(`Dystopia + residual`, na.rm = TRUE),
#                   Ladder_score = median(`Ladder score`, na.rm = TRUE),
#                   .groups = "drop")
#
#     # South Africa real values (aggregated to match structure)
#     SA_scores <- df %>%
#         filter(`Country name` == "South Africa") %>%
#         select(`Regional indicator`, starts_with("Explained by"), `Dystopia + residual`, `Ladder score`) %>%
#         summarise(across(everything(), ~ median(.x, na.rm = TRUE))) %>%
#         mutate(`Regional indicator` = "South Africa")
#
#     # Combine
#     df_plot <- bind_rows(SA_scores, region_scores_nosa)
#
#     # Reorder: SA first
#     df_plot <- df_plot %>%
#         filter(!is.na(Ladder_score)) %>%
#         mutate(`Regional indicator` = fct_reorder(`Regional indicator`, Ladder_score, .desc = TRUE)) %>%
#         mutate(`Regional indicator` = fct_relevel(`Regional indicator`, "South Africa"))
#
#     # Tidy data
#     tidy_df <- df_plot %>%
#         pivot_longer(cols = starts_with("Explained by") | matches("Dystopia \\+ residual"),
#                      names_to = "Component", values_to = "Value")
#     print(tidy_df)
#
#     # Plot
#     g <- ggplot(tidy_df) +
#         geom_bar(aes(x = `Regional indicator`, y = Value, fill = Component), stat = "identity") +
#         theme_bw() +
#         scale_x_discrete(guide = guide_axis(n.dodge = xaxis_rows)) +
#         theme(axis.text.x = element_text(angle = 90, size = xaxis_size, hjust = 1),
#               legend.title = element_text(size = 10),
#               plot.title = element_text(size = 14),
#               plot.subtitle = element_text(size = 12)) +
#         labs(title = "Ladder Score Breakdown by Region",
#              subtitle = "With South Africa shown first",
#              x = "", y = "Ladder Score",
#              caption = "Data source: World Happiness Index")
#
#     return(g)
# }
