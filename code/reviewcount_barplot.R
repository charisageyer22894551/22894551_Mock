reviewcount_barplot(df, xaxis_size = 6, xaxis_rows = 2){


    # data preparation
    df_plot <- df %>%
        group_by("country") %>%
        summarise(Count_reviews = n(),
                  Median_score = median(points, na.rm = T)) %>%
        arrange(desc(Count_reviews)) %>%
        filter(!is.na(country))

# create orders for consistent bar order - use  the q1 plotorder function
    country_order <- df_plot %>% pull(country)
    df_plot <- df_lot %>%
        plot_orderset("country", country_order)

    # create the ggplot

    g <- df_plot %>%
        ggplot() +
        geom_bar(aes(x = country, y = Count_reviews, fill = country), stat = "identity") +
        geom_text(aes(x = country, y = Count_reviews, label = round(Median_score, 1)), vjust = -0.3, size = 3.5) +
        theme_bw() +
        scale_x_discrete(guide = guide_axis(n.dodge = xaxis_rows)) +
        labs(title = "Number of Wine Reviews by Country",
             subtitle = "With Median Wine Score Displayed Above Each Bar",
             caption = "Data source: WineMag.com",
             x = "Country", y = "Number of Reviews") +
       theme(plot.title = element_text(size = 14),
                      plot.subtitle = element_text(size = 12),
                      axis.text.x = element_text(size = xaxis_size),
             axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


g

}