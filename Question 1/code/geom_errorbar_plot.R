# Creating a function for the errorgeombarplot

geom_errorbar_plot <- function(df, xaxis_size = 5, xaxis_rows = 3){
    dfplot <- df %>%
        group_by(`Regional indicator`) %>%
        summarise_at(vars(c(`Ladder score`, ends_with("whisker"))), ~median(.))

    # for life expectancy
    Healthy_LE <- df %>%
        group_by(`Regional indicator`) %>%
        summarise_at(vars(`Healthy life expectancy`), ~median(.)) %>%
        rename(HE = `Healthy life expectancy`) %>%
        mutate(HE = round(HE, 1))

    # now join the two

    dfplot <-
        left_join(dfplot,
                  Healthy_LE,
                  by = "Regional indicator")

    # now reorder
    # create the correct order

    order <- dfplot %>% arrange(Healthy_LE) %>%
        pull(`Regional indicator`)

    #update dfplot with new order
    dfplot <- dfplot %>% plot_orderset(., Column = "Regional indicator", Order = order)

    # create ggplot
    g <-
        dfplot %>%
        ggplot() +
        geom_point(aes(x = `Regional indicator`, y = `Ladder score`, color = `Regional indicator`), size = 4, alpha = 0.8) +
        geom_errorbar(aes(x = `Regional indicator`,
                          ymin = lowerwhisker, ymax = upperwhisker, color = `Regional indicator`)) +
        geom_text(aes(`Regional indicator`, y = upperwhisker, label = HE), vjust = 0) +


        theme_bw() +
        scale_x_discrete(guide = guide_axis(n.dodge = xaxis_rows)) +
        labs(title = "Happiness Index", subtitle = "Distribution by region", caption = "Data source: World Happiness Index", x = "", y = "Happiness Score") +
        theme(legend.position = "top", legend.title = element_blank()) +
        theme(plot.title = element_text(size = 14),
              plot.subtitle = element_text(size = 12),
              axis.text.x = element_text(size = xaxis_size)) +
        guides(scale = "none", color = F)
    # If you wanted to make the x-axis vertical:
    # theme(axis.text.x=element_text(angle = 90, hjust = 1))

    g

}


