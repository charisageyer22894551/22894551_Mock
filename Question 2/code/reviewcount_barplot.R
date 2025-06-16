reviewcount_barplot(df, xaxis_size = 6, xaxis_rows = 2){


    # data preparation
    df %>%
        group_by("country") %>%
        count()


}