######################
## UTILITY FUNCTIONS #
######################

## function that provides summary statistics about clusters
summary_stats <- function(features, clust) {
  require(dplyr)
  require(tidyr)
  require(stringr)
  
  features |>
    mutate(clust = factor(clust)) -> df
  
  df |>
    group_by(clust) |>
    rstatix::get_summary_stats(type = "mean_sd", show = c("mean", "sd")) |>
    mutate(mean_sd = str_glue("{mean} ({sd})")) |>
    tidyr::pivot_wider(id_cols = clust, names_from = variable, values_from = mean_sd) |> 
    t() |>
    as.data.frame() -> stats_df
  
  stats_df[1, ] <- table(clust) |> as.integer()
  row.names(stats_df)[1] <- "N"
  colnames(stats_df) <- paste0("CLUST_", 1:n_distinct(clust))
  
  stats_df
}


## The following two functions are for creating and arranging a set of
## box plots, one plot for each attribute that was used for clustering.
## The purpose is to visually compare the distribution of the attributes
## across the clusters
# 
## The 'main' function is create_comparison_plots() that receives 2 input parameters:
## - a data frame with the attributes used for clustering
## - a vector of cluster assignments
## The function creates and arranges box plots for all the attributes.
# 
## The create_attr_boxplot() is a helper function for the create_comparison_plots() f.
## that creates a box plot for one attribute
create_attr_boxplot <- function(df, attribute, clust_var) {
  ggplot(data = df,
         mapping = aes(x=.data[[clust_var]], 
                       y=.data[[attribute]], 
                       fill=.data[[clust_var]])) +
    geom_boxplot() + 
    labs(y = attribute, x = "") +
    theme_classic()
}

create_comparison_plots <- function(df, clust, ncol=3) {
  require(dplyr)
  require(ggpubr)
  
  df_clust <- df
  df_clust[['cluster']] <- as.factor(clust)
  boxplots <- lapply(colnames(df), 
                     function(x) create_attr_boxplot(df_clust, x, 'cluster'))
  
  ggarrange(plotlist = boxplots,
            ncol = ncol, 
            common.legend = TRUE, legend = "bottom",
            vjust = 1, hjust = -1, font.label = list(size=12))
  
}



