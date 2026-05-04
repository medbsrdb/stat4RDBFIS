
lfd_plot_rdbfis1 <- function(raised_lfd, out_dir, catch_cat){
  
plot_lfd <- raised_lfd
mth <- unique(plot_lfd$method) 

plot_lfd %>%
  group_by(species, area) %>%
  group_walk(~{
    n_rows <- length(unique(.x$metier))
    n_cols <- length(unique(.x$year))
    p <- ggplot(.x, aes(lc, N_final, color = factor(quarter))) +
      geom_line(linewidth = 0.5) + geom_point(size = 1)+
      facet_grid(year ~ metier, scales = "free_y") +
      ylab("N") +xlab('length (mm)') +
      theme_bw() + theme(legend.position = "bottom") +
      ggtitle(paste(.y$species, .y$area))+
      labs(color = "quarter")
    
    ggsave(
      file.path(out_dir,paste0(catch_cat, '_lfd1_', .y$species, "_", .y$area, "_", mth, ".png")),
      width = 10 + n_cols * 2,   # scale with columns
      height = 8 + n_rows * 2,  # scale with rows
      units = "cm", 
      dpi = 300,
      plot = p
    )
  })
}