
#' Plot raised LFDs by species, area, quarter and metier
#'
#' Internal helper used by `landings_rdbfis()` and `discards_rdbfis()`.
#'
#' @param raised_lfd data frame with raised LFD results.
#' @param out_dir output folder for PNG plots.
#' @param output_type label used in output filenames. Usually "Land" or "Disc".
#' @keywords internal
#' @import ggplot2
#' @noRd
lfd_plot_rdbfis <- function(raised_lfd, out_dir, output_type = "Land"){
  
  plot_lfd <- raised_lfd %>%
    mutate(year_quarter = paste0(year, " Q", quarter))
  
  output_prefix <- switch(
    tolower(output_type),
    land = "landings",
    landings = "landings",
    disc = "discards",
    discard = "discards",
    discards = "discards",
    tolower(output_type)
  )
  mth <- unique(plot_lfd$method) 
  plot_lfd %>%
    group_by(species, area) %>%
    group_walk(~{
      n_rows <- length(unique(.x$metier))
      n_cols <- length(unique(interaction(.x$year, .x$quarter)))
      p <- ggplot(.x, aes(lc, N_final)) +
        geom_line(color = "#1f77b4", linewidth = 0.5) + geom_point(color = "#1f77b4", size = 1)+
        facet_grid(year_quarter ~ metier, scales = "free_y") +
        ylab("N") +xlab('length (mm)') +
        theme_bw() + theme(legend.position = "bottom") +
        ggtitle(paste(.y$species, .y$area))
      
      ggsave(
        file.path(out_dir,paste0(output_prefix, '_lfd_', .y$species, "_", .y$area, "_", mth, ".png")),
        width = 6 + n_cols * 2,   # scale with columns
        height = 12 + n_rows * 2,  # scale with rows
        units = "cm", 
        dpi = 300,
        plot = p
      )
    })

}

