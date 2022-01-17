library(extrafont)

# Theme 1 ----
theme_mg1 <- function() {theme_bw() +
    theme(panel.grid = element_blank()) +
    theme(text = element_text(family = "Garamond", color = "white"),
          panel.grid.major.y = element_line(color = "#333399"),
          panel.grid.major.x = element_line(color = "#333399"),
          panel.background = element_rect(fill = "#000077"),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "#000077", color = "#000077"),
          axis.title = element_text(color = "white"),
          axis.title.x = element_text(vjust = -1),
          axis.title.y = element_text(angle = 90, vjust = 1),
          axis.text = element_text(color = "white"),
          plot.title = element_text(color = "white"),
          axis.ticks = element_line(color = "#000077"),
          legend.background = element_rect(fill = NA),
          legend.key = element_rect(fill = "#000077"),
          plot.caption = element_text(size = 7))
}

# Theme 2. MFF ----
theme_mg2_mff <- function() {
  bg_col <- mff_cols[1]
  text_col <- mff_cols[3]
  
  theme_bw() +
    theme(panel.grid = element_blank()) +
    theme(text = element_text(family = "Garamond", color = text_col),
          panel.grid.major.y = element_line(color = mff_cols[5]),
          panel.grid.major.x = element_line(color = mff_cols[5]),
          panel.background = element_rect(fill = bg_col),
          panel.border = element_blank(),
          plot.background = element_rect(fill = bg_col, color = bg_col),
          axis.title = element_text(color = text_col),
          axis.title.x = element_text(vjust = -1),
          axis.title.y = element_text(angle = 90, vjust = 1),
          axis.text = element_text(color = text_col),
          plot.title = element_text(color = text_col),
          axis.ticks = element_line(color = bg_col),
          legend.background = element_rect(fill = NA),
          legend.key = element_rect(fill = bg_col),
          strip.background = element_rect(fill = text_col),
          strip.text = element_text(color = "white"), 
          plot.caption.position = "plot")
}

# Theme 3. Map 1 ----
theme_mg3 <- function() {
  bg_col <- "#F5F8DC"
  text_col <- "black"
  
  theme_bw() +
    theme(panel.grid = element_blank()) +
    theme(text = element_text(family = "Garamond", color = text_col),
          panel.background = element_rect(fill = bg_col),
          plot.background = element_rect(fill = bg_col, color = bg_col),
          axis.text = element_text(color = text_col),
          plot.title = element_text(color = text_col),
          legend.background = element_rect(fill = NA),
          legend.key = element_rect(fill = bg_col),
          legend.title = element_blank()
          )
}

# Theme 4. Climate ----
theme_mg4 <- function() {
  bg_col <- c("#ffbbbb", "#fffafa")
  text_col <- "black"
  
  theme_bw() +
    theme(panel.grid = element_blank()) +
    theme(text = element_text(family = "Garamond", color = text_col, size = 14),
          panel.background = element_rect(fill = bg_col[2]),
          plot.background = element_rect(fill = bg_col[1], color = bg_col[1]),
          axis.text = element_text(color = text_col),
          plot.title = element_text(color = text_col),
          legend.background = element_rect(fill = NA),
          legend.key = element_rect(fill = "white", color = "black"),
          legend.title = element_blank(),
          legend.text = element_text(size = 15)
    )
}

# Theme 5. Politics ----
theme_mg5 <- function() {
  bg_col <- rep("#110000", 2)
  text_col <- "white"
  
  theme_bw() +
    theme(text = element_text(family = "Garamond", color = text_col, size = 16),
          panel.background = element_rect(fill = bg_col[2], color = bg_col[1]),
          plot.background = element_rect(fill = bg_col[1], color = bg_col[1]),
          axis.text = element_text(color = text_col),
          plot.title = element_text(color = text_col),
          legend.background = element_rect(fill = NA),
          legend.key = element_rect(fill = bg_col[1], color = bg_col[1]),
          legend.title = element_blank()
    )
}
