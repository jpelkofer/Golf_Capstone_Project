library(tidyverse)
library(gt)

# Create Table 1
dkpg_example_tbl <- tibble(
  Player = c("A", "B", "C"),
  `Course ID` = 100,
  Round = 1,
  Hole = 9,
  `Draft Kings Pts Scored` = c(.5, 1.5, 1),
  `Field Average` = 1,
  `Draft Kings Pts Gained` = c(-.5, .50, 0)
)

apa <- function(x, title = " ", subtitle = " ") {
  gt(x) %>%
    tab_options(
      table.border.top.style = "hidden",
      heading.title.font.size = px(16),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = pct(100),
      table.background.color = "white",
      table.font.names = "calibri"
    ) %>%
    cols_align(align="center") %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("top", "bottom", "left", "right"),
          # color = "white",
          weight = px(0)
        ),
        cell_text(
          align="center"
        ),
        cell_fill(color = "white", alpha = NULL)
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) %>%
    #title setup
    tab_header(
      title = title, subtitle = subtitle
    ) %>%
    opt_align_table_header(align = "left")
}

table_1 <- apa(dkpg_example_tbl, html("<b><font size = 3>Table 1</font></b>"), 
               subtitle = html("<br><b><font size = 3><em>DraftKings Adjusted Points Example</em></font></b>
                               <br>
                               <br>"))

# Create Table 2
roi_sim_lineup <- read_rds("misc/roi_sim_lineup.rds")

apa(roi_sim_lineup, html("<b><font size = 3>Table 2</font></b>"),
    subtitle = html("<br><b><font size = 3><em>Positive ROI Model Lineup Combinations</em></font></b>
                    <br>
                    <br>"))

# Create Table 3
roi_by_model <- read_rds("misc/roi_by_model.rds")

apa(roi_by_model, html("<b><font size = 3>Table 3</font></b>"),
    subtitle = html("<br><b><font size = 3><em>ROI Results by Model</em></font></b>
                    <br>
                    <br>"))
