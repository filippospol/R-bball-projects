mydf = mtcars

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# {gt} table
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pacman::p_load(tidyverse,gt,gtExtras)
# caption
mycaption = '<meta name="viewport" content="width=device-width, initial-scale=1"> <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"> <i class="fa fa-twitter"></i> filippos_pol'

# Regular
head(mydf) %>%
  gt() %>% 
  # Columns alignment
  cols_align(align = "center") %>% 
  # Column labels # old=new
  cols_label() %>% 
  ## Title and subtitle
  # tab_header(
  #   title = md("**head(10) mtcars table**"),
  #   subtitle = html("<div style='color:#8e8e8e;'>Example table using my dark theme</div><br/>")
  # ) %>% 
  # add logo on the top right:
  tab_header(
    title = htmltools::HTML(
      paste0(
        "<div style='display:flex; justify-content:space-between; align-items:center;'>",
        "<div style='font-size:25px'><b>head(10) mtcars table</b><br>
         <span style='font-weight:normal;color:#8e8e8e;font-size:16px'>Example table using my theme</span></div>",
        "<div><img src='https://i.ibb.co/TxnGsfZm/Hn-R-No-Backround-Black.png' style='height:80px;'></div>",
        "</div>"
      )
    )
  ) %>% 
  # Bold column labels
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()))  %>%  
  # # Convert html to image
  # gt_img_rows(columns =, height =) %>%
  # Highlight row
  gt_highlight_rows(rows = 2, font_weight = "normal") %>%
  # # Format row as percentage
  # fmt_percent(columns = , decimals = 1) %>% 
  # # Add footnote
  # tab_footnote(footnote = "", locations = cells_column_labels()) %>%
  gt_color_rows(c(qsec), palette = c("#1f77b4", "#aec7e8")) %>%
  # Table caption
  tab_source_note(source_note = md(paste("Table by: Filippos Polyzos |",mycaption))) %>% 
  # Design additional options
  tab_options(data_row.padding = px(1),
              row.striping.background_color = "#eeeeee",
              heading.align = "left",
              heading.title.font.size = 25,
              heading.subtitle.font.size = 18,
              table.font.size = 14,
              table.border.top.color = "transparent",
              heading.border.bottom.color = "transparent",
              column_labels.border.bottom.width = px(3),
              column_labels.border.bottom.color = "#4c4c4c",
              table_body.border.bottom.width = px(3),
              table_body.border.bottom.color = "#4c4c4c",
              table.border.bottom.color = "transparent") %>% 
  # Row striping
  opt_row_striping() %>% 
  opt_table_font(font = google_font("Karla"))

# Dark
head(mydf) %>%
  gt() %>% 
  # Columns alignment
  cols_align(align = "center") %>% 
  # Column labels
  cols_label() %>% 
  ## Title and subtitle
  # tab_header(
  #   title = md("**head(10) mtcars table**"),
  #   subtitle = html("<div style='color:#cccccc;'>Example table using my dark theme</div><br/>")
  # ) %>% 
  # add logo on the top right:
  tab_header(
    title = htmltools::HTML(
      paste0(
        "<div style='display:flex; justify-content:space-between; align-items:center;'>",
        "<div style='font-size:25px'><b>head(10) mtcars table</b><br>
         <span style='font-weight:normal;color:#cccccc;font-size:16px'>Example table using my theme</span></div>",
        "<div><img src='https://i.ibb.co/TxnGsfZm/Hn-R-No-Backround-White.png' style='height:80px;'></div>",
        "</div>"
      )
    )
  ) %>% 
  # Bold column labels
  tab_style(
    style = cell_text(weight = "bold", color = "white"),
    locations = cells_column_labels(everything())
  ) %>%  
  # Highlight row (adjusted to stand out in dark mode)
  gt_highlight_rows(
    rows = 2,
    fill = "yellow4",
    font_weight = "normal",
    font_color = "white"
  ) %>%
  # Color a column's rows
  gt_color_rows(
    c(qsec),
    palette = c("#1f77b4", "#aec7e8")
  ) %>%
  # Table caption
  tab_source_note(
    source_note = md(paste("Table by: Filippos Polyzos |", mycaption))
  ) %>% 
  # Design additional options
  tab_options(
    data_row.padding = px(1),
    row.striping.background_color = "#2a2a2a",
    heading.align = "left",
    heading.title.font.size = 25,
    heading.subtitle.font.size = 18,
    table.font.size = 14,
    table.border.top.color = "transparent",
    heading.border.bottom.color = "transparent",
    column_labels.border.bottom.width = px(3),
    column_labels.border.bottom.color = "#555555",
    table_body.border.bottom.width = px(3),
    table_body.border.bottom.color = "#555555",
    table.border.bottom.color = "transparent",
    table.background.color = "#1e1e1e",
    heading.background.color = "#1e1e1e",
    column_labels.background.color = "#1e1e1e"
  ) %>% 
  # Row striping
  opt_row_striping() %>% 
  # White text for all cells
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_body()
  ) %>%
  # White caption text
  tab_style(
    style = cell_text(color = "#aaaaaa"),
    locations = cells_source_notes()
  ) %>% 
  opt_table_font(font = google_font("Karla"))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

######################################################################################################
# {ggplot2}
######################################################################################################

pacman::p_load(sysfonts,showtext,ggtext)

# Download font awesome locally first: https://fontawesome.com/download
# Setup caption and import font
font_add_google("Karla","myfont")
sysfonts::font_add("FA","C:/Users/phili/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()
pc = "<span style='font-family:FA;'>&#xf099;</span> @filippos_pol"

baseplot = mydf %>% 
  ggplot(aes(y=qsec,x=as.factor(gear),fill=gear)) +
  geom_violin() +
  labs(title=md("**mt cars Violin plot**"),
       subtitle="Example chart using my theme\n",
       x="Gears",y="1/4 mile time",
       caption=paste0("Chart by: Filippos Polyzos | ",pc)) 

# Regular
baseplot +
  theme_minimal(base_family="myfont") +
  theme(legend.position="none",
        text=element_text(family="myfont"),
        plot.title=element_markdown(face="bold"),
        plot.subtitle=element_text(color="#8e8e8e"),
        plot.caption=element_markdown(color="#777777",hjust=0))
  
# Dark
baseplot +
  theme_minimal(base_family="myfont") +
  theme(
    panel.background = element_rect(fill = "#222222", color = NA),
    plot.background = element_rect(fill = "#222222", color = NA),
    legend.position = "none",
    text = element_text(color = "white"),
    plot.title = element_markdown(face = "bold", color = "white"),
    plot.subtitle = element_text(color = "#cccccc"),
    plot.caption = element_markdown(color = "#aaaaaa", hjust = 0),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_line(color = "#333333")
  )


