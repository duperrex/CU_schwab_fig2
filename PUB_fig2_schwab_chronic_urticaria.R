## PUB_fig2_schwab_chronic_urticaria.R
## olivier.duperrex@unisante.ch
## latest update: 2024-10-22
## 
## Aim : code for fig2a and fig2b for publication 'Features of chronic urticaria after COVID-19 mRNA vaccine'

## 0.a libraries ----------------------------------------------------

## install pacman unless you already have it : simpler to call libraries and keep them up-to-date
packages <- installed.packages()
if ("pacman" %in% packages[,1] == FALSE){
  install.packages('pacman')
}

pacman::p_load(
  data.table,
  sjPlot,
  tidyverse
)


## . set up parameters and load data ----
# date_start_filter <- data.table::as.IDate('2021-12-01', '%Y-%m-%d')
# date_latest_data  <- data.table::as.IDate('2022-08-31', '%Y-%m-%d')

data_path <- 'path/to/data'


data_fig2a <- paste0(data_path, 'data_fig2a.csv') |>
  data.table::fread()


data_fig2b <- paste0(data_path, 'data_fig2b.csv') |>
  data.table::fread()

data_fig2b
data_fig2b |> labelled::look_for()

data_fig2b[, variable := factor(variable, levels = c("booster_first", "csu_start", "cases" ))]
data_fig2b |> labelled::look_for()


canton <- "VD"
last_import_bis <- as.IDate("2024-02-20")
 


## 0.c themes and title for plots -----------------------------------
theme_set(sjPlot::theme_sjplot2(base_size = 14))
theme_update(
  
  legend.title = element_blank(),
  legend.position = "right",
  strip.background = element_rect(fill = "white"),
  
  plot.title = ggtext::element_markdown(lineheight = 1.5, color = 'grey40'),
  plot.subtitle = ggtext::element_markdown(lineheight = 1.5, color = 'grey40'),
  plot.caption = ggtext::element_markdown(lineheight = 2),
  
  # axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 12)
)

out_png <- here::here('output', 'png')


if (!(fs::dir_exists(out_png))) {
  fs::dir_create(out_png)
}


## . Figure_2A ----
# title <- "Days before (negative values) and after (positive values) the chronic urticaria appeared for each participant"
# 
# subtitle <- "for latest SARS-Cov-2 vaccination (black circles) and COVID-19 infection (gray squares)"
# 
# caption_g <- glue::glue("Source : OMC-VD/Unisanté + CHUV-CSU [data on {last_import_bis}]")
# caption_g

p2a <-
  data_fig2a |>
  ggplot(aes(y = id_anon)) +
  
  # circles for delay_1
  geom_point(
    shape = 1,
    color = "black",
    size = 3,
    aes(x = days_csu_vax, group = 1)
  ) +
  
  # squares for delay_2
  geom_point(
    shape = 15,
    color = "gray",
    size = 3,
    aes(x = days_csu_covid, group = 2)
  ) +
  ## add lines
  geom_linerange(
    aes(xmin = days_csu_vax, xmax = delay_csu),
    color = "black",
    linewidth = .8
  ) +
  geom_linerange(
    aes(xmin = days_csu_covid, xmax = delay_csu),
    color = "gray",
    linewidth = .8
  ) +
  
  labs(
    x = "Days",
    y = "",
    title = NULL,
    subtitle = NULL,
    caption = NULL
  ) +
  # use pseudo_log scale- inspired by https://stackoverflow.com/a/67788909/6176250
  scale_x_continuous(
    trans = 'pseudo_log',
    limits = c(-1000, 1000),
    breaks = c(-1, -10, -100, -1000, 0, 1, 10, 100, 1000),
    minor_breaks = NULL
  ) +
  scale_y_continuous(breaks = NULL) +
  theme(axis.text.x = element_text(size = 16))

p2a

plot_name <- glue::glue("Figure_2A_w6_h10_600dpi.png")
ggsave(plot_name, path = out_png, width = 6, height = 10, dpi = 600)

plot_name <- glue::glue("Figure_2A_w6_h10_600dpi.pdf")
ggsave(plot_name, path = out_png, width = 6, height = 10, dpi = 600)




## . Figure_2B ----

week_for_tag_left <-
  aweek::get_date(week = 40, year = 2021)

global_size <- 20
axis_ticks_size <- 14


data_fig2b[, .(Total = sum(value, na.rm =T)), keyby = variable]

my_tag <-
  c(
    glue::glue("A - First Booster {canton}"),
    glue::glue("B - Start of chronic urticaria"),
    glue::glue("C - COVID-19 cases {canton}")
  )

my_tag


# (booster_first_N <- data_fig2b[variable == 'booster_first', sum(value, na.rm = T)] |> format(big.mark = "'"))
# (csu_start_N <- data_fig2b[variable == 'csu_start', sum(value, na.rm = T)] |> format(big.mark = "'"))
# (cases_N <- data_fig2b[variable == 'cases', sum(value, na.rm = T)] |> format(big.mark = "'"))

# my_tag_with_N <-
#   c(
#     glue::glue("A - First Booster {canton} (N = {booster_first_N})"),
#     glue::glue("B - Start of chronic urticaria (N = {csu_start_N})"),
#     glue::glue("C - COVID-19 cases {canton} (N = {cases_N})")
#   )
# my_tag_with_N


p2b <- 
  data_fig2b |>
  ggplot(aes(x = date_week, y = value, fill = top)) +
  geom_col() +
  scale_fill_manual(values = c('white','grey80', 'grey40')) +
  # geom_col(aes(x = dt_top$date_week, y = dt_top$value), fill = 'magenta4') +
  facet_grid(variable ~ ., scales = "free_y") +
  
  scale_y_continuous(labels = scales::label_comma(big.mark = "'"),
                     expand = expansion(mult = c(0, 0.2))
                     ) +

  scale_x_date(
    expand = c(0,0),                     # remove excess x-axis space below and after case bars
    date_breaks = "2 months",            # Monday every 4 weeks
    date_minor_breaks = "week",          # Monday weeks
    labels = scales::label_date_short()  # label formatting
  ) + 
  
  
  labs(title = NULL,
       subtitle = NULL,
       caption = NULL,
       x = 'Weeks',
       y = "Number of individuals")

p2b

p2b_tag <- 
  egg::tag_facet(
    p2b,
    x = week_for_tag_left,
    y = Inf,
    hjust = -.005,
    vjust = 1,
    color = 'grey40',
    open = "",
    close = "",
    tag_pool = my_tag,
    size = 6,
  ) +
  
  theme(
    panel.spacing = unit(2, "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position="none",
    axis.text.x = ggtext::element_markdown(size = axis_ticks_size),
    axis.text.y = ggtext::element_markdown(size = axis_ticks_size),
    axis.title = ggtext::element_markdown(size = global_size)
    # ,
    # axis.title.y = ggtext::element_markdown(size = global_size)
    # plot.margin = unit(c(1, 1, 1, 1), "cm")
    # , text = ggtext::element_markdown(size = global_size)
  ) 

p2b_tag 

plot_name <- 
  glue::glue("Figure_2B_w6h10_600dpi.png")
ggsave(plot_name, path = out_png, width = 6, height = 10, dpi = 600)


plot_name <- 
  glue::glue("Figure_2B_w6h10_600dpi.pdf")
ggsave(plot_name, path = out_png, width = 6, height = 10, dpi = 600)





