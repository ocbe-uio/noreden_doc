library(dplyr)
library(ggplot2)
library(patchwork)
library(ggiraph)

dat <- gapminder::gapminder |> 
  janitor::clean_names() |> 
  mutate(
    # ID that is shared for boxplots (this one uses factors, i.e. numbers, as ID instead of continents)
    id = as.numeric(continent),
    continent = forcats::fct_reorder(continent, life_exp)
  )

color_palette <- thematic::okabe_ito(5)
names(color_palette) <- unique(dat$continent)

base_size <- 24

mean_life_exps <- dat |> 
  group_by(continent, year, id) |> 
  summarise(mean_life_exp = mean(life_exp)) |> 
  mutate(mean_text = glue::glue('In {year}, the mean life expectancy in\n{continent} was {scales::number(mean_life_exp, accuracy = 0.01)}'))

line_chart <- mean_life_exps |> 
  ggplot(aes(x = year, y = mean_life_exp, col = continent, data_id = id)) +
  geom_line_interactive(linewidth = 2.5) +
  geom_point_interactive(size = 6, aes(tooltip = mean_text)) +
  theme_minimal(base_size = base_size) +
  labs(
    x = element_blank(),
    y = 'Life expectancy (in years)',
    title = 'Life expectancy over time'
  ) +
  theme(
    text = element_text(
      color = 'grey20'
    ),
    legend.position = 'none',
    panel.grid.minor = element_blank(),
    plot.title.position = 'plot'
  ) +
  scale_color_manual(values = color_palette)

selected_year <- 2007
box_stats <- dat |> 
  filter(year == selected_year) |> 
  group_by(continent) |> 
  summarise(
    n = n(), 
    iqr = IQR(life_exp) |> round(2), 
    range = paste(range(life_exp) |> round(2), collapse = ' - '),
    mean = mean(life_exp) |> round(2)
  )


box_plot <- dat |> 
  filter(year == selected_year) |> 
  full_join(box_stats) |> 
  ggplot(aes(x = life_exp, y = continent, fill = continent, data_id = id)) +
  geom_boxplot_interactive(
    aes(
      tooltip = glue::glue(
        '
        {levels(dat$continent)[continent]}\n
        {n} Countries\n
        Mean life expectancy: {mean}\n
        Range: {range}\n
        IQR: {iqr}
        '
      ),
      onclick = glue::glue('window.open("http://en.wikipedia.org/wiki/{levels(dat$continent)[continent]}")')
    ),
    position = position_nudge(y = 0.25), 
    width = 0.5
  ) +
  geom_point_interactive(
    aes(col = continent),
    position = position_nudge(y = -0.2),
    size = 11,
    shape = '|',
    alpha = 0.75
  ) +
  scale_fill_manual(values = color_palette) +
  scale_color_manual(values = color_palette) +
  labs(
    x = 'Life expectancy (in years)',
    y = element_blank(),
    title = glue::glue('Distribution of Life Expectancy in {selected_year}')
  ) +
  theme_minimal(base_size = base_size) +
  theme(
    text = element_text(
      color = 'grey20'
    ),
    legend.position = 'none',
    panel.grid.minor = element_blank(),
    plot.title.position = 'plot'
  ) 

# ggiraph ----
ggiraph(
  ggobj = box_plot + plot_spacer() + line_chart + plot_layout(widths = c(0.45, 0.1, 0.45)),
  options = list(
    opts_hover_inv(css = "opacity:0.1;"),
    opts_tooltip(offx = 0, offy = 0, css = 'font-size: larger;')
  ),
  hover_css = "",
  height_svg = 9,
  width_svg = 16
)
