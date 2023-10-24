# This is a simple script that ships with rmodvege. It allows comparing 
# results of two or more rmodvege simulation runs.
#
# Use the `runs` vector to select the simulation runs to be compared (use full 
# site + run name if applicable, e.g. "posieux2_run1").
#
# Copy and adjust the script to your personal needs. It can always be 
# "regenerated" by running `setup_directory()`.
#
# See also `?compare.R`
#
library(growR)
library(ggplot2)

#-Parameters--------------------------------------------------------------------

runs = c("posieux1", "sorens1")
basename = "output/%s_%s.dat"
outname = "%s_vs_%s.pdf"
years = 2013:2022

y_key = "cBM"
y_key = "dBM"
box_width = 28

#-Dependent-variables-----------------------------------------------------------

measured_data_sites = unlist(lapply(runs, function(s) { 
                                      strsplit(s, "_")[[1]][1] 
}))
n_runs = length(runs)

#-Load-data---------------------------------------------------------------------

D = list()
ymax = -Inf
ymin = 0
for (run in runs) {
  D[[run]] = list()
  for (year in years) {
    data = read.table(sprintf(basename, run, year), header = TRUE)
    if (y_key == "dBM") {
      y = box_smooth(data$dBM, box_width = box_width)
    } else {
      y = data[[y_key]]
    }
    D[[run]][[as.character(year)]] = y
    # Keep track of global max values
    if (max(y) > ymax) { ymax = max(y) }
  }
}

measured_data = load_data_for_sites(measured_data_sites)
measured_colors = c("#0000CC", "#00CC00", "#CC0000", "#666666")

cb_palette = c(
"#56b4e9", # skyblue
"#e69f00", # orange
"#009e73", # green
"#000000", # black
"#0072b2", # blue
"#d55e00", # vermillion
"#cc79a7", # purple
"#f0e442"  # yellow
)
.n_colors = length(cb_palette)
palettes = list(
                cb_palette
                )
options(ggplot2.discrete.colour = palettes)
options(ggplot2.discrete.fill = palettes)

#-Plot--------------------------------------------------------------------------

DOY = 1:365
axes = list()
for (year in as.character(years)) {
  ax = ggplot()
  # Start with measured data
  for (i in 1:length(measured_data_sites)) {
    measured_y = measured_data[[i]]
    mask = as.character(measured_y$year) == year
    measured_y = measured_y[mask, ]
    ax = add_lines(measured_y, ax, y_key = y_key, style = "line", 
                   label = "ref",
                   linewidth = 4, color = measured_colors[i], alpha = 0.2)
  }

  # Plot each dataset to be compared
  for (i in 1:n_runs) {
    run = runs[[i]]
    y = D[[run]][[year]]
    # Pack into data.frame for ggplot
    df = data.frame(DOY = DOY, y = y, color = factor(run))
    ax = ax + geom_line(data = df, aes(x = DOY, y = y, color = color, group = color))
#                        color = cb_palette[i])
#    ax = add_metrics(ax, predicted = y[DOY %in% measured_y[["DOY"]]], 
#                     observed = measured_y[[y_key]], y = i * 0.35 )
  }
  ax = ax + scale_color_manual("", values = cb_palette[1:n_runs],
                               breaks = rev(runs))
  ax = ax + theme(legend.position = c(0, 1), legend.justification = "left")

  # Limits
  ax = ax + ylim(c(ymin, ymax))
  # Labels
  ax = ax + labs(title = year) + xlab("") + ylab("")
  axes[[year]] = ax
}
#legend("topright", c("new", "old"), col = c("tomato", "navy"), lwd = c(2, 2), 
#       lty = c(1, 1))

theme(legend.position = c(0, 0.9),
      legend.direction = "horizontal")

pw = patchwork::wrap_plots(axes, ncol = 5)
print(pw)

