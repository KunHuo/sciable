#
#
# theme_jama <- function(base_size = 7,
#                        base_family = "sans",
#                        ink = "black",
#                        paper = "white",
#                        accent = "#DF8F44") {
#   # JAMA figure colour palette (from ggsci::pal_jama("default"))
#   jama_colours <- c("#374E55",
#                     "#DF8F44",
#                     "#00A1D5",
#                     "#B24745",
#                     "#79AF97",
#                     "#6A6599",
#                     "#807D73")
#
#   ggplot2::theme_minimal(
#     base_size = base_size,
#     base_family = base_family,
#     ink = ink,
#     paper = paper,
#     accent = accent
#   ) +
#     ggplot2::theme(
#       # --- Panel and plot background: plain white ---
#       panel.background = ggplot2::element_rect(
#         fill = paper,
#         colour = NA,
#         linewidth = 0
#       ),
#       plot.background = ggplot2::element_rect(
#         fill = paper,
#         colour = NA,
#         linewidth = 0
#       ),
#
#       # --- Grid lines: none ---
#       panel.grid.major = ggplot2::element_blank(),
#       panel.grid.minor = ggplot2::element_blank(),
#
#       # --- Panel border: none ---
#       panel.border = ggplot2::element_blank(),
#
#       # --- Axis lines ---
#       axis.line = ggplot2::element_line(
#         colour = ink,
#         linewidth = 0.25,
#         lineend = "square"
#       ),
#       axis.line.x = ggplot2::element_line(
#         colour = ink,
#         linewidth = 0.25,
#         lineend = "square"
#       ),
#       axis.line.y = ggplot2::element_line(
#         colour = ink,
#         linewidth = 0.25,
#         lineend = "square"
#       ),
#
#       # --- Axis ticks (absolute cm values) ---
#       axis.ticks = ggplot2::element_line(
#         colour = ink,
#         linewidth = 0.25,
#         lineend = "square"
#       ),
#       axis.ticks.length = ggplot2::unit(0.04, "cm"),
#       axis.ticks.length.x = ggplot2::unit(0.06, "cm"),
#       axis.ticks.length.y = ggplot2::unit(0.08, "cm"),
#
#       # --- Axis text (all base_size, no bold) ---
#       axis.text = ggplot2::element_text(
#         size = base_size,
#         family = base_family,
#         colour = ink
#       ),
#       axis.text.x = ggplot2::element_text(
#         margin = ggplot2::margin(t = 0.025, unit = "cm"),
#         angle = 0,
#         hjust = 0.5
#       ),
#       axis.text.y = ggplot2::element_text(
#         margin = ggplot2::margin(r = 0.035, unit = "cm"),
#         angle = 0,
#         hjust = 1
#       ),
#
#       # --- Axis titles (all base_size, no bold, Y vertical) ---
#       axis.title = ggplot2::element_text(
#         size = base_size,
#         family = base_family,
#         colour = ink
#       ),
#       axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 0.060, unit = "cm")),
#       axis.title.y = ggplot2::element_text(
#         angle = 90,
#         margin = ggplot2::margin(r = 0.080, unit = "cm")
#       ),
#
#       # --- Legend (all base_size, no bold) ---
#       legend.position = "right",
#       legend.direction = "vertical",
#       legend.justification = "center",
#       legend.box = "vertical",
#       legend.background = ggplot2::element_blank(),
#       legend.key = ggplot2::element_rect(
#         fill = paper,
#         colour = NA,
#         linewidth = 0
#       ),
#       legend.key.size = ggplot2::unit(0.150, "cm"),
#       legend.key.height = ggplot2::unit(0.26, "cm"),
#       legend.title = ggplot2::element_text(
#         size = base_size,
#         family = base_family,
#         colour = ink
#       ),
#       legend.text = ggplot2::element_text(
#         size = base_size,
#         family = base_family,
#         colour = ink
#       ),
#       legend.spacing.x = ggplot2::unit(0.5, "cm"),
#       legend.spacing.y = ggplot2::unit(0.5, "cm"),
#       legend.margin = ggplot2::margin(
#         t = 0,
#         r = 0,
#         b = 0,
#         l = 0,
#         unit = "cm"
#       ),
#
#       # --- Facet strip (all base_size, no bold) ---
#       strip.background = ggplot2::element_rect(
#         fill = paper,
#         colour = NA,
#         linewidth = 0
#       ),
#       strip.text = ggplot2::element_text(
#         size = base_size,
#         family = base_family,
#         colour = ink
#       ),
#       strip.text.x = ggplot2::element_text(margin = ggplot2::margin(b = 0.030, unit = "cm")),
#       strip.text.y = ggplot2::element_text(margin = ggplot2::margin(l = 0.040, unit = "cm")),
#       strip.clip = "off",
#       strip.switch.pad.grid = ggplot2::unit(0.005, "cm"),
#       strip.switch.pad.wrap = ggplot2::unit(0.010, "cm"),
#
#       # --- Plot title, subtitle, caption (ALL base_size, no bold) ---
#       plot.title = ggplot2::element_text(
#         size = base_size,
#         family = base_family,
#         colour = ink,
#         hjust = 0,
#         margin = ggplot2::margin(b = 0.2, unit = "cm")
#       ),
#       plot.subtitle = ggplot2::element_text(
#         size = base_size,
#         family = base_family,
#         colour = ink,
#         hjust = 0,
#         margin = ggplot2::margin(b = 0.045, unit = "cm")
#       ),
#       plot.caption = ggplot2::element_text(
#         size = base_size,
#         family = base_family,
#         colour = "grey50",
#         hjust = 1,
#         margin = ggplot2::margin(t = 0.055, unit = "cm")
#       ),
#       plot.tag = ggplot2::element_text(
#         size = base_size,
#         family = base_family,
#         colour = ink,
#         hjust = 0,
#         vjust = 1
#       ),
#       plot.margin = ggplot2::margin(
#         t = 0.2,
#         r = 0.2,
#         b = 0.2,
#         l = 0.2,
#         unit = "cm"
#       ),
#
#       # --- Global layer defaults via element_geom() ---
#       geom = ggplot2::element_geom(
#         ink = ink,
#         paper = paper,
#         accent = accent,
#         linewidth = 0.25,
#         linetype = "solid",
#         borderwidth = 0.25,
#         bordertype = "solid",
#         family = base_family,
#         fontsize = base_size,
#         pointsize = 2.5,
#         pointshape = 21
#       ),
#
#       # --- Per-geom micro overrides ---
#       geom.point = ggplot2::element_geom(
#         colour = ink,
#         fill = paper,
#         pointsize = 2.5,
#         pointshape = 21
#       ),
#       geom.line = ggplot2::element_geom(
#         colour = ink,
#         linewidth = 0.25,
#         linetype = "solid"
#       ),
#       geom.col = ggplot2::element_geom(
#         colour = ink,
#         fill = NA,
#         borderwidth = 0.25,
#         bordertype = "solid"
#       ),
#       geom.boxplot = ggplot2::element_geom(
#         colour = ink,
#         borderwidth = 0.25,
#         bordertype = "solid"
#       ),
#       geom.text = ggplot2::element_geom(
#         family = base_family,
#         fontsize = base_size,
#         colour = ink
#       ),
#       geom.smooth = ggplot2::element_geom(linewidth = 0.25),
#       geom.errorbar = ggplot2::element_geom(linewidth = 0.25, borderwidth = 0.3),
#
#       # --- Theme-side default palettes (JAMA colours) ---
#       palette.colour.discrete = jama_colours,
#       palette.colour.continuous = c("#DF8F44", "#374E55"),
#       palette.fill.discrete = jama_colours,
#       palette.fill.continuous = c("#DF8F44", "#374E55"),
#       palette.shape.discrete = c(19, 17, 15, 18, 16, 1, 0),
#       palette.linetype.discrete = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
#
#       # --- Complete theme flag ---
#       complete = TRUE
#     )
# }
#
#
# save_jama_pdf <- function(filename,
#                           plot = ggplot2::last_plot(),
#                           width = 3.5,
#                           height = 2.625,
#                           dpi = 600,
#                           ...) {
#   ggplot2::ggsave(
#     filename = filename,
#     plot = plot,
#     device = "pdf",
#     width = width,
#     height = height,
#     dpi = dpi,
#     units = "in",
#     bg = "white",
#     ...
#   )
#   message("Saved: ",
#           filename,
#           " (",
#           width,
#           "\" x ",
#           height,
#           "\", ",
#           dpi,
#           " dpi)")
#   invisible(filename)
# }
