# herring_theme <- theme(
#   legend.box.background = element_rect(fill = alpha("white", 0.7)),
#   legend.box.margin = margin(1, 1, 1, 1, "mm"),
#   legend.key = element_blank(),
#   legend.margin = margin(),
#   legend.text.align = 1,
#   # panel.grid.major = element_line(colour = "grey10", size = 0.2),
#   # panel.grid.minor = element_line(colour = "grey10", size = 0.1),
#   axis.text = element_text(colour = "black"),
#   legend.background = element_rect(fill = "transparent"),
#   plot.margin = unit(c(0.1, 0.6, 0.1, 0.1), "lines"),
#   panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
# )

#' Map of BC.
#'
#' Map of BC with Pacific Herring stock assessment regions.
#'
#' @template param-shape_land
#' @template param-shape_sars
#' @template param-map_buffer
#' @template param-french
#' @importFrom Rdpack reprompt
#' @importFrom ggplot2 ggplot aes geom_sf geom_sf_label annotate labs coord_sf
#' @importFrom sf st_buffer st_bbox st_crop st_crs
#' @return Map of the BC coast with Pacific Herring stock assessment regions.
#' @family maps
#' @export
#' @examples
#' require(SpawnIndex)
#' require(rnaturalearth)
#' data(sections)
#' data(database_info)
#' areas <- load_area_data(
#'   reg = "All", db = database_info$herring_con, where = database_info$area_loc
#' )
#' polys <- load_sections(sections = sections, areas = areas)
#' bc_coast <- ne_countries(
#'   scale = "large", returnclass = "sf",
#'   country = c("Canada", "United States of America")
#' )
#' map_bc(shape_land = bc_coast, shape_sars = polys$regions)
map_bc <- function(shape_land, shape_sars, map_buffer = 25000, french = FALSE) {
  # Wrangle sections
  shape_sars <- shape_sars %>%
    filter(SAR != -1)
  # Big buffer to trim land
  big_bbox <- shape_sars %>%
    st_buffer(dist = map_buffer * 2) %>%
    st_bbox()
  # Buffer around sections
  bc_bbox <- shape_sars %>%
    st_buffer(dist = map_buffer) %>%
    st_bbox()
  # Trim land
  shape_land <- shape_land %>%
    st_crop(y = big_bbox)
  # Build the map
  map <- ggplot(data = shape_land) +
    geom_sf(fill = "lightgrey") +
    geom_sf(
      data = shape_sars, linewidth = 0.5, fill = "transparent", colour = "black"
    ) +
    geom_sf_label(data = shape_sars, alpha = 0.5, aes(label = Region)) +
    annotate(
      geom = "text", x = -125, y = 52, label = "British\nColumbia", size = 5
    ) +
    annotate(
      geom = "text", x = -131, y = 49.5, label = "Pacific\nOcean", size = 5
    ) +
    labs(x = "Longitude", y = "Latitude") +
    coord_sf(
      xlim = c(bc_bbox$xmin, bc_bbox$xmax),
      ylim = c(bc_bbox$ymin, bc_bbox$ymax), expand = FALSE,
      crs = st_crs(4326))
  # Return the map
  map
}

# # Create a base map for the region
# BaseMap <- ggplot(data = reg_coast) +
#   geom_sf(fill = "lightgrey", colour = "transparent") +
#   geom_sf(
#     data = shapes$regions, fill = "transparent", linewidth = 0.75,
#     colour = "black", linetype = "dashed"
#   ) +
#   # coord_equal() +
#   labs(x = "Longitude", y = "Latitude") +
#   myTheme

# if(exists("FN_map")) {
#   BaseMap <- BaseMap +
#     geom_sf(
#       data = FN_map, colour = "blue", fill = "transparent", linewidth = 0.5
#     )
# }

# # Plot the region, and statistical areas
# RegionMap <- BaseMap +
#   geom_sf(
#     data = shapes$stat_areas, linewidth = 0.25, fill = "transparent",
#     colour = "black"
#   ) +
#   geom_sf(
#     data = shapes$sections, linewidth = 0.25, fill = "transparent",
#     colour = "black", linetype = "dotted"
#   ) +
#   scale_fill_viridis(discrete = TRUE) +
#   labs(fill = "Group") +
#   theme(legend.position = c(0.01, 0.01), legend.justification = c(0, 0))

# if (!all(is.na(shapes$groups$Group)) & region %in% c("CC", "SoG", "All")) {
#   RegionMap <- RegionMap +
#     geom_sf(
#       data = shapes$groups, mapping = aes(fill = Group), alpha = 0.25
#     )
# }

# if (nrow(shapes$stat_areas) >= 1) {
#   RegionMap <- RegionMap +
#     geom_sf_label(
#       data = shapes$stat_areas, alpha = 0.25,
#       mapping = aes(label = paste("SA", StatArea, sep = " "))
#     )
# }

# RegionMap <- RegionMap +
#   coord_sf(
#     xlim = c(reg_bbox_small$xmin, reg_bbox_small$xmax),
#     ylim = c(reg_bbox_small$ymin, reg_bbox_small$ymax), expand = FALSE)

# ggsave(
#   RegionMap, filename = file.path(regName, "Region.png"), width = figWidth,
#   height = min(7.5, 9 / reg_ratio_small), dpi = figRes
# )
