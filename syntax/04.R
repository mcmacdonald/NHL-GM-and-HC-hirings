


# don't run
# install packages
# install.packages(
  # c(
    # "tidyverse",
    # "tidygraph",
    # "ggraph"
    # )
  # )



# don't run
# order seasons chronologically and select every 5th one
# target_seasons <- edge_list %>%
  # dplyr::distinct(season) %>%
  # dplyr::arrange(season) %>%
  # dplyr::filter((row_number() - 1) %% 5 == 0) %>%   # selects the 1st, 6th, 11th, etc., season
  # dplyr::pull(season)

# don't run
# filter edgelist to include only target seasons
# targeted_edges <- edge_list %>%
  # dplyr::select(from, to, season) %>%
  # dplyr::filter(season %in% target_seasons)

# list of weighted edges that indicate the continuity of the relationship
weighted_edges <- edge_list %>%
  dplyr::group_by(from, to) %>%
  dplyr::summarise(seasons_together = dplyr::n(), .groups = "drop")

# list nodes at either end of the pairing

  # node i who started their job with an organization first
  nodes_from <- dplyr::tibble(name = weighted_edges$from)
  
  # nodes came on the job after the debut of node i
  nodes_to <- dplyr::tibble(name = weighted_edges$to)

# assign labels based on the list of names and their debuts
  
  # general managers
  gm_list1 <- unique(edge_list$from[edge_list$from %in% gm_debuts$fullName]) # sender column
  gm_list2 <- unique(edge_list$to[edge_list$to %in% gm_debuts$fullName]) # reciever column
  gm_list <- append(gm_list1, gm_list2); gm_list <- unique(gm_list) # append lists together
  
  # head coaches
  hc_list1 <- unique(edge_list$from[edge_list$from %in% hc_debuts$coachName]) # sender column
  hc_list2 <- unique(edge_list$to[edge_list$to %in% hc_debuts$coachName]) # reciever column
  hc_list <- append(hc_list1, hc_list2); hc_list <- unique(hc_list) # append lists together

# join back together again
nodes <- dplyr::bind_rows(nodes_from, nodes_to) %>%
  dplyr::distinct(name) %>%
  dplyr::mutate(
    Job = dplyr::case_when(
      name %in% gm_list ~ "GM",
      name %in% hc_list ~ "HC",
      unmatched = TRUE ~ "?"
      )
    )

# directed graph object and calculate degree centrality to identify hubs
nhl <- tidygraph::tbl_graph(
  nodes = nodes, 
  edges = weighted_edges, 
  directed = TRUE
  ) %>%
  tidygraph::activate(nodes) %>%
  dplyr::mutate(
    degree = tidygraph::centrality_degree(mode = "all")
    )

# plot hiring carousel
fig01 <- ggraph::ggraph(nhl, layout = "stress") +
  ggraph::geom_edge_fan(
    ggplot2::aes(edge_alpha = 0.4),
    arrow = grid::arrow(length = grid::unit(2.5, 'mm'), type = 'closed'),
    start_cap = ggraph::circle(3, 'mm'),
    end_cap = ggraph::circle(3, 'mm'),
    color = "grey50",
    show.legend = FALSE
    ) +
  ggraph::geom_edge_link(
    ggplot2::aes(edge_width = seasons_together, edge_alpha = 0.5),
    arrow = grid::arrow(length = grid::unit(2.2, 'mm'), type = 'closed'),
    start_cap = ggraph::circle(3.2, 'mm'),
    end_cap = ggraph::circle(3.2, 'mm'),
    color = "grey60",
    show.legend = FALSE
    ) +
  ggraph::geom_node_point(
    ggplot2::aes(
      color = Job,
      size = degree
      ),
    fill = "white",
    shape = 21,
    stroke = 1.5,
    alpha = 1.0
    ) +
  # ggraph::geom_node_text(
    # ggplot2::aes(label = name), 
    # repel = TRUE, 
    # size = 3, 
    # max.overlaps = 20
    # ) +
  ggraph::geom_node_text(
    ggplot2::aes(label = ifelse(degree >= 3, name, "")), 
    repel = TRUE, 
    size = 3.2, 
    fontface = "bold",
    max.overlaps = 30
    ) +
  ggplot2::scale_color_manual(values = c("GM" = "#FF6A3D", "HC" = "#06B6D4")) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(override.aes = list(fill = "white", shape = 21, stroke = 1.5))
    ) +
  ggplot2::scale_size_continuous(range = c(2, 9), guide = "none") +
  ggraph::scale_edge_width_continuous(range = c(0.4, 2.5), name = "Seasons Worked Together") +
  ggraph::scale_edge_alpha_continuous(guide = "none") +
  ggraph::theme_graph() +
  ggplot2::labs(
    title = "NHL General Manager's and Head Coaches, Post-2004/05 Lockout Era",
    caption = "Figure notes: The thickness of the arrows illustrate the number of seasons GMs and HCs worked together. The direction of arrows indicate seniority on the job. Names show GMs and HCs who have worked at least 3 seasons in the league since the 2005-06. The size of each node indicates the number of seasons they have worked since the 2004-05 NHL lockout."
    )

# output graph
# ggplot2::ggsave("fig01.png", fig01, path = "~/Desktop", height = 10, width = 20, dpi = 500)





# end .R script


