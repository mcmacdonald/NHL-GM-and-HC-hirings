


# this script forms longtitudinal bipartite graphs of general managers - head coaches

# first, find the career start date for each coach
hc_debuts <- hc %>%
  dplyr::mutate(firstCoachedDate = as.Date(firstCoachedDate)) %>%
  dplyr::group_by(coachName) %>%
  dplyr::summarise(hc_start = min(firstCoachedDate, na.rm = TRUE), .groups = "drop")

# second, find the career start date for each general manager
gm_debuts <- gm %>%
  dplyr::mutate(startDate = as.Date(startDate)) %>%
  dplyr::group_by(fullName) %>%
  dplyr::summarise(gm_start = min(startDate, na.rm = TRUE), .groups = "drop")

# find career debut dates rather than the seasonal dates

  # for coaches
  hc_points <- hc %>%
    dplyr::select(coachName, teamName, season) %>%
    dplyr::left_join(hc_debuts, by = "coachName")
  
  # for general managers
  gm_points <- gm %>%
    dplyr::select(fullName, teamName, season) %>%
    dplyr::left_join(gm_debuts, by = "fullName")

# join points together to pair the staff together
edge_list <- dplyr::inner_join(
  hc_points,
  gm_points,
  by = c("teamName", "season")
  )

# order the direction of arcs on the basis of seniority with the team
edge_list <- edge_list %>%
  dplyr::mutate(
    from = dplyr::case_when(
      gm_start < hc_start  ~ fullName,  # GM has been with the team before the HC
      hc_start < gm_start  ~ coachName, # HC has been with the team before the GM
      gm_start == hc_start ~ fullName,  # in case of a tie, order GM before the HC
      TRUE                              ~ fullName
      ),
    to = dplyr::if_else(from == fullName, coachName, fullName),
    team = teamName,
    season = season
    ) %>%
  dplyr::select(from, to, team, season)





# end .R script
