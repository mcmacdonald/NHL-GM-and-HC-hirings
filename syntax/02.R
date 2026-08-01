


# url to historical database of general manager season records
response <- httr::GET(
  url = "https://records.nhl.com/site/api/general-manager-franchise-season-records", 
  httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
                   )
  )

# JSON file straight to long dataset
json <- httr::content(response, as = "text", encoding = "UTF-8")

# pull general manager records
gm <- jsonlite::fromJSON(json, flatten = TRUE)$data

# don't run
# write to .csv file
# write.csv(gm, "~/Dekstop/gm.csv")

# display the cleanly structured row-by-row season history
head(gm)

# drop duplicate coach stints for the SAME team in the SAME season/game type
gm <- gm %>%  dplyr::distinct(fullName, seasonId, teamName, gameTypeId, .keep_all = TRUE)

# don't run
# write to .csv file
# write.csv(gm, "~/Dekstop/gm.csv")

# display the cleanly structured row-by-row season history
head(gm)

# format column that lists NHL seasons
gm$season <- stringr::str_replace(gm$seasonId, "^(\\d{4})(\\d{4})$", "\\1-\\2")

# separate season into columns
gm <- tidyr::separate_wider_delim(
  gm, 
  cols = season, 
  delim = "-", 
  names = c("season_start", "season_end"),
  cols_remove = FALSE 
  )

# career coaching record: career games, career wins, career losses, career overtime/shootout wins, career overtime/shootout losses, career points percentage, career wins percentage
career <- gm %>%
  dplyr::select(
    fullName, 
    firstName,
    lastName,
    startDate, 
    endDate, 
    gameTypeId,
    season, 
    season_start,
    season_end,
    games, 
    wins, 
    losses,
    winsInOt, 
    lossesInOt,
    winsInShootout,
    lossesInShootout
    )

# calculate cumulative career totals
career <- career %>%
  # sort cases chronologically per coach to ensure correct cumulative order
  dplyr::arrange(fullName, season_start, startDate, endDate) %>%
  dplyr::mutate(
    games            = dplyr::coalesce(games, 0),
    wins             = dplyr::coalesce(wins, 0),
    losses           = dplyr::coalesce(losses, 0),
    winsInOt         = dplyr::coalesce(winsInOt, 0),
    lossesInOt       = dplyr::coalesce(lossesInOt, 0),
    winsInShootout   = dplyr::coalesce(winsInShootout, 0),
    lossesInShootout = dplyr::coalesce(lossesInShootout, 0)
    ) %>%
  # group by coach and leg of season i.e., regular season or playoffs
  dplyr::group_by(fullName, gameTypeId) %>%
  # calculate running totals and percentages
  dplyr::mutate(
    career_games    = cumsum(dplyr::coalesce(games, 0)),
    career_wins     = cumsum(dplyr::coalesce(wins, 0)),
    career_losses   = cumsum(dplyr::coalesce(losses, 0)),
    career_winsOT   = cumsum(dplyr::coalesce(winsInOt, 0) + dplyr::coalesce(winsInShootout, 0)),
    career_lossesOT = cumsum(dplyr::coalesce(lossesInOt, 0) + dplyr::coalesce(lossesInShootout, 0)),
    career_winsPctg   = dplyr::if_else(career_games > 0, career_wins / career_games, 0),
    career_points     = (career_wins * 2) + (career_lossesOT * 1),
    career_pointsPctg = dplyr::if_else(career_games > 0, career_points / (career_games * 2), 0)
    ) %>%
  dplyr::ungroup()

# arrange columns
career <- career %>%
  dplyr::select(
    fullName, 
    firstName, 
    lastName, 
    # startDate, 
    # endDate, 
    gameTypeId,
    season, 
    season_start, 
    season_end, 
    career_games,
    career_wins,
    career_losses, 
    career_winsOT,
    career_lossesOT, 
    career_winsPctg, 
    career_points, 
    career_pointsPctg
    )

# career regular season statisrtics
career_regular <- career %>% 
  dplyr::filter(gameTypeId == 2) %>% 
  dplyr::select(-gameTypeId) %>%
  dplyr::distinct(fullName, firstName, lastName, season, season_start, season_end, .keep_all = TRUE) %>%
  dplyr::rename_with( # apppend to columns
    ~ paste0(., "_regular"), 
    starts_with("career_")
    )

# career post-season statistics
career_playoff <- career %>% 
  dplyr::filter(gameTypeId == 3) %>% 
  dplyr::select(-gameTypeId) %>%
  dplyr::distinct(fullName, firstName, lastName, season, season_start, season_end, .keep_all = TRUE) %>%
  dplyr::rename_with( # apppend to columns
    ~ paste0(., "_playoffs"), 
    starts_with("career_")
    )

# join together
career <- dplyr::left_join(
  career_regular,
  career_playoff,
  by = c(
    "fullName",                 
    "firstName",                 
    "lastName",                  
    "season",                    
    "season_start",              
    "season_end" 
    )
  )
rm(career_regular, career_playoff)

# fill missing cases
career <- career %>%
  # arrange chronologically to ensure data fills forward correctly
  dplyr::arrange(fullName, season_start, season_end) %>%
  # group by coach and the season
  dplyr::group_by(fullName) %>%
  # fill missing values down, then up to cover all rows in that season
  tidyr::fill(dplyr::ends_with("_playoffs"), .direction = "down") %>%
  # fill seasons before first playoff appearance = 0
  dplyr::mutate(
    dplyr::across(dplyr::ends_with("_playoffs"), ~ dplyr::coalesce(.x, 0))
    ) %>%
  # ungroup
  dplyr::ungroup()





# reorder columns for the general manager records ------------------------------
gm <- gm %>%
  dplyr::select( # columns to retain
    id, 
    fullName, 
    firstName, 
    lastName, 
    startDate, 
    endDate, 
    season, 
    season_start,
    season_end,
    activeGm, 
    teamId, 
    franchiseId, 
    teamName, 
    franchiseName, 
    teamAbbrev, 
    gameTypeId, # notes regular seasons or playoffs
    games, 
    homeGames, 
    roadGames,
    wins, 
    losses, 
    winsInOt, 
    winsInShootout, 
    lossesInOt, 
    lossesInShootout, 
    points, 
    pointPctg, 
    winPctg, 
    # playoffResult, 
    seriesTitle, 
    wonStanleyCup, 
    wonGmOfTheYear
    )

# use the 2004-2005 nhl lockout as the starting point
gm <- gm %>% dplyr::filter(season_start >= 2005)

# sort cases in this order
gm <- gm %>% dplyr::arrange(season_start, season_end, teamName, lastName, firstName)

# separate regular season coaching records
regular <- gm %>% dplyr::filter(gameTypeId == 2)

# separate playoff coaching records
playoffs <- gm %>% dplyr::filter(gameTypeId == 3)


# recode regular season results for playoff qualification 

# print outcomes 
table(regular$seriesTitle, useNA = "always")

# did not qualify for playoffs
regular$seriesTitle[is.na(regular$seriesTitle)] <- "DNQ"

# don't run 
# pandemic-shortened season had play-in qualifiers ... not part of the actual playoffs 
# dplyr::filter(regular, seriesTitle == "Stanley Cup Qualifiers")

# did not qualify for playoffs
regular$seriesTitle[regular$seriesTitle == "Stanley Cup Qualifiers"] <- "DNQ"

# 1st round
regular$seriesTitle[regular$seriesTitle == "Conference Quarterfinals"] <- "1st Round"

# 2nd round
regular$seriesTitle[regular$seriesTitle == "Conference Semifinals"] <- "2nd Round"

# conference finals
regular$seriesTitle[regular$seriesTitle == "Stanley Cup Semifinals"] <- "Conference Finals"

# manually set the reference 
regular$seriesTitle <- forcats::fct_relevel(regular$seriesTitle, "DNQ")

# columns for playoff results 
playoffs <- playoffs %>% 
  dplyr::select(
    fullName, 
    firstName,
    lastName,
    season, 
    teamName,
    games, 
    wins, 
    losses,
    winsInOt, 
    lossesInOt
    )

# rename columns
playoffs <- playoffs %>%
  dplyr::rename(
    playoff_games = games,
    playoff_wins = wins,
    playoff_losses = losses,
    playoff_winsInOt = winsInOt,
    playoff_lossesInOt = lossesInOt
    )

# join regular season and playoff results into single cases
gm <- dplyr::left_join(
  regular, 
  playoffs, 
  by = c(
    "fullName", 
    "firstName",
    "lastName",
    "season", 
    "teamName"
    )
  )
rm(regular, playoffs)





# join single season and career records together -------------------------------
gm <- dplyr::left_join(
  gm,
  career,
  by = c(
    "fullName",
    "firstName",
    "lastName",
    "season",
    "season_start",
    "season_end"
    )
  )
rm(career)





# end . R script


