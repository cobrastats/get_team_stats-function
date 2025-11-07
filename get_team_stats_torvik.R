# Season should be single year 
## Example, for 2025-26 season; get_game_stats_torvik(2026)

get_game_stats_torvik = function(season) {
  start_time = Sys.time()
  
  # Grab csv files from Torvik's site
  data_raw = read.csv(
    paste0(
      "https://barttorvik.com/getgamestats.php?year=",
      season,
      "&csv=1"
    ),
    header = FALSE
  )
  sked = read.csv(paste0("https://barttorvik.com/", season, "_super_sked.csv"),
                  header = FALSE)
  
  # Codes for Game Type
  ## 3 = post
  ## 2 = conf_t
  ## 1 = conf
  ## 0 = nc
  ## 99 = nonD1
  
  # Codes for Location
  ## 0 = H/A
  ## 1 = N
  
  # Get schedule columns needed and convert type code
  sked = sked %>%
    dplyr::select(V1, V2, V7, V8) %>%
    dplyr::rename(
      game_id = V1,
      date = V2,
      type_code = V7,
      loc_code = V8
    ) %>%
    dplyr::mutate(
      date = as.Date(date, format = "%m/%d/%y"),
      type = if_else(
        type_code == 3,
        "post",
        if_else(
          type_code == 2,
          "conf_t",
          if_else(type_code == 1, "conf", if_else(type_code ==
                                                    0, "nc", "nonD1"))
        )
      )
    )
  
  
  # Clean up data from stats csv file
  data_raw = data_raw %>%
    dplyr::mutate(clean = str_remove_all(V30, "\\[|\\]|\"")) %>%
    tidyr::separate_wider_delim(clean, delim = ",", names_sep = "",too_few = "align_start") %>% 
    dplyr::select(V1:clean38)
  
  # Add column names
  names(data_raw) =
    c(
      "date",
      "unknown1",
      "team",
      "conf",
      "opp",
      "location",
      "result",
      "adjo",
      "adjd",
      "off_eff",
      "off_efg",
      "off_to",
      "off_orb",
      "off_ftr",
      "def_eff",
      "def_efg",
      "def_to",
      "def_orb",
      "def_ftr",
      "gscore",
      "opp_conf",
      "row2",
      "season",
      "pace",
      "game_id",
      "coach",
      "opp_coach",
      "avg_diff",
      "unknown2",
      "text",
      "unknown3",
      "date2",
      "minutes",
      "away",
      "home",
      "away_fgm",
      "away_fga",
      "away_tpm",
      "away_tpa",
      "away_ftm",
      "away_fta",
      "away_orb",
      "away_drb",
      "away_reb",
      "away_ast",
      "away_stl",
      "away_blk",
      "away_to",
      "away_pf",
      "away_pts",
      "home_fgm",
      "home_fga",
      "home_tpm",
      "home_tpa",
      "home_ftm",
      "home_fta",
      "home_orb",
      "home_drb",
      "home_reb",
      "home_ast",
      "home_stl",
      "home_blk",
      "home_to",
      "home_pf",
      "home_pts",
      "temp",
      "unknown4",
      "home2",
      "away2"
    )
  
  
  # Create preliminary clean stats dataset
  game_stats = suppressMessages(
    data_raw %>%
      
      # Remove some unnecessary columns
      dplyr::select(-c(unknown1, result)) %>%
      
      # Remove some unnecessary white space
      dplyr::mutate(across(where(is.character), str_trim)) %>%
      
      # Begin to convert columns
      dplyr::mutate(
        date = as.Date(date, format = "%m/%d/%y"),
        pts = if_else(team == away, away_pts, home_pts),
        opp_pts = if_else(team == home, away_pts, home_pts),
        result = if_else(pts > opp_pts, "W", "L"),
        fgm = if_else(team == home, home_fgm, away_fgm),
        fga = if_else(team == home, home_fga, away_fga),
        tpm = if_else(team == home, home_tpm, away_tpm),
        tpa = if_else(team == home, home_tpa, away_tpa),
        ftm = if_else(team == home, home_ftm, away_ftm),
        fta = if_else(team == home, home_fta, away_fta),
        orb = if_else(team == home, home_orb, away_orb),
        drb = if_else(team == home, home_drb, away_drb),
        reb = if_else(team == home, home_reb, away_reb),
        ast = if_else(team == home, home_ast, away_ast),
        stl = if_else(team == home, home_stl, away_stl),
        blk = if_else(team == home, home_blk, away_blk),
        to = if_else(team == home, home_to, away_to),
        pf = if_else(team == home, home_pf, away_pf),
        opp_fgm = if_else(team == away, home_fgm, away_fgm),
        opp_fga = if_else(team == away, home_fga, away_fga),
        opp_tpm = if_else(team == away, home_tpm, away_tpm),
        opp_tpa = if_else(team == away, home_tpa, away_tpa),
        opp_ftm = if_else(team == away, home_ftm, away_ftm),
        opp_fta = if_else(team == away, home_fta, away_fta),
        opp_orb = if_else(team == away, home_orb, away_orb),
        opp_drb = if_else(team == away, home_drb, away_drb),
        opp_reb = if_else(team == away, home_reb, away_reb),
        opp_ast = if_else(team == away, home_ast, away_ast),
        opp_stl = if_else(team == away, home_stl, away_stl),
        opp_blk = if_else(team == away, home_blk, away_blk),
        opp_to = if_else(team == away, home_to, away_to),
        opp_pf = if_else(team == away, home_pf, away_pf),
        tempo = pace
      ) %>%
      
      # Remove lots of unneeded columns
      dplyr::select(
        -c(
          row2,
          pace,
          temp,
          unknown2,
          text,
          unknown3,
          date2,
          away,
          home,
          away_fgm,
          away_fga,
          away_tpm,
          away_tpa,
          away_ftm,
          away_fta,
          away_orb,
          away_drb,
          away_reb,
          away_ast,
          away_stl,
          away_blk,
          away_to,
          away_pf,
          away_pts,
          home_fgm,
          home_fga,
          home_tpm,
          home_tpa,
          home_ftm,
          home_fta,
          home_orb,
          home_drb,
          home_reb,
          home_ast,
          home_stl,
          home_blk,
          home_to,
          home_pf,
          home_pts,
          unknown4,
          home2,
          away2,
          off_eff:def_ftr
        )
      ) %>%
      
      # Convert columns that were character to numeric
      dplyr::mutate(across(
        c(adjo:gscore, pts:opp_pts, fgm:tempo), as.numeric
      )) %>%
      
      # Relocate to rearrange for best layout of dataframe
      dplyr::relocate(c(minutes:tempo), .before = adjo) %>%
      dplyr::relocate(c(season:game_id), .before = date) %>%
      dplyr::relocate(opp_conf, .before = location) %>%
      
      # Calculate "Advanced Stats"
      dplyr::mutate(
        fg_pct = fgm / fga,
        three_pct = tpm / tpa,
        two_pct = (fgm - tpm) / (fga - tpa),
        ft_pct = ftm / fta,
        ppp = pts / tempo,
        efg = (fgm + 0.5 * tpm) / fga,
        orb_rate = orb / (orb + opp_drb),
        drb_rate = drb / (drb + opp_orb),
        ft_rate = fta / fga,
        to_rate = to / tempo,
        opp_fg_pct = opp_fgm / opp_fga,
        opp_three_pct = opp_tpm / opp_tpa,
        opp_two_pct = (opp_fgm - opp_tpm) / (opp_fga - opp_tpa),
        opp_ft_pct = opp_ftm / opp_fta,
        opp_ppp = opp_pts / tempo,
        opp_efg = (opp_fgm + 0.5 * opp_tpm) / opp_fga,
        opp_orb_rate = opp_orb / (opp_orb + drb),
        opp_ft_rate = opp_fta / opp_fga,
        opp_to_rate = opp_to / tempo
      ) %>%
      
      # Join Schedule Columns
      dplyr::left_join(sked)
  )
  
  # Prepare Output to match cbbdata::cbd_torvik_game_stats
  output = game_stats %>%
    dplyr::mutate(location = if_else(type_code == 1, "N", location)) %>%
    dplyr::mutate(
      loc = location,
      pts_scored = pts,
      pts_allowed = opp_pts,
      pos = tempo
    ) %>%
    dplyr::select(
      game_id,
      date,
      year = season,
      type,
      location,
      result,
      team,
      conf,
      opp,
      opp_conf,
      min = minutes,
      pos,
      fgm,
      fga,
      fg_pct,
      tpm,
      tpa,
      tp_pct = three_pct,
      ftm,
      fta,
      ft_pct,
      oreb = orb,
      dreb = drb,
      reb,
      ast,
      stl,
      blk,
      to,
      pf,
      pts,
      opp_fgm,
      opp_fga,
      opp_fg_pct,
      opp_tpm,
      opp_tpa,
      opp_tp_pct = opp_three_pct,
      opp_ftm,
      opp_fta,
      opp_ft_pct,
      opp_oreb = opp_orb,
      opp_dreb = opp_drb,
      opp_reb,
      opp_ast,
      opp_stl,
      opp_blk,
      opp_to,
      opp_pf,
      opp_pts,
      loc,
      pts_scored,
      pts_allowed,
      adj_o = adjo,
      adj_d = adjd,
      off_ppp = ppp,
      off_efg = efg,
      off_to = to_rate,
      off_or = orb_rate,
      off_ftr = ft_rate,
      def_ppp = opp_ppp,
      def_efg = opp_efg,
      def_to = opp_to_rate,
      def_or = opp_orb_rate,
      def_ftr = opp_ft_rate,
      game_score = gscore,
      tempo,
      coach,
      opp_coach,
      avg_marg = avg_diff
    ) %>%
    dplyr::mutate(across(c(off_ppp:def_ftr), ~ round(.x * 100, 2)))
  
  end_time = Sys.time()
  message(
    paste0(
      "Complete | ",
      nrow(output) / 2,
      " games (",
      nrow(output),
      " team stat lines) from ",
      min(output$date),
      " to ",
      max(output$date),
      " (",
      round(as.numeric(
        difftime(end_time, start_time, units = "secs")
      ), 2),
      " seconds)"
    )
  )
  
  return(output)
}

