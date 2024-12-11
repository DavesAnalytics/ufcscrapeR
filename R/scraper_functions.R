#' @importFrom rvest read_html html_elements html_text2 html_attr html_node html_text
#' @importFrom httr GET content
#' @importFrom stringr str_squish str_detect str_replace_all str_extract_all str_replace str_split_fixed
#' @importFrom dplyr %>% filter mutate select everything bind_rows
#' @importFrom purrr map map_dfr map_chr
#' @importFrom utils head write.csv
#' @importFrom stats na.omit
NULL

############## Helper Functions ##############

safe_get_html <- function(url){
  resp <- httr::GET(url)
  if (httr::http_error(resp)) {
    stop("Failed to retrieve ", url, call. = FALSE)
  }
  pg <- httr::content(resp, as="text", encoding="UTF-8")
  xml2::read_html(pg)
}

trim_na <- function(x) {
  x <- str_squish(x)
  x[x %in% c("", "---")] <- NA
  x
}

ctrl_to_seconds <- function(x) {
  # Convert mm:ss to numeric seconds
  if (!is.na(x) && str_detect(x, ":")) {
    parts <- unlist(strsplit(x, ":"))
    if (length(parts) == 2 && all(grepl("^[0-9]+$", parts))) {
      return(as.character(as.numeric(parts[1]) * 60 + as.numeric(parts[2])))
    }
  }
  x
}

time_to_seconds <- function(x) {
  if (!is.na(x) && str_detect(x, ":")) {
    parts <- unlist(strsplit(x, ":"))
    if (length(parts) == 2 && all(grepl("^[0-9]+$", parts))) {
      return(as.character(as.numeric(parts[1]) * 60 + as.numeric(parts[2])))
    }
  }
  x
}

##############################################
# Searching Fighter URL by Name
##############################################

search_fighter_by_name_part <- function(query) {
  base_url <- "http://ufcstats.com/statistics/fighters/search"
  resp <- httr::GET(base_url, query = list(query = query))
  if (httr::http_error(resp)) {
    return(data.frame(full_name=character(), link=character()))
  }
  pg <- httr::content(resp, as="text", encoding="UTF-8")
  doc <- xml2::read_html(pg)
  
  tab <- rvest::html_node(doc, "table.b-statistics__table")
  if (is.null(tab)) {
    return(data.frame(full_name=character(), link=character()))
  }
  
  rows <- rvest::html_elements(tab, "tbody tr.b-statistics__table-row")
  candidates <- map_dfr(rows, function(r) {
    cols <- rvest::html_elements(r, "td")
    if (length(cols) < 2) return(NULL)
    first_a <- rvest::html_node(cols[[1]], "a.b-link_style_black")
    last_a <- rvest::html_node(cols[[2]], "a.b-link_style_black")
    if (is.null(first_a) || is.null(last_a)) return(NULL)
    first_name <- trim_na(rvest::html_text2(first_a))
    last_name <- trim_na(rvest::html_text2(last_a))
    fighter_link <- rvest::html_attr(last_a, "href")
    full_name <- paste(first_name, last_name)
    data.frame(full_name=full_name, link=fighter_link, stringsAsFactors = FALSE)
  })
  
  candidates
}

get_fighter_url_by_name <- function(fighter_name) {
  fighter_name_clean <- tolower(str_squish(fighter_name))
  name_parts <- strsplit(fighter_name_clean, "\\s+")[[1]]
  
  candidates <- NULL
  if (length(name_parts) > 1) {
    last_name <- name_parts[length(name_parts)]
    first_name <- name_parts[1]
    
    candidates <- search_fighter_by_name_part(last_name)
    if (nrow(candidates) == 0) {
      candidates <- search_fighter_by_name_part(first_name)
    }
    if (nrow(candidates) == 0) {
      # try each part
      for (p in name_parts) {
        candidates <- search_fighter_by_name_part(p)
        if (nrow(candidates) > 0) break
      }
    }
  } else {
    candidates <- search_fighter_by_name_part(name_parts[1])
  }
  
  if (is.null(candidates) || nrow(candidates) == 0) {
    stop("No suitable match found for fighter: ", fighter_name_clean, call.=FALSE)
  }
  
  candidates$link[1]
}

##############################################
# Getting Fight Links
##############################################

get_fight_links <- function(fighter_url) {
  doc <- safe_get_html(fighter_url)
  tab <- rvest::html_node(doc, "table.b-fight-details__table_type_event-details")
  if (is.null(tab)) stop("No fight details table found on the fighter page.")
  
  rows <- rvest::html_elements(tab, "tbody tr.b-fight-details__table-row__hover")
  
  get_two_values_from_col <- function(col) {
    ps <- rvest::html_elements(col, "p.b-fight-details__table-text")
    if (length(ps)==2) {
      v1 <- trim_na(rvest::html_text2(ps[[1]]))
      v2 <- trim_na(rvest::html_text2(ps[[2]]))
      return(c(v1,v2))
    }
    return(c(NA,NA))
  }
  
  fights_data <- map_dfr(rows, function(r) {
    link <- rvest::html_attr(r, "data-link")
    cols <- rvest::html_elements(r, "td.b-fight-details__table-col")
    
    if (length(cols) < 10) return(NULL)
    result_tag <- rvest::html_node(cols[[1]], "p.b-fight-details__table-text")
    result <- if(!is.null(result_tag)) trim_na(rvest::html_text2(result_tag)) else NA
    
    f_td <- rvest::html_elements(cols[[2]], "p.b-fight-details__table-text")
    fighter_name <- if(length(f_td)>=1) trim_na(rvest::html_text2(f_td[[1]])) else NA
    opponent_name <- if(length(f_td)>=2) trim_na(rvest::html_text2(f_td[[2]])) else NA
    
    kd_vals <- get_two_values_from_col(cols[[3]])
    str_vals <- get_two_values_from_col(cols[[4]])
    td_vals <- get_two_values_from_col(cols[[5]])
    sub_vals <- get_two_values_from_col(cols[[6]])
    
    event_td <- rvest::html_elements(cols[[7]], "p.b-fight-details__table-text")
    event_name <- if(length(event_td)>0) trim_na(rvest::html_text2(event_td[[1]])) else NA
    event_date <- if(length(event_td)>1) trim_na(rvest::html_text2(event_td[[2]])) else NA
    
    method_td <- rvest::html_elements(cols[[8]], "p.b-fight-details__table-text")
    method_main <- if(length(method_td)>0) trim_na(rvest::html_text2(method_td[[1]])) else NA
    method_detail <- if(length(method_td)>1) trim_na(rvest::html_text2(method_td[[2]])) else NA
    
    round_val <- rvest::html_node(cols[[9]], "p.b-fight-details__table-text")
    round_val <- if(!is.null(round_val)) trim_na(rvest::html_text2(round_val)) else NA
    
    time_val <- rvest::html_node(cols[[10]], "p.b-fight-details__table-text")
    time_val <- if(!is.null(time_val)) trim_na(rvest::html_text2(time_val)) else NA
    time_val <- time_to_seconds(time_val)
    
    data.frame(
      result=result,
      fighter_name=fighter_name,
      opponent_name=opponent_name,
      kd_fighter=kd_vals[1], kd_opponent=kd_vals[2],
      str_fighter=str_vals[1], str_opponent=str_vals[2],
      td_fighter=td_vals[1], td_opponent=td_vals[2],
      sub_fighter=sub_vals[1], sub_opponent=sub_vals[2],
      event_name=event_name,
      event_date=event_date,
      method_main=method_main,
      method_detail=method_detail,
      round=round_val,
      Time=time_val,
      fight_link=link,
      stringsAsFactors = FALSE
    )
  })
  
  list(links = fights_data$fight_link, df = fights_data)
}

##############################################
# Parsing Fight Details
##############################################

parse_totals_table <- function(doc, main_fighter_name) {
  totals_heading <- rvest::html_node(doc, "p.b-fight-details__collapse-link_tot:contains('Totals')")
  if (is.null(totals_heading)) return(list())
  
  totals_section <- rvest::html_node(totals_heading, xpath="following-sibling::section[1]")
  if (is.null(totals_section)) return(list())
  
  totals_table <- rvest::html_node(totals_section, "table")
  if (is.null(totals_table)) return(list())
  
  rows <- rvest::html_elements(totals_table, "tbody tr.b-fight-details__table-row")
  if (length(rows)==0) return(list())
  
  get_two_val <- function(cell) {
    ps <- rvest::html_elements(cell, "p.b-fight-details__table-text")
    if (length(ps)==2) {
      v1 <- trim_na(rvest::html_text2(ps[[1]]))
      v2 <- trim_na(rvest::html_text2(ps[[2]]))
      return(c(v1,v2))
    }
    c(NA,NA)
  }
  
  first_row <- rows[[1]]
  cols <- rvest::html_elements(first_row, "td")
  if (length(cols)<10) return(list())
  
  fighter_col <- get_two_val(cols[[1]])
  fighter1 <- fighter_col[1]
  fighter2 <- fighter_col[2]
  
  if (is.na(fighter1) || is.na(fighter2)) return(list())
  
  main_is_first <- (tolower(main_fighter_name)==tolower(fighter1))
  
  kd <- get_two_val(cols[[2]])
  sig_str <- get_two_val(cols[[3]])
  sig_str_pct <- get_two_val(cols[[4]])
  total_str <- get_two_val(cols[[5]])
  td <- get_two_val(cols[[6]])
  td_pct <- get_two_val(cols[[7]])
  sub_att <- get_two_val(cols[[8]])
  rev <- get_two_val(cols[[9]])
  ctrl <- get_two_val(cols[[10]])
  
  ctrl[1] <- ctrl_to_seconds(ctrl[1])
  ctrl[2] <- ctrl_to_seconds(ctrl[2])
  
  make_pair <- function(f1_val,f2_val) {
    if (main_is_first) c(f1_val,f2_val) else c(f2_val,f1_val)
  }
  
  out <- list(
    TOT_fighter_KD = make_pair(kd[1], kd[2])[1],
    TOT_opponent_KD = make_pair(kd[1], kd[2])[2],
    TOT_fighter_SigStr = make_pair(sig_str[1], sig_str[2])[1],
    TOT_opponent_SigStr = make_pair(sig_str[1], sig_str[2])[2],
    TOT_fighter_SigStr_pct = make_pair(sig_str_pct[1], sig_str_pct[2])[1],
    TOT_opponent_SigStr_pct = make_pair(sig_str_pct[1], sig_str_pct[2])[2],
    TOT_fighter_TotalStr = make_pair(total_str[1], total_str[2])[1],
    TOT_opponent_TotalStr = make_pair(total_str[1], total_str[2])[2],
    TOT_fighter_Td = make_pair(td[1], td[2])[1],
    TOT_opponent_Td = make_pair(td[1], td[2])[2],
    TOT_fighter_Td_pct = make_pair(td_pct[1], td_pct[2])[1],
    TOT_opponent_Td_pct = make_pair(td_pct[1], td_pct[2])[2],
    TOT_fighter_SubAtt = make_pair(sub_att[1], sub_att[2])[1],
    TOT_opponent_SubAtt = make_pair(sub_att[1], sub_att[2])[2],
    TOT_fighter_Rev = make_pair(rev[1], rev[2])[1],
    TOT_opponent_Rev = make_pair(rev[1], rev[2])[2],
    TOT_fighter_Ctrl = make_pair(ctrl[1], ctrl[2])[1],
    TOT_opponent_Ctrl = make_pair(ctrl[1], ctrl[2])[2]
  )
  out
}

parse_per_round_totals <- function(doc, main_fighter_name) {
  totals_heading <- rvest::html_node(doc, "p.b-fight-details__collapse-link_tot:contains('Totals')")
  if (is.null(totals_heading)) return(list())
  
  per_round_link <- rvest::html_node(totals_heading, xpath=".//following::a[contains(., 'Per round')]")
  if (is.null(per_round_link)) return(list())
  
  per_round_table <- rvest::html_node(per_round_link, xpath="following::table[1]")
  if (is.null(per_round_table)) return(list())
  
  get_two_val <- function(cell) {
    ps <- rvest::html_elements(cell, "p.b-fight-details__table-text")
    if (length(ps)==2) {
      v1 <- trim_na(rvest::html_text2(ps[[1]]))
      v2 <- trim_na(rvest::html_text2(ps[[2]]))
      return(c(v1,v2))
    }
    c(NA,NA)
  }
  
  round_headers <- rvest::html_elements(per_round_table, "thead.b-fight-details__table-row_type_head")
  if (length(round_headers)==0) return(list())
  
  # Determine order from first round
  first_header <- round_headers[[1]]
  first_data_row <- rvest::html_node(first_header, xpath="following::tr[contains(@class,'b-fight-details__table-row')][1]")
  if (is.null(first_data_row)) return(list())
  cells <- rvest::html_elements(first_data_row, "td.b-fight-details__table-col")
  if (length(cells)<9) return(list())
  
  f_col <- get_two_val(cells[[1]])
  if (is.na(f_col[1]) || is.na(f_col[2])) return(list())
  main_is_first <- (tolower(main_fighter_name)==tolower(f_col[1]))
  
  data_out <- list()
  
  make_pair <- function(f1_val,f2_val) {
    if (main_is_first) c(f1_val,f2_val) else c(f2_val,f1_val)
  }
  
  for (rh in round_headers) {
    round_name <- trim_na(str_replace_all(rvest::html_text(rh),"Round ",""))
    data_row <- rvest::html_node(rh, xpath="following::tr[contains(@class,'b-fight-details__table-row')][1]")
    if (is.null(data_row)) next
    cells <- rvest::html_elements(data_row, "td.b-fight-details__table-col")
    if (length(cells)<9) next
    
    kd <- get_two_val(cells[[2]])
    sig_str <- get_two_val(cells[[3]])
    sig_str_pct <- get_two_val(cells[[4]])
    total_str <- get_two_val(cells[[5]])
    td_pct <- get_two_val(cells[[6]])
    sub_att <- get_two_val(cells[[7]])
    rev <- get_two_val(cells[[8]])
    ctrl <- get_two_val(cells[[9]])
    
    ctrl[1] <- ctrl_to_seconds(ctrl[1])
    ctrl[2] <- ctrl_to_seconds(ctrl[2])
    
    prefix <- paste0("Round", round_name, "_")
    
    data_out[[paste0(prefix,"fighter_KD")]] <- make_pair(kd[1],kd[2])[1]
    data_out[[paste0(prefix,"opponent_KD")]] <- make_pair(kd[1],kd[2])[2]
    data_out[[paste0(prefix,"fighter_SigStr")]] <- make_pair(sig_str[1],sig_str[2])[1]
    data_out[[paste0(prefix,"opponent_SigStr")]] <- make_pair(sig_str[1],sig_str[2])[2]
    data_out[[paste0(prefix,"fighter_SigStr_pct")]] <- make_pair(sig_str_pct[1],sig_str_pct[2])[1]
    data_out[[paste0(prefix,"opponent_SigStr_pct")]] <- make_pair(sig_str_pct[1],sig_str_pct[2])[2]
    data_out[[paste0(prefix,"fighter_TotalStr")]] <- make_pair(total_str[1],total_str[2])[1]
    data_out[[paste0(prefix,"opponent_TotalStr")]] <- make_pair(total_str[1],total_str[2])[2]
    data_out[[paste0(prefix,"fighter_Td_pct")]] <- make_pair(td_pct[1],td_pct[2])[1]
    data_out[[paste0(prefix,"opponent_Td_pct")]] <- make_pair(td_pct[1],td_pct[2])[2]
    data_out[[paste0(prefix,"fighter_SubAtt")]] <- make_pair(sub_att[1],sub_att[2])[1]
    data_out[[paste0(prefix,"opponent_SubAtt")]] <- make_pair(sub_att[1],sub_att[2])[2]
    data_out[[paste0(prefix,"fighter_Rev")]] <- make_pair(rev[1],rev[2])[1]
    data_out[[paste0(prefix,"opponent_Rev")]] <- make_pair(rev[1],rev[2])[2]
    data_out[[paste0(prefix,"fighter_Ctrl")]] <- make_pair(ctrl[1],ctrl[2])[1]
    data_out[[paste0(prefix,"opponent_Ctrl")]] <- make_pair(ctrl[1],ctrl[2])[2]
    
  }
  
  data_out
}

parse_per_round_significant_strikes <- function(doc, main_fighter_name) {
  sections <- rvest::html_elements(doc, "section.b-fight-details__section")
  sig_strikes_section <- NULL
  for (s in sections) {
    heading <- rvest::html_node(s, "p.b-fight-details__collapse-link_tot")
    htxt <- ifelse(is.null(heading), "", rvest::html_text2(heading))
    if (!is.na(htxt) && htxt!="" && str_detect(htxt, "Significant Strikes")) {
      sig_strikes_section <- s
      break
    }
  }
  if (is.null(sig_strikes_section)) return(list())
  
  per_round_link <- rvest::html_node(sig_strikes_section, xpath=".//a[contains(., 'Per round')]")
  if (is.null(per_round_link)) return(list())
  per_round_table <- rvest::html_node(per_round_link, xpath="following::table[1]")
  if (is.null(per_round_table)) return(list())
  
  get_two_val <- function(cell) {
    ps <- rvest::html_elements(cell, "p.b-fight-details__table-text")
    if (length(ps)==2) {
      v1 <- trim_na(rvest::html_text2(ps[[1]]))
      v2 <- trim_na(rvest::html_text2(ps[[2]]))
      return(c(v1,v2))
    }
    c(NA,NA)
  }
  
  col_map <- c("SIG_STR","SIG_STR_pct","HEAD","BODY","LEG","DISTANCE","CLINCH","GROUND")
  
  round_headers <- rvest::html_elements(per_round_table, "thead.b-fight-details__table-row_type_head")
  if (length(round_headers)==0) return(list())
  
  first_header <- round_headers[[1]]
  first_data_row <- rvest::html_node(first_header, xpath="following::tr[contains(@class,'b-fight-details__table-row')][1]")
  if (is.null(first_data_row)) return(list())
  first_cells <- rvest::html_elements(first_data_row, "td.b-fight-details__table-col")
  if (length(first_cells)<9) return(list())
  f_col <- get_two_val(first_cells[[1]])
  if (is.na(f_col[1])||is.na(f_col[2])) return(list())
  main_is_first <- (tolower(main_fighter_name)==tolower(f_col[1]))
  
  data_out <- list()
  
  make_pair <- function(v1,v2) {
    if (main_is_first) c(v1,v2) else c(v2,v1)
  }
  
  for (rh in round_headers) {
    round_name <- str_replace_all(rvest::html_text(rh),"Round ","")
    data_row <- rvest::html_node(rh, xpath="following::tr[contains(@class,'b-fight-details__table-row')][1]")
    if (is.null(data_row)) next
    cells <- rvest::html_elements(data_row, "td.b-fight-details__table-col")
    if (length(cells)<9) next
    
    for (i in seq_along(col_map)) {
      cidx <- i+1
      vals <- get_two_val(cells[[cidx]])
      prefix <- paste0("Round",round_name,"_")
      data_out[[paste0(prefix,"fighter_",col_map[i])]] <- make_pair(vals[1],vals[2])[1]
      data_out[[paste0(prefix,"opponent_",col_map[i])]] <- make_pair(vals[1],vals[2])[2]
    }
  }
  
  data_out
}

parse_fight_details <- function(fight_url, main_fighter_name, opponent_name) {
  doc <- safe_get_html(fight_url)
  fight_data <- list()
  
  event_title <- rvest::html_node(doc, "h2.b-content__title")
  fight_data[["Event"]] <- if(!is.null(event_title)) trim_na(rvest::html_text2(event_title)) else NA
  
  fight_info <- rvest::html_node(doc, "div.b-fight-details__fight")
  if (!is.null(fight_info)) {
    text_blocks <- rvest::html_elements(fight_info, "p.b-fight-details__text")
    all_texts <- map_chr(text_blocks, rvest::html_text2)
    
    txt <- paste(all_texts, collapse=" ")
    txt <- str_squish(txt)
    
    method_main <- NA
    method_detail <- NA
    rd_val <- NA
    time_val <- NA
    timeformat <- NA
    referee <- NA
    details <- NA
    
    if (str_detect(txt, "Method:")) {
      part <- str_split_fixed(str_split_fixed(txt, "Method:",2)[,2], "Round:",2)[,1]
      method_main <- trim_na(part)
    }
    if (str_detect(txt,"Round:")) {
      part <- str_split_fixed(str_split_fixed(txt,"Round:",2)[,2],"Time:",2)[,1]
      rd_val <- trim_na(part)
    }
    if (str_detect(txt,"Time:")) {
      part <- str_split_fixed(str_split_fixed(txt,"Time:",2)[,2],"Time format:",2)[,1]
      part <- trim_na(part)
      part <- time_to_seconds(part)
      time_val <- part
    }
    if (str_detect(txt,"Time format:")) {
      part <- str_split_fixed(str_split_fixed(txt,"Time format:",2)[,2],"Referee:",2)[,1]
      timeformat <- trim_na(part)
    }
    if (str_detect(txt,"Referee:")) {
      part <- str_split_fixed(txt,"Referee:",2)[,2]
      if (str_detect(part,"Details:")) {
        part_ref <- str_split_fixed(part,"Details:",2)
        referee <- trim_na(part_ref[,1])
        details <- trim_na(part_ref[,2])
      } else {
        referee <- trim_na(part)
      }
    } else if (str_detect(txt,"Details:")) {
      part <- str_split_fixed(txt,"Details:",2)[,2]
      details <- trim_na(part)
    }
    
    fight_data[["method_main"]] <- method_main
    fight_data[["round"]] <- rd_val
    fight_data[["Time"]] <- time_val
    fight_data[["TimeFormat"]] <- timeformat
    fight_data[["Referee"]] <- referee
    fight_data[["Details"]] <- details
  }
  
  totals_data <- parse_totals_table(doc, main_fighter_name)
  if (length(totals_data)>0) {
    fight_data <- c(fight_data, totals_data)
  }
  
  per_round_totals_data <- parse_per_round_totals(doc, main_fighter_name)
  if (length(per_round_totals_data)>0) {
    fight_data <- c(fight_data, per_round_totals_data)
  }
  
  round_data <- parse_per_round_significant_strikes(doc, main_fighter_name)
  if (length(round_data)>0) {
    fight_data <- c(fight_data, round_data)
  }
  
  fight_data
}

transform_columns <- function(df) {
  df[df=="---"] <- NA
  
  # Identify of_cols
  of_cols <- names(df)[apply(df,2,function(x) any(str_detect(x," of ")))]
  
  for (col in of_cols) {
    valid_rows <- !is.na(df[[col]]) & str_detect(df[[col]], " of ")
    landed_col <- paste0(col,"_landed")
    attempted_col <- paste0(col,"_attempted")
    percentage_col <- paste0(col,"_percentage")
    df[[landed_col]] <- NA_real_
    df[[attempted_col]] <- NA_real_
    df[[percentage_col]] <- NA_real_
    
    if (any(valid_rows)) {
      split_vals <- str_split_fixed(df[[col]][valid_rows], " of ", 2)
      landed <- as.numeric(split_vals[,1])
      attempted <- as.numeric(split_vals[,2])
      perc <- (landed/attempted)*100
      
      df[[landed_col]][valid_rows] <- landed
      df[[attempted_col]][valid_rows] <- attempted
      df[[percentage_col]][valid_rows] <- round(perc,2)
    }
  }
  
  if (length(of_cols)>0) {
    df <- df[ , setdiff(names(df), of_cols)]
  }
  
  textual_cols <- c("fighter_name","opponent_name","event_name","event_date","method_main","method_detail","Details","Referee","Event","TimeFormat","result","fight_link","round")
  numeric_cols <- setdiff(names(df), textual_cols)
  
  for (col in numeric_cols) {
    v <- suppressWarnings(as.numeric(df[[col]]))
    if (sum(!is.na(v)) > 0) {
      df[[col]] <- v
    }
  }
  
  numeric_cols <- numeric_cols[numeric_cols %in% names(df)]
  for (col in numeric_cols) {
    if (is.numeric(df[[col]])) {
      df[[col]] <- round(df[[col]],2)
    }
  }
  
  df
}

#' Get UFC Fight Data for a Given Fighter
#'
#' @param fighter_name Character. Name of the fighter, e.g. "Israel Adesanya".
#'
#' @return A data frame of fight data for the specified fighter.
#'
#' @examples
#' \dontrun{
#'   # This example fetches data for Israel Adesanya.
#'   df <- get_ufc_data("Israel Adesanya")
#'   head(df)
#' }
#'
#' @export
get_ufc_data <- function(fighter_name) {
  fighter_url <- get_fighter_url_by_name(fighter_name)
  fl <- get_fight_links(fighter_url)
  main_fights_df <- fl$df
  
  all_fight_details <- map_dfr(fl$links, function(lk){
    row_data <- main_fights_df[main_fights_df$fight_link==lk, ][1, , drop=FALSE]
    dets <- parse_fight_details(lk, row_data$fighter_name, row_data$opponent_name)
    
    # remove duplicates
    dets <- dets[setdiff(names(dets), names(row_data))]
    
    combined <- as.data.frame(c(row_data, dets), stringsAsFactors=FALSE)
    combined
  })
  
  all_fight_details <- transform_columns(all_fight_details)
  all_fight_details
}
