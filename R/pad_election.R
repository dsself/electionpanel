#'Function to pad electoral data

#' @param data A data frame with electoral data
#' @param time A vector of time data delimited by year (enclosed in "")
#' @param group A vector of country identifiers used to group data (enclosed in "")
#' @param start Numeric value for first year of time-series
#' @param end Numeric value for last year of time-series

`%ni%` <- Negate(`%in%`)


pad_election <- function(data, time, group, start, end) {
  df <- dplyr::mutate_(data, election = time) %>%
    dplyr::filter(election > 1899) %>%
    dplyr::group_by_(group) %>%
    dplyr::mutate(lead = lead(election), lead = ifelse(is.na(lead), election + 1, lead))

  years = start:end
  temp <- df[,1:ncol(df)]
  d2 <- merge(years, temp, all = TRUE)
  d2 <- d2[which(d2$x < d2$lead & d2$x >= d2$election),]
  `%ni%` <- function(x,y) {!(x %in% y)}
  d2 <- d2[names(d2) %ni% c("year")]
  d2$year <- d2$x
  d2 <- d2[names(d2) %ni% c("lead", "x")]

  d2$election <- as.numeric(d2$election)
  return(d2)
}


