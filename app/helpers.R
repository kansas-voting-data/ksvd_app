map_link <- function(locale, district) {
  if (locale == "county") {
    url <- "https://en.wikipedia.org/wiki/List_of_counties_in_Kansas"
  } else if (locale == "congressional") {
    url <- "https://en.wikipedia.org/wiki/Kansas%27s_congressional_districts"
  } else if (locale == "sldu") {
    char <- nchar(district)
    url <- paste0(
      "http://kansas-voting-data.s3.amazonaws.com/district_maps/district_map_",
      "s_",
      paste(c(rep(0, 3 - char), district), collapse = ""),
      ".pdf"
    )
  } else if (locale == "sldl") {
    char <- nchar(district)
    url <- paste0(
      "http://kansas-voting-data.s3.amazonaws.com/district_maps/district_map_",
      "h_",
      paste(c(rep(0, 3 - char), district), collapse = ""),
      ".pdf"
    )
  }
  return(
    shiny::a(
      paste(
        "Where is this",
        ifelse(locale == "county", "county", "district"),
        "located?"
      ),
      href = url, 
      target = "_blank"
    )
  )
}
