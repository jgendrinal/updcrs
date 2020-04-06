#' @importFrom assertthat assert_that
#' @importFrom lubridate today year month
#' @importFrom stringr str_c
#' @importFrom purrr map
#' @importFrom dplyr `%>%` tibble transmute mutate as_tibble mutate_all 
#'   select filter
#' @importFrom tidyr unnest
#' @importFrom rvest html_node html_table
#' @importFrom xml2 read_html
#' @export
read_crs <- function(year, sem) {
  crs_raw <- tibble(url = get_crs_link(year, sem), 
         letter = LETTERS) %>% 
    transmute(url = str_c(url, letter)) %>% 
    mutate(page = map(url, function(x) {
      read_html(x) %>% 
        html_node(xpath = '//*[@id="tbl_schedule"]') %>% 
        html_table(fill = TRUE) %>% 
        mutate_all(as.character) %>% 
        as_tibble()
    })) %>% 
    unnest(page) %>% 
    select(-url) %>% 
    filter(`Class Code` != "No classes to display")
  
  assert_that(nrow(crs_raw) != 0, 
              msg = "No classes for this year and semester")
  
  crs_raw
}

get_crs_link <- function(year, sem) {
  
  assert_that(year %in% c(year(today()) - 1, year(today())), 
              msg = "Year provided not available in CRS website")
  
  assert_that(sem %in% c(1, 2, 4), 
              msg = "Please input only the following for sem:\n
                     1 - if First Semester,\n 
                     2 - if Second Semester,\n 
                     4 - if Midyear Semester")
  
  # TODO Prevent inputs that will not show links from CRS
  
  if (sem != 1) {
    
    year <- year - 1
    
  }
  
  return(str_c("https://crs.upd.edu.ph/schedule/1", year, sem, "/"))
  
}