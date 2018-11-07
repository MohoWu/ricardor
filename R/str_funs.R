#' Trim white spaces
#' 
#' Returns string w/o leading or trailing whitespace.
#' 
#' @export
#' 
str_trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#' Replace white space(s)with underscore
#'
#' Returns string separated by underscore.
#'   
#' @export
#' 
str_underscore <- function(x) gsub('([[:punct:]])|\\s+', '_', x)

#' Convert string to HTML link
#' 
#' @param web.link The embedded web link.
#' @param display The text for the hyperlink.
#' @param new.tab Should the link be opened in a new tab in the browser?
#'
#' @export
str_to_link <- function(web.link, display, new.tab = TRUE) {
  
  if (new.tab) 
    paste0("<a href='", web.link, "' target = '_blank'>", display, "</a>") else
      paste0("<a href='", web.link, "'>", display, "</a>")
  
}