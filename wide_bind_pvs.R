wide_cat <- function(df, vars, ids, cat) {
  if(identical(cat, "total")) {
    cate <- NULL
  } else {
    cate <- cat
  }
  
  df %>% drop_na(ids, cate) %>%
    group_by_at(c(ids, cate)) %>%
    summarise(across(vars, ~weighted.mean(., SENWGT, na.rm = TRUE)), n = n()) %>%
    mutate(category =   paste(str_to_title(cat), collapse = ' & ')) %>%
    rowwise() %>%
    mutate(value = mean(c_across(vars))) %>%
    dplyr::select(c(ids, category, cate, value, n))
  
}

wide_bind <- function(df, dvs, ids, groups) {
  groups <- sort(groups)
  myinter <- c("total", purrr::lmap(1:length(groups), function(n) combn(groups, n, simplify = FALSE)))
  purrr::map_dfr(myinter, function(c) wide_cat(df, dvs, ids, c))
}