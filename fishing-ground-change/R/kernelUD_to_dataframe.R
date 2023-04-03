kernelUD_to_dataframe = function(x) {
  x[lower.tri(x)] <- NA
  out <- as.data.frame(x) %>%
    rownames_to_column("group1") %>% pivot_longer(cols=2:(dim(x)[2]+1), names_to="group2",values_to="BAindex") %>%
    filter(group1 != group2) %>% filter(!is.na(BAindex))
  return(out)
}