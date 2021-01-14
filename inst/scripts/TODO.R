

rowData(se) %>%
  as_tibble() %>%
  select(starts_with(
    paste0( "Sequence.coverage.", c("A","B","C"))),name) %>% 
      pivot_longer(-name,names_to="condition",values_to="values") %>% 
  mutate(condition=str_remove(condition,"Sequence.coverage")) %>% 
  ggplot(aes(y=condition,x=values/100)) + geom_density_ridges()+
  scale_x_continuous(labels=scales::percent)+
  labs(y="", x="")
