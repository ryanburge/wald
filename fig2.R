graph <- cc %>% 
  group_by(group) %>% 
  mutate(inc = frcode(income <= 5 ~ "<$50k",
                      income >= 6 & income <= 9 ~ "$50k-$100k",
                      income >= 10 & income <= 16 ~ ">$100k")) %>% 
  ct(inc, wt = weight, show_na = FALSE) %>% 
  na.omit()


graph %>% 
  ggplot(., aes(x = inc, y = pct, fill = group)) +
  geom_col(color = "black") +
  facet_wrap(~ group, ncol = 3) +
  fill4_3() +
  theme_gg("Jost") +
  y_pct() + 
  lab_bar(type = pct, pos = .02, sz = 5.5) +
  labs(x = "", y = "", title = "", caption = "") +
  ggsave("D://wald/images/fig2.png", dpi = 300, type = "cairo-png", width = 8)
