
graph <- cc %>% 
  ct(group, wt = weight)

graph %>% 
  mutate(n = round(n, 0)) %>% 
  filter(group != "NA") %>% 
  ggplot(., aes(x = group, y = n, fill = group)) +
  geom_col(color = "black") +
  fill4_3() +
  theme_gg("Jost") +
  coord_flip() +
  geom_text(aes(y = n + 125, label = n), position = position_dodge(width = .9), size = 5.5, family = "font") +
  labs(x = "", y = "", title = "") +
  ggsave("D://wald/images/fig1.png", type = "cairo-png", width = 7, height = 3)

