
cc <- cc %>% 
  mutate(trump = case_when(vote == 1 ~ 1, 
                           vote == 2 ~ 0)) %>% 
  mutate(male = case_when(gender == 1 ~ 1, 
                          gender == 2 ~ 0)) %>% 
  mutate(att = case_when(pew_churatd == 6 ~ 1, 
                         pew_churatd == 5 ~ 2, 
                         pew_churatd == 4 ~ 3,
                         pew_churatd == 3 ~ 4, 
                         pew_churatd == 2 ~ 5, 
                         pew_churatd == 1 ~ 6)) %>% 
  mutate(income = car::recode(income, "20:99 = NA")) %>% 
  mutate(grp = frcode(evangelical == 1 ~ "Evangelical",
                      race == 2 ~ "African-American")) %>% 
  mutate(white = case_when(race == 1 ~ 1, 
                           TRUE ~ 0))

reg1 <- glm(trump ~ income*grp + age + educ + male + att + income, family = "binomial", data = cc)

gg <- interact_plot(reg1, pred= income, modx = grp, int.width = .76, interval = TRUE) 


gg + 
  fill4_3() + 
  color4_3() + 
  y_pct() + 
  theme_gg("Jost") +
  add_text(x = 5, y = .70, word = "Evangelical", sz = 4) +
  add_text(x = 5, y = .10, word = "African-American", sz = 4) +
  # theme(legend.position = c(.2, .75)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), labels = c("<$10k", "$10k", "$20k", "$30k", "$40k", 
                                                                                    "$50k", "$60k", "$70k", "$80k", "$100k", 
                                                                                    "$120k", "$150k", "$200k", "$250k", "$350k", ">$500k")) +
  labs(x = "", y = "Pr(Trump Vote)", title = "") +
  ggsave("D://wald/images/fig5.png", type = "cairo-png", width = 8)