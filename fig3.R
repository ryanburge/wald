
graph <- cc %>% 
  group_by(group, year) %>% 
  mutate(vote = frcode(vote == 1 ~ "Trump",
                       vote == 2 ~ "Clinton",
                       vote == 3 | vote == 4 | vote == 5 | vote == 8 ~ "Someone Else")) %>% 
  ct(vote, wt = weight, show_na = FALSE)

one <- graph %>% 
  filter(group == "Episcopalian") %>% 
  ggplot(., aes(x = 1, y = pct, fill = fct_rev(vote))) +
  geom_col(color = "black") + 
  coord_flip() +
  facet_wrap(~ year, ncol =1, strip.position = "left") +
  scale_fill_manual(values = c("forestgreen","dodgerblue3", "firebrick3")) +
  theme_gg("Jost") +
  # theme(legend.position = "bottom") +
  scale_y_continuous(labels = percent) +
  theme(strip.text.y.left = element_text(angle=0)) +
  guides(fill = guide_legend(reverse=T, nrow = 1)) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank()) +
  geom_text(aes(label = ifelse(pct >.05, paste0(pct*100, '%'), '')), position = position_stack(vjust = 0.5), size = 4, family = "font", color = "black") +
  labs(x = "", y = "", title = "", subtitle = "Episcopalians", caption = "") +
  ggsave("D://wald/images/epis_vote.png", width = 5, height = 3)


two <- graph %>% 
  filter(group == "Jewish") %>% 
  ggplot(., aes(x = 1, y = pct, fill = fct_rev(vote))) +
  geom_col(color = "black") + 
  coord_flip() +
  facet_wrap(~ year, ncol =1, strip.position = "left") +
  scale_fill_manual(values = c("forestgreen","dodgerblue3", "firebrick3")) +
  theme_gg("Jost") +
  # theme(legend.position = "bottom") +
  scale_y_continuous(labels = percent) +
  theme(strip.text.y.left = element_text(angle=0)) +
  guides(fill = guide_legend(reverse=T, nrow = 1)) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank()) +
  geom_text(aes(label = ifelse(pct >.05, paste0(pct*100, '%'), '')), position = position_stack(vjust = 0.5), size = 4, family = "font", color = "black") +
  labs(x = "", y = "", title = "", subtitle = "Jewish", caption = "") +
  ggsave("D://wald/images/jewish_vote.png", width = 5, height = 3)


three <- graph %>% 
  filter(group == "Puerto Rican") %>% 
  ggplot(., aes(x = 1, y = pct, fill = fct_rev(vote))) +
  geom_col(color = "black") + 
  coord_flip() +
  facet_wrap(~ year, ncol =1, strip.position = "left") +
  scale_fill_manual(values = c("forestgreen","dodgerblue3", "firebrick3")) +
  theme_gg("Jost") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = percent) +
  theme(strip.text.y.left = element_text(angle=0)) +
  guides(fill = guide_legend(reverse=T, nrow = 1)) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank()) +
  geom_text(aes(label = ifelse(pct >.05, paste0(pct*100, '%'), '')), position = position_stack(vjust = 0.5), size = 4, family = "font", color = "black") +
  labs(x = "", y = "", title = "", subtitle = "Puerto Rican", caption = "") +
  ggsave("D://wald/images/pr_vote.png", width = 5, height = 3)

library(patchwork)

all <- one / two / three

ggsave("D://wald/images/fig3.png", type = "cairo-png", height = 7, width = 7,  all)