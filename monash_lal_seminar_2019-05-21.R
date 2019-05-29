library(tidyverse)
library(happyr)

submapping_desired_goal <- phd_data_metaphor %>% 
  filter(str_detect(metaphors, 'desired'), 
         synonyms %in% c("kebahagiaan", "kesenangan")) %>% 
  count(synonyms, submappings) %>% 
  mutate(aspect = "attainment",
         aspect = if_else(str_detect(submappings, "pursuing|is motion to"),
                          "pursuing", aspect),
         aspect = if_else(str_detect(submappings, "of a path"),
                          "goal of a path", aspect),
         aspect = if_else(str_detect(submappings, "access"),
                          "aids to a goal", aspect),
         aspect = if_else(str_detect(submappings, "guided"),
                          "aids to a goal", aspect),
         aspect = if_else(str_detect(submappings, "impeding"),
                          "impediment to goal", aspect))

submapping_desired_goal1 <- submapping_desired_goal %>% 
  mutate(aspect = replace(aspect, 
                          aspect %in% c("goal of a path", 
                                        "aids to a goal", 
                                        "impediment to goal"),
                          "others")) %>% 
  group_by(synonyms, aspect) %>% 
  summarise(n = sum(n)) %>% 
  mutate(perc = n/sum(n) * 100) %>% 
  ungroup() %>% 
  arrange(synonyms, desc(n))

# plot for submapping comparison between *kebahagiaan* 'happiness' and *kesenangan* 'pleasure' for the DESIRED GOAL metaphor attracted to these words
# under the filename "monash_lal_seminar-desiredgoal-plot.png"
submapping_desired_goal1 %>% 
  ggplot(aes(x = reorder(aspect, n), 
             y = n, 
             fill = synonyms)) + 
  geom_col(position = 'dodge') + 
  geom_text(aes(label = n), 
            position = position_dodge(0.9),
            hjust = 2) +
  coord_flip() +
  labs(x = NULL,
       y = "Token Frequency") +
  theme_light() +
  ggsave('monash_lal_seminar-desiredgoal-plot.png', width = 6, height = 5, dpi = 300, units = "in")

# check the categorised semantic aspects of the desired goal metaphor
# that is attracted to both *kebahagiaan* 'happiness' & *kesenangan* 'pleasure'
submapping_desired_goal1 %>% arrange(aspect)

# fisher.test for freq. of attainment aspect across *kebahagiaan* & *kesenangan*
## create matrix and feed via pipe into fisher.test()
fye1 <- matrix(c(73, 30, 52, 88), ncol = 2, byrow = TRUE,
               dimnames = list(sem = c("attainment",
                                       "others"),
                               syn = c("kebahagiaan",
                                       "kesenangan"))) %>% 
  fisher.test()
## get the p-value
pval1 <- fye1$p.value

# fisher.test for freq. of pursuing aspect across *kebahagiaan* & *kesenangan*
## create matrix and feed via pipe into fisher.test()
fye2 <- matrix(c(35, 71, 90, 39), ncol = 2, byrow = TRUE,
               dimnames = list(sem = c("pursuing",
                                       "others"),
                               syn = c("kebahagiaan",
                                       "kesenangan"))) %>% 
  fisher.test()
## get the p-value
pval2 <- fye2$p.value

# fisher.test for freq. of others aspect across *kebahagiaan* & *kesenangan*
## create matrix and feed via pipe into fisher.test()
fye3 <- matrix(c(17, 9, 108, 101), ncol = 2, byrow = TRUE,
       dimnames = list(sem = c("others",
                               "otherss"),
                       syn = c("kebahagiaan",
                               "kesenangan"))) %>% 
  fisher.test()
## get the p-value
pval3 <- fye3$p.value

# p.adjust (i.e. correction of the p-value of fisher tests) with Holm's method
pvals <- c(pval1, pval2, pval3)
pholms <- p.adjust(pvals, method = "holm")
names(pholms) <- c("attainment", "pursuing", "others")
## get which aspect(s) still shows significant difference (at the corrected level) in its distribution with *kebahagiaan* and *kesenangan*
pholms[pholms < 0.05]
# attainment     pursuing 
# 3.771774e-07 7.347900e-08


# Plot for submappings of LIQUID IN A CONTAINER
# under the filename "monash_lal_seminar-containedliquid-plot.png"
liquidsubmet <- get_submappings('liquid in a container', 
                df = phd_data_metaphor, 
                word = 'kegembiraan') %>% 
  mutate(submappings = str_replace(submappings,
                                   "^.*\\sis\\s",
                                   "")) 
liquidsubmet %>% 
  ggplot(aes(x = reorder(submappings, -n), y = n)) + 
  geom_col() + 
  labs(x = NULL, 
       y = "Token frequency") + 
  theme_light() + 
  geom_text(aes(label = n), 
            vjust = if_else(liquidsubmet$n < 5, -0.5, 1.5),
            colour = if_else(liquidsubmet$n < 5, "black", "white")) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  ggsave("monash_lal_seminar-containedliquid-plot.png", width = 6, height = 5, dpi = 300, units = "in")

# Plot for submappings in the CONTAINED ENTITY metaphor
# under the filename "monash_lal_seminar-containedentity-plot.png"
containing <- get_submappings('contained entity', 
                              df = phd_data_metaphor, 
                              word = "keceriaan|keriangan")
containing <- mutate(containing,
                     submappings = str_replace(submappings, "^.*\\sis\\s",
                                               ""))
containing %>% 
  ggplot(aes(x = reorder(submappings, n),
             y = n,
             fill = synonyms)) +
  geom_col(position = "dodge") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  geom_text(aes(label = n), 
            position = position_dodge(0.9),
            vjust = if_else(containing$n > 10, -1, 2)) +
  labs(x = NULL,
       y = "Token frequency") +
  ggsave("monash_lal_seminar-containedentity-plot.png", width = 6, height = 5, dpi = 300, units = "in")
