# correspondence analysis script for data from `happyr` package
# knitr::opts_chunk$set(fig.width = 6, 
#                       fig.asp = 0.618,
#                       dpi = 300,
#                       echo = FALSE)
# knitr::knit_hooks$set(webgl = rgl::hook_webgl)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(rgl)
library(happyr)

# function to extract information from the output of CA in FactoMineR
get_ca_df_tidy <- function(ca.obj) {
  
  # get the row information
  x <- factoextra::get_ca_row(ca.obj)
  row_labs <- rownames(x$coord)
  coord <- x$coord
  colnames(coord) <- gsub("^Dim ", "dim_", colnames(coord), perl = TRUE)
  contrib <- x$contrib
  colnames(contrib) <- gsub("^Dim ", "contrib_", colnames(contrib), perl = TRUE)
  cos2 <- x$cos2
  colnames(cos2) <- gsub("^Dim ", "cos2_", colnames(cos2), perl = TRUE)
  inertia <- round(x$inertia/sum(x$intertia)*100, 2) # inertia in percentage
  df.x <- data.frame(labels = row_labs, coord, contrib, cos2, inertia, id = "row", stringsAsFactors = FALSE, row.names = NULL)
  #df.x$interp_along <- ifelse(df.x$cos2_1 > df.x$cos2_2, "dim_1", "dim_2")
  
  # get the column information
  x <- factoextra::get_ca_col(ca.obj)
  row_labs <- rownames(x$coord)
  coord <- x$coord
  colnames(coord) <- gsub("^Dim ", "dim_", colnames(coord), perl = TRUE)
  contrib <- x$contrib
  colnames(contrib) <- gsub("^Dim ", "contrib_", colnames(contrib), perl = TRUE)
  cos2 <- x$cos2
  colnames(cos2) <- gsub("^Dim ", "cos2_", colnames(cos2), perl = TRUE)
  inertia <- round(x$inertia/sum(x$intertia)*100, 2) # inertia in percentage
  df.y <- data.frame(labels = row_labs, coord, contrib, cos2, inertia, id = "col", stringsAsFactors = FALSE, row.names = NULL)
  #df.y$interp_along <- ifelse(df.y$cos2_1 > df.y$cos2_2, "dim_1", "dim_2")
  
  return(tibble::as_tibble(rbind(df.x, df.y)))
}

## run MDCA for synonyms-metaphor distinctive association
mdca_res <- happyr::mdca(df = phd_data_metaphor,
                         cxn_var = "synonyms",
                         coll_var = "metaphors",
                         correct_holm = TRUE,
                         concise_output = TRUE,
                         already_count_table = FALSE,
                         assocstr_digits = 3L)

## retrieve the distinctive metaphors with AssocStr higher than 3
distinctive_metaphors <- mdca_res %>% 
  filter(assocstr > 3) %>% 
  pull(metaphors) %>% 
  unique()

## get the data frame for Correspondence Analysis of the distinctive metaphors
ca_df <- phd_data_metaphor %>% 
  filter(metaphors %in% distinctive_metaphors) %>% 
  select(metaphors, synonyms)

## create crosstabulation for input of CA
ca_table <- table(ca_df$metaphors, ca_df$synonyms)

## get rows whose cells contain at least 10 tokens
ca_table <- ca_table[apply(ca_table, 
                           1, function(x) any(x >= 10)), , drop = FALSE]

## shorten the metaphor labels
rownames(ca_table) <- if_else(str_detect(rownames(ca_table), 
                                         "^(intensity|effect|expression) of"), 
                              str_replace(rownames(ca_table), 
                                          "^.+?\\sis\\s(an?\\s)?", ""),
                              str_replace(rownames(ca_table), 
                                          "^.+?\\sis\\s(an?\\s)?", ""))

## compute CA with FactoMineR----
ca_obj_fmr <- CA(ca_table, graph = FALSE)

## generate a tidy-output CA table----
ca_res_tidy <- get_ca_df_tidy(ca_obj_fmr) 

## get CA eigenvalues----
eigenvalues <- get_eigenvalue(ca_obj_fmr)

ca_res_tidy <- ca_res_tidy  %>%
  mutate(contrib_sum_dim1.2 = (contrib_1 * eigenvalues[1,1]) + (contrib_2 * eigenvalues[2,1]),
         contrib_sum_dim1.3 = (contrib_1 * eigenvalues[1,1]) + (contrib_3 * eigenvalues[3,1]),
         contrib_sum_dim2.3 = (contrib_2 * eigenvalues[2,1]) + (contrib_3 * eigenvalues[3,1]),
         
         # the sum (in %) for the quality of the display in X and Y dimension
         cos2_sum_dim1.2 = round((cos2_1 + cos2_2)*100, 2),
         cos2_sum_dim1.3 = round((cos2_1 + cos2_3)*100, 2),
         cos2_sum_dim2.3 = round((cos2_2 + cos2_3)*100, 2))

## CA row and column data----
### row
ca_res_row <- ca_res_tidy %>%
  filter(id == "row")
### column
ca_res_col <- ca_res_tidy %>%
  filter(id == "col")

## Eigenvalue visualisation----
mean_eig_row <- round(1/(nrow(ca_table)-1)*100, 2)
mean_eig_col <- round(1/(ncol(ca_table)-1)*100, 2)
fviz_screeplot(ca_obj_fmr, barfill = "#0072B2", barcolor = NA) +
  geom_hline(yintercept = max(mean_eig_col, mean_eig_row),
             linetype = 2,
             colour = "red") +
  labs(caption = paste("\nThe first two dimensions represent ", 
                       round(eigenvalues[2,3], 2), 
                       "% of the explained variation.", sep = "")) +
  theme_light()

## CA visualisation in 3D----
ca_dim_1 <- ca_res_tidy$dim_1
ca_dim_2 <- ca_res_tidy$dim_2
ca_dim_3 <- ca_res_tidy$dim_3
plot3d(ca_dim_1, 
       ca_dim_2, 
       ca_dim_3, type = "n", size = 1, lit = TRUE)
texts3d(ca_res_tidy$dim_1, 
        ca_res_tidy$dim_2, 
        ca_res_tidy$dim_3, 
        texts = ca_res_tidy$labels, 
        color = if_else(ca_res_tidy$id=="row", 
                        "navyblue", 
                        "firebrick"))
grid3d(side = c("x", "y", "z"), at = c(0, 0, 0))
rglwidget(width = 800, height = 800)

## CA plot for dimension 1 and 2----

### glossing label data frame 
labels_df <- ca_res_tidy %>%
  summarise(dim_1 = min(dim_1),
            dim_2 = max(dim_2),
            labels = paste("GLOSS (root --- nominalised):\nbahagia '(peaceful and) happy' --- kebahagiaan 'happiness'\nceria 'cheerful' --- keceriaan 'cheerfulness'\ngembira 'excited; enthusiastic' --- kegembiraan 'joy; cheerfulness'\nriang 'very happy; joyous' --- keriangan 'cheer(fulness)'\nsenang 'happy; to feel well' --- kesenangan 'pleasure'\n"))

### plotting
dim_1_2 <- ggplot(ca_res_tidy, aes(x = dim_1, y = dim_2)) +
  geom_hline(yintercept = 0, colour = "gray75") +
  geom_vline(xintercept = 0, colour = "gray75") +
  theme_light() +
  
  # plot for row elements
  # geom_point(data = ca_res_row, aes(size = cos2_sum_dim1.2), colour = "blue", show.legend = FALSE) +
  # ggrepel::geom_text_repel(data = ca_res_row, aes(label = labels, size = contrib_sum_dim1.2), colour = "blue", show.legend = FALSE) +
  # 
  # plot for column elements
  # geom_point(data = ca_res_col, aes(size = cos2_sum_dim1.2), colour = "red", show.legend = FALSE) +
  # ggrepel::geom_text_repel(data = ca_res_col, aes(label = labels, size = contrib_sum_dim1.2), colour = "red", show.legend = FALSE) +
  
  geom_text(aes(label = labels), data = labels_df, vjust = "top", hjust = "left", size = 2.65) +
  geom_point(aes(shape = id, colour = id)) +
  geom_text_repel(aes(label = labels, colour = id), show.legend = F, size = 3) +
  # geom_text(aes(label = labels, colour = id), show.legend = F) +
  scale_color_manual(values = c("red", "blue"),
                     breaks = c("col", "row"),
                     label = c("synonyms", "metaphors")) +
  scale_shape_manual(values = c(15, 17),
                     breaks = c("col", "row"),
                     label = c("synonyms", "metaphors")) +
  labs(x = paste("Dim_1 (", round(eigenvalues[1,2], 2), "%)", sep = ""),
       y = paste("Dim_2 (", round(eigenvalues[2,2], 2), "%)", sep = ""),
       caption = paste("\nThe first two dimensions represent ", round(eigenvalues[2,3], 2), "% of the variation.", sep = "")) +
  theme(legend.position = "none")
dim_1_2
ggsave("monash_lal_seminar_CA_plot_dim_1_&_2.png", width = 6.5, height = 6, units = "in", dpi = 300)

## CA plot for dimension 1 and 3----

### glossing label data frame
labels_df2 <- ca_res_tidy %>%
  summarise(dim_1 = max(dim_1),
            dim_3 = max(dim_3),
            labels = paste("GLOSS (root --- nominalised):\nbahagia '(peaceful and) happy' --- kebahagiaan 'happiness'\nceria 'cheerful; lit. pure, clean' --- keceriaan 'purity; cheerfulness'\ngembira 'excited; enthusiastic' --- kegembiraan 'joy; cheerfulness'\nriang 'very happy; joyous' --- keriangan 'cheer(fulness)'\nsenang 'happy; to feel well' --- kesenangan 'pleasure'\n"))

dim_1_3 <- ggplot(ca_res_tidy, aes(x = dim_1, y = dim_3)) +
  geom_hline(yintercept = 0, colour = "gray75") +
  geom_vline(xintercept = 0, colour = "gray75") +
  theme_light() +
  
  # plot for row elements
  #geom_point(data = ca_res_row, aes(size = contrib_sum_dim1.3, colour = cos2_sum_dim1.3)) +
  #ggrepel::geom_text_repel(data = ca_res_row, aes(label = labels, size = cos2_sum_dim1.3), colour = "blue") +
  # plot for column elements
  #geom_point(data = ca_res_col, aes(size = contrib_sum_dim1.3, colour = cos2_sum_dim1.3)) +
  #ggrepel::geom_text_repel(data = ca_res_col, aes(label = labels, size = cos2_sum_dim1.3), colour = "red") +
  geom_text(aes(label = labels, x = 1.0, y = -0.7), data = labels_df2, vjust = "top", hjust = "right", size = 2.65) +
  geom_point(aes(shape = id, colour = id)) +
  geom_text_repel(aes(label = labels, colour = id), show.legend = FALSE, size = 3) +
  #geom_text(data = filter(ca_res_tidy, !labels %in% c("contained entity", "(un)veiled object")), aes(label = labels, colour = id), show.legend = FALSE, size = 3, nudge_y = -0.025) +
  #geom_text_repel(data = filter(ca_res_tidy, labels %in% c("contained entity", "(un)veiled object")), aes(label = labels, colour = id), show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("red", "blue"),
                     breaks = c("col", "row"),
                     label = c("synonyms", "metaphors")) +
  scale_shape_manual(values = c(15, 17),
                     breaks = c("col", "row"),
                     label = c("synonyms", "metaphors")) +
  labs(x = paste("Dim_1 (", round(eigenvalues[1,2], 2), "%)", sep = ""),
       y = paste("Dim_3 (", round(eigenvalues[3,2], 2), "%)", sep = ""),
       caption = paste("\nThe first and third dimensions represent ", (round(eigenvalues[1,2], 2) + round(eigenvalues[3,2], 2)), "% of the variation.", sep = "")) +
  theme(legend.position = "none")
dim_1_3
ggsave("monash_lal_seminar_CA_plot_dim_1_&_3.png", width = 6.5, height = 6, units = "in", dpi = 300)
