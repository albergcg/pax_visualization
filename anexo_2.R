# Visualización de datos: PEC4 -------------------------------------------------
# Anexo: Código desarrollado para el AFC y Clustering de acuerdos --------------

# Librerías --------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(cluster)
library(colormap)
library(FactoMineR)
library(ggrepel)
library(dendextend)

# Lectura ----------------------------------------------------------------------
pax <- read_excel('datasets/pax_subset.xlsx')
pax %>% dim()

# Clustering ordinal -----------------------------------------------------------
study_vars <- pax %>%
  select(GRef, GRa, GCh) %>%
  mutate_all(factor)


agreements_distance <- daisy(study_vars, "gower")
clustering <- hclust(agreements_distance, method = "ward")
dendogram <- as.dendrogram(clustering)
leafcolor <- colormap(colormap = colormaps$viridis,
                      nshades = 3,
                      format = "hex",
                      alpha = 1,
                      reverse = FALSE)

dendogram %>%
  set("labels_col", "white") %>%
  set("branches_k_color", value = leafcolor, k = 3) %>%
  plot(axes = FALSE)

groups <- cutree(clustering, k = 3)
pax$cluster <- groups
write_xlsx(pax, "../../datasets/pax_groups.xlsx")

# Análisis Factorial de Correspondencias----------------------------------------
study_vars <- pax %>%
  select(GRef, GRa, GCh) %>%
  mutate_all(factor) %>%
  transmute(GRef = case_when(GRef == "0" ~ "NoReference",
                             GRef == "1" ~ "Rhetorical",
                             GRef == "2" ~ "Antidiscriminative",
                             GRef == "3" ~ "Substantial"),
            GRa = case_when(GRa == "0" ~ "NoReference",
                            GRa == "1" ~ "Rhetorical",
                            GRa == "2" ~ "Antidiscriminative",
                            GRa == "3" ~ "Substantial"),
            GCh = case_when(GCh == "0" ~ "NoReference",
                            GCh == "1" ~ "Rhetorical",
                            GCh == "2" ~ "Antidiscriminative",
                            GCh == "3" ~ "Substantial"))

fca <- MCA(study_vars, graph = FALSE)
coordinates <- fca$var$coord[, 0:2]
coordinates <- data.frame(names = row.names(coordinates), coordinates)
coordinates$variable <- as.factor(rep(1:3, each = 4))

ggplot(data = coordinates, aes(x=Dim.1, y=Dim.2, color = variable)) +
  geom_point(size = 2) +
  theme_minimal() +
  geom_text_repel(label=rownames(coordinates)) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("seagreen4", "orchid", "darkblue")) +
  xlab("First dimension") +
  ylab("Second dimension")
