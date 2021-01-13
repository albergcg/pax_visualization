# Visualizaci�n de datos: PEC2 -------------------------------------------------
# Anexo: C�digo desarrollado para llevar a cabo la exploraci�n de datos --------

# Librer�as --------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(lubridate)
library(FactoMineR)
library(factoextra)

# Lectura ----------------------------------------------------------------------
pax <- read_excel('../datasets/pax_all_agreements_data.xlsx')
pax %>% dim()

# Preprocesado de variables ---------------------------------------------------
pax <- pax %>%
  mutate(Dat = ymd(Dat)) %>%
  mutate(pax_year = year(Dat))

write_excel_csv2(pax, file = "../datasets/pax_preprocessed.csv")

# Taxonom�a de tratados --------------------------------------------------------

## Geograf�a
pax %>%
  select(Reg) %>%
  table()

ggplot(data = pax) +
  aes(x = Reg) +
  geom_bar(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90))

## Tipolog�a de conficto
pax %>%
  select(Contp) %>%
  table()

ggplot(data = pax) +
  aes(x = Contp) +
  geom_bar(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90))

## Distribuci�n conjunta
ggplot(data = pax) +
  aes(x = Reg, fill = Contp) +
  geom_bar(position = "fill")

## Estado del conflicto
pax %>%
  select(Status) %>%
  table()

ggplot(data = pax) +
  aes(x = Status) +
  geom_bar(fill = "steelblue")


# Evoluci�n temporal -----------------------------------------------------------
pax_by_year <- pax %>%
  group_by(pax_year) %>%
  summarise(agreements = n())  

ggplot(pax_by_year) +
  aes(x = pax_year, y = agreements) +
  geom_line(color = "steelblue") +
  geom_point(size = 2)

# An�lisis de correspondencias
correspondence_data <- pax %>%
  select(Contp, Reg, GeWom) %>%
  mutate_all(factor)

plot.MCA(ca, invisible=c("ind","quali.sup"), cex=0.7)
ca$row$coord

ca2 <- CA(table(pax$Reg, pax$GRef))
