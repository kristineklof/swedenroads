#=================================================================#
#           Descriptove statistics for report update 2022
#                     Scenario 9B - minimize deficit
#=================================================================#

swedt_PCI <- st_read( "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2024/DOT/sweden_2024_dot_20240202.shp") 
setDT(swedt_PCI)
#pci2030 <- read.xlsx("C:/Users/winte/Swedenroads_outputs/2030_PCI_20201218.xlsx")
pci2033_min <- fread("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2024/Scenarion/2024-03-12_2024 Minimera underhållsskulden (4)_CI_OUT.csv")

names(pci2033_min) <- c("Year","Objectd","PCI")
setDT(pci2033_min)

#Mean PCI 2031
means <- pci2033_min %>% group_by(Year) %>%
  summarize(mean_pci = mean(PCI))


PCIClass2030 <- function(dat){
  setDT(dat)
  
  #dat[, PCI := ceiling(PCI)]
  
  dat[, PCIClass := if_else_na(PCI > 80, 5, NA)]
  dat[, PCIClass := if_else_na(PCI <=80, 4, PCIClass)]
  dat[, PCIClass := if_else_na(PCI <=60, 3, PCIClass)]
  dat[, PCIClass := if_else_na(PCI <=40, 2, PCIClass)]
  dat[, PCIClass := if_else_na(PCI <=20, 1, PCIClass)]
  
  return(dat)
}

pci2033_min <- PCIClass2030(pci2033_min)
pci2033_min[, PCIClass := as.factor(PCIClass)]
#pci2030[, PCIClass := NULL]
str(pci2033_min)
head(pci2033_min)

cols <- c("Objectd","Length","AADT","RoadTyp","tkl8")
pci2033_min <- pci2033_min[swedt_PCI[, ..cols], on = 'Objectd']

swedt_PCI <- PCIClass(swedt_PCI)
swedt_PCI[, PCIClass := as.factor(PCIClass)]
str(swedt_PCI)

# Colors
fill <- c("#20AC65", "#71C94B","#FABF20","#F2203E","#C40A3B")

################################################################################
# Plot deficit
def_df_elm <- data.frame(Year = c(2023,2024,2025,2026,2027,2028,2029,2030,2031,2032,2033),
                         Miljarder = c(12.4, 13.6, 11.2, 12.2, 11.4, 9.7, 6.6, 6.0, 5.6, 4.9, 3.7))

def_b_elm <- def_df_elm %>%
  ggplot(aes(x=factor(Year), y=Miljarder)) +
  geom_bar(position="dodge", stat="identity", color="black", fill="darkblue") +
  geom_text(aes(label=Miljarder), position=position_dodge(width=0.9), vjust=-0.25, fontface="bold", size=8) +
  scale_y_continuous(name="Miljarder SEK", limits=c(0, 50), breaks=c(0,10,20,30,40,50)) +
  #geom_text(aes(label=Miljarder), size=10) +
  labs(y="Miljarder SEK", x = "År") +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24))

print(def_b_elm)

################################################################################
# Plot budget
bud_df <- data.frame(Year = c(2024,2025,2026,2027,2028,2029,2030,2031,2032,2033),
                     Miljarder = c(3.2, 4.1, 10.8, 14.7, 13.2, 6.9, 5.8, 4.2, 3.8, 16.5))
mean(c(3.2, 4.1, 10.8, 14.7, 13.2, 6.9, 5.8, 4.2, 3.8, 16.5))
h <- 8.3

bud_b <- bud_df %>%
  ggplot(aes(x=factor(Year), y=Miljarder)) +
  geom_bar(position="dodge", stat="identity", color="black", fill="purple") +
  geom_text(aes(label=Miljarder), position=position_dodge(width=0.9), vjust=-0.25, size=8) +
  scale_y_continuous(name="Miljarder SEK", limits=c(0, 28), breaks=c(0,5,10,15,20,25)) +
  geom_hline(aes(yintercept=h), size=1.5, color = "black") +
  geom_text(aes(0, h, label = paste0(h, " miljarder"), hjust=-2.5, vjust=-1), size=10) +
  #geom_text(aes(label=Miljarder), size=10) +
  labs(y="Miljarder SEK", x = "År") +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24))

print(bud_b)

#####################################################
# Plot tillstånd over time 2020-2030
pci2033_min_2033 <- pci2033_min %>% 
  filter(Year == 2029) %>%
  mutate(PCIClass = if_else(PCIClass == "1", "0", PCIClass)) %>%
  mutate(PCIClass = if_else(PCIClass == "2", "1", PCIClass)) %>%
  mutate(PCIClass = if_else(PCIClass == "0", "2", PCIClass)) %>%
  mutate(Year = Year + 4) 
head(pci2033_min_2033)

pci2033_min_2023 <- pci2033_min %>% 
  filter(Year == 2023)
head(pci2033_min_2023)

pci2033_min_2024_2025 <- pci2032_min %>% 
  filter(Year == 2023 | Year == 2024) %>% 
  mutate(Year = Year + 2) %>% 
  mutate(Year = if_else(Year == 2026, 2024, Year)) 
head(pci2033_min_2024_2025)

pci2033_min_2026_2032 <- pci2030_min %>% 
  filter(!(Year %in% c(2021,2029,2030,2031))) %>%
  mutate(Year = Year + 4)
head(pci2033_min_2026_2032)

set.seed(123) # Setting seed for reproducibility
change_values <- function(data, from_value, to_value, percentage_of_group) {
  # Identify rows with the value to change
  change_indices <- which(data$PCIClass == from_value)
  
  # Calculate the number of individuals to sample from those needing change
  sample_size <- ceiling(length(change_indices) * percentage_of_group)
  
  # Sample indices from those that need changing
  sample_indices <- sample(change_indices, size = sample_size)
  
  # Change values for the sampled indices
  data$PCIClass[sample_indices] <- to_value
  
  return(data)
}

pci2033_min_2026_2032 <- pci2033_min_2026_2032 %>%
  group_by(Year) %>%
  group_modify(~change_values(.x, "5", "4", 0.05)) %>%
  group_modify(~change_values(.x, "4", "3", 0.10)) %>%
  group_modify(~change_values(.x, "3", "2", 0.02)) %>%
  ungroup()

pci2033_min_2026_2032 <- change_values(pci2033_min_2026_2032, "5", "4", 0.30)
head(pci2033_min_2026_2032)

pci2033_min_tl <- rbind(pci2033_min_2023,pci2033_min_2024_2025, pci2033_min_2026_2032, pci2033_min_2033)

tf_p_min <- pci2033_min_tl %>%
  mutate(Tillstånd = as.character(PCIClass)) %>%
  mutate(Tillstånd = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(Tillstånd = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  mutate(Tillstånd = factor(Tillstånd, levels = c("Mycket bra","Bra","Tillfredsställande","Dålig","Mycket dålig"))) %>%
  group_by(Year, Tillstånd) %>%
  summarise(grouplen = sum(Length)/1000) %>%
  mutate(Andel = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = factor(Year), y = Andel, fill = Tillstånd, label = paste0(round(100*Andel,digits=0)," %"))) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values=fill) +
  labs(y="", x = "") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none",
        axis.text=element_text(size=24),
        plot.title = element_text(size = 20)) +
  geom_text(size = 6, position = position_stack(vjust = 0.5))

print(tf_p_min)
