#=================================================================#
#           Descriptove statistics for report update 2022
#                     Scenario 9B - minimize deficit
#=================================================================#

swedt_PCI <- st_read( "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2023/DOT/sweden_2023_dot_20230126.shp") 
setDT(swedt_PCI)
#pci2030 <- read.xlsx("C:/Users/winte/Swedenroads_outputs/2030_PCI_20201218.xlsx")
pci2032_min <- fread("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2023/Scenarion/20230313_ 2023 Scenario 9B (4)_CI_OUT.csv")
pci2032_min_budget <- fread("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2023/Scenarion/2023-03-20_2023 Scenario 9B (4) Budget_CI_OUT.csv")

names(pci2032_min) <- c("Year","Objectd","PCI")
setDT(pci2032_min)
names(pci2032_min_budget) <- c("Year","Objectd","PCI")
setDT(pci2032_min_budget)

#Mean PCI 2031
means <- pci2032_min %>% group_by(Year) %>%
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

pci2032_min <- PCIClass2030(pci2032_min)
pci2032_min[, PCIClass := as.factor(PCIClass)]
pci2032_min_budget <- PCIClass2030(pci2032_min_budget)
pci2032_min_budget[, PCIClass := as.factor(PCIClass)]
#pci2030[, PCIClass := NULL]
str(pci2032_min)
head(pci2032_min)

cols <- c("Objectd","Length","AADT","RoadTyp","tkl8")
pci2032_min <- pci2032_min[swedt_PCI[, ..cols], on = 'Objectd']
pci2032_min_budget <- pci2032_min_budget[swedt_PCI[, ..cols], on = 'Objectd']

swedt_PCI <- PCIClass(swedt_PCI)
swedt_PCI[, PCIClass := as.factor(PCIClass)]
str(swedt_PCI)

# Colors
fill <- c("#20AC65", "#71C94B","#FABF20","#F2203E","#C40A3B")

###############################################################################
# PCI 2020 vs 2030 barchart traffic
df_long <- pci2032_min[Year == 2021 | Year == 2031]
df_long[, Y := Year]
head(df_long)

# Trafikarbete
cond_y <- df_long %>%
  mutate(Y = as.factor(Y)) %>%
  #mutate(Y = recode(Y, "PCIClass" = "2020", "PCIClass_2030" = "2030")) %>%
  mutate(Y = factor(Y, levels = c('2020', '2030'))) %>%
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  group_by(Y, PCIClass) %>%
  summarise(grouplen = sum(Length*AADT)/1000) %>%
  mutate(percentage = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = Y, y = percentage, fill = PCIClass, label = paste0(round(100*percentage,digits=0)," %"))) +
  geom_bar(position = 'fill', stat = 'identity') +
  scale_fill_manual(values=fill) +
  labs(y="", x = "") +
  ggtitle("Trafikarbete") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="none", 
        axis.text=element_text(size=16),
        plot.title = element_text(size = 20)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

# Väglängd
cond_yv <- df_long %>%
  mutate(Y = as.factor(Y)) %>%
  #mutate(Y = recode(Y, "PCIClass" = "2020", "PCIClass_2030" = "2030")) %>%
  mutate(Y = factor(Y, levels = c('2020', '2030'))) %>%
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  group_by(Y, PCIClass) %>%
  summarise(grouplen = sum(Length)/1000) %>%
  mutate(percentage = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = Y, y = percentage, fill = PCIClass, label = paste0(round(100*percentage,digits=0)," %"))) +
  geom_bar(position = 'fill', stat = 'identity') +
  scale_fill_manual(values=fill) +
  labs(y="", x = "") +
  ggtitle("Väglängd") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none",
        axis.text=element_text(size=16),
        plot.title = element_text(size = 20)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

print(cond_yv)

# Length vs trafikarbete
grid.arrange(cond_yv, cond_y, ncol=2)

################################################################################
# Plot deficit
def_df_elm <- data.frame(Year = c(2022,2023,2024,2025,2026,2027,2028,2029,2030,2031,2032),
                         Miljarder = c(10.6, 10.6, 12.1, 13.2, 14.4, 14.8, 14.5, 13.8, 12.7, 11.3, 3.4))

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
bud_df <- data.frame(Year = c(2023,2024,2025,2026,2027,2028,2029,2030,2031,2032),
                     Miljarder = c(14.3, 9.5, 6.6, 4.9, 4.3, 4.5, 3.8, 3.4, 2.8, 24.5))
mean(c(14.3, 9.5, 6.6, 4.9, 4.3, 4.5, 3.8, 3.4, 2.8, 24.5))
h <- 7.9

bud_b <- bud_df %>%
  ggplot(aes(x=factor(Year), y=Miljarder)) +
  geom_bar(position="dodge", stat="identity", color="black", fill="purple") +
  geom_text(aes(label=Miljarder), position=position_dodge(width=0.9), vjust=-0.25, size=8) +
  scale_y_continuous(name="Miljarder SEK", limits=c(0, 28), breaks=c(0,5,10,15,20,25)) +
  geom_hline(aes(yintercept=h), size=1.5, color = "black") +
  geom_text(aes(0, h, label = paste0(h, " miljarder"), hjust=-2.25, vjust=-1), size=10) +
  #geom_text(aes(label=Miljarder), size=10) +
  labs(y="Miljarder SEK", x = "År") +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24))

print(bud_b)

#####################################################
# Plot tillstånd over time 2020-2030
bcols <- c("Objectd", "Year","PCIClass")
pb <- pci2032_min_budget[,..bcols]
pb <- pb %>% rename(PCIClass_budget = PCIClass)
head(pb)
pci2032_min_comb <- left_join(pci2032_min, pb, by=c("Objectd", "Year"))
pci2032_min_comb <- pci2032_min_comb %>% 
  mutate(PCIClass_budget = if_else(Year == "2032", PCIClass, PCIClass_budget))
head(pci2032_min_comb)

tf_p_min <- pci2032_min_comb %>%
  mutate(Tillstånd = as.character(PCIClass_budget)) %>%
  mutate(Tillstånd = factor(PCIClass_budget, levels = c("5","4","3","2","1"))) %>%
  mutate(Tillstånd = recode(PCIClass_budget, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
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
