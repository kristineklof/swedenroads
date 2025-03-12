#=================================================================#
#           Descriptove statistics for 2025 update report
#                   Scenario 1 - current budget
#=================================================================#

swedt_PCI <- st_read( "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2025/DOT/sweden_2025_dot_20250130.shp") 
setDT(swedt_PCI)
#pci2034 <- read.xlsx("C:/Users/winte/Swedenroads_outputs/2030_PCI_20201218.xlsx")
pci2034 <- fread("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2025/Scenarion/CI_OUT_2025 Nuvarande budget_(4)_With_GISID.csv")
#pci2034_2022 <- fread("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/7018-Scenario6.csv")

head(pci2034)
names(pci2034) <- c("Year","Objectd","PCI")
setDT(pci2034)
str(pci2034)

#Mean PCI 2034
means <- pci2034 %>% group_by(Year) %>%
  summarize(mean_pci = mean(PCI))

#names(pci2034_2022) <- c("Objectd","Year","PCI")
#setDT(pci2034_2022)

PCIClass2030 <- function(dat){
  setDT(dat)
  
  dat[, PCIClass := if_else_na(PCI > 80, 5, NA)]
  dat[, PCIClass := if_else_na(PCI <=80, 4, PCIClass)]
  dat[, PCIClass := if_else_na(PCI <=60, 3, PCIClass)]
  dat[, PCIClass := if_else_na(PCI <=40, 2, PCIClass)]
  dat[, PCIClass := if_else_na(PCI <=20, 1, PCIClass)]
  
  return(dat)
}

pci2034 <- PCIClass2030(pci2034)
str(pci2034)
head(pci2034)

cols <- c("Objectd","Length","AADT","RoadTyp","tkl8","Region")
pci2034 <- pci2034[swedt_PCI[, ..cols], on = 'Objectd']

unique(pci2034$Year)
sum(is.na(pci2034$Year))
sum(is.na(pci2034$PCI))
sum(is.na(pci2034$Length))
length(pci2034[pci2034$Year == 2025,])

swedt_PCI <- PCIClass(swedt_PCI)
swedt_PCI[, PCIClass := as.factor(PCIClass)]
str(swedt_PCI)
#pci2034 <- pci2034_2022[pci2034, on = c('Objectd','Year')]

fill <- c("#20AC65", "#71C94B","#FABF20","#F2203E","#C40A3B")

###############################################################################
# PCI 2020 vs 2030 barchart traffic
#df_long <- melt(data = pci2034, 
#                id.vars = c("Objectd","AADT","Length"),
#                measure.vars = c("PCIClass_2030", "PCIClass"),
#                variable.name = "Y",
#                value.name = "PCIClass")

df_long <- pci2034[Year == 2024 | Year == 2034]
df_long[, Y := Year]

# Region
cond_reg <- df_long  %>%
  dplyr::mutate(Region = ChangeRegion(Region)) %>%
  mutate(Region = recode(Region, Ost="Öst", Vast="Väst")) %>%
  mutate(PCIClass = factor(PCIClass, levels = c("5","4","3","2","1"))) %>%
  mutate(PCIClass = recode(PCIClass, "5" ="Mycket bra", "4" = "Bra", "3" = "Tillfredsställande", "2" = "Dålig", "1" = "Mycket dålig")) %>%
  group_by(Region, PCIClass) %>%
  summarise(grouplen = sum(Length)/1000) %>%
  mutate(percentage = grouplen/sum(grouplen)) %>%
  ggplot(aes(x = Region, y = percentage, fill = PCIClass, label = paste0(round(100*percentage,digits=0)," %"))) +
  geom_bar(position = 'fill', stat = 'identity') +
  scale_fill_manual(values=fill) +
  labs(y="", x = "") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="right", legend.direction="vertical",
        legend.title = element_blank(), 
        legend.text=element_text(size=16), 
        axis.text=element_text(size=16)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

print(cond_reg)

# Trafikarbete
cond_y <- df_long %>%
  mutate(Y = as.factor(Y)) %>%
  #mutate(Y = recode(Y, "PCIClass" = "2020", "PCIClass_2030" = "2030")) %>%
  mutate(Y = factor(Y, levels = c('2024', '2034'))) %>%
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
  mutate(Y = factor(Y, levels = c('2024', '2034'))) %>%
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

print(cond_y)

# Length vs trafikarbete
grid.arrange(cond_yv, cond_y, ncol=2)

#####################################################
# Plot tillstånd over time 2022-2034

tf_p <- pci2034 %>%
  na.omit() %>% 
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
        axis.text =element_text(size=16),
        #axis.text.y =element_blank(),
        #axis.ticks.y=element_blank(),
        plot.title = element_text(size = 16)) +
  geom_text(size = 4, position = position_stack(vjust = 0.5))

print(tf_p)

#####################################################
# Plot deficit
def_df_urek <- data.frame(Year = c(2024,2025,2026,2027,2028,2029,2030,2031,2032,2033,2034),
                          Miljarder = c(8.1, 9.5, 12.1, 16.6, 20.9, 24.1, 25.9, 27.1, 28.2, 29.3, 28.1))

def_b_rek <- def_df_urek %>%
  ggplot(aes(x=factor(Year), y=Miljarder)) +
  geom_bar(position="dodge", stat="identity", color="black", fill="darkblue") +
  geom_text(aes(label=Miljarder), position=position_dodge(width=0.9), vjust=-0.25, fontface="bold", size=8) +
  scale_y_continuous(name="Miljarder SEK", limits=c(0, 35), breaks=c(0,10,20,30,40,50)) +
  #geom_text(aes(label=Miljarder), size=10) +
  labs(y="Miljarder SEK", x = "År") +
  theme(axis.text=element_text(size=24),
        #axis.text.y =element_blank(),
        #axis.ticks.y=element_blank(),
        #axis.title.x=element_blank(),
        axis.title=element_text(size=24))

print(def_b_rek)

##########################################################
# Roadlength under PCI 5 2020 vs 2030
pci_5_2024 <- swedt_PCI %>%
  group_by(RoadTyp) %>%
  summarise(grouplen = sum(Length)/1000,
            percbelow= sum(Length[PCI <= 5], na.rm = TRUE)/1000/grouplen,
            lenbelow= percbelow*grouplen)
print(pci_5_2020)
sum(pci_5_2020$lenbelow)/sum(pci_5_2020$grouplen)

# Roadlength under PCI 5 2020
pci_5_2024_tkl <- swedt_PCI %>%
  group_by(tkl8) %>%
  summarise(grouplen = sum(Length)/1000,
            percbelow= sum(Length[PCI <= 5], na.rm = TRUE)/1000/grouplen,
            lenbelow= percbelow*grouplen) %>%
  ungroup() %>%
  mutate(percbelow_margin = lenbelow/sum(lenbelow))
print(pci_5_2024_tkl)
sum(pci_5_2024_tkl$lenbelow)/sum(pci_5_2020_tkl$grouplen)

# Roadlength under PCI 5 2030
pci_5_2034 <- pci2034 %>%
  filter(Year == 2034) %>%
  group_by(RoadTyp) %>%
  summarise(grouplen = sum(Length)/1000,
            percbelow= sum(Length[PCI <= 5])/1000/grouplen,
            lenbelow= percbelow*grouplen)
print(pci_5_2034, n=Inf)
sum(pci_5_2034$lenbelow)/sum(pci_5_2030$grouplen)

# Trafikklass under PCI 5 2030
pci_5_2034_tkl <- pci2034 %>%
  filter(Year == 2034) %>%
  group_by(tkl8) %>%
  summarise(grouplen = sum(Length)/1000,
            percbelow= sum(Length[PCI <= 5])/1000/grouplen,
            lenbelow= percbelow*grouplen) %>%
  ungroup() %>%
  mutate(percbelow_margin = lenbelow/sum(lenbelow))
print(pci_5_2034_tkl, n=Inf)
sum(pci_5_2034_tkl$lenbelow)/sum(pci_5_2030_tkl$grouplen)

