#=================================================================#
#                 Descriptove statistics for report
#               Scenario 4A - maintain current condition
#=================================================================#

swedt_PCI <- st_read( "C:/Users/winte/Swedenroads_outputs/sweden_v3_pci201206.shp") 
setDT(swedt_PCI)
#pci2030 <- read.xlsx("C:/Users/winte/Swedenroads_outputs/2030_PCI_20201218.xlsx")
pci2030 <- fread("C:/Users/winte/Swedenroads_outputs/7013-Scenario4A.csv")

head(pci2030)
names(pci2030) <- c("Objectd","Year","PCI")
setDT(pci2030)

swedt_PCI <- PCIClass(swedt_PCI)
swedt_PCI[, PCIClass := as.factor(PCIClass)]
str(swedt_PCI)

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

pci2030 <- PCIClass2030(pci2030)
pci2030[, PCIClass := as.factor(PCIClass)]
#pci2030[, PCIClass := NULL]
str(pci2030)
head(pci2030)

cols <- c("Objectd","Length","AADT","RoadTyp","tkl8")
pci2030 <- pci2030[swedt_PCI[, ..cols], on = 'Objectd']

# Colors
fill <- c("#20AC65", "#71C94B","#FABF20","#F2203E","#C40A3B")

###############################################################################
# PCI 2020 vs 2030 barchart traffic
df_long <- pci2030[Year == 2020 | Year == 2030]
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

def_df <- data.frame(Year = c(2020,2021,2022,2023,2024,2025,2026,2027,2028,2029,2030),
                     Miljarder = c(19.7, 29.5, 44.6, 39.7, 51.8, 60.1, 65.0, 70.8, 48.6, 51.0, 54.3))

def_b <- def_df_maint_current %>%
  ggplot(aes(x=factor(Year), y=Miljarder)) +
  geom_bar(position="dodge", stat="identity", color="black", fill="darkblue") +
  geom_text(aes(label=Miljarder), position=position_dodge(width=0.9), vjust=-0.25, fontface="bold", size=6) +
  scale_y_continuous(name="Miljarder SEK", limits=c(0, 80), breaks=c(0,20,40,60,80)) +
  #geom_text(aes(label=Miljarder), size=10) +
  labs(y="Miljarder SEK", x = "År") +
  theme(axis.text=element_text(size=16),
          axis.title=element_text(size=16))

print(def_b)

#####################################################
# Plot tillstånd over time 2020-2030

tf_p <- pci2030 %>%
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
          axis.text=element_text(size=16),
          plot.title = element_text(size = 20)) +
    geom_text(size = 3, position = position_stack(vjust = 0.5))

print(tf_p)
