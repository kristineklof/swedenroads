#=================================================================#
#                 Descriptove statistics for report
#               Scenario 4E - maintain current condition
#=================================================================#

swedt_PCI <- st_read( "C:/Users/winte/Swedenroads_outputs/sweden_v3_pci201206.shp") 
setDT(swedt_PCI)
#pci2030 <- read.xlsx("C:/Users/winte/Swedenroads_outputs/2030_PCI_20201218.xlsx")
pci2030_mcur <- fread("C:/Users/winte/Swedenroads_outputs/7045-Scenario4e.csv")

head(pci2030_mcur)
names(pci2030_mcur) <- c("Objectd","Year","PCI")
setDT(pci2030_mcur)

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

pci2030_mcur <- PCIClass2030(pci2030_mcur)
pci2030_mcur[, PCIClass := as.factor(PCIClass)]
#pci2030[, PCIClass := NULL]
str(pci2030_mcur)
head(pci2030_mcur)

cols <- c("Objectd","Length","AADT","RoadTyp","tkl8")
pci2030_mcur <- pci2030_mcur[swedt_PCI[, ..cols], on = 'Objectd']

# Colors
fill <- c("#20AC65", "#71C94B","#FABF20","#F2203E","#C40A3B")

###############################################################################
# PCI 2020 vs 2030 barchart traffic
df_long <- pci2030_mcur[Year == 2020 | Year == 2030]
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

def_df_maint_current <- data.frame(Year = c(2020,2021,2022,2023,2024,2025,2026,2027,2028,2029,2030),
                     Miljarder = c(13.8, 14.8, 13.2, 14.4, 15.5, 16.5, 17.9, 10.8, 11.8, 12.9, 14.3))

def_b <- def_df_maint_current %>%
  ggplot(aes(x=factor(Year), y=Miljarder)) +
  geom_bar(position="dodge", stat="identity", color="black", fill="darkblue") +
  geom_text(aes(label=Miljarder), position=position_dodge(width=0.9), vjust=-0.25, fontface="bold", size=8) +
  scale_y_continuous(name="Miljarder SEK", limits=c(0, 50), breaks=c(0,10,20,30,40,50)) +
  #geom_text(aes(label=Miljarder), size=10) +
  labs(y="Miljarder SEK", x = "År") +
  theme(axis.text=element_text(size=24),
          axis.title=element_text(size=16))

print(def_b)


################################################################################
# Plot budget
bud_df_maint_current <- data.frame(Year = c(2021,2022,2023,2024,2025,2026,2027,2028,2029,2030),
                     Miljarder = c(1.7, 15.7, 2.2, 2.6, 2.7, 2.8, 24.2, 0.8, 0.7, 0.6))
h <- 5.4

bud_b_mcur <- bud_df_maint_current %>%
  ggplot(aes(x=factor(Year), y=Miljarder)) +
  geom_bar(position="dodge", stat="identity", color="black", fill="purple") +
  geom_text(aes(label=Miljarder), position=position_dodge(width=0.9), vjust=-0.25, size=8) +
  scale_y_continuous(name="Miljarder SEK", limits=c(0, 25), breaks=c(0,5,10,15,20,25)) +
  geom_hline(aes(yintercept=h), size=1.5, color = "black") +
  geom_text(aes(0, h, label = paste0(h," miljarder"), hjust=-2.5, vjust=-1), size=10) +
  #geom_text(aes(label=Miljarder), size=10) +
  labs(y="Miljarder SEK", x = "År") +
  theme(axis.text=element_text(size=24),
          axis.title=element_text(size=24))

print(bud_b_mcur)

#####################################################
# Plot tillstånd over time 2020-2030

tf_p_cur <- pci2030_mcur %>%
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

print(tf_p_cur)
