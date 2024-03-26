#=================================================================#
#           Descriptive statistics for report 2022 update report
#               Scenario 4E - maintain current condition
#=================================================================#

swedt_PCI <- st_read( "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2023/DOT/sweden_2023_dot_20230126.shp") 
setDT(swedt_PCI)
#pci2030 <- read.xlsx("C:/Users/winte/Swedenroads_outputs/2030_PCI_20201218.xlsx")
pci2032_mcur <- fread("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2023/Scenarion/20230313_ 2023 Scenario 4E (3)_CI_OUT.csv")
pci2032_mcur_budget <- fread("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2023/Scenarion/2023-03-20_2023 Scenario 4E (3) Budget_CI_OUT.csv")

head(pci2032_mcur)
names(pci2032_mcur) <- c("Year","Objectd","PCI")
names(pci2032_mcur_budget) <- c("Year","Objectd","PCI")
setDT(pci2032_mcur)
setDT(pci2032_mcur_budget)

#Mean PCIs
means <- pci2032_mcur %>% group_by(Year) %>%
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

pci2032_mcur <- PCIClass2030(pci2032_mcur)
pci2032_mcur[, PCIClass := as.factor(PCIClass)]
pci2032_mcur_budget <- PCIClass2030(pci2032_mcur_budget)
pci2032_mcur_budget[, PCIClass := as.factor(PCIClass)]
#pci2030[, PCIClass := NULL]
str(pci2032_mcur)
head(pci2032_mcur)

cols <- c("Objectd","Length","AADT","RoadTyp","tkl8")
pci2032_mcur <- pci2032_mcur[swedt_PCI[, ..cols], on = 'Objectd']
pci2032_mcur_budget <- pci2032_mcur_budget[swedt_PCI[, ..cols], on = 'Objectd']

swedt_PCI <- PCIClass(swedt_PCI)
swedt_PCI[, PCIClass := as.factor(PCIClass)]
str(swedt_PCI)

# Colors
fill <- c("#20AC65", "#71C94B","#FABF20","#F2203E","#C40A3B")

###############################################################################
# PCI 2020 vs 2030 barchart traffic
df_long <- pci2032_mcur[Year == 2021 | Year == 2031]
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

def_df_maint_current <- data.frame(Year = c(2022,2023,2024,2025,2026,2027,2028,2029,2030,2031,2032),
                                   Miljarder = c(10.6, 10.7, 12.4, 13.8, 15.2, 15.9, 15.7, 15.1, 14.2, 13.0, 11.3))



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
bud_df_maint_current <- data.frame(Year = c(2023,2024,2025,2026,2027,2028,2029,2030,2031,2032),
                                   Miljarder = c(7.8, 6.8, 6.2, 6.9, 4.5, 4.6, 3.8, 4.5, 7.9, 7.9))
mean(c(7.8, 6.8, 6.2, 6.9, 4.5, 4.6, 3.8, 4.5, 7.9, 7.9))
h <- 6.1

bud_b_mcur <- bud_df_maint_current %>%
  ggplot(aes(x=factor(Year), y=Miljarder)) +
  geom_bar(position="dodge", stat="identity", color="black", fill="purple") +
  geom_text(aes(label=Miljarder), position=position_dodge(width=0.9), vjust=-0.25, size=8) +
  scale_y_continuous(name="Miljarder SEK", limits=c(0, 15), breaks=c(0,2.5,5,7.5,10,12.5,15)) +
  geom_hline(aes(yintercept=h), size=1.5, color = "black") +
  geom_text(aes(0, h, label = paste0(h," miljarder"), hjust=-3.0, vjust=-1), size=10) +
  #geom_text(aes(label=Miljarder), size=10) +
  labs(y="Miljarder SEK", x = "År") +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24))

print(bud_b_mcur)

#####################################################
# Plot tillstånd over time 2020-2030

tf_p_cur <- pci2032_mcur %>%
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
