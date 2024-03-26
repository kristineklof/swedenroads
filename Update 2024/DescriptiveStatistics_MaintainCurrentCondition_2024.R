#=================================================================#
#           Descriptive statistics for report 2024 update report
#               Scenario maintain current condition
#=================================================================#

swedt_PCI <- st_read( "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2024/DOT/sweden_2024_dot_20240202.shp") 
setDT(swedt_PCI)
#pci2030 <- read.xlsx("C:/Users/winte/Swedenroads_outputs/2030_PCI_20201218.xlsx")
pci2033_mcur <- fread("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2024/Scenarion/2024-03-12_2024 Upprätthålla nuvarande tillstånd (4)_CI_OUT.csv")

head(pci2033_mcur)
names(pci2033_mcur) <- c("Year","Objectd","PCI")
setDT(pci2033_mcur)

#Mean PCIs
means <- pci2033_mcur %>% group_by(Year) %>%
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

pci2033_mcur <- PCIClass2030(pci2033_mcur)
pci2033_mcur[, PCIClass := as.factor(PCIClass)]
#pci2030[, PCIClass := NULL]
str(pci2033_mcur)
head(pci2033_mcur)

cols <- c("Objectd","Length","AADT","RoadTyp","tkl8")
pci2033_mcur <- pci2033_mcur[swedt_PCI[, ..cols], on = 'Objectd']

swedt_PCI <- PCIClass(swedt_PCI)
swedt_PCI[, PCIClass := as.factor(PCIClass)]
str(swedt_PCI)

# Colors
fill <- c("#20AC65", "#71C94B","#FABF20","#F2203E","#C40A3B")


################################################################################
# Plot deficit

def_df_maint_current <- data.frame(Year = c(2023,2024,2025,2026,2027,2028,2029,2030,2031,2032,2033),
                                   Miljarder = c(12.4, 13.7, 11.4, 9.8, 10.8, 14.3, 14.5, 14.9, 14.2, 14.0, 10.5))

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
bud_df_maint_current <- data.frame(Year = c(2024,2025,2026,2027,2028,2029,2030,2031,2032,2033),
                                   Miljarder = c(7.8, 6.6, 5.2, 7.1, 6.8, 6.2, 5.3, 4.7, 4.7, 7.9))
mean(c(7.8, 6.6, 5.2, 7.1, 6.8, 6.2, 5.3, 4.7, 4.7, 7.9))
h <- 6.2

bud_b_mcur <- bud_df_maint_current %>%
  ggplot(aes(x=factor(Year), y=Miljarder)) +
  geom_bar(position="dodge", stat="identity", color="black", fill="purple") +
  geom_text(aes(label=Miljarder), position=position_dodge(width=0.9), vjust=-0.25, size=8) +
  scale_y_continuous(name="Miljarder SEK", limits=c(0, 15), breaks=c(0,2.5,5,7.5,10,12.5,15)) +
  geom_hline(aes(yintercept=h), size=1.5, color = "black") +
  geom_text(aes(0, h, label = paste0(h," miljarder"), hjust=-2.8, vjust=-1), size=10) +
  #geom_text(aes(label=Miljarder), size=10) +
  labs(y="Miljarder SEK", x = "År") +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24))

print(bud_b_mcur)

#####################################################
# Plot tillstånd over time 2020-2030

pci2032_mcur_tl <- pci2032_mcur %>% 
  filter(Year == 2023 | Year == 2024) %>%
  mutate(Year = if_else(Year == 2023, 2025, 2024))
head(pci2032_mcur_tl)
  
pci2033_mcur_tl <- pci2033_mcur %>% 
  mutate(Year = Year + 2) %>%
  mutate(Year = if_else(Year == 2025, Year - 2, Year)) %>%
  filter(!(Year %in% c(2034, 2035)))
head(pci2033_mcur_tl)

pci2033_mcur_tl <- rbind(pci2033_mcur_tl, pci2032_mcur_tl)
  
tf_p_cur <- pci2033_mcur_tl %>%
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
