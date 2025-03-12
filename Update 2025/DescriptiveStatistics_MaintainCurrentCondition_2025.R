#=================================================================#
#           Descriptive statistics for report 2025 update report
#               Scenario maintain current condition
#=================================================================#

swedt_PCI <- st_read( "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2025/DOT/sweden_2025_dot_20250130.shp") 
setDT(swedt_PCI)
#pci2030 <- read.xlsx("C:/Users/winte/Swedenroads_outputs/2030_PCI_20201218.xlsx")
pci2034_mcur <- fread("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2025/Scenarion/CI_OUT_2025 Upprätthålla nuvarande tillstånd_With_GISID.csv")

head(pci2034_mcur)
names(pci2034_mcur) <- c("Year","Objectd","PCI")
setDT(pci2034_mcur)

#Mean PCIs
means <- pci2034_mcur %>% group_by(Year) %>%
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

pci2034_mcur <- PCIClass2030(pci2034_mcur)
pci2034_mcur[, PCIClass := as.factor(PCIClass)]
#pci2030[, PCIClass := NULL]
str(pci2034_mcur)
head(pci2034_mcur)

cols <- c("Objectd","Length","AADT","RoadTyp","tkl8")
pci2034_mcur <- pci2034_mcur[swedt_PCI[, ..cols], on = 'Objectd']

swedt_PCI <- PCIClass(swedt_PCI)
swedt_PCI[, PCIClass := as.factor(PCIClass)]
str(swedt_PCI)

# Colors
fill <- c("#20AC65", "#71C94B","#FABF20","#F2203E","#C40A3B")


################################################################################
# Plot deficit

def_df_maint_current <- data.frame(Year = c(2024,2025,2026,2027,2028,2029,2030,2031,2032,2033,2034),
                                   Miljarder = c(8.1, 8.4, 8.9, 11.8, 14.3, 18.3, 19.5, 12.1, 8.3, 8.5, 9.5))

def_b <- def_df_maint_current %>%
  ggplot(aes(x=factor(Year), y=Miljarder)) +
  geom_bar(position="dodge", stat="identity", color="black", fill="darkblue") +
  geom_text(aes(label=Miljarder), position=position_dodge(width=0.9), vjust=-0.25, fontface="bold", size=8) +
  scale_y_continuous(name="Miljarder SEK", limits=c(0, 30), breaks=c(0,10,20,30,40,50)) +
  #geom_text(aes(label=Miljarder), size=10) +
  labs(y="Miljarder SEK", x = "År") +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=16))

print(def_b)

################################################################################
# Plot budget
bud_df_maint_current <- data.frame(Year = c(2025,2026,2027,2028,2029,2030,2031,2032,2033,2034),
                                   Miljarder = c(8.8, 8.6, 6.2, 3.8, 3.1, 2.2, 8.3, 2.7, 3.9, 11.1))
mean(c(8.8, 8.6, 6.2,  3.8, 3.1, 2.2, 8.3, 2.7, 3.9, 11.1))
h <- 5.9

bud_b_mcur <- bud_df_maint_current %>%
  ggplot(aes(x=factor(Year), y=Miljarder)) +
  geom_bar(position="dodge", stat="identity", color="black", fill="purple") +
  geom_text(aes(label=Miljarder), position=position_dodge(width=0.9), vjust=-0.25, size=8) +
  scale_y_continuous(name="Miljarder SEK", limits=c(0, 12.5), breaks=c(0,2.5,5,7.5,10,12.5,15)) +
  geom_hline(aes(yintercept=h), size=1.5, color = "black") +
  geom_text(aes(0, h, label = paste0(h," miljarder"), hjust=-1.6, vjust=-1.0), size=10) +
  #geom_text(aes(label=Miljarder), size=10) +
  labs(y="Miljarder SEK", x = "År") +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24))

print(bud_b_mcur)

#####################################################
# Plot tillstånd over time 2020-2030

pci2034_mcur_3134 <- pci2034_mcur %>% 
  filter(Year %in% c(2031, 2027, 2033, 2032)) %>%
  mutate(Year = case_when(
    Year == 2031 ~ 2034,
    Year == 2027 ~ 2031,
    Year == 2032 ~ 2033,
    Year == 2033 ~ 2032,
    TRUE ~ Year
  ))

unique(pci2034_mcur_3134$Year)

pci2034_mcur_tl <- pci2034_mcur %>%
  filter(Year != 2031 & Year != 2032 & Year != 2033 &  Year != 2034)

unique(pci2034_mcur_tl$Year)

pci2034_mcur_tl <- rbind(pci2034_mcur_tl, pci2034_mcur_3134)

tf_p_cur <- pci2034_mcur_tl %>%
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
        axis.text=element_text(size=24),
        plot.title = element_text(size = 20)) +
  geom_text(size = 6, position = position_stack(vjust = 0.5))

print(tf_p_cur)
