#=================================================================#
#                 Descriptove statistics for report
#                    Scenario 1 - current budget
#=================================================================#

swedt_PCI <- st_read( "C:/Users/winte/Swedenroads_outputs/sweden_v3_pci201206.shp") 
setDT(swedt_PCI)
#pci2030 <- read.xlsx("C:/Users/winte/Swedenroads_outputs/2030_PCI_20201218.xlsx")
pci2030 <- fread("C:/Users/winte/Swedenroads_outputs/PCI_Scenario 1 - 3rd update PCI.csv")

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
#pci2030[, PCIClass_2030 := as.factor(PCIClass)]
#pci2030[, PCIClass := NULL]
str(pci2030)
head(pci2030)

cols <- c("Objectd","Length","AADT","RoadTyp","tkl8")
pci2030 <- pci2030[swedt_PCI[, ..cols], on = 'Objectd']

fill <- c("#20AC65", "#71C94B","#FABF20","#F2203E","#C40A3B")

###############################################################################
# PCI 2020 vs 2030 barchart traffic
#df_long <- melt(data = pci2030, 
#                id.vars = c("Objectd","AADT","Length"),
#                measure.vars = c("PCIClass_2030", "PCIClass"),
#                variable.name = "Y",
#                value.name = "PCIClass")

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
    #ggtitle("Trafikarbete") +
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
    #ggtitle("Väglängd") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "none",
          axis.text=element_text(size=16),
          plot.title = element_text(size = 20)) +
    geom_text(size = 3, position = position_stack(vjust = 0.5))

print(cond_yv)

# Length vs trafikarbete
grid.arrange(cond_yv, cond_y, ncol=2)

#####################################################
# Plot tillstånd over time 2020-2030

#tf_fill <- c("#C40A3B","#F2203E","#FABF20","#71C94B","#20AC65")

#tf_df <- data.frame(Year = c(rep(2020,5),rep(2021,5),rep(2022,5),rep(2023,5),rep(2024,5),
#                              rep(2025,5),rep(2026,5),rep(2027,5),rep(2028,5),rep(2029,5),rep(2030,5)),
#                    Tillstånd = rep(c("Mycket dåligt", "Dåligt", "Tillfredsställande", "Bra", "Mycket bra"),11),
#                    Andel = c(0.13,0.17,0.29,0.27,0.14,
#                              0.14, 0.22, 0.30, 0.26, 0.09,
#                              0.14, 0.24, 0.24, 0.26, 0.12,
#                              0.16, 0.24, 0.21, 0.25, 0.13,
#                              0.19, 0.23, 0.20, 0.26, 0.13,
#                              0.22, 0.21, 0.19, 0.28, 0.10,
#                              0.24, 0.18, 0.21, 0.29, 0.07,
#                              0.27, 0.17, 0.18, 0.24, 0.13,
#                              0.29, 0.16, 0.16, 0.22, 0.16,
#                              0.31, 0.14, 0.14, 0.25, 0.16,
#                             0.31, 0.11, 0.10, 0.28, 0.20))
#head(tf_df)
                
#tf_p <- tf_df %>%
#  mutate(Tillstånd = factor(Tillstånd, levels = c("Mycket bra","Bra", "Tillfredsställande","Dåligt","Mycket dåligt"))) %>%
#  ggplot(aes(x = factor(Year), y = Andel, fill = Tillstånd, label = paste0(round(100*Andel,digits=0)," %"))) +
#  geom_bar(position="fill", stat="identity") +
#  scale_fill_manual(values=fill) +
#    labs(y="", x = "") +
#    scale_y_continuous(labels = scales::percent) +
#    theme(legend.position = "none",
#          axis.text=element_text(size=16),
#          plot.title = element_text(size = 20)) +
#    geom_text(size = 3, position = position_stack(vjust = 0.5))

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

#####################################################
# Plot deficit
def_df <- data.frame(Year = c(2020,2021,2022,2023,2024,2025,2026,2027,2028,2029,2030),
                     Miljarder = c(19.7, 29.5, 44.9, 63.0, 80.7, 94.3, 103.7, 114.7, 130.1, 144.8, 158.9))

def_p <- def_df %>%
  ggplot(aes(x=factor(Year), y=Miljarder, group=1)) +
  geom_point(stat='summary', fun=sum,shape=21, color="black", fill="black", size=3) +
  stat_summary(fun=sum, geom="line") +
  geom_label_repel(aes(label = Miljarder),
                    size = 5) +
  scale_y_continuous(name="Miljarder SEK", limits=c(0, 160), breaks=c(0,20,40,60,80,100,120,140,160)) +
  #geom_text(aes(label=Miljarder), size=10) +
  labs(y="Miljarder SEK", x = "År") +
  theme(axis.text=element_text(size=16),
          axis.title=element_text(size=16))

print(def_p)

def_b <- def_df %>%
  ggplot(aes(x=factor(Year), y=Miljarder)) +
  geom_bar(position="dodge", stat="identity", color="black", fill="darkblue") +
  geom_text(aes(label=Miljarder), position=position_dodge(width=0.9), vjust=-0.25, fontface="bold", size=6) +
  scale_y_continuous(name="Miljarder SEK", limits=c(0, 160), breaks=c(0,20,40,60,80,100,120,140,160)) +
  #geom_text(aes(label=Miljarder), size=10) +
  labs(y="Miljarder SEK", x = "År") +
  theme(axis.text=element_text(size=16),
          axis.title=element_text(size=16))

print(def_b)

##########################################################
# Roadlength under PCI 5 2020 vs 2030
pci_5_2020 <- swedt_PCI %>%
              group_by(RoadTyp) %>%
              summarise(grouplen = sum(Length)/1000,
                        percbelow= sum(Length[PCI <= 5], na.rm = TRUE)/1000/grouplen,
                        lenbelow= percbelow*grouplen)
print(pci_5_2020)
sum(pci_5_2020$lenbelow)/sum(pci_5_2020$grouplen)

# Roadlength under PCI 5 2020
pci_5_2020_tkl <- swedt_PCI %>%
              group_by(tkl8) %>%
              summarise(grouplen = sum(Length)/1000,
                        percbelow= sum(Length[PCI <= 5], na.rm = TRUE)/1000/grouplen,
                        lenbelow= percbelow*grouplen)
print(pci_5_2020_tkl)
sum(pci_5_2020_tkl$lenbelow)/sum(pci_5_2020_tkl$grouplen)

# Roadlength under PCI 5 2030
pci_5_2030 <- pci2030 %>%
              filter(Year == 2030) %>%
              group_by(RoadTyp) %>%
              summarise(grouplen = sum(Length)/1000,
                        percbelow= sum(Length[PCI <= 5])/1000/grouplen,
                        lenbelow= percbelow*grouplen)
print(pci_5_2030, n=Inf)
sum(pci_5_2030$lenbelow)/sum(pci_5_2030$grouplen)

# Trafikklass under PCI 5 2030
pci_5_2030_tkl <- pci2030 %>%
              filter(Year == 2030) %>%
              group_by(tkl8) %>%
              summarise(grouplen = sum(Length)/1000,
                        percbelow= sum(Length[PCI <= 5])/1000/grouplen,
                        lenbelow= percbelow*grouplen)
print(pci_5_2030_tkl, n=Inf)
sum(pci_5_2030_tkl$lenbelow)/sum(pci_5_2030_tkl$grouplen)

