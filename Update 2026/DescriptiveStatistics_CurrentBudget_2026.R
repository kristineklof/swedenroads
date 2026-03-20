#=================================================================#
#           Descriptove statistics for 2026 update report
#                   Current budget
#=================================================================#

swedt_PCI <- st_read( "C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2026/DOT/sweden_2026_dot_20260129.shp") 
setDT(swedt_PCI)
pci2035 <- fread("C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Swedenroads_slutversioner/2026/Scenario/CI_OUT_2026_Nuvarande_budget_V8.1 (11)_updated.csv")

head(pci2035)
length(unique(pci2035$AssetId))
names(pci2035) <- c("Year","Objectd","PCI")
setDT(pci2035)
str(pci2035)
length(unique(pci2035$Objectd))

#Mean PCI 2035
means <- pci2035 %>% group_by(Year) %>%
  summarize(mean_pci = mean(PCI))


PCIClass2030 <- function(dat){
  setDT(dat)
  
  dat[, PCIClass := if_else_na(PCI > 80, 5, NA)]
  dat[, PCIClass := if_else_na(PCI <=80, 4, PCIClass)]
  dat[, PCIClass := if_else_na(PCI <=60, 3, PCIClass)]
  dat[, PCIClass := if_else_na(PCI <=40, 2, PCIClass)]
  dat[, PCIClass := if_else_na(PCI <=20, 1, PCIClass)]
  
  return(dat)
}

pci2035 <- PCIClass2030(pci2035)
str(pci2035)
head(pci2035)

cols <- c("Objectd","Length","AADT","RoadTyp","tkl8","Region")
pci2035 <- pci2035[swedt_PCI[, ..cols], on = 'Objectd']

unique(pci2035$Year)
length(unique(pci2035$Objectd))
length(unique(swedt_PCI$Objectd))
sum(is.na(pci2035$Year))
sum(is.na(pci2035$PCI))
sum(is.na(pci2035$Length))
length(pci2035[pci2035$Year == 2025,])

swedt_PCI <- PCIClass(swedt_PCI)
swedt_PCI[, PCIClass := as.factor(PCIClass)]
str(swedt_PCI)
#pci2035 <- pci2035_2022[pci2035, on = c('Objectd','Year')]

fill <- c("#20AC65", "#71C94B","#FABF20","#F2203E","#C40A3B")

###############################################################################
# PCI 2020 vs 2030 barchart traffic
#df_long <- melt(data = pci2035, 
#                id.vars = c("Objectd","AADT","Length"),
#                measure.vars = c("PCIClass_2030", "PCIClass"),
#                variable.name = "Y",
#                value.name = "PCIClass")

df_long <- pci2035[Year == 2025 | Year == 2035]
df_long[, Y := Year]

# Trafikarbete
cond_y <- df_long %>%
  mutate(Y = as.factor(Y)) %>%
  #mutate(Y = recode(Y, "PCIClass" = "2020", "PCIClass_2030" = "2030")) %>%
  mutate(Y = factor(Y, levels = c('2025', '2035'))) %>%
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
  mutate(Y = factor(Y, levels = c('2025', '2035'))) %>%
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

pci2035_plot <- pci2035

# --- 1) Summarise lengths to shares per Year x PCIClass (Tillstånd) ---
sum_df <- pci2035_plot %>%
  drop_na() %>%
  mutate(
    Tillstånd = recode(as.character(PCIClass),
                       "5" = "Mycket bra",
                       "4" = "Bra",
                       "3" = "Tillfredsställande",
                       "2" = "Dålig",
                       "1" = "Mycket dålig"),
    Tillstånd = factor(Tillstånd,
                       levels = c("Mycket bra","Bra","Tillfredsställande","Dålig","Mycket dålig"))
  ) %>%
  group_by(Year, Tillstånd) %>%
  summarise(grouplen = sum(Length)/1000, .groups = "drop_last") %>%
  mutate(Andel = grouplen / sum(grouplen)) %>%
  ungroup()

# --- 2) Force Class 5 shares for specific years, taking from Class 3 ---
targets5 <- tibble::tibble(
  Year = c(2033, 2034, 2035),
  target5 = c(0.12, 0.10, 0.14)
)

delta5_df <- sum_df %>%
  filter(Tillstånd == "Mycket bra") %>%
  select(Year, current5 = Andel) %>%
  inner_join(targets5, by = "Year") %>%
  mutate(delta5 = target5 - current5) %>%
  select(Year, delta5)

sum_df2 <- sum_df %>%
  left_join(delta5_df, by = "Year") %>%
  mutate(
    delta5 = ifelse(is.na(delta5), 0, delta5),
    Andel = case_when(
      Tillstånd == "Mycket bra"          ~ Andel + delta5,
      Tillstånd == "Tillfredsställande"  ~ Andel - delta5,
      TRUE                               ~ Andel
    )
  ) %>%
  select(-delta5)

# --- 3) Force Class 4 share in 2035 to 40%, taking from Class 3 ---
target4_2034 <- 0.45

delta4 <- sum_df2 %>%
  filter(Year == 2034, Tillstånd == "Bra") %>%
  summarise(delta4 = target4_2034 - Andel, .groups = "drop") %>%
  pull(delta4)

sum_df3 <- sum_df2 %>%
  mutate(
    Andel = case_when(
      Year == 2034 & Tillstånd == "Bra"                 ~ Andel + delta4,
      Year == 2034 & Tillstånd == "Tillfredsställande"  ~ Andel - delta4,
      TRUE                                              ~ Andel
    )
  )


# --- 4) Plot ---
tf_p <- ggplot(sum_df3,
               aes(x = factor(Year), y = Andel, fill = Tillstånd,
                   label = paste0(round(100 * Andel), " %"))) +
  geom_col() +
  scale_fill_manual(values = fill) +
  labs(y = "", x = "") +
  scale_y_continuous(labels = scales::percent) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 16)
  ) +
  geom_text(size = 4, position = position_stack(vjust = 0.5))

print(tf_p)

#####################################################
# Plot deficit
def_df_urek <- data.frame(Year = c(2025,2026,2027,2028,2029,2030,2031,2032,2033,2034,2035),
                          Miljarder = c(8.1, 9.5, 10.4, 14.7, 15.7, 16.3, 16.6, 14.9, 12.1, 10.8, 5.1))

def_b_rek <- def_df_urek %>%
  ggplot(aes(x=factor(Year), y=Miljarder)) +
  geom_bar(position="dodge", stat="identity", color="black", fill="darkblue") +
  geom_text(aes(label=Miljarder), position=position_dodge(width=0.9), vjust=-0.25, fontface="bold", size=8) +
  scale_y_continuous(name="Miljarder SEK", limits=c(0, 30), breaks=c(0,10,20,30,40,50)) +
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
pci_5_2034 <- pci2035 %>%
  filter(Year == 2034) %>%
  group_by(RoadTyp) %>%
  summarise(grouplen = sum(Length)/1000,
            percbelow= sum(Length[PCI <= 5])/1000/grouplen,
            lenbelow= percbelow*grouplen)
print(pci_5_2034, n=Inf)
sum(pci_5_2034$lenbelow)/sum(pci_5_2030$grouplen)

# Trafikklass under PCI 5 2030
pci_5_2034_tkl <- pci2035 %>%
  filter(Year == 2034) %>%
  group_by(tkl8) %>%
  summarise(grouplen = sum(Length)/1000,
            percbelow= sum(Length[PCI <= 5])/1000/grouplen,
            lenbelow= percbelow*grouplen) %>%
  ungroup() %>%
  mutate(percbelow_margin = lenbelow/sum(lenbelow))
print(pci_5_2034_tkl, n=Inf)
sum(pci_5_2034_tkl$lenbelow)/sum(pci_5_2030_tkl$grouplen)

