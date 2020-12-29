#=================================================================#
#                 Descriptove statistics beläggning
#=================================================================#

#####################################################
# Beläggning topp 10 i Sverige
swebel <- swedt_PCI %>%
              filter(!is.na(Tretmnt)) %>%
              group_by(Tretmnt) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen)) %>%
              top_n(n=10) %>%
              arrange(desc(prop))
print(swebel, n=Inf)
sum(swebel$grouplen)/(sum(swedt_PCI$Length)/1000)
sum(swedt_PCI[is.na(Tretmnt)]$Length)/sum(swedt_PCI$Length)

# Beläggningsåtgärder per region
regbel <- swedt_PCI %>%
              filter(!is.na(Tretmnt)) %>%
              group_by(Region,Tretmnt) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen)) %>%
              top_n(n=10) %>%
              arrange(Region, desc(prop))
print(regbel, n=Inf)

write_xlsx(regbel,"C:/Users/winte/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Data/regbel.xlsx")

# Klassificerad
swebel_klass <- swedt_PCI %>%
              filter(!is.na(PvmntTy)) %>%
              group_by(PvmntTy) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen)) %>%
              top_n(n=10) %>%
              arrange(desc(prop))
print(swebel_klass, n=Inf)

# Metod + vägtyp
klass_manf <- swedt_PCI %>%
              filter(!is.na(PvngMth)) %>%
              group_by(RoadTyp, PvngMth) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen)) %>%
              top_n(n=5) %>%
              arrange(RoadTyp, desc(prop))
print(klass_manf, n=Inf)

# Metod + region
klass_reg <- swedt_PCI %>%
              filter(!is.na(PvngMth)) %>%
              group_by(Region, PvngMth) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen)) %>%
              top_n(n=5) %>%
              arrange(Region, desc(prop))
print(klass_reg, n=Inf)

# Klass + trafikmängd
swebel_klass_tkl <- swedt_PCI %>%
              filter(!is.na(Tretmnt)) %>%
              group_by(tkl8, Tretmnt) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen)) %>%
              top_n(n=5) %>%
              arrange(tkl8, desc(prop))
print(swebel_klass_tkl, n=Inf)

# Klass + vägtyp
swebel_klass_rt <- swedt_PCI %>%
              filter(!is.na(Tretmnt)) %>%
              group_by(RoadTyp, Tretmnt) %>%
              summarise(grouplen = sum(Length)/1000) %>%
              mutate(prop = grouplen/sum(grouplen)) %>%
              top_n(n=8) %>%
              arrange(RoadTyp, desc(prop))
print(swebel_klass_rt, n=Inf)


# Klassificerad historisk (PMS)
head(lans_dt)

historisk_bel <- lans_dt %>%
              filter(!is.na(Atgard1)) %>%
              group_by(Atgard1) %>%
              summarise(grouplen = sum(langd)/1000) %>%
              mutate(prop = grouplen/sum(grouplen)) %>%
              #top_n(n=10) %>%
              arrange(desc(prop))
print(historisk_bel, n=Inf)

# Join with gruppering
swebel_kat <- left_join(swebel, historisk_bel[,1:2], by = c("Tretmnt" = "Beltyp"))
swebel_kat$Atgard2 <- substring(swebel_kat$Atgard2,5)
print(swebel_kat, n=Inf)

write_xlsx(swebel_kat,"C:/Users/winte/OneDrive - Salbo Konsult AB/salbo.ai/Transportföretagen/Data/beläggningsåtgärder_grupperad.xlsx")