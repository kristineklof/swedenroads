###########################################################
# Länsanalys av % vägar som ej klarar underhållsstandarden

sw24_län <- dplyr::left_join(sw24,lankom,by=c("län_nr" = "Län",
                                          "kmmn_nr" = "Kommunnr"))
head(sw24_län)

maintstandlength_lan <- sw24_län %>%
  group_by(Länsnamn) %>%
  summarise(grouplen = sum(längd)/1000,
            percabove = sum(längd[(sparm17_24 > SP_maint & vägbrdd > 6) | (sparm15_24 > SP_maint & vägbrdd <= 6)| irih_24 > IRI_maint], na.rm = TRUE)/1000/grouplen,
            lenabove = percabove*grouplen,
            perc_lan = lenabove/3111.52)
print(maintstandlength_lan)
sum(maintstandlength_lan$lenabove)/sum(maintstandlength_lan$grouplen)
sum(maintstandlength_lan$lenabove)
sum(maintstandlength_lan$perc_lan)

# Export to Excel
wb <- createWorkbook()
addWorksheet(wb, "Överskriden underhållsstandard")
writeData(wb, sheet = 1, maintstandlength_lan)
#Save Workbook
saveWorkbook(wb,"C:/Users/krist/OneDrive - Salbo Konsult AB/salbo.ai/Svenskt Näringsliv/Underhållsstandard_län.xlsx", overwrite = TRUE)

