settlement_sankey <- function(excel_path) {
  library(readxl)
  library(dplyr)
  library(networkD3)
  
  Table_ImpactChains<-read_xlsx(path = "data.xlsx", sheet = "3a.Impact chains - Settlements")
  Table_ImpactChains<-Table_ImpactChains %>% filter(InCaseStudy=="YES")%>%
    select(1:5)
  Mynodes<-tibble(Name=c(unique(Table_ImpactChains$Sector),unique(Table_ImpactChains$Pressure),unique(Table_ImpactChains$EcoComponent)),
                  ID=c(1:length(unique(Name)))-1,
                  Group="unique")
  ImpactChains <- Table_ImpactChains %>% left_join(Mynodes,by=c('Sector'='Name')) %>% rename(ID.Sector=ID) %>% 
    left_join(Mynodes,by=c('Pressure'='Name')) %>% rename(ID.Pressure=ID) %>%
    left_join(Mynodes,by=c('EcoComponent'='Name')) %>% rename(ID.EcoComponent=ID)
  Mylinks<-bind_rows( tibble(source=ImpactChains$ID.Sector,target=ImpactChains$ID.Pressure,value=1),
                      tibble(source=ImpactChains$ID.Pressure,target=ImpactChains$ID.EcoComponent,value=1))
  
  sankeyNetwork(
    Links = Mylinks,
    Nodes = Mynodes,
    Source = "source",
    Target = "target",
    Value  = "value",
    NodeID = "Name",
    NodeGroup = "Group",
    fontSize = 12,
    nodeWidth = 30
  )
}
