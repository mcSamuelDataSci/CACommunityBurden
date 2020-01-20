# makes object that contains ccs codes and names

ccsLinker  <- function(keep=c("ccsCode","ccsName","birth"))
  
{  

  
  
ccsMap  <- as.data.frame(read_excel( path(myPlace,"myInfo/CCS Code and Names Linkage.xlsx")))

 ccsMap %>% 
     mutate(ccsCode = str_pad(ccsCode, 5,"left",pad="o")) %>%
     select(keep)
}

# "ccsCode"
# "ccsName"
# "ccsCode2"
# "ccsName2"
# "ccsCode3"
# "ccsName3"
# "birthPregnancyRelated"
