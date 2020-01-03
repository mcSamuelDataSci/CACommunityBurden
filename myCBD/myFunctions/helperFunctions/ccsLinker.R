

ccsLinker  <- function()
  
{  
 
# ccsMap  <- as.data.frame(read_excel( path(myPlace,"myInfo/CCS Code and Names Linkage.xlsx")))

ccsMap <- ccsMap %>% 
            mutate(ccsCode = str_pad(ccsCode, 5,"left",pad="o"))



}