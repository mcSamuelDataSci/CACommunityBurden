# ######## Helpful Resources ###############
# 
# # 1) Color-blindness simulator - input HEX color values - https://davidmathlogic.com/colorblind/#%23332288-%23117733-%2344AA99-%2388CCEE-%23DDCC77-%23CC6677-%23AA4499-%23882255
# # 2) Paul Tol's notes on color schemes - https://personal.sron.nl/~pault/#fig:orbits
# # 3) Color-blindness simluator on images - https://www.color-blindness.com/coblis-color-blindness-simulator/
# # 4) Suggested algorithm for calculating color contrast ratio - https://www.w3.org/TR/AERT/#color-contrast
# 
# 
# 
# 
# library(colorspace)  
# 
# 
# myCol    <- c("lightblue","green","firebrick","red")
# myDarker <- darken(myCol,0.3)
# myDark   <- darken(myCol,0.7)
# 
# par(mfrow=c(1,1))
# barplot(1:4,col=myCol)
# barplot(1:4,col=myDarker)
# barplot(1:4,col=myDark)
# 
# col2rgb(myCol)
# col2rgb(myDarker)
# col2rgb(myDark)
# 
# 
# rgb2hsv(col2rgb(myCol))
# rgb2hsv(col2rgb(myDarker))
# rgb2hsv(col2rgb(myDark))
# 
# 
# barplot(1:length(raceNameShortColors),col=raceNameShortColors)
# barplot(1:length(raceNameShortColors),col=darken(raceNameShortColors,.5))
# 
# 
# # These 12 colors below have enough contrast with a white background. 
# # We should create a dark color pallette specifically to be used for line plots where the ONLY requirement is sufficient contrast with a white background
# par(mfrow=c(2,1))
# myCol <- c("red", "blue", "purple")
# myCol2Dark <- c("grey", "red", "blue", "green", "pink", "cyan", "purple", "orange", "black")
# myDark <- darken(myCol2Dark, 0.7)
# barplot(1:3, col = myCol)
# barplot(1:8,col=myDark)
# 
# myDarkPallette <- c(myCol, myCol2Dark)
# 
# 
# 
# ####### Color-blind check in R ################
# 
# # Resources:
# 
# # 1) Checking rcartocolor for color-blind friendly pallettes  - https://nowosad.github.io/colorblindcheck/articles/articles/check_rcartocolor.html
# # 2) colorblindcheck github - https://github.com/Nowosad/colorblindcheck
# # 3) rcartocolor github - https://github.com/Nowosad/rcartocolor
# 
# remotes::install_github("nowosad/colorblindcheck")
# library(colorblindcheck)
# library(rcartocolor)
# par(mfrow=c(1,1))
# 
# # Only display rcartocolor colorblind friendly pallettes
# display_carto_all(colorblind_friendly = TRUE)
# 
# # Check Safe Pallette
# safePalette <- carto_pal(12, "Safe")
# colorblindcheck::palette_check(safePalette, plot = TRUE)
# 
# # 'light' - insufficient contrast with white background
# # Safe Color: 1,   3,   7
# # Ratio:     1.8, 1.6, 2.8
# 
# # 'dark' - sufficient contrast with white background
# # Safe Color: 2,   4,    5,   6,  8,  9,    10,  11,  12
# # Ratio:     3.7, 5.7, 12.2, 5.3, 3, 8.7,  12.8,  3,  3.5
# 
# names(safePalette) <- c("light", "dark", "light", "dark", "dark", "dark", "light", "dark", "dark", "dark", "dark", "dark")
# safePalette        
# 
# # Re-order safePallette - Put the colors (8, 11) that barely passed the threshold at the end
# myCB_palette <- safePalette[c(1:7, 9:10, 12, 11, 8)]
# myCB_palette
# 
# # Check myCB_pallette
# colorblindcheck::palette_check(myCB_palette, plot = TRUE)
# 
# # save color palette
# # saveRDS(myCB_palette, "/mnt/projects/FusionData/0.CCB/myCCB/Standards/colorPalettes/colorblind_palette_12.RDS")
