BAR_WIDTH             <-  0.9
PLOT_WIDTH_MULTIPLIER <- 1

createBurdenView <- function(myIdNum, myCounty, myObserv, decrease = FALSE, myCustomTitle = "") {
  
  myCountyTitle <- ifelse(myCounty == STATE, STATE, 
                          paste0(myCounty, " County"))
  
  if (myIdNum == 3) {
    myTitle <- myCustomTitle
  } else if (myIdNum == 7) {
    myTitle <- plot_title[myIdNum]
  } else {
    myTitle <- paste0(plot_title[myIdNum], ", ", unique(dataSets[[myIdNum]]$year))
  }
  
  
  plotMeasures(IDnum=myIdNum, myCounty=myCounty, myObserv=myObserv, decrease=decrease) + 
    labs(title = myTitle) +
    theme(panel.border = element_blank(), 
          plot.title = element_text(size=16, face="bold", vjust=-4, hjust = 0), 
          plot.subtitle = element_text(size=14, face="bold", hjust= 1, vjust=-2))
}

capwords_burdenView <- function(x) {
  s <- tolower(x) 
  s <- strsplit(s," ")[[1]]
  paste0(toupper(substring(s, 1,1)), substring(s, 2), collapse=" ")
}

AppText<-function(Tbl=Datasources,TblRw=1) {list(
  HTML(paste(Tbl[TblRw,1],Tbl[TblRw,2],Tbl[TblRw,3],tagList(a(Tbl[TblRw,4],href=Tbl[TblRw,5],target="_blank")) )),
  if(Tbl[TblRw,6]!=""){
    tags$span(tipify(bsButton(paste0("pB",deparse(substitute(Tbl)),TblRw),Tbl[TblRw,6],size="extra-small"),Tbl[TblRw,7],placement = "top"))
  }
)}

hght<-2
wdth<-3.2

Summary_doc <- function (Title,Figure1,Figure2,Figure3,Figure4,Figure5,Figure6,Figure7,Figure8,Figure9,Figure10) {
  
  # read_docx(paste0(tempdir(),"/report_files/County_Snapshot_Report.docx"))  %>%
  officer::read_docx("myData/Burden View Report/County_Snapshot_Report.docx")  %>%
    # page 1  
    #   cursor_reach("WordSummaryText1") %>%
    # body_add_par(value = paste0(MeasureText$GeneralText[1]), style="Subtitle",pos="on") %>%
    cursor_reach("WordTitle1") %>%
    body_add_par(value = paste0(capwords_burdenView(Title)," Mortality Measures"), style = "heading 1", pos="on") %>%
    cursor_reach("WordFigure1") %>%
    body_add_img(src = Figure1, width = wdth, height = hght , style = "Normal", pos="on") %>%
    cursor_reach("WordFigure2") %>%
    body_add_img(src = Figure2, width = wdth, height = hght , style = "Normal", pos="on") %>%
    cursor_reach("WordFigure3") %>%
    body_add_img(src = Figure3, width = wdth, height = hght , style = "Normal", pos="on") %>%
    cursor_reach("WordFigure4") %>%
    body_add_img(src = Figure4, width = wdth, height = hght , style = "Normal", pos="on") %>%
    cursor_reach("WordFigure5") %>%
    body_add_img(src = Figure5, width = wdth, height = hght , style = "Normal", pos="on") %>%
    
    cursor_reach("WordTitle2") %>%
    body_add_par(value = paste0(capwords_burdenView(Title)," Morbidity Measures"), style = "heading 1", pos="on") %>%
    cursor_reach("WordFigure6") %>%
    body_add_img(src = Figure6, width = wdth, height = hght , style = "Normal", pos="on") %>%
    cursor_reach("WordFigure7") %>%
    body_add_img(src = Figure7, width = wdth, height = hght , style = "Normal", pos="on") %>%
    cursor_reach("WordFigure8") %>%
    body_add_img(src = Figure8, width = wdth, height = hght , style = "Normal", pos="on") %>%
    
    # page 2
    cursor_reach("WordTitle3") %>%
    body_add_par(value = "State Measures", style = "heading 1", pos="on") %>%
    cursor_reach("WordFigure9") %>%
    body_add_img(src = Figure9, width = wdth, height = hght , style = "Normal", pos="on") %>%
    cursor_reach("WordFigure10") %>%
    body_add_img(src = Figure10, width = wdth, height = hght , style = "Normal", pos="on") 

}