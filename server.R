library(tidyverse)
library(readxl)
library(data.table)
library(stringr)
library(plyr)

options(max.print=999999)
options(width = 999)
file_prefix <- "LCA_FY"
curryear <- 0
currbeg <- 0
currend <- 0
trace <- ""

shinyServer(
    function(input, output, session) {
        output$myUsage <- renderUI({
            includeHTML("http://econdataus.com/lca1.htm")
        })
        output$myText <- renderPrint({
            trace <<- ""
            xx1 <- getDataByYear()
            if (input$groupyear){
                xx <- getDataByYearGroups()
            }
            else{
                xx <- getDataByGroups()
            }
            minyear <- input$minyear
            maxyear <- input$maxyear
            minqtr <- input$minqtr
            maxqtr <- input$maxqtr
            sminyear <- as.character(minyear)
            smaxyear <- as.character(maxyear)
            if (minqtr != 1 | maxqtr != 4){
                sminyear <- paste0(minyear," Q",minqtr)
                smaxyear <- paste0(maxyear," Q",maxqtr)
            }
            if (minyear == maxyear){
                cat(paste0("LCA DISCLOSURE DATA, FY"," ",sminyear,"\n"))
            }
            else{
                cat(paste0("LCA DISCLOSURE DATA, FY"," ",sminyear,"-",smaxyear,"\n"))
            }
            if (nchar(gsfilter1) > 0){
                cat(paste0("(", gsfilter1,")\n"))
            }
            cat("\n")
            
            # Output sums and totals
            ngroup <- length(input$xgroup)
            if (ngroup == 0){
                cat(paste0("NUMBER OF ROWS     = ", format(dim(xx)[1], big.mark=",",scientific=FALSE),"\n"))
                xxyr <- xx[as.character(xx$WAGE_OFFER_UNIT_OF_PAY_9089) == "Year",]
            }
            else{
                cat(paste0("NUMBER OF ROWS     = ", format(sum(xx$APPLICATIONS), big.mark=",",scientific=FALSE),"\n"))
            }
            cat(paste0("MEAN(SALARY)       = ", mean(as.numeric(xx$SALARY), na.rm = TRUE),"\n"))
            cat("\n")
            print(xx1)
            cat("\n")
            
            # Limit to Maximum Total Rows if necessary
            itotrows <- as.integer(input$totrows)
            if (nrow(xx) > itotrows) xx <- head(xx, n = itotrows)
            
            # Number all rows and set total width to Maximum Total Width
            if (nrow(xx) > 0) row.names(xx) <- 1:nrow(xx)
            options(width = input$totwidth)
            
            if (ngroup == 0){
                # Display only the specified fields
                xshowcols <- cc$label[cc$label %in% input$xshow]
                xshowcols <- c("YEAR", xshowcols)
                updateSelectInput(session,"dist3", choices = names(xx))
                xx <- subset(xx, select = append(xshowcols, input$xshow2))
            }
            wdthcols <- cc$label[cc$wdth == 1]
            for (col in wdthcols){
                if (col %in% colnames(xx)){
                    xx[[col]] <- strtrim(xx[[col]], width=input$colwidth)
                }
            }
            cat(file=stderr(), paste0("##### ",input$minyear,"|",input$maxyear,"|",
                input$CASE_STATUS,"|",input$EMPLOYER_NAME,"|",
                input$JOB_INFO_WORK_CITY,"|",input$WORK_STATE,"|",input$xsearch1,"|",
                input$ysearch1,"|",input$xsearch2,"|",input$ysearch2,"|",
                paste(input$xgroup,collapse = ';'),"|",input$xsort,"|",input$xsortdir,"|",
                paste(input$xshow,collapse = ';'),"|",paste(input$xshow2,collapse = ';'),"|",
                input$colwidth,"|",input$totwidth,"|",input$totrows,"|\n"))
            print(xx)
            cat(paste0("\n",trace))
        })
        getTitle <- function(varname, short_title){
            title <- paste0(varname,": ",input$minyear)
            if (input$minyear != input$maxyear){
                title <- paste0(title,"-",input$maxyear)
            }
            if (input$minqtr == input$maxqtr){
                title <- paste0(title," Q",input$minqtr)
            }
            else if (input$minqtr != 1 | input$maxqtr != 4){
                title <- paste0(title," Q",input$minqtr,"-Q",input$maxqtr)
            }
            if (short_title){
                if (nchar(gsfilter2) > 0) title <- paste0(title," LCAs (",gsfilter2,")")
            }
            else{
                if (nchar(gsfilter1) > 0) title <- paste0(title," LCAs (",gsfilter1,")")
            }
            return(title)
        }
        getTitle2 <- function(xsort, group1, ngroups, baseyear, short_title){
            title <- paste0(group1,": ",input$minyear)
            if (baseyear == 0){
                years <- input$minyear
                if (input$minyear != input$maxyear){
                    years <- paste0(years,"-",input$maxyear)
                }
                if (input$minqtr == input$maxqtr){
                    years <- paste0(years," Q",input$minqtr)
                }
                else if (input$minqtr != 1 | input$maxqtr != 4){
                    years <- paste0(years," Q",input$minqtr,"-Q",input$maxqtr)
                }
            }
            else years <- baseyear
            title <- paste0(group1,": TOP ",ngroups," BY ",xsort," IN ",years)
            if (short_title){
                if (nchar(gsfilter2) > 0) title <- paste0(title," (",gsfilter2,")")
            }
            else{
                if (nchar(gsfilter1) > 0) title <- paste0(title," (",gsfilter1,")")
            }
            return(title)
        }
        output$myDistribution <- renderPlot({
            if (input$groupyear){
                xx <- getDataByYearGroups()
            }
            else{
                xx <- getDataByGroups()
            }
            ngroup <- length(input$xgroup)
            if (ngroup == 0){
                varname <- input$uxvar
                if (varname %in% c("YEAR","SALARY","PSALARY","WAGE_PWAGE")){
                    type <- "num"
                }
                else type <- cc[cc$label == varname,'type']
                if (type == "date"){
                    xx[[varname]] <- as.Date(xx[[varname]])
                }
                title <- getTitle(varname, input$stitle3)
                xlabel <- input$uxvar
                ylabel <- input$uyvar
                if (input$uyvar == "APPLICATIONS"){
                    gg <- ggplot(xx, aes_string(x=varname))
                }
                else{
                    gg <- ggplot(xx, aes_string(x=varname, weight=input$uyvar))
                }
                ubins <- input$binwidth
                if (ubins < 0){
                    gg <- gg + geom_histogram(binwidth= -ubins, color="black", fill=input$ucolor)
                }
                else if (ubins > 0){
                    gg <- gg + geom_histogram(bins= ubins, color="black", fill=input$ucolor)
                }
                else{
                    gg <- gg + geom_histogram(color="black", fill=input$ucolor)
                }
                gg <- gg + labs(title=title, x=xlabel, y=ylabel)
                gg <- gg + scale_y_continuous(labels = scales::comma)
            }
            else{
                xx <- xx[1:input$topn,]
                gvar <- input$xgroup[1]
                yvar <- "APPLICATIONS"
                if ("Workers" %in% input$gopts) yvar <- "WORKERS"
                title <- getTitle(gvar, input$stitle3)
                xlabel <- gvar
                ylabel <- yvar
                if ("Alpha" %in% input$gopts) ovar <- gvar
                else ovar <- yvar
                if (input$groupyear){
                    xx$YEAR <- as.character(xx$YEAR)
                    if ("Reverse" %in% input$gopts){
                        gg <- ggplot(xx, aes(x=reorder(get(gvar), -get(ovar)), y=get(yvar), fill=YEAR))
                    }
                    else{
                        gg <- ggplot(xx, aes(x=reorder(get(gvar), get(ovar)), y=get(yvar), fill=YEAR))
                    }
                    gg <- gg + geom_bar(position = "dodge", stat="identity")
                }
                else{
                    if ("Reverse" %in% input$gopts){
                        gg <- ggplot(xx, aes(x=reorder(get(gvar), -get(ovar)), y=get(yvar)))
                    }
                    else{
                        gg <- ggplot(xx, aes(x=reorder(get(gvar), get(ovar)), y=get(yvar)))
                    }
                    gg <- gg + geom_bar(stat="identity", color=input$gcolor, fill=input$gcolor)
                }
                if ("Vertical" %in% input$gopts){
                    gg <- gg + coord_flip()
                }
                gg <- gg + labs(title=title, x=xlabel, y=ylabel)
                gg <- gg + scale_y_continuous(labels = scales::comma)
            }
            gg <- addScales(gg, input$uxscale, input$uyscale)
            gg
        })
        output$myPlotYear <- renderPlot({
            xx <- getDataByYear()
            title <- getTitle("APPLICATIONS", input$stitle)
            ylabel <- "APPLICATIONS"
            if (input$plotk){
                ylabel <- "THOUSANDS OF APPLICATIONS"
                xx$APPLICATIONS <- xx$APPLICATIONS / 1000
            }
            gg <- ggplot(data=xx,aes_string(x="YEAR",y="APPLICATIONS"))
            gg <- gg + ggtitle(title)
            gg <- gg + xlab("YEAR")
            gg <- gg + ylab(ylabel)
            gg <- gg + geom_point(size=3, color=input$pcolor)
            gg <- gg + geom_line(size=1, alpha=0.7, color=input$pcolor)
            gg <- addScales(gg, input$xscale, input$yscale)
            gg
        })
        output$myPlotGroups <- renderPlot({
            group1 <- input$xgroup[1]
            if (input$baseyear2 < 2008){
                gg <- getDataByGroups()
                gg <- gg[1:input$ngroups2,]
            }
            else{
                gg <- getDataByYearGroups()
                gg <- gg[gg$YEAR == input$baseyear2,]
                gg <- gg[1:input$ngroups2,]
                
            }
            xx <- getDataByYearGroups()
            xx <- xx[xx[[group1]] %in% gg[[group1]],]
            xsort <- input$xsort
            if (xsort == "TOTAL_WORKERS") xsort <- "WORKERS"
            title <- getTitle2(xsort, group1, input$ngroups2, input$baseyear2, input$stitle2)
            ylabel <- xsort
            if (input$plotk2){
                ylabel <- paste0("THOUSANDS OF ",xsort)
                xx[[xsort]] <- xx[[xsort]] / 1000
            }
            gg <- ggplot(data=xx,aes_string(x="YEAR",y=xsort,
                                            color=group1,shape=group1))
            gg <- gg + ggtitle(title)
            gg <- gg + xlab("YEAR")
            gg <- gg + ylab(ylabel)
            gg <- gg + geom_point(size=3)
            gg <- gg + geom_line(size=1, alpha=0.7)
            gg <- addScales(gg, input$xscale2, input$yscale2)
            gg
        })
        addScales <- function(gg, xscale, yscale){
            xx <- NULL
            yy <- NULL
            if(xscale != ""){
                sxx <- unlist(strsplit(xscale, ","))
                xx <- as.numeric(sxx)
                if (length(sxx) == 3){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[3]))
                }
                else if (length(sxx) == 4){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[4]))
                }
            }
            if(yscale != ""){
                syy <- unlist(strsplit(yscale, ","))
                yy <- as.numeric(syy)
                if (length(syy) == 3){
                    gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[3]),
                                                  labels = scales::comma)
                }
                else if (length(syy) == 4){
                    gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[4]),
                                                  labels = scales::comma)
                }
            }
            if (length(xx) >= 2){
                if (length(yy) >= 2){
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]), ylim = c(yy[1], yy[2]))
                }
                else{
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]))
                }
            }
            else if (length(yy) >= 2){
                gg <- gg + coord_cartesian(ylim = c(yy[1], yy[2]))
            }
            return(gg)
        }
        getDataFiltered <- eventReactive(input$submit, {
            xx <- NULL
            minyear <- input$minyear
            maxyear <- input$maxyear
            if (maxyear < minyear) maxyear <- minyear
            for (year in minyear:maxyear){
                yy <- getOneYear(year)
                if (NROW(yy) < 1) next
                yy <- cbind(YEAR = year, yy)
                whichX <- which(names(yy) == "X")
                if (length(whichX) > 0){
                    yy <- yy[-whichX[1]]
                }
                dd <- data.table(yy)
                gg <- dd[, .(APPLICATIONS = NROW(dd)), by = YEAR]
                for (i in 1:NCOL(yy)){if (class(yy[[i]])[1] == "POSIXct") yy[i] <- as.character(yy[[i]])}
                if (is.null(xx)){
                    xx <- yy
                }
                else{
                    xx <- rbind(xx, yy)
                }
            }
            for (i in 1:NCOL(xx)){
                if (!is.numeric(xx[,i])){
                    xx[,i] <- gsub("[^ -~]","?",xx[,i]) #CHANGE UNPRINTABLE CHAR TO ?
                    xx[,i] <- toupper(xx[,i])
                }
            }
            return(xx)
        }, ignoreNULL = FALSE)
        getDataByYear <- eventReactive(input$submit, {
            xx <- getDataFiltered()
            if (is.null(xx)) return(NULL)
            dd <- data.table(xx)
            gg <- dd[, .(WORKERS = sum(TOTAL_WORKERS), APPLICATIONS =.N), by = YEAR] #ADD WORKERS
            gg <- gg[order(YEAR)]
            xx <- data.frame(gg)
            return(xx)
        }, ignoreNULL = FALSE)
        getDataByGroups <- eventReactive(input$submit, {
            xx <- getDataFiltered()
            if (is.null(xx)) return(NULL)
            dd <- data.table(xx)
            ngroup <- length(input$xgroup)
            if (ngroup > 0){
                groups <- paste(input$xgroup, collapse=',')
                if (input$empname2na == "Remove"){
                    if ("EMPLOYER_NAME2" %in% groups){
                        dd <- dd[!is.na(dd$EMPLOYER_NAME2)]
                    }
                }
                if (input$xsubgroups){
                    gg <- dd[, .(APPLICATIONS = .N,
                                 WORKERS = sum(TOTAL_WORKERS),
                                 NEW_EMPLOYMENT = sum(NEW_EMPLOYMENT),
                                 CONTINUED_EMP = sum(CONTINUED_EMP),
                                 CHANGE_PREV_EMP = sum(CHANGE_PREV_EMP),
                                 NEW_CONCUR_EMP = sum(NEW_CONCUR_EMP),
                                 CHANGE_EMPLOYER = sum(CHANGE_EMPLOYER),
                                 AMENDED_PETITION = sum(AMENDED_PETITION),
                                 TOTAL_SUBGROUPS = sum(NEW_EMPLOYMENT+CONTINUED_EMP+CHANGE_PREV_EMP+
                                                           NEW_CONCUR_EMP+CHANGE_EMPLOYER+AMENDED_PETITION)),
                             by = groups] #ADD WORKERS AND SUBGROUPS
                }
                else{
                    gg <- dd[, .(WORKERS = sum(TOTAL_WORKERS), APPLICATIONS = .N), by = groups] #ADD WORKERS
                }
                if (input$xsort == "TOTAL_WORKERS"){
                    gg <- gg[order(-WORKERS)]
                }
                else{
                    gg <- gg[order(-APPLICATIONS)]
                }
                xx <- data.frame(gg)
            }
            else{
                # Sort by specified sort fields
                if (input$xsortdir == "Ascending") xx <- xx[order(xx[[input$xsort]]),]
                else{
                    if (class(xx[[input$xsort]]) != "numeric") xx <- xx[rev(order(xx[[input$xsort]])),]
                    else xx <- xx[order(-xx[[input$xsort]]),]
                }
                Sys.setlocale(category = "LC_ALL", locale = "C")
            }
            return(xx)
        }, ignoreNULL = FALSE)
        getDataByYearGroups <- eventReactive(input$submit, {
            xx <- getDataFiltered()
            if (is.null(xx)) return(NULL)
            dd <- data.table(xx)
            ngroup <- length(input$xgroup)
            if (ngroup > 0){
                groups <- paste(input$xgroup, collapse=',')
                groups <- paste0("YEAR,",groups) #group also by YEAR
                if (input$empname2na == "Remove"){
                    if ("EMPLOYER_NAME2" %in% groups){
                        dd <- dd[!is.na(dd$EMPLOYER_NAME2)]
                    }
                }
                gg <- dd[, .(WORKERS = sum(TOTAL_WORKERS), APPLICATIONS = .N), by = groups] #ADD WORKERS
                if (input$xsort == "TOTAL_WORKERS"){
                    gg <- gg[order(-WORKERS)]
                }
                else{
                    gg <- gg[order(-APPLICATIONS)]
                }
                xx <- data.frame(gg)
            }
            else{
                # Sort by specified sort fields
                if (input$xsortdir == "Ascending") xx <- xx[order(xx[[input$xsort]]),]
                else{
                    if (class(xx[[input$xsort]])=="factor") xx <- xx[rev(order(xx[[input$xsort]])),]
                    else xx <- xx[order(-xx[[input$xsort]]),]
                }
                Sys.setlocale(category = "LC_ALL", locale = "C")
            }
            return(xx)
        }, ignoreNULL = FALSE)
        get_ccdefs <- function(ccfile){
            #combines ccfile and cols_def to create xccfile
            cc <- read.table(ccfile, stringsAsFactors = FALSE, header = TRUE)
            ddfile <- paste0("cols/cols_def.txt")
            dd <- read.table(ddfile, stringsAsFactors = FALSE, header = TRUE)
            for (i in 1:NROW(dd)){
                cci <- which(cc$match == dd$match[i])
                len <- length(cci)
                if (len == 0){
                    trace <<- paste0(trace,"> WARNING: ",dd$match[i], " not found in ",ccfile,"\n")
                    cc <- add_row(cc)
                    cci <- NROW(cc)
                    cc$column[cci] <- NA
                    cc$match[cci] <- dd$match[i]
                    cc$type[cci]  <- dd$type[i]
                    cc$label[cci] <- dd$label[i]
                    cc$csv[cci]  <- dd$csv[i]
                    cc$grp[cci]  <- dd$grp[i]
                    cc$show[cci] <- dd$show[i]
                    cc$sort[cci] <- dd$sort[i]
                    cc$srch[cci] <- dd$srch[i]
                    cc$wdth[cci] <- dd$wdth[i]
                }
                else{
                    cc$label[cci] <- dd$label[i]
                    cc$csv[cci] <- dd$csv[i]
                    cc$grp[cci] <- dd$grp[i]
                    cc$show[cci] <- dd$show[i]
                    cc$sort[cci] <- dd$sort[i]
                    cc$srch[cci] <- dd$srch[i]
                    cc$wdth[cci] <- dd$wdth[i]
                }
            }
            xccfile <- paste0("x",ccfile)
            sink(file = xccfile)
            print(cc, row.names = FALSE)
            sink()
            return(cc)
        }
        getOneYear <- function(year)({
            # Initialize any local variables
            firstyear <- (year == input$minyear)
            csvfile  <- paste0("data/",file_prefix,year,".csv")
            xls_prefix <- "H-1B_Disclosure_Data_FY"
            xlsfile  <- paste0("input/",xls_prefix,year,".xlsx")
            ccfile <- paste0("cols/cols",year,".txt")
            if (year == 2018) xlsfile <- "input/H-1B_Disclosure_Data_FY2018_EOY.xlsx"
            else if(year == 2017)  xlsfile <- "input/H-1B_Disclosure_Data_FY17.xlsx"
            else if(year == 2016)  xlsfile <- "input/H-1B_Disclosure_Data_FY16.xlsx"
            else if(year == 2015)  xlsfile <- "input/H-1B_Disclosure_Data_FY15_Q4.xlsx"
            else if(year == 2014)  xlsfile <- "input/H-1B_FY14_Q4.xlsx"
            else if(year == 2013)  xlsfile <- "input/LCA_FY2013.xlsx"
            else if(year == 2012)  xlsfile <- "input/LCA_FY2012_Q4.xlsx"
            else if(year == 2011)  xlsfile <- "input/H-1B_iCert_LCA_FY2011_Q4.xlsx"
            else if(year == 2010)  xlsfile <- "input/H-1B_FY2010.xlsx"
            else if(year == 2009)  xlsfile <- "input/H-1B_Case_Data_FY2009.xlsx"
            else if(year == 2008)  xlsfile <- "input/H-1B_Case_Data_FY2008.xlsx"
            
            if (year > 2019){
                csvfile  <- paste0("data/",file_prefix,year,"_Q1.csv")
                ccfile <- paste0("cols/cols",year,"_Q1.txt")
            }

            # Read csv file if necessary
            ibeg <- iend <- 1
            if (year > 2019){
                iend <- 4
                if (year == input$minyear){
                    ibeg <- input$minqtr
                }
                if (year == input$maxyear){
                    iend <- input$maxqtr
                }
                if (year == 2021 & ibeg == 1 & iend == 4){
                    ibeg <- 3 # for 2021, this contains quarters 1-3
                    csvfile  <- paste0("data/",file_prefix,year,"_Q3.csv") #DEBUG-CHANGE FOR FULL 2021
                    ccfile <- paste0("cols/cols",year,"_Q3.txt")
                }
                else if (year == 2023 & ibeg == 1 & iend == 2){
                    ibeg <- 2 # for 2023, this contains quarters 1-2
                    csvfile  <- paste0("data/",file_prefix,year,"_Q2.csv") #DEBUG-CHANGE FOR Q1-Q2 2023
                    ccfile <- paste0("cols/cols",year,"_Q2.txt")
                }
                else if (ibeg < 1){
                    ibeg <- 1
                }
            }
            usecache <- TRUE
            if (input$cache){
                ooyear <- paste0("oo",year,"q",ibeg,"q",iend)
            }
            else{
                ooyear <- "ooxx"
                if (curryear != year || currbeg != ibeg || currend != iend){
                    curryear <<- year
                    currbeg <<- ibeg
                    currend <<- iend
                    usecache <- FALSE
                    if (exists("ooxx")){
                        rm("ooxx")
                    }
                }
            }
            if (usecache & exists(ooyear)){
                qq <- get(ooyear)
                whichX <- which(names(qq) == "X")
                if (length(whichX) > 0){
                    qq <- qq[-whichX[1]]
                    if (firstyear) trace <<- paste0(trace,"> WARNING: deleted column X\n") #DEBUG-DETAIL
                }
            }
            else{
                qq <- NULL
                if (!file.exists(csvfile)){
                    msg = paste("Loading XLS data for FY", year)
                    withProgress(message = msg, detail = "this can take a minute or so...", value = 0, {
                        for (i in 1:9){
                            incProgress(1/10)
                            Sys.sleep(0.5)
                        }
                        for (i in ibeg:iend){
                            if (year > 2019){
                                xlsfile <- paste0("input/LCA_Disclosure_Data_FY",year,"_Q",i,".xlsx")
                                csvfile  <- paste0("data/",file_prefix,year,"_Q",i,".csv")
                                ccfile <- paste0("cols/cols",year,"_Q",i,".txt")
                            }
                            oo <- read_xlsx(xlsfile, guess_max = 1000000) # max excel rows = 1048576
                            # Create ccfile to relate all columns to matches
                            qnames <- paste0('"',names(oo),'"')
                            qblank <- '""'
                            dfcols <- data.frame(qnames,qnames,qnames,qblank,0,0,0,0,0,0, stringsAsFactors = FALSE)
                            names(dfcols) <- c("column","match","label","type","csv","grp","show","sort","srch","wdth")
                            tt <- oo %>% summarize_all(class)
                            for (i in 1:length(tt)){
                                if (as.character(tt[1,i]) == "character"){
                                    dfcols$type[i] <- "chr"
                                }
                                else if (as.character(tt[1,i]) == "numeric"){
                                    dfcols$type[i] <- "num"
                                }
                                else if (as.character(tt[1,i]) == "logical"){
                                    dfcols$type[i] <- "log"
                                }
                                else if (as.character(tt[1,i]) == "POSIXct"){
                                    dfcols$type[i] <- "date"
                                }
                                else{
                                    dfcols$type[i] <- as.character(tt[1,i])
                                }
                            }
                            if (!file.exists(ccfile)){
                                sink(file = ccfile)
                                print(dfcols, row.names = FALSE)
                                sink()
                            }
                            # Add definitions to ccfile
                            cc <- get_ccdefs(ccfile)
                            for (i in 1:NROW(cc)){
                                if (is.na(cc$column[i])){
                                    oo[[cc$label[i]]] <- NA
                                }
                            }
                            cols <- cc$label[which(cc$csv == 1)]
                            colnames(oo) <- cc$label
                            oo <- oo[,cols]
                            write.csv(oo, csvfile)
                            if (is.null(qq)){
                                qq <- oo
                            }
                            else{
                                qq <- rbind(qq, oo)
                            }
                        }
                        incProgress(1/10)
                    })
                }
                else{
                    msg = paste("Loading CSV data for FY", year)
                    withProgress(message = msg, detail = "this can take a minute or so...", value = 0, {
                        for (i in 1:9){
                            incProgress(1/10)
                            Sys.sleep(0.5)
                        }
                        for (i in ibeg:iend){
                            if (year > 2019){
                                csvfile  <- paste0("data/",file_prefix,year,"_Q",i,".csv")
                            }
                            if (file.exists(csvfile)){
                                if (substr(input$encoding,1,1) != "#"){
                                    oo <- read.csv(csvfile, fileEncoding=input$encoding)
                                }
                                else {
                                    oo <- read.csv(csvfile)
                                }
                                if (is.null(qq)){
                                    qq <- oo
                                }
                                else{
                                    qq <- rbind(qq, oo)
                                }
                            }
                            else{
                                cat(file=stderr(), paste0("> WARNING: File ", csvfile," not found\n"))
                            }
                        }
                        incProgress(1/10)
                    })
                    cat(names(qq), sep = "\n", file = paste0("cnames",year,".txt"))
                }
                assign(ooyear,qq,envir = globalenv())
            }
            xx <- data.frame(qq)
            if (year == 2010){
                xx$WAGE_UNIT <-xx$PW_UNIT
            }
            # Make value changes and create variables before searches
            levels(xx$CASE_STATUS)[toupper(levels(xx$CASE_STATUS))=="CERTIFIED - WITHDRAWN"] <- "CERT-WITHDRAWN"
            levels(xx$CASE_STATUS)[toupper(levels(xx$CASE_STATUS))=="CERTIFIED-WITHDRAWN"] <- "CERT-WITHDRAWN"
            xx$WAGE_RATE_FROM[is.na(xx$WAGE_RATE_FROM)] <- -1
            xx$PREVAILING_WAGE[is.na(xx$PREVAILING_WAGE)] <- -1
            xx <- xx[xx$WAGE_UNIT != "Select Pay Range",] # appears in 2011-2013
            xx$WAGE_UNIT <- toupper(xx$WAGE_UNIT)
            xx$PW_UNIT <- toupper(xx$PW_UNIT)
            xsalary <- input$xsalary
            wunit <- c("HOUR", "WEEK", "BI-WEEKLY", "MONTH", "YEAR")
            if (year < 2010){
                wunit <- c("HR", "WK", "BI", "MTH", "YR")
                xsalary[xsalary == "HOUR"] <- "HR"
                xsalary[xsalary == "YEAR"] <- "YR"
            }
            if ("OTHER" %in% xsalary){
                xsalary <- c(xsalary, wunit[2], wunit[3], wunit[4])
            }
            xx <- xx[xx$WAGE_UNIT %in% xsalary | xx$WAGE_UNIT %in% xsalary,]
            xx$SALARY <- as.numeric(xx$WAGE_RATE_FROM)
            xx$SALARY[xx$WAGE_UNIT == wunit[1]] <- 2000 * as.numeric(xx$SALARY[xx$WAGE_UNIT == wunit[1]])
            xx$SALARY[xx$WAGE_UNIT == wunit[2]] <-   52 * as.numeric(xx$SALARY[xx$WAGE_UNIT == wunit[2]])
            xx$SALARY[xx$WAGE_UNIT == wunit[3]] <-   26 * as.numeric(xx$SALARY[xx$WAGE_UNIT == wunit[3]])
            xx$SALARY[xx$WAGE_UNIT == wunit[4]] <-   12 * as.numeric(xx$SALARY[xx$WAGE_UNIT == wunit[4]])
            xx$PSALARY <- as.numeric(xx$PREVAILING_WAGE)
            xx$PW_UNIT[is.na(xx$PW_UNIT)] <- "" #DEBUG (unnecessary?)
            xx$PW_UNIT <- str_trim(xx$PW_UNIT) #DEBUG (unnecessary?)
            xx$PSALARY[xx$PW_UNIT  == wunit[1]] <- 2000 * as.numeric(xx$PSALARY[xx$PW_UNIT  == wunit[1]])
            xx$PSALARY[xx$PW_UNIT  == wunit[2]] <-   52 * as.numeric(xx$PSALARY[xx$PW_UNIT  == wunit[2]])
            xx$PSALARY[xx$PW_UNIT  == wunit[3]] <-   26 * as.numeric(xx$PSALARY[xx$PW_UNIT  == wunit[3]])
            xx$PSALARY[xx$PW_UNIT  == wunit[4]] <-   12 * as.numeric(xx$PSALARY[xx$PW_UNIT  == wunit[4]])
            xx$WAGE_PWAGE <- xx$SALARY / xx$PSALARY

            if (input$empclean %in% c("Clean Both","Clean EMPLOYER_NAME")){
                for (i in input$ignore){
                    if (i == "case")   xx$EMPLOYER_NAME <- toupper(xx$EMPLOYER_NAME)
                    if (i == "comma")  xx$EMPLOYER_NAME <- gsub("[,]$", "", xx$EMPLOYER_NAME)
                    if (i == "comma")  xx$EMPLOYER_NAME <- gsub("[,]", " ", xx$EMPLOYER_NAME)
                    if (i == "period") xx$EMPLOYER_NAME <- gsub("[.]$", "", xx$EMPLOYER_NAME)
                    if (i == "period") xx$EMPLOYER_NAME <- gsub("[.][ ]", " ", xx$EMPLOYER_NAME)
                    if (i == "the")    xx$EMPLOYER_NAME <- gsub("^THE ", "", xx$EMPLOYER_NAME)
                    if (i == "and")    xx$EMPLOYER_NAME <- gsub(" AND ", " & ", xx$EMPLOYER_NAME)
                    if (i == "blanks") xx$EMPLOYER_NAME <- trimws(gsub("[ ]+", " ", xx$EMPLOYER_NAME))
                }
                for (i in input$trailer){
                    pattern <- paste0(" ",i,"$")
                    if (grepl(pattern, input$EMPLOYER_NAME) == FALSE){
                        xx$EMPLOYER_NAME <- gsub(pattern, "", xx$EMPLOYER_NAME)
                    }
                }
            }
            if (input$empclean %in% c("Clean Both","Clean EMPLOYER_NAME2")){
                for (i in input$ignore){
                    if (i == "case")   xx$EMPLOYER_NAME2 <- toupper(xx$EMPLOYER_NAME2)
                    if (i == "comma")  xx$EMPLOYER_NAME2 <- gsub("[,]$", "", xx$EMPLOYER_NAME2)
                    if (i == "comma")  xx$EMPLOYER_NAME2 <- gsub("[,]", " ", xx$EMPLOYER_NAME2)
                    if (i == "period") xx$EMPLOYER_NAME2 <- gsub("[.]$", "", xx$EMPLOYER_NAME2)
                    if (i == "period") xx$EMPLOYER_NAME2 <- gsub("[.][ ]", " ", xx$EMPLOYER_NAME2)
                    if (i == "the")    xx$EMPLOYER_NAME2 <- gsub("^THE ", "", xx$EMPLOYER_NAME2)
                    if (i == "and")    xx$EMPLOYER_NAME2 <- gsub(" AND ", " & ", xx$EMPLOYER_NAME2)
                    if (i == "blanks") xx$EMPLOYER_NAME2 <- trimws(gsub("[ ]+", " ", xx$EMPLOYER_NAME2))
                }
                for (i in input$trailer){
                    xx$EMPLOYER_NAME2 <- gsub(paste0(" ",i,"$"), "", xx$EMPLOYER_NAME2)
                }
            }
            if (input$empname2na == "Set to EMPLOYER_NAME"){
                xx$EMPLOYER_NAME2[is.na(xx$EMPLOYER_NAME2)] <- xx$EMPLOYER_NAME[is.na(xx$EMPLOYER_NAME2)]
            }
            # Do searches
            cc <- get_ccdefs(ccfile)
            xsearch <- cc$label[cc$srch == 1]
            sfilter1 <- ""
            sfilter2 <- ""
            nblank = 0
            if (length(xsearch) > 0){
                for (i in 1:length(xsearch)){
                    pattern <- input[[xsearch[i]]]
                    if (nchar(pattern) > 0){
                        xx <- xx[grep(pattern, xx[[xsearch[i]]], ignore.case = TRUE),]
                        if (firstyear) trace <<- paste0(trace,"> Search ", xsearch[i], " for ", pattern,"\n")
                        sfilter1 <- paste0(sfilter1,", ", xsearch[i], "=", pattern)
                        if (nblank > 0) sfilter2 <- paste0(sfilter2,strrep("|",nblank),pattern)
                        else if (sfilter2 == "") sfilter2 <- pattern
                        else sfilter2 <- paste0(sfilter2,"|", pattern)
                        nblank <- 0
                    }
                    else nblank <- nblank + 1
                }
            }
            if (nchar(input$ysearch1) > 0){
                xx <- xx[grep(input$ysearch1, xx[[input$xsearch1]], ignore.case = TRUE),]  
                if (firstyear) trace <<- paste0(trace,"> Search ", input$xsearch1, " for ", input$ysearch1,"\n")
                sfilter1 <- paste0(sfilter1,", ", input$xsearch1, "=", input$ysearch1)
                sfilter2 <- paste0(sfilter2,"|", input$xsearch1, "=", input$ysearch1)
            }
            if (nchar(input$ysearch2) > 0){
                xx <- xx[grep(input$ysearch2, xx[[input$xsearch2]], ignore.case = TRUE),]  
                if (firstyear) trace <<- paste0(trace,"> Search ", input$xsearch2, " for ", input$ysearch2,"\n")
                sfilter1 <- paste0(sfilter1,", ", input$xsearch2, "=", input$ysearch2)
                sfilter2 <- paste0(sfilter2,"|", input$xsearch2, "=", input$ysearch2)
            }
            sfilter1 <- sub("^, ","",sfilter1)
            gsfilter1 <<- sfilter1
            gsfilter2 <<- sfilter2

            ngroup <- length(input$xgroup)
            return(xx)
        })
    }
)
