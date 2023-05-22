cc <<- read.table("xcols/cols2023_Q2_0.txt", stringsAsFactors = FALSE, header = TRUE) # make global to debug

shinyUI(pageWithSidebar(
    headerPanel("LCA Disclosure Data"),
    sidebarPanel(
        width = 2,
        splitLayout(
            numericInput("minyear", "Start Year", min = 2000, max = 2023, value = 2023),
            numericInput("maxyear", "End Year", min = 2000, max = 2023, value = 2023)
        ),
        splitLayout(
            numericInput("minqtr", "Start Qtr", min = 1, max = 4, value = 1),
            numericInput("maxqtr", "End Qtr", min = 1, max = 4, value = 1)
        ),
        splitLayout(
            actionButton("submit", "Submit",
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            checkboxInput("groupyear", "Group year", value = FALSE)
        ),
        # List fields that can be searched
        textInput("CASE_STATUS",    "Search CASE_STATUS",    value = "CERTIFIED") ,
        textInput("EMPLOYER_NAME",  "Search EMPLOYER_NAME",  value = "^APPLE$") ,
        textInput("WORK_CITY",  "Search WORK_CITY",  value = "") ,
        textInput("WORK_STATE", "Search WORK_STATE", value = "") ,
        # List fields to be sorted by
        selectInput("xgroup", "Group by",
            choices = sort(cc$label[cc$grp == 1]),
            selected = "", multiple = TRUE),
        checkboxInput("xsubgroups", "Show worker subgroups", value = FALSE),
        selectInput("xsort", "Sort by",
            choices = sort(c(cc$label[cc$sort == 1],"APPLICATIONS","SALARY","PSALARY","WAGE_PWAGE")),
            selected = "TOTAL_WORKERS", multiple = FALSE), 
        radioButtons("xsortdir", NULL, c("Ascending","Descending"), "Descending", inline = TRUE),
        selectInput("xsearch1", "Search selected fields",
                    choices = cc$label[cc$csv == 1],
                    selected = ""),
        textInput("ysearch1", NULL, value = ""),
        selectInput("xsearch2", NULL,
                    choices = cc$label[cc$csv == 1],
                    selected = ""),
        textInput("ysearch2", NULL, value = ""),
        checkboxGroupInput("xsalary", "Salary",
            choices = c("YEAR","HOUR","OTHER"),
            selected = c("YEAR","HOUR","OTHER"),
            inline = TRUE),
        # List choice and selected checkboxes for selecting which fields to display
        checkboxGroupInput("xshow", "Show",
            choices = cc$label[cc$csv == 1],
            selected = cc$label[cc$show == 1],
            inline = TRUE),
        selectInput("xshow2", "Show (other)",
            choices = c(cc$label[cc$csv == 1],"SALARY","PSALARY","WAGE_PWAGE"),
            selected = "WAGE_PWAGE", multiple = TRUE),
        # List options for maximum column width, total width, and total rows
        numericInput("colwidth", "Maximum Column Width", value = "40") ,
        numericInput("totwidth", "Maximum Total Width",  value = "240") ,
        numericInput("totrows",  "Maximum Total Rows",   value = "900"),
        # Rules to combine employer names
        splitLayout(
            textInput("encoding", "Encoding", value = "#latin1"), #set latin1 for shinyapps.io
            checkboxInput("cache", "Cache data", value = TRUE)   #set FALSE for limited memory
        ),
        selectInput("empname2na", "EMPLOYER_NAME2 NAs",
                    choices = c("Remove","Retain","Set to EMPLOYER_NAME"),
                    selected = "Remove"),
        selectInput("empclean", "Clean EMPLOYER_NAMEs",
                    choices = c("Clean None","Clean EMPLOYER_NAME","Clean EMPLOYER_NAME2","Clean Both"),
                    selected = "Clean Both"),
        checkboxGroupInput("ignore","Ignore in Employer",
                           choices  = c("case","comma","period","blanks","the","and"),
                           selected = c("case","comma","period","blanks","the","and"), inline = "TRUE"),
        # Select all but BANK and GROUP by default
        selectInput("trailer", "Delete Trailer in Employer",
                    choices = c("INC","INCORPORATED","LLC","LLP","LTD","LIMITED","N A","NA","GROUP",
                                "& CO","& COMPANY", "CORPORATE SERVICES","FINANCIAL SERVICES","BANK",
                                "CO","COMPANY","CORP","CORPORATION",
                                "FINANCIAL SERVICES GROUP","TRAVEL RELATED SERVICES",
                                "SERVICES","TECHNOLOGY"),
                    selected = c("INC","INCORPORATED","LLC","LLP","LTD","LIMITED","N A","NA",
                                "& CO","& COMPANY", "CORPORATE SERVICES","FINANCIAL SERVICES",
                                "CO","COMPANY","CORP","CORPORATION",
                                "FINANCIAL SERVICES GROUP","TRAVEL RELATED SERVICES",
                                "SERVICES","TECHNOLOGY"),
                    multiple = TRUE) ),
        mainPanel(
        div(
            tabsetPanel(id = "tabs",
                tabPanel("Output",
                    width = 10,
                    verbatimTextOutput("myText")
                ),
                tabPanel("Distribution",
                         sidebarPanel(
                             width = 2,
                             checkboxInput("stitle3", "Short title", value = FALSE),
                             h4(HTML("<b>UNGROUPED</b>")),
                             selectInput("uxvar", "X Variable",
                                         choices = c(cc$label[cc$csv == 1],"SALARY","PSALARY","WAGE_PWAGE"),
                                         selected = "WAGE_PWAGE", multiple = FALSE),
                             selectInput("uyvar", "Y Variable",
                                         choices = c("APPLICATIONS","TOTAL_WORKERS",
                                                     "NEW_EMPLOYMENT","CONTINUED_EMP","CHANGE_PREV_EMP",
                                                     "NEW_CONCUR_EMP","CHANGE_EMPLOYER","AMENDED_PETITION"),
                                         selected = "APPLICATIONS", multiple = FALSE),
                             numericInput("binwidth", "Bins / Binwidth(-)", 0),
                             textInput("ucolor", "Color", value = "red"),
                             textInput("uxscale", "X From,To,Step,Tick", value = ""),
                             textInput("uyscale", "Y From,To,Step,Tick", value = ""),
                             checkboxInput("uplotk", "Scale Y in thousands", value = FALSE),
                             h4(HTML("<b>GROUPED</b>")),
                             checkboxGroupInput("gopts",NULL,
                                                choices  = c("Workers","Vertical","Reverse","Alpha"),
                                                selected = c("Workers","Vertical"), inline="TRUE"),
                             numericInput("topn", "Top N", 10, min = 1),
                             textInput("gcolor", "Color", value = "red"),
                         ),
                         mainPanel(
                             width = 8,
                             plotOutput(outputId = "myDistribution")
                         )
                ),
                tabPanel("Plot Year",
                    sidebarPanel(
                        width = 2,
                        checkboxInput("stitle", "Short title", value = FALSE),
                        textInput("pcolor", "Line color", value = "red"),
                        textInput("xscale", "X From,To,Step,Tick", value = ""),
                        textInput("yscale", "Y From,To,Step,Tick", value = ""),
                        checkboxInput("plotk", "Scale Y in thousands", value = FALSE)
                    ),
                    mainPanel(
                        width = 8,
                        plotOutput(outputId = "myPlotYear")
                    )
                ),
                tabPanel("Plot Groups",
                    sidebarPanel(
                        width = 2,
                        checkboxInput("stitle2", "Short title", value = FALSE),
                        textInput("pcolor2", "Line color", value = "red"),
                        textInput("xscale2", "X From,To,Step,Tick", value = ""),
                        textInput("yscale2", "Y From,To,Step,Tick", value = ""),
                        numericInput("ngroups2", "N Groups", min = 1, value = 6),
                        numericInput("baseyear2", "Base Year (0=all)", min = 0, value = 0),
                        checkboxInput("plotk2", "Scale Y in thousands", value = FALSE)
                    ),
                    mainPanel(
                        width = 8,
                        plotOutput(outputId = "myPlotGroups")
                    )
                ),
                tabPanel("Usage",
                    width = 10,
                    htmlOutput(outputId = "myUsage")
                )
            )
        ),
        width = 10)
    )
)