# lca1
#### Analyzes Disclosure Data for Labor Condition Applications from Department of Labor

This application generates intermediate data files from the original LCA Disclosure Files posted by the Department of Labor at
https://www.dol.gov/agencies/eta/foreign-labor/performance#dis .  Clicking on the Disclosure Data tab will display a number of links.
The ones for the LCA Disclosure Files used by the application are as follows:

> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/LCA_Disclosure_Data_FY2022_Q2.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/LCA_Disclosure_Data_FY2021_Q1.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/LCA_Disclosure_Data_FY2021_Q2.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/LCA_Disclosure_Data_FY2021_Q3.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/LCA_Disclosure_Data_FY2021_Q4.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/LCA_Disclosure_Data_FY2020_Q1.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/LCA_Disclosure_Data_FY2020_Q2.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/LCA_Disclosure_Data_FY2020_Q3.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/LCA_Disclosure_Data_FY2020_Q4.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-1B_Disclosure_Data_FY2019.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-1B_Disclosure_Data_FY2018_EOY.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-1B_Disclosure_Data_FY17.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-1B_Disclosure_Data_FY16.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-1B_Disclosure_Data_FY15_Q4.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-1B_FY14_Q4.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/LCA_FY2013.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/LCA_FY2012_Q4.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-1B_iCert_LCA_FY2011_Q4.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-1B_FY2010.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-1B_Case_Data_FY2009.xlsx
> https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/H-1B_Case_Data_FY2008.xlsx

These xlsx files load very slowly so the application creates corresponding csv (comma-separated values) files the first time that the
xlsx files are loaded and will use only the csv files after that.  To create the csv files, create an input and data directory and put
all of the xlsx files into the input directory.  The application should then write the corresponding csv file into the data directory
after each xlsx file is read for the first time.  Once all of the csv files are created, the input directory and xlsx files can be
removed if desired.

A couple of items specific to these files should be noted.  Due to the problem described at https://econdataus.com/lca1.htm#emp9qtrs ,
only the Q3 and Q4 files are needed for 2021 and are created by default.  The Q1 and Q2 file should be created by temporarily setting
"Start Year" and "End Year" to 2021, setting "Start Qtr" to 1 and "End Qtr" to 2, and hitting the Submit button.  "End Qtr" should then
be set back to 4.  Also, the Department of Labor website currently only has a file for Q2.  The file name needs to be modified to end
with "Q1" instead of "Q2" as the application currently does not expect for there to be any missing quarters.  A fair amount of memory
may be required to generate these files.  To create and hold all of them in memory may take up to 16 gigabytes in combined memory and
hard drive space.  However, it may be possible to generate them one at a time with less memory.
