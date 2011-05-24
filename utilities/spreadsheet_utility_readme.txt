NOTE: Any use of trade, product or firm names is for descriptive purposes only and does
      not imply endorsement by the U.S. Government.

SUMMARY OF 'gsflowAnalysis.xls' SPREADSHEET FOR GSFLOW CSV FILE (Version 1.0)

HISTORY
      Released with GSFLOW Version 1.0.00 03/05/2008.

SYSTEM REQUIREMENTS
     This spreadsheet is programmed in Microsoft Excel 2003.
     
     The code has been used on personal computers running various forms of the Microsoft
     Windows operating system.

BACKGROUND
     The GSFLOW simulation model has an optional output file that contains a header
     record followed by a data lines consisting of 57 model calculated values for each time
     step of a GSFLOW simulation. The name of the output file is set using variable
     csv_output_file and is turned on or off using variable gsf_rpt, both specified in 
     the GSFLOW Control File. The first value of each data line is the time-step date in the
     form: month/day/year. The next 55 values are areally averaged (over the modeled
     domain), daily time series storagess and fluxes. These values can be used to check the
     water balance, compute statistics, generate plots, and so forth. The last value is the 
     number of iterations of the MODFLOW solution for the time step (variable KKITER).
     The number of values in this file may increase as GSFLOW is enhanced. The number
     of iterations will always be the last value and the order of variables will otherwise
     not be changed.
     
     Table 12 in the GSFLOW documentation report (USGS TM 6-D1) provides the name,
     description, and units for 55 values. Two values have been added before the number of
     MODFLOW iterations (KKITER). These are:

          Variable Name                Description                          Units
         basinfarfieldflow       Reserved for future development             N/A
         basinsoiltogw           Volumetric flow rate of direct gravity      L3/T
                                 drainage from excess capillary water
                                 to the unsaturated zone

     The format of this file is "comma separated values" (CSV) which makes it convenient 
     to load directly into an Microsoft Excel spreadsheet.
 
RUNNING THE SPREADSHEET   
     1. Using your browser, navigate to the "GSFLOW_1.0\utilities\" folder.
        Double click on "gsflowAnalysis.xls". The spreadsheet will appear.
        
     2. Go to (click on) the "data" worksheet. Worksheets are identified by their tabs
        across the bottom of the spreadsheet window. Scroll to the upper left corner of
        this worksheet.
        
     3. This spreadsheet has been preloaded with the CSV file created by a 16-year 
        simulation of the Sagehen example problem --
        "GSFLOWv1.1\data\sagehen\output-test\gsflow.csv". Use these data to evaluate
        the performance of GSFLOW or load a CSV file created by subsequent runs of
        GSFLOW by clicking on the "Load New gsflow.csv File" button on the "data"
        worksheet and select the desired CSV file in the Excel file browser. The
        data in this worksheet will be updated with the data from the loaded file.
        
     4. Click through the other worksheets and you will see the corresponding
         water-balance analysis. These are described below.
        
     5. You can add new worksheets (or modify existing ones) to come up with
        different analyses. If you find a bug or develop a worksheet that may be of
        interest to the GSFLOW community, please send the information to the
        GSFLOW development team (gsflowhelp@usgs.gov).

     6. WARNING: do not add/delete, rename, or rearrange columns and rows in the 
        worksheets because the computations in all worksheets are dependent on the
        row and column positions of other worksheets, especially the data worksheet.
        Additionally, do not delete or rename worksheets because data is loaded
        according to the name of the existing worksheets. 


     7. WARNING: the spreadsheet uses macros, thus you must allow macros to be
        enabled. One way is to set the macro security setting to medium. In Excel
        select Tools>>Options then Security tab, then Macro Security button.

     8. You can set the default directory for loading a CSV file in Excel by selecting
        Tools>>Options then General tab, then set the Default File location path. For
        example, you could set the Default File location to:
        C:\WRDAPP\GSFLOW_1.0\data\sagehen\output if GSFLOW was installed in that directory.
        
ET WATER BALANCE
     Click on the "total ET" worksheet tab. This worksheet sums all components of
     evapotranspiration and compares this sum to the areally averaged total.
     
HRU WATER BALANCE
     Click on the "hru wb" worksheet tab. This worksheet sums all inflows to and
     outflows from all Hydrologic Response Units (HRUs). The HRU water-balance error
     is the difference between this sum and the change in storage.
     
UZF WATER BALANCE
     Click on the "uzf wb" worksheet tab. This worksheet sums all inflows to
     and outflows from the Unsaturated-Zone Flow (UZF) Package in all MODFLOW
     finite-difference cells. The UZF water-balance error is the difference between
     this sum and the change in storage.

SATURATED ZONE WATER BALANCE
     Click on the "sat wb" worksheet tab. This worksheet sums all inflows to
     and outflows from the saturated zone in all MODFLOW finite-difference
     cells. The saturated zone water-balance error is the difference between
     this sum and the change in storage.
     
SURFACE-WATER RUNOFF WATER BALANCE
     Click on the "sroff" worksheet tab. This worksheet sums all components
     of surface runoff and compares this sum to of the areally averaged total.
     
SOIL-ZONE WATER BALANCE
     Click on the "soil zone wb" worksheet tab. This worksheet sums all inflows to
     and outflows from the soil-zone reservoir in all PRMS HRUs. The soil-zone, 
     water-balance error is the difference between this sum and the change in storage.
     
BASIN WATER BALANCE
     Click on the "basin wb" worksheet tab. This worksheet sums all inflows to
     and outflows from the entire coupled GSFLOW model. The basin water-
     balance error is the difference between this sum and the change in storage.
     
GSFLOW DOCUMENTATION
     Markstrom, S.L., Niswonger, R.G., Regan, R.S., Prudic, D.E., and Barlow, P.M.,
        2008, GSFLOW--Coupled ground-water and surface-water flow model based on
        the integration of the Precipitation-Runoff Modeling System (PRMS) and the
        Modular Ground-Water Flow Model (MODFLOW-2005): U.S. Geological Survey
        Techniques and Methods 6-D1, 240 p.
         
