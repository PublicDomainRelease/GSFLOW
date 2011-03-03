
                      Sagehen Sample Problem for GSFLOW
                                 February 2011
 
A sample problem with GSFLOW data sets is provided in this subdirectory
to verify that GSFLOW is correctly installed and running on the system.
The sample problem also may be looked at as an example of how to use the 
program. The sample GSFLOW model is for the Sagehen Creek Watershed, 
and is described in the GSFLOW documentation (Markstrom and others,
2008).

The input files for the problem consist of the following:
1. The GSFLOW Control File:
    File gsflow.control for the GSFLOW mode simulaton
    File gsflow_prms.control for the PRMS-only mode simulation
    File gsflow_modflow.control for the MODFLOW-only mode simulation
2. The PRMS Data File: ./input/sagehen.data
3. The PRMS Parameter Files (subdirectory input):
   a. original parameter files for pre-version 1.1.3 release:
    File sagehen_gsflow.params for the GSFLOW mode simulation
    File sagehen_prms.params for the PRMS-only mode simulation
   b. multiple parameter files for version 1.1.3 release:
    Files for GSFLOW model simulation: gsflow.params, gis.params,
      gvr.params, ncascade.params
    Files for PRMS model simulation: prms.params, gis.params,
      gvr.params, ncascade.params, ncascdgw.params
4. MODFLOW Name Files: GSFLOW mode: sagehen.nam; 
      MODFLOW-only: sagehen.mf.nam
5. The MODFLOW Input Files (all are located in the input subdirectory):
      sagehen.bas, sagehen.dis, sagehen.mf.dis, sagehen.gag, 
      sagehen.lpf, sagehen.mf.lpf, sagehen.oc, sagehen.pcg, 
      sagehen.sfr, and sagehen.uzf

Three Linux shell scripts (gsflow.sh, gsflow_prms.sh, gsflow_modflow.sh)
are included that can be used to execute the sample problem. File
gsflow.sh is used to run the full GSFLOW simulation (based on the
gsflow.control file); file gsflow_prms.sh is used to run the
PRMS-only simulation (based on the prms_only.control file); and 
file gsflow_modflow.sh is used to run the MODFLOW-only simulation 
(based on the gsflow_modflow.control file). In an xterm window type
one of the shell script file names to run the model. The sample
problem also can be run by typing the command ../../bin/gsflow in
an xterm window. 

Several output files are created by running the GSFLOW simulation,
which can be found in the ./output subdirectory. Each type of file
is described in detail in the GSFLOW documentation. Only one of the
output files is written to the user's current directory (gsflow.log).
All other output files are written to the ./output subdirectory.
These are:

1. The GSFLOW water-budget file: gsflow.out
2. The GSFLOW comma-separated-values (CSV) File: gsflow.csv
3. The PRMS water-budget file: sagehen_prms.out
4. The MODFLOW listing file: sagehen.mf.list
5. The calculated heads file for MODFLOW: head_sagehen.out
6. MODFLOW GAGE Package output for four stream reaches:
    Files sagehen_sfrseg13.out, sagehen_sfrseg15.out, 
    sagehen_sfrseg16.out, and sagehen_sfrseg17.out.
7. MODFLOW UZF Package output:
    Files uz1_sagehen.out, uz2_sagehen.out, uz3_sagehen.out, and
    uz4_sagehen.out

These results can be compared to those provided in the ./output-test 
subdirectory.

Reference:

Markstrom, S.L., Niswonger, R.G., Regan, R.S., Prudic, D.E., and
Barlow, P.M., 2008, GSFLOW--Coupled Ground-water and Surface-water
FLOW model based on the integration of the Precipitation-Runoff
Modeling System (PRMS) and the Modular Ground-Water Flow Model
(MODFLOW-2005): U.S. Geological Survey Techniques and Methods
6-D1, 240 p.
