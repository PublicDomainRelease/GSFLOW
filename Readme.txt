

                      GSFLOW - Version: 1.0.00
          Coupled Ground-water and Surface-water FLOW model


NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

This version of GSFLOW is packaged for personal computers using the Linux
operating system.  An executable file is provided as well as the source code.
The executable file was generated on a personal computer, running KDE Linux
Relase 3.5.6, using the GNU gfortran and gcc compilers, version 4.3.0, target
i386-pc-lunux-gnu. The source code and Makefiles are provided to aid users in
compilation on other computers. However, no support is provided for compilation.

Instructions for installation, execution, and testing of this version of
GSFLOW are provided below.



                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING THE SOFTWARE
                         D. TESTING
                         E. COMPILING


A. DISTRIBUTION FILE

The following compressed tar file is for use on linux-based personal computers:

         gsflowv1_0.tar.gz

The distribution file contains:

          Executable and source code for GSFLOW.
          GSFLOW documentation.
          Related documentation for PRMS and MODFLOW.
          A GSFLOW example problem.
          An Excel spreadsheet for analysis of GSFLOW results.

Extraction of the tar file creates numerous individual files. Place the tar
file in a directory under which you want to install GSFLOW. The following
commands can be used to then extract GSFLOW:

gunzip gsflowv1_0.tar.gz
tar xvf gsflowv1_0.tar

The following directory structure will be created in users current directory:


   |
   |--GSFLOW_1.0
   |    |--bin           ; Compiled GSFLOW executable for personal computers
   |    |--data
   |         |--sagehen  ; Input and output files for a GSFLOW sample problem
   |    |--doc           ; Written information
   |        |--gsflow    ; GSFLOW docmentation report and conference paper
   |        |--modflow   ; MODFLOW docmentation reports for MODFLOW-2005, SFR,
   |                        and UZF and update information 
   |        |--prms      ; PRMS docmentation 
   |    |--src
   |        |--gsflow    ; Source code for GSFLOW Modules
   |        |--mms       ; Source code for MMS software
   |        |--modflow   ; Source code for MODFLOW-2005 Packages
   |        |--prms      ; Source code for PRMS Modules
   |    |--utilities     ; Utility programs for analysis of GSFLOW output
                         ; and .bat file for running sample problem


It is recommended that no user files are kept in the GSFLOW_1.0 directory
structure.  If you do plan to put your own files in the GSFLOW_1.0
directory structure, do so only by creating additional subdirectories of
the GSFLOW_1.0\data subdirectory.

Included in directory GSFLOW_1.0\doc\gsflow is the GSFLOW documentation report, 
which is a Portable Document Format (PDF) file. The PDF file is readable
and printable on various computer platforms using Acrobat Reader from Adobe.
The Acrobat Reader is freely available from the following World Wide Web
site:
      http://www.adobe.com/




B. INSTALLING

To make the executable version of GSFLOW accessible from any
directory, the directory containing the executable (GSFLOW_1.0\bin)
should be included in the PATH environment variable.  

As an alternative, the executable file, gsflow, in the
GSFLOW_1.0\bin directory can be copied into a directory already
included in the PATH environment variable.
 


C. EXECUTING THE SOFTWARE

After the executable file in the GSFLOW_1.0\bin directory is installed in
a directory that is included in your PATH, GSFLOW is initiated in
a Windows Command-Prompt window using the command:

          gsflow [Fname]

The optional Fname argument is the name of the GSFLOW Control File.  If 
no argument is used, then GSFLOW will look for a Control File named 
"gsflow.control" in the user's current directory.

The data arrays in GSFLOW are dynamically allocated, so models are not
limited by hard-coded array limits. However, it is best to have at least 
2MB of random-access memory (RAM) available to hold all of the required
data.  If there is less available RAM than the model requires, which depends
on the size of the application, the program will use virtual memory; however,
this can slow execution significantly. 


Some of the files written by GSFLOW are unformatted files.  The structure
of these files depends on the compiler and options in the Fortran write
statement.  GSFLOW is compiled using with the unformatted file type specified
as "UNFORMATTED". Any program that reads the unformatted files produced by
GSFLOW must be compiled with a compiler that produces programs that
use the same structure for unformatted files.  For example, Zonebudget
and Modpath use unformatted budget files produced by the MODFLOW component
of GSFLOW.  Another example is head files that are generated by one GSFLOW
simulation and used in a following simulation as initial heads.  Both 
simulations must be run using an executable version of GSFLOW that uses 
the same unformatted file structure.


D. TESTING

An example problem with GSFLOW data sets is provided to verify that GSFLOW 
is correctly installed and running on the system.  The example problem may 
also be looked at as an example of how to use the program.  The directory
GSFLOW_1.0\data\sagehen\input contains the input data and 
GSFLOW_1.0\data\sagehen\output-test contains the output files for the Sagehen
Creek Watershed problem described in the GSFLOW documentation report
(USGS Techniques and Methods 6-D1).
 
A shell script file is included in the GSFLOW_1.0\utilities directory that
can be used to excute the example problem simulation without destroying the 
original results in the GSFLOW_1.0\data\sagehen\output-test directory.  
GSFLOW_1.0\data\sagehen contains GSFLOW Control File (gsflow.control) and 
MODFLOW Name File (sagehen.nam) for running the example problem.  The example
problem can be run by going to the GSFLOW_1.0\data\sagehen directory and typing
the command ..\..\bin/gsflow.



E. COMPILING

The executable file provided in GSFLOW_1.0\bin was created using the GNU
Fortran (gfortran) and C (gcc) compilers, version 4.3.0.  Although an
executable version of the program is provided, the source code is provided
in the GSFLOW_1.0\src directory so that GSFLOW can be recompiled if
necessary.  However, the USGS cannot provide assistance to those compiling
GSFLOW. In general, the requirements are a Fortran compiler, a compatible
C compiler, and the knowledge of using the compilers. Makefiles are included
in the GSFLOW_1.0\src directories as an example for compiling GSFLOW.




