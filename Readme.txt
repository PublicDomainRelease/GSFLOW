

                      GSFLOW - Version: 1.1.3
          Coupled Groundwater and Surface-water FLOW model


NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

GSFLOW version 1.1.3 is packaged for personal computers using one of the 
or Unix-based operating system. An executable file is provided as well
as the source code.

IMPORTANT: Users should review the file Summary_gsflow.txt for a description
of, and references for, this software. Users should also review the file 
release.txt, which describes changes that have been introduced into GSFLOW
with each official release; these changes may substantially affect users.

Instructions for installation, execution, and testing of this version of
GSFLOW are provided below.



                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING THE SOFTWARE
                         D. TESTING
                         E. COMPILING


A. DISTRIBUTION FILE

The following compressed distribution file is for use on personal computers:

         gsflowv1_1_3.tar.gz

The distribution file contains:

          Executable and source code for GSFLOW.
          GSFLOW documentation.
          Related documentation for PRMS and MODFLOW.
          A GSFLOW example problem.
          An Excel spreadsheet for analysis of GSFLOW results.

Place the tar file in the directory under which you want to install GSFLOW.
The following commands can be used to extract GSFLOW:

    gunzip GSFLOWv1_1_3.tar.gz
    tar -xvf GSFLOWv1_1_3.tar

The tar command will extract the directory GSFLOWv1.1.3 that contains
numerous individual files.

The following directory structure will be created in the installation
directory:


   |
   |--GSFLOW_1.1.3
   |    |--bin           ; Compiled GSFLOW executable for personal computers
   |    |--data
   |         |--sagehen  ; Input and output files for a GSFLOW sample problem
   |    |--doc           ; Written documentation
   |        |--GSFLOW    ; GSFLOW docmentation report and additional 
   |                       references
   |        |--MODFLOW   ; MODFLOW-2005 documentation report and additional
   |                       references for the SFR2 and UZF Packages 
   |        |--PRMS      ; PRMS and MMS documentation reports 
   |    |--src
   |        |--gsflow    ; Source code for GSFLOW Modules
   |        |--mms       ; Source code for MMS software
   |        |--modflow   ; Source code for MODFLOW-2005 Packages
   |        |--prms      ; Source code for PRMS Modules
   |    |--utilities     ; Utility program for analysis of GSFLOW output


It is recommended that no user files are kept in the GSFLOW_1.1.3 directory
structure.  If you do plan to put your own files in the GSFLOW_1.1.3
directory structure, do so only by creating additional subdirectories of
the GSFLOW_1.1.3/data subdirectory.

Included in directory GSFLOW_1.1.3/doc/GSFLOW is the GSFLOW documentation 
report, which is a Portable Document Format (PDF) file. The PDF file is 
readable and printable on various computer platforms using Acrobat Reader 
from Adobe. The Acrobat Reader is freely available from the following World
Wide Web site:
      http://www.adobe.com/


B. INSTALLING

To make the executable version of GSFLOW accessible from any
directory, the directory containing the executable (GSFLOW_1.1.3/bin)
should be included in the PATH environment variable (see explanation below).  

As an alternative, the executable file, gsflow.exe, in the
GSFLOW_1.1.3/bin directory can be copied into a directory already
included in the PATH environment variable found in your Linux
profile and shell resource file.

C. EXECUTING THE SOFTWARE

After the executable file in the GSFLOW_1.1.3/bin directory is installed in
a directory that is included in your PATH, GSFLOW is initiated in
in an xterm command window using the command:

          gsflow [Fname]

The optional Fname argument is the name of the GSFLOW Control File.  If 
no argument is used, then GSFLOW will look for a Control File named 
"control" in the user's current directory.

The data arrays in GSFLOW are dynamically allocated, so models are not
limited by hard-coded array limits. However, it is best to have at least 
2 MB of random-access memory (RAM) available to hold all of the required
data.  If there is less available RAM than the model requires, which depends
on the size of the application, the program will use virtual memory; however,
this can slow execution significantly. If there is insufficient random 
memory to run the model, then GSFLOW will not initiate the beginning of 
the simulation.

Some of the files written by GSFLOW are unformatted files. The structure
of these files depends on the compiler and options in the Fortran write
statement.  GSFLOW is compiled with the unformatted file type specified
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
also be looked at as an example of how to use the program. A description 
of the file structure for the example problem and of directions for
running the example problem are described in the 'Readme.sagehen.txt' file 
located in directory GSFLOW_1.1.3/data/sagehen.


E. COMPILING

The executable file provided in GSFLOW_1.1.3/bin was was compiled on a
personal computer running Mandriva Linux with the GNU gfortran and gcc
compilers, version 4.4.3, target i586-manbo-linux-gnu. Although an executable
version of the program is provided, the source code also is provided in the
GSFLOW_1.1.3/src directory so that GSFLOW can be recompiled if
necessary. However, the USGS cannot provide assistance to those 
compiling GSFLOW. In general, the requirements are a Fortran compiler, a 
compatible C compiler, and the knowledge of using the compilers. Makefiles 
are included in the GSFLOW_1.1.3/src directories as an example for compiling
GSFLOW.

