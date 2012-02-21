

                      GSFLOW - Version: 1.1.5
          Coupled Groundwater and Surface-water FLOW model


NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

GSFLOW version 1.1.5 is packaged for personal computers using one of the 
Microsoft Windows operating systems. Two executable files compiled for 32-bit
and 64-bit operating systems are provided, as well as the source code. The
64-bit operating systems provide more random access memory (RAM) than do
32-bit systems, and may execute simulations in less time than 32-bit systems.
Also, large simulations may not run on a 32-bit operating system because of
limitations in the amount of available RAM. It is recommended that the 64-bit 
executable be used on computers running a 64-bit operating system. The 32-bit
version is provided for those who do not have a 64-bit Windows operating
system.

The executables were compiled on a personal computer with the Intel(R) Core(TM)
2Duo CPU T9500 chipset, running the Microsoft Windows 7 operating system,
using the Microsoft Visual Studio 2008 Version 9.0.21022.8 development 
environment and the Intel(R) Visual Fortran Version 11.1.054 and Microsoft 
Visual C++ 2008 91851-136-7089065-60512 compilers.

The source code and Linux Makefiles are provided to aid users in compilation
on other computers. However, no support is provided for compilation.

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

The following self-extracting distribution file is for use on personal
computers:

         gsflowv1_1_5.exe

The distribution file contains:

          Executable and source code for GSFLOW.
          GSFLOW documentation.
          Related documentation for PRMS, MODFLOW, and MODFLOW-NWT.
          A GSFLOW example problem.
          An Excel spreadsheet for analysis of GSFLOW results.

The distribution file is a self-extracting program for use on personal
computers running Windows operating systems.  Execution of the 
distribution file creates numerous individual files contained in several
directories. The extraction program allows you to specify the directory
in which the files should be restored.  The default installation 
directory is C:\WRDAPP.  You have the opportunity to specify an alternate 
installation directory during extraction of the software. The following 
directory structure will be created in the installation directory:


   |
   |--GSFLOW_1.1.5
   |    |--bin           ; Compiled GSFLOW executables for personal computers
   |    |--data
   |         |--sagehen  ; Input and output files for a GSFLOW sample problem
   |    |--doc           ; Written documentation
   |        |--GSFLOW    ; GSFLOW documentation report and additional 
   |                       references
   |        |--MODFLOW   ; MODFLOW-2005 and MODFLOW-NWT documentation reports
   |                       and additional references for the SFR2 and UZF 
                           Packages 
   |        |--PRMS      ; PRMS and MMS documentation reports 
   |    |--src
   |        |--gsflow    ; Source code for GSFLOW Modules
   |        |--mms       ; Source code for MMS software
   |        |--modflow   ; Source code for MODFLOW-2005 and MODFLOW-NWT 
                           Packages
   |        |--prms      ; Source code for PRMS Modules
   |    |--utilities     ; Utility program for analysis of GSFLOW output


It is recommended that no user files are kept in the GSFLOW_1.1.5 directory
structure.  If you do plan to put your own files in the GSFLOW_1.1.5
directory structure, do so only by creating additional subdirectories of
the GSFLOW_1.1.5\data subdirectory.

Included in directory GSFLOW_1.1.5\doc\GSFLOW is the GSFLOW documentation 
report, which is a Portable Document Format (PDF) file. The PDF file is 
readable and printable on various computer platforms using Acrobat Reader 
from Adobe. The Acrobat Reader is freely available from the following World
Wide Web site: http://www.adobe.com/


B. INSTALLING

To make the executable version of GSFLOW accessible from any
directory, the directory containing the executable (GSFLOW_1.1.5\bin)
should be included in the PATH environment variable. Also, if a prior release 
of GSFLOW is installed on your system, the directory containing the executables 
for the prior release should be removed from the PATH environment variable.
  
As an alternative, the executable files in the GSFLOW_1.1.5\bin directory 
can be copied into a directory already included in the PATH environment 
variable. The sample problem provided with the release (described below)
has sample batch files that provide an alternative, additional approach for
accessing the executable files.


C. EXECUTING THE SOFTWARE

A 32-bit (gsflow.exe) and a 64-bit (gsflow_64.exe) executable are provided
in the GSFLOW_1.1.5\bin directory. After the GSFLOW_1.1.5\bin directory is
included in your PATH, GSFLOW is initiated in a Windows Command-Prompt window 
using the commands:

      gsflow.exe [Fname]

or
      gsflow_64.exe [Fname]


The optional Fname argument is the name of the GSFLOW Control File.  If 
no argument is used, then GSFLOW will look for a Control File named 
"control" in the user’s current directory.

The arrays in GSFLOW are dynamically allocated, so models are not limited
by the size of input data. However, it is best to have at least 4 MB of 
random-access memory (RAM) for model execution and more RAM for large models.
If there is less available RAM than the model requires, which depends
on the size of the application, the program will use virtual memory; however,
this can slow execution significantly. If there is insufficient memory to 
run the model, then GSFLOW will not initiate the beginning of the simulation; 
however, the Windows Command-Prompt window may continue to indicate that 
GSFLOW is executing. For this circumstance, the program must be terminated 
manually using the Windows Task Manager application.

Some of the files written by GSFLOW are unformatted files. The structure
of these files depends on the compiler and options in the code. For Windows
based computers, GSFLOW is compiled with the unformatted file type specified
as "BINARY". Any program that reads the unformatted files produced by GSFLOW 
must be compiled with a compiler that produces programs that use the same 
structure for unformatted files.  For example, Zonebudget and Modpath use 
unformatted budget files produced by the MODFLOW component of GSFLOW. Another 
example are head files that are generated by one GSFLOW simulation and used 
in a following simulation as initial heads. Both simulations must be run 
using an executable version of GSFLOW that uses the same unformatted file 
structure.


D. TESTING

An example problem with GSFLOW data sets is provided to verify that GSFLOW 
is correctly installed and running on the system.  The example problem may 
also be looked at as an example of how to use the program. A description 
of the file structure for the example problem and of directions for
running the example problem are described in the 'Readme.sagehen.txt' file 
located in directory GSFLOW_1.1.5\data\sagehen.


E. COMPILING

The executable file provided in GSFLOW_1.1.5\bin was created using the Intel
Visual Fortran and Microsoft Visual C++ compilers.  Although executable
versions of the program are provided, the source code also is provided in
the GSFLOW_1.1.5\src directory so that GSFLOW can be recompiled if
necessary.  However, the USGS cannot provide assistance to those compiling
GSFLOW. In general, the requirements are a Fortran compiler, a compatible
C compiler, and the knowledge of using the compilers. Makefiles are included 
in the GSFLOW_1.1.5\src directories as an example for compiling GSFLOW.
