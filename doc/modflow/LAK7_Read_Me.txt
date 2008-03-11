LAK7_Read_Me.txt

Modifications to the Lake(LAK3) Package since it was documented by
Merritt and Konikow (2000). The modifications are listed starting from
the most recent.

LAK3 Package updates (May-July 2006): Modifications by D.E. Prudic and R.G.
Niswonger

	An important change was made in the computation of lake stage for
steady state and transient simulations as described in the documentation report
for the Lake(LAK3) Package (Merritt and Konikow, 2000). The Newton iteration
loop used to calculate the steady-state lake stage (equation 12 in Merritt
and Konikow, p. 12) was revised to include the calculation of outflow to
streams and its derivative with respect to lake stage. Previously, lake outflow
and its derivative were computed in the Streamflow-Routing (SFR1 or SFR2)
Package, which did not allow the outflow and its derivative to change as the
lake stage changed in the Newton iteration within the Lake Package (Prudic and
others, 2004 and Niswonger and Prudic, 2005). The Streamflow-Routing
Package(SFR2) also was modified to be compatible with these most recent
changes in the Lake Package.

	The Lake Package did not iterate internally to calculate lake stage
during transient simulations. The original method of calculating transient lake
stages was based on equation 10 of Merritt and Konikow (2000, p. 9), which
relied on the lake outflow from the previous MODFLOW iteration. This approach
resulted in non-convergences in ground-water head during transient simulations
caused by oscillations in lake stage and lake outflow to streams. This problem
did not necessarily cause non-convergence in ground-water cells connected to
lake cells. Rather, oscillations in lake outflow could cause non-convergence in
MODFLOW cells that interacted with stream reaches downstream of the lake
outflow. To fix this problem, the computation of lake stage for transient
simulations was changed to solve for lake stage using equation 12 of Merritt
and Konikow (2000, p.9) such that lake outflow to streams and its derivative
are now calculated within the Lake(LAK3)Package Formulate Module during
transient simulations.

	Although the data input structure was not altered, the new method
of solving for lake stage uses only the time-weighting factor THETA
(Merritt and Konikow, 2000, p. 52) for transient simulations. THETA is
automatically set to a value of 1.0 for all steady-state stress periods.
THETA for transient simulations has been revised and is now limited to
range from 0.5 to 1.0. A value of 0.5 represents the stage midway between
the previous time step and the end of the current time step. A value of 1.0
(fully implicit) represents the lake stage at the end of the current time step.
A THETA of less than 0.5 does not perform well and a zero value is undefined in
the Newton iteration method. Slight errors in the solution of lake stage and
seepage may result when THETA is greater than 0.5 (Fread, 1993). Values greater
than 0.5 have been recommended for damping oscillations in streamflow-routing
equations (Fread, 1993). A value of 0.5 represents a semi-implicit method that
is often called Crank-Nicolson (Wang and Anderson, 1982, p. 81). Wang and
Anderson present results for drawdown for a simple confined aquifer with
pumping in which the Crank-Nicolson method produced the best results when
compared with the Theis solution for different times. A value of 0.5 is
generally recommended.

	Test simulation 1 presented by Merritt and Konikow (2000, p. 21-23) was
used to test different values of THETA for a transient simulation using the
original method of computing lake stage and the revised method. Three
simulations were done using the original method (THETA=0.0, 0.5, and 1.0). A
THETA of 0.5 produced changes in lake stage and ground inflow and outflow that
were midway between those computed using THETA of 0.0 and 1.0. The maximum
difference in lake stage from that computed using a THETA of 0.5 was 0.324 feet
at time steps 38 and 39. The percent difference with respect to the total lake
stage was 0.25 percent. The maximum difference in lake inflow was 3,400 cubic
feet per day at time steps 21 and 22 or 2.3 percent different than the lake
inflow when THETA was 0.5. The greatest percent difference was 2.66 percent at
time step 57.
 
	Two simulations were done using the new method of iterating on lake
stage during a MODFLOW iteration using the Newton method. The results for
THETA = 0.5 and 1.0 were identical to those of the original method. All
solutions resulted in the same lake stage and lake inflow at the end of the
simulation. A THETA of 1.0 decreased the number of Newton iterations. However,
the total number of MODFLOW iterations increased.

	If the model simulation includes a steady-state stress period, then the
number of iterations (NSSITER) and the closure tolerance (SSCNCR) defined by
Merritt and Konikow (2000, p. 52) for ending the Newton iteration method is
used for all subsequent transient or steady-state stress periods. If the model
simulation only includes transient stress periods, a default value of 0.0001 is
assigned to SSCNCR and a default value of 100 is assigned to NSSITR. An option
was created in which values of SSCNCR and NSSITR can be read for a transient
only simulation by placing a negative sign immeditately in front of THETA. A
negative THETA sets a flag which assumes input values for NSSITR and SSCNCR will
follow THETA in the format as described by Merritt and Konikow (p. 52). A
negative THETA is automatically reset to a positive value after values of 
NSSITR and SSCNCR are read.

	The revised Newton method may not always find a solution to the lake
stage because of discontinuities among lake stage, area, and outflow. This is
particularly so for steady-state simulations of lakes with stream outflow. A
warning statement is printed whenever the Newton method can not find a
solution within NSSITR iterations for the stage of a particular lake. The
printed warning statement also lists the last two lake stages prior to ending
the Newton iteration loop in the Formulate Module of the Lake Package.

	The results using the new solution method may differ from the results
using the old method. The differences will be most evident for previous
simulations that have lake outflow to one or more streams or when a fully
explicit time weighting (THETA = 0.0) was used for transient simulations. 
Differences in results of transient simulations when THETA was fully explicit
will be dependent on the time steps used in the simulation.

	The water budget of each lake also was revised to continue accounting 
for all inflows and outflows while a lake is dry. The method of filling a 
dry lake was also changed. All surface water inflows to a lake are summed 
prior to computing lake seepage losses and lake outflow to streams. The inflow
volume of a dry lake is first checked against the specified withdrawals and 
then against lake evaporation. IF the combination of these values exceed inflow
they are reduced to equal the surface inflow. Any excess surface inflow is
then available to limit the seepage from the lake to ground water. Ground-water
seepage to the lake is added to surface inflow through each Newton iteration 
and compared with the specified lake evaporation rate. If the revised total
inflow exceeds the specified evaporation rate times the area of the lowest
elevation of the lake bottom, then the lake is allowed to fill on the basis
of the lakebed conductances and the difference in head between lake and ground
water. The rate of filling of a dry lake will be dependent on the elevation
of the outlet elevation of the stream or streams. A stream outlet cannot be
less than the elevation of the lowest part of the lake otherwise the Newton
method will not solve properly.    


Modifications to the Lake(LAK3) Package by L.F. Konikow (February 2006):

	Several minor coding changes were made during the past year
to fix bugs and improve allocation accuracy.  This required that
the unit number for the GAGE Package be passed into the LAK3 allocation
subroutine when called from main.  Also, LSKLK and LSDONE are now
allocated space in the IR array rather than into the RX array.  A bug in
the LAK3 budget subroutine was fixed (previously, in cases in which a
lake that had gone dry was underlain by a confined type of layer, an
undefined variable caused a run-time error).


Modifications to the Lake(LAK3) Package by M.L. Merritt and L.F. Konikow
(Oct. 2003 and Jan. 2004):

	The Lake Package has been modified to enable it to run with (1)
mixed steady-state and transient stress periods, and (2) the top layer being
a confined layer.  Neither of these conditions was previously allowed in a
simulation using the Lake Package.

(1) For mixed steady-state/transient runs, NSSITR and SSCNCR must be defined
in Record 2 in the input file for the Lake Package, even if the steady-state
stress period is not the first one.  If more than one steady-state stress
period is included in the total simulation period, then the initial values
of NSSITR and SSCNCR will apply to all subsequent steady-state stress
periods.  If NSSITR and SSCNCR are needed and read in as a value of zero,
then default values of NSSITR = 50 and SSCNCR = 0.01 will automatically
override the zero values.

	The capability for using mixed steady-state/transient runs with the
Lake Package does not depend on the sequence of steady-state and transient
stress periods.  Input of minimum and maximum lake stages is as before (in
Record 3) if the first stress period is steady-state (this assures
compatibility with older data sets).  If the second or a subsequent stress
period is steady-state, then the minimum and maximum stages (SSMN and SSMX)
for those stress periods are defined at the end of Record 9a.  With this new
option, the definition for Record 9a on p. 55 of Merritt and Konikow (2000)
is modified to:

Record 9a.  Data: PRCPLK EVAPLK RNF WTHDRW {SSMN} {SSMX}

	Where SSMN and SSMX are optional parameters that are only defined
for a steady-state stress period that is not the first stress period in the
simulation.

(2) The Lake package can now be used when all or some of the model layers
containing the lake are confined.  We recommend using the Layer-Property
Flow Package (LPF) for this case, although the BCF and HUF Packages will
work too.  However, when using the BCF6 package to define aquifer
properties, lake/aquifer conductances in the lateral direction are based
solely on the lakebed leakance (and not on the lateral transmissivity of the
aquifer layer).  As before, when the BCF6 package is used, vertical
lake/aquifer conductances are based on lakebed conductance and on the
vertical hydraulic conductivity of the aquifer layer underlying the lake
when the wet/dry option is implemented, and only on the lakebed leakance
when the wet/dry option is not implemented.

(3) Other minor changes to output formats have been made.

(4) Because the Lake Stage represents a hydraulic head value that is
consistent with the calculated heads in the aquifer, the calculated values
of lake stage will now be inserted into the HNEW array at the appropriate
locations.  This will enable contouring or visualization of heads based on
hydraulic continuity between the aquifer and a lake.  Previously, because
lake cells correspond with values of IBOUND=0, the HNEW array contained
values of HNOFLO at lake cells.  If drawdown is printed, the drawdown values
shown for lake cells will represent the initial lake stage rather than
HNOFLO values (as in earlier versions of the code).

(5) The Lake Package now checks for consistency between lake cell locations
and values of IBOUND, which are supposed to be 0 at lake cells.  If the code
detects a lake cell where the value of IBOUND is not = 0, it will print a
warning message (but execution will continue).

(6) A bug was corrected to assure that a lake cell can occur in the bottom
layer of the grid.  Previously, if the executable code had been generated
with certain compilers, an array bounds error would occur in this situation. 

References:

Fread, D.L., 1993, Flow Routing, in Maidment, D.R., ed., Handbook of 
hydrology: New York, McGraw-Hill, Chapter 10, p. 10.1-10.36.

Merritt, M.L., and Konikow, L.F., 2000, Documentation of a computer
program to simulate lake-aquifer interaction using the MODFLOW
ground-water model and the MOC3D solute-transport model: U.S. 
Geological Survey Water Resources-Investigations Report 00-4167,
146 p.

Niswonger, R.G., and Prudic, D.E., 2005, Documentation of the 
Streamflow-Routing (SFR2) Package to include unsaturated flow 
beneath streams--A Modification to SFR1: U.S. Geological Survey 
Techniques and Methods 6-A13, 48 p.

Prudic, D.E., Konikow, L.F., and Banta, E.R., 2004, A new streamflow-
routing (SFR1) Package to simulate stream-aquifer interaction with 
MODFLOW-2000: U.S. Geological Survey Open-File Report 2004-1042, 95 p.

Wang, H.F., and Anderson, M.P., 1982, Introduction to groundwater modeling--
finite difference and finite element methods: San Francisco, Calif., W.H.
Freeman and Co., 233 p.