SFR7_Read_Me.txt

Modifications to the Streamflow Routing Package originally documented
by Prudic and others (2004) and Niswonger and Prudic (2005):

Update (June 2006):

	Several minor coding changes were made since the initial
release to fix bugs in relation to the different input options for
SFR2 (Niswonger and Prudic, 2005). These fixes to SFR2 resulted
from inconsistencies in the original data input by stream segments
in the SFR1 documentation (Prudic and others, 2004) and the new
data input option by stream reaches in the instructions published by
Niswonger and Prudic (2005). The SFR2 documentation report has
been revised to better explain the different options available in
SFR2. The latest version of the SFR2 documentation report is
version 1.10 and is available in PDF form at the persistent URL
http://pubs.water.usgs.gov/tm6A13/. Corrections to the original
printed document are listed in file tma6a13_SFR2revision_history.pdf
included in the document (doc) directory distributed with MODFLOW.

	An important change was made to SFR2 in the Formulate
and Budget modules that pertain to the computation of outflow from
lakes when streams are connected to lakes in the LAKE(LAK3) Package
(Merritt and Konikow, 2000). These changes were necessary to
remain compatible with the most recent version of the Lake Package.
The initial version of SFR1 and SFR2 made the computation of lake
outflow on the basis of either the lake stage from the previous
time step or the previous MODFLOW iteration or a combination of
both. This formulation of lake outflow can produce an oscillation
in the lake outflow that affects streamflow leakage downstream of
the lake and could prevent MODFLOW from reaching convergence during
a time step. 

	A new subroutine named GWF1SFR2LAKOUTFLW was added to SFR2.
The new subroutine computes the relation of stream stage with
streamflow at the beginning of a stream segment that receives outflow
from a lake. The relation between stream stage and streamflow are
saved in tables that are passed to the revised Lake (LAK3) Package
where the tables are used in computing lake stage and lake outflow to
the stream segment using the Newton iteration method.

	The changes to the Formulate and Budget modules in SFR2
do not affect previous model results unless outflow from a
lake is simulated as inflow to a stream. Model results when
lake outflow is simulated as inflow to a stream could differ
from eariler models that used the previous method of computing
lake outflow. The greatest differences will occur for steady-state
simulations with computed lake outflows to streams or for transient
simulations when a time weighting factor (THETA) of 0.0 was used
for computing lake stage and lake outflow when time steps were long
and lake outflow was sensitive to small changes in lake stage.



References:

Merritt, M.L., and Konikow, L.F., 2000, Documentation of a computer
program to simulate lake-aquifer interaction using the MODFLOW
ground-water model and the MOC3D solute-transport model: U.S.
Geological Survey Water Resources-Investigations Report 00-4167,
146 p.

Niswonger, R.G., and Prudic, D.E.,2005, Documentation of the
Streamflow-Routing (SFR2) Package to include unsaturated flow
beneath streams--A Modification to SFR1: U.S. Geological Survey
Techniques and Methods 6-A13, 48 p.

Prudic, D.E., Konikow, L.F., and Banta, E.R.,2004, A new streamflow-
routing (SFR1) Package to simulate stream-aquifer interaction with
MODFLOW-2000: U.S. Geological Survey Open-File Report 2004-1042, 95 p.


