../../../bin/gsflow ./Tahoe.control -set start_time 1983,11,25,0,0 -set end_time 1983,12,01,0,0,0 -set modflow_name ../input/MODFLOW/tahoe14.nam -set gsflow_output_file ../output/gsflow_14.out -set model_output_file ../output/PRMS/prms14.out -set csv_output_file ../output/gsflow_14.csv -set var_init_file ../output/PRMS/prms_ic_13 -set var_save_file ../output/PRMS/prms_ic_14
../../../bin/gsflow ./Tahoe.control -set start_time 1983,12,02,0,0 -set modflow_name ../input/MODFLOW/tahoe15.nam -set gsflow_output_file ../output/gsflow_15.out -set model_output_file ../output/PRMS/prms15.out -set csv_output_file ../output/gsflow_15.csv -set var_init_file ../output/PRMS/prms_ic_14 -set var_save_file ../output/PRMS/prms_ic_15
cd ../..
../bin/CSV_merge_tahoe
cd tahoe_restart/linux

../../../bin/gsflow ./Tahoe.control -set init_vars_from_file 0 -set gsflow_output_file ../output/gsflow_cont.out -set csv_output_file ../output/gsflow_cont.csv


