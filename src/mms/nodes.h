typedef struct node_type {
  char  *name;
  int   x, y;
  int   num_connectnions;
} NODE;

NODE nodes[] = {
   {"/prms/basin_prms.f", 10, 20, 0},
   {"/gsflow/gsflow_prms.f", 560, 20, 0},
   {"/prms/soltab_hru_prms.f", 10, 290, 0},
   {"/prms/obs_prms.f", 10, 200, 0},
   {"/prms/cascade_prms.f", 10, 110, 0},
   {"/prms/xyz_dist.f", 206, 380, 5},
   {"/gsflow/gsflow_setconv.f", 554, 200, 1},
   {"/prms/temp_1sta_prms.f", 182, 20, 3},
   {"/prms/temp_2sta_prms.f", 182, 110, 3},
   {"/prms/precip_laps_prms.f", 179, 290, 11},
   {"/prms/potet_hamon_hru_prms.f", 165, 730, 5},
   {"/prms/precip_prms.f", 194, 200, 11},
   {"/prms/ccsolrad_hru_prms.f", 174, 470, 10},
   {"/prms/ddsolrad_hru_prms.f", 174, 660, 9},
   {"/prms/potet_jh_prms.f", 188, 560, 6},
   {"/prms/soilzone_gsflow.f", 549, 380, 24},
   {"/gsflow/gsflow_mf2prms.f", 550, 380, 2},
   {"/gsflow/gsflow_modflow.f", 551, 110, 2},
   {"/prms/intcp_prms.f", 380, 20, 11},
   {"/prms/snowcomp_prms.f", 363, 110, 17},
   {"/prms/srunoff_smidx_casc.f", 354, 200, 21},
   {"/prms/srunoff_carea_casc.f", 355, 290, 22},
   {"/gsflow/gsflow_prms2mf.f", 550, 290, 10},
   {"/gsflow/gsflow_budget.f", 556, 470, 9},
   {"/prms/hru_sum_prms.f", 900, 250, 22},
   {"/prms/gwflow_casc_prms.f", 710, 20, 10},
   {"/prms/strmflow_prms.f", 723, 110, 7},
   {"/prms/basin_sum_prms.f", 900, 500, 33},
   {"/gsflow/gsflow_sum.f", 563, 560, 39},
   NULL
};

int  con_index[] = {
   0, 3, 3, 3, 3,
   0,
   0, 3, 3,
   0, 3, 3,
   0, 3, 3, 3, 3, 7, 7, 7, 7, 7, 7,
   0, 2, 7, 7, 7,
   0, 3, 3, 3, 3, 7, 7, 7, 7, 7, 7,
   0, 2, 2, 2, 2, 3, 3, 3, 9, 10,
   0, 2, 2, 2, 2, 3, 3, 9, 10,
   0, 7, 7, 7, 7, 12,
   0, 0, 0, 0, 0, 1, 4, 4, 4, 4, 17, 10, 10, 18, 19, 19, 20, 20, 20, 20, 0, 0, 9, 16,
   0, 15,
   1, 22,
   0, 9, 9, 9, 9, 10, 9, 9, 3, 10, 19,
   0, 12, 12, 12, 18, 18, 18, 18, 9, 9, 9, 9, 7, 7, 7, 10, 10,
   0, 0, 0, 4, 4, 4, 4, 3, 18, 18, 18, 9, 15, 10, 19, 19, 19, 19, 19, 0, 0,
   0, 0, 0, 4, 4, 4, 4, 3, 18, 18, 18, 9, 15, 10, 19, 19, 19, 19, 19, 15, 0, 0,
   0, 4, 17, 15, 15, 15, 10, 15, 15, 20,
   0, 15, 16, 22, 15, 15, 15, 15, 15,
   10, 12, 7, 7, 9, 18, 18, 18, 10, 15, 15, 20, 20, 20, 19, 19, 19, 19, 19, 19, 15, 15,
   0, 15, 4, 4, 0, 0, 20, 15, 15, 0,
   0, 20, 25, 15, 15, 25, 20,
   0, 15, 25, 15, 0, 3, 9, 15, 15, 18, 19, 15, 18, 20, 20, 25, 25, 15, 20, 25, 15, 12, 18, 10, 19, 19, 3, 3, 7, 7, 15, 26, 26,
   1, 22, 19, 17, 15, 19, 20, 18, 15, 26, 3, 9, 15, 16, 23, 15, 20, 15, 20, 15, 22, 15, 15, 15, 15, 25, 23, 23, 25, 18, 20, 15, 15, 15, 25, 18, 20, 23, 23
};