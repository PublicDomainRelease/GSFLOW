/*
 *  $Id: structs.h 3058 2007-01-25 22:25:59Z rsregan $
 */

#ifndef _STRUCTS_H
#define _STRUCTS_H

#include <stdio.h>

typedef struct dimen_t{
  char *name;
  long value;
  long max;
  char *descr;
  char **names;
  char **notes;
  FILE **files;
  long column_width;
  char *format;
  int fixed;
} DIMEN;             /* dimension pointer structure */

typedef struct {
  char *key;
  char *module;
  char *name;
  long ndimen;
  struct dimen_t **dimen;
  long size;
  long type;
  long bound_status;
  struct dimen_t *bound_dimen;
  char *value;
  char *min;
  char *max;
  char *def;
  char *descr;
  char *help;
  char *units;
  char *format;
  long column_width;
  char **value_desc;
  char *value_string;
  char *min_string;
  char *max_string;
  char *def_string;
  long read_in;
} PARAM;                 /* parameter pointer structure */

typedef struct {
  char *key;
  long size;
  long type;
  char *start_ptr;
} CONTROL;                 /* control variable pointer structure */

typedef struct {
  long year, month, day, hour, min, sec, jd;
  double jt;
} DATETIME;                 /* date and time structure */

typedef struct list_t {
    char        *name;
    int         size;
    int         count;
    int         type;
    int         out;
    void        *user_data;
    void        **itm;
} LIST;

typedef struct {
  char *key;
  char *module;
  char *name;
  long ndimen;
  struct dimen_t **dimen;
  long size;
  long type;
  char *help;
  char *units;
  char *value;
  int private;
} PUBVAR;                 /* public variable pointer structure */

typedef struct {
  PUBVAR *var;
  long count;
  union {
    long   * valuel;
    float  * valuef;
    double * valued;
  }Types;
} READCHECK; /* for checking the readvar function calls */

typedef struct file_data_t {
	FILE    *fp;
	char    *name;
	char    line[MAXDATALNLEN];
	char    *start_of_data;
	float   delta_t;
	char    info[MAXDATALNLEN];
	DATETIME    time;
} FILE_DATA;

typedef struct STAT_LIST_TYPE {
  char key[MAXKEYLEN];
  char *element;
  long type;
  char *value;
  struct STAT_LIST_TYPE *next;
} STAT_LIST_TYPE;   /* linked list element of stat vars */

typedef  struct opt_var_types {
	long type;
	union {
		long	*lvar;
		float	*fvar;
		double	*dvar;
	} v_type;
} OPT_VAR_TYPES, ESP_VAR_TYPES;

typedef struct _rosen_param {
    PARAM   *params;    /* This will point to the mms parameter structure */
    double  *val;       /* array of values for this parameter */
    int     *elem;      /* offsets to array of values for this parameter */
    double  *tr_val;    /* Array of transformed values for this parameter */
    double  *tr_dev;    /* Array of transformed deviations for this parameter */
    double  hi;         /* Upper constraint */
    double  lo;         /* Lower constraint */
    double  tr_hi;      /* Trans Upper constraint */
    double  tr_lo;      /* Trans Lower constraint */
    double  bdry_hi;    /* Upper boundry */
    double  bdry_lo;    /* Lower boundry */
    double  pen_hi;     /* penalty value of obj fun for each constraint */
    double  pen_lo;     /* penalty value of obj fun for each constraint*/
    double  init_ss;    /* Initial step size as % */
    double  ss;         /* Step size */
    double  tr_ss;      /* Trans step size */
    int     num_val;    /* Array size */
    double  ave;        /* average of elements */
    double  tr_ave;     /* average of transformed elements */
    double  tr_old_ave; /* previous average of transformed elements */
    double  e;          /* current step size */
    int     ilopl;      /* increment flag  0 = equal; 1 = proportional */
} ROSEN_PARAM;

typedef struct _rosen_data {
    double  (*func) (); /* obj val function "rmse" */
    double  toler;
    double  u;          /* current Value of objective function */
    double  u_best;     /* best value of objective function */
    int     maxitr;
    ROSEN_PARAM  *params;     /* array of parameter structures*/
    int     npar;       /* number of parameter structures*/
    double  *x;         /* array of changing parameter values */
    int     ne;         /* total num of param elements */
    int     flag;
    int     itrans;
    int     iobf;
    int     iopt;
    int     isen;       /*  Sens switch 0 = none; 1,2 = daily; 3,4 = storm */
    int     ntry;       /*  max no. of iterations of param set to be run */
    int     ndop;       /*  Optimization switch  0 = continue  1 = end */
    int     warmup;
    int     ndsn;       /*  Sens continue switch  0 = continue, 1 = end  */
    int     begin_mon_of;
    int     end_mon_of;
    int     nmobf;
    int     nobf;       /*  Number of time steps included in objective function */
    float   b4;         /*  Obj function switch -1 = minimize +1 = maximize */
    FILE    *fp;
} ROSEN_DATA;

#endif
