/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : alloc_space
 * COMMENT  : allocates space for variables
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define ALLOC_SPACE_C
#include <string.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : alloc_space
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void alloc_space (void) {
	static DATETIME start, end, now, next;

	cont_db = ALLOC_list ("Control Data Base", 0, 100);

  /*
   * space for the dimension pointer  array
   */

/*
  max_dims = 50;
  Mdimbase = (DIMEN **) umalloc (max_dims * sizeof(DIMEN *));
  Mndims = 0;
*/
	dim_db = ALLOC_list ("Dimension Data Base", 0, 50);

  /*
   * default dimension "one"
   */

  decldim ("one", 1, 1, "Dimension of scalar parameters and variables");

  /*
   * space for the public variable pointer array
   */

  max_vars = 500;
  Mvarbase = (PUBVAR **) umalloc (max_vars * sizeof(PUBVAR *));
  Mnvars = 0;

/*
	var_db = ALLOC_list ("Variable data base", 0, 100);
*/

  /*
   * space for the parameter pointer  array
   */

  max_params = 500;
  Mparambase = (PARAM **) umalloc (max_params * sizeof(PARAM *));
  Mnparams = 0;
/*
	param_db = ALLOC_list ("Paraameter data base", 0, 100);
*/

  /*
   * space for the read check data base
   */

  max_read_vars = 50;
  Mcheckbase = (READCHECK **) umalloc (max_read_vars * sizeof(READCHECK *));
  Mnreads = 0;

/*
	read_var_db = ALLOC_list ("Paraameter data base", 0, 100);
*/

/*
* space for time structures
*/
	Mstrttime = &start;
	Mendtime = &end;
	Mnowtime = &now;
	Mnexttime = &next;

/*
* space for run info string
*/
	Mparaminfo = strdup ("Default case");
	Mdatainfo = strdup ("Default case");
}

/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : batch_run
 * COMMENT  : runs the MMS time loop
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/

#define BATCH_RUN_C

#include <string.h>
#include <errno.h>
#include "mms.h"

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/
extern int call_modules (char *);
extern char *single_run_pre_init (void);
extern char *single_run_post_init (void);
extern char *single_run_pre_run (void);
extern char *single_run_post_run (void);
extern char *single_run_post_cleanup (void);

/*--------------------------------------------------------------------*\
 | FUNCTION     : BATCH_run
 | COMMENT      :
 | PARAMETERS   : None
 | RETURN VALUE : char * - error message if there is one.
 | RESTRICTIONS : None
\*--------------------------------------------------------------------*/
int BATCH_run (void) {
   char *ret;
   long endofdata = 0;

   ret = single_run_pre_init ();
   if (ret) {
      fprintf (stderr, ret);
      return(1);
   }

   if (call_modules("initialize")) {
      //closeUserFiles();
      fprintf (stderr, "single_run:  Problem with initializing modules.");
      return(1);
   }

   ret = single_run_post_init ();
   if (ret) return(1);

/*
* perform the main loop
*/

   M_stop_run = 0;
   Mprevjt = -1.0;

   while(!endofdata) {
      if(!(endofdata = read_line ())) {
         ret = single_run_pre_run ();
         if (ret) return(1);

/*
         if ((Mnowtime->month == 1) && (Mnowtime->day == 1)) {
             printf ("  running year = %ld\n", Mnowtime->year);
         }
*/
         errno = 0;

         if(call_modules("run")) {
            //closeUserFiles ();
            fprintf (stderr, "Problem while running modules.");
            return(1);
         }

         ret = single_run_post_run ();
         if (ret) return(1);
      }
   }

   ret = single_run_post_cleanup ();
   if (ret) return(1);

/*
* cleanup modules
*/

   if (call_modules("cleanup")) {
       fprintf (stderr, "Problem with module cleanup.");
       return(1);
   }

   return(0);
}

/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : batch_run_functions
 * COMMENT  :
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include "mms.h"

/**5*********************** LOCAL VARIABLES ***************************/
  static FILE *statvar_file;
  static FILE **ani_out_files;
  static long nstatVars, naniVars;
  static char **statVar_names, **statVar_element;
  static char **aniVar_names;
  static char statvar_path[MAXPATHLEN];
  static char ani_path[MAXPATHLEN];
  static char buf[256];
  static long i, j, stats_flag, ani_out_flag;
  static char  *err_message, *c;
  static char   err[256];
  static int       started;
  static PUBVAR    **ani_out_vars, *var;
  static DIMEN **ani_out_dims, *dim;
  static DIMEN *nhrudim, *ngwdim, *nssrdim, *foobar;
  static FILE **ani_var_files;
  static int num_ani_dims, found, k;
  static DATETIME start_of_data, end_of_data;

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : single_run_pre_init
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : char *
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *single_run_pre_init () {

  stats_flag = *control_lvar ("statsON_OFF");
  if (stats_flag == 1)
     nstatVars = *control_lvar("nstatVars");
  else
     nstatVars = 0;

  if (!nstatVars)
    stats_flag = 0;

  ani_out_flag = *control_lvar ("aniOutON_OFF");
  if (ani_out_flag == 1)
     naniVars = *control_lvar("naniOutVars");
  else
     naniVars = 0;

  if (!naniVars)
    ani_out_flag = 0;

/*
**  Make sure that all of the selected stat and display variables are OK
**  before a run is attempted.
*/
  if (stats_flag) {
    err_message = CHECK_stat_vars();
    if (err_message) return (err_message);
  }

  if (ani_out_flag) {
    err_message = CHECK_ani_vars ();
    if (err_message) return (err_message);
  }

/*
* create stats vars linked list
*/
  if (stats_flag)
    create_vstats();

/*
* open data file
* ensure datainfo is up to date
*/
  err_message = DATA_read_init ();
  if (err_message) {
     (void)printf ("%s\n", err_message);
     return (err_message);
  }

/*
 * **  Reset run period to period of record if -por flag set
 */
  if (run_period_of_record) {
    DATA_find_end (&start_of_data, &end_of_data);
    julday (&start_of_data);
    julday (&end_of_data);

    printf ("resetting model start time to period of record %ld %ld %ld %ld %ld %ld\n",
            start_of_data.year, start_of_data.month, start_of_data.day,
            start_of_data.hour, start_of_data.min, start_of_data.sec);

    printf ("resetting model end time to period of record %ld %ld %ld %ld %ld %ld\n",
            end_of_data.year, end_of_data.month, end_of_data.day,
            end_of_data.hour, end_of_data.min, end_of_data.sec);


    Mendtime = &end_of_data;
    Mstrttime = &start_of_data;
  }

  err_message = DATA_check_start ();
  if (err_message) {
     (void)printf ("%s\n", err_message);
     return (err_message);
  }

/*
* Open statvar file, and store number of variables and variable names
*/
  if (stats_flag) {
    (void)sprintf(statvar_path, "%s", *((char **) control_var("stat_var_file")));

    if ((statvar_file = fopen(statvar_path, "w")) == NULL) {
      (void)sprintf (err, "ERROR - single_run: Could not open statvar file '%s'\n",
		     statvar_path);
      return (err);
    }

    statVar_names = (char **) control_var("statVar_names");
    statVar_element = (char **) control_var("statVar_element");

/*
* write number of variables and statVars names to stats data file.
*/
    (void)fprintf(statvar_file,"%ld\n",nstatVars);

    for (i = 0; i < nstatVars; i++)
      (void)fprintf(statvar_file,"%s %s\n", statVar_names[i], statVar_element[i]);
  }

/*
* Open ani output files.
*/
  if (ani_out_flag) {
    aniVar_names = (char **) control_var("aniOutVar_names");
    (void)sprintf(ani_path, "%s", *((char **) control_var("ani_output_file")));

    ani_out_dims = (DIMEN **)malloc (naniVars * sizeof (DIMEN *));
    ani_var_files = (FILE **)malloc (naniVars * sizeof (FILE *));
    ani_out_vars = (PUBVAR **)malloc (naniVars * sizeof (PUBVAR *));

/*
**  Get the pubvars.
*/
    for (i = 0; i < naniVars; i++) {

       c = strchr (aniVar_names[i], '.');
         if (c)
           *c = '\0';

       sprintf (buf, "%s.%s", ani_path, aniVar_names[i]);
       c = strchr (buf, ' ');
         if (c)
           *c = '\0';

       ani_out_vars[i] = var_addr (aniVar_names[i]);
    }

/*
**  List of unique ANIMATION dimensions.
*/
    num_ani_dims = 0;
    nhrudim = dim_addr("nhru");
    ngwdim = dim_addr("ngw");
    nssrdim = dim_addr("nssr");
    for (i = 0; i < naniVars; i++) {
       found = FALSE;
       
/* Reset ngw and nssr dimensions to nhru for the purposes of the animation file.*/
       foobar = ani_out_vars[i]->dimen[0];
       if (foobar == ngwdim || foobar == nssrdim) {
          foobar = nhrudim;
       }

       for (j = 0; j < num_ani_dims; j++) {
          if (foobar == ani_out_dims[j]) {
             found = TRUE;
          }
       }

       if (!found) {
          ani_out_dims[j] = foobar;
          num_ani_dims++;
       }
    }

/*
**  Open a file for each dimension.
*/
    ani_out_files = (FILE **)malloc (num_ani_dims * sizeof (FILE *));

    for (i = 0; i < num_ani_dims; i++) {
       sprintf (buf, "%s.%s", ani_path, ani_out_dims[i]->name);
       if ((ani_out_files[i] = fopen(buf, "w")) == NULL) {
          (void)sprintf (err, "ERROR - single_run: Could not open ani file '%s'\n", buf);
          return (err);
       }

       fprintf (ani_out_files[i], "#\n# Begin DBF\n");
       fprintf (ani_out_files[i], "# timestamp,#FIELD_ISODATE,10,0\n");
       fprintf (ani_out_files[i], "# %s,#FIELD_DECIMAL,10,2\n", ani_out_dims[i]->name);
    }

/*
**  Map each variable to a file.
*/
    for (i = 0; i < naniVars; i++) {
/* Reset ngw and nssr dimensions to nhru for the purposes of the animation file.*/
       foobar = ani_out_vars[i]->dimen[0];
       if (foobar == ngwdim || foobar == nssrdim) {
          foobar = nhrudim;
       }

       for (j = 0; j < num_ani_dims; j++) {
          if (foobar == ani_out_dims[j]) {
             ani_var_files[i] = ani_out_files[j];
          }
       }
    }

/*
**  Finish writing the headers.
*/
    for (i = 0; i < naniVars; i++)
       fprintf (ani_var_files[i], "# %s,#FIELD_DECIMAL,10,2\n", ani_out_vars[i]->name);

    for (i = 0; i < num_ani_dims; i++) {
       fprintf (ani_out_files[i], "# End DBF\n#\n");
    }

/*
**  Write variable name line
*/
    for (i = 0; i < num_ani_dims; i++) {
       fprintf (ani_out_files[i], "timestamp	%s", ani_out_dims[i]->name);
    }

    for (i = 0; i < naniVars; i++) {
       fprintf (ani_var_files[i], "	%s", ani_out_vars[i]->name);
    }

    for (i = 0; i < num_ani_dims; i++)
       fprintf (ani_out_files[i], "\n");

/*
**  Write variable size line
*/
    for (i = 0; i < num_ani_dims; i++) {
       fprintf (ani_out_files[i], "10d	10n");
    }

    for (i = 0; i < naniVars; i++) {
       fprintf (ani_var_files[i], "	10n");
    }

    for (i = 0; i < num_ani_dims; i++)
       fprintf (ani_out_files[i], "\n");

  }

/*
**  set initial values of nsteps global variable if they
**  don't come from the var init file
*/
    Mnsteps = 0;

/*
* initialize modules
*/

  errno = 0;
  return(NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : single_run_post_init
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : char *
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *single_run_post_init () {

  initializeRuntimeGraphs();

/*
**  set initial values of nsteps global variable if they
**  don't come from the var init file
*/
    Mnsteps = 0;

  started = FALSE;

   return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : single_run_pre_run
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : char *
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *single_run_pre_run () {

	started = TRUE;

	errno = 0;
	return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : single_run_post_run
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : char *
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *single_run_post_run () {

      if (stats_flag)
         write_vstats (statvar_file);

/*
**  Write the ANIMATION output variables to their respective files.
*/
      if (ani_out_flag) {

/*
**  Each dimension has it's own file.
*/
         for (i = 0; i < num_ani_dims; i++) {
            dim = ani_out_dims[i];
            for (j = 0; j < dim->value; j++) {

/*
**  Write the current time stamp to the dimension file.
*/
            fprintf (ani_out_files[i], "%4ld-%02ld-%02ld %10ld",
                     Mnowtime->year,
                     Mnowtime->month, Mnowtime->day, j + 1);

/*
**  Write the variable values to the dimension file.
*/
               for (k = 0; k < naniVars; k++) {
                  var = ani_out_vars[k];
/*                  if (var->dimen[0] == dim) {
*/
                     switch (var->type) {
                        case M_DOUBLE:
                           fprintf (ani_var_files[k], " %10.3e",
                                    *((double *) var->value + j));
                           break;

                        case M_FLOAT:
/*                           fprintf (ani_var_files[k], "\t%14.6e",
*/
                           fprintf (ani_var_files[k], " %10.3e",
                                    *((float *) var->value + j));
                           break;

                        case M_LONG:
// markstro test
//                           fprintf (ani_var_files[i], "\t%10ld",
//                                    *((long *) var->value + j));
                           fprintf (ani_var_files[i], "\t%10ld",
                                    *((int *) var->value + j));
                           break;
                     }
/*
                  }
*/
               }
               fprintf (ani_out_files[i], "\n");
            }
         }
      }

      plotRuntimeGraphValue();

   return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : single_run_pre_cleanup
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : char *
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *single_run_pre_cleanup () {

/*
  if (GISProcessRunning)
    GISEndAnimation();

  if (ani_init) {
    RESET_animation_control ();
  }
*/

   return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : single_run_post_cleanup
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : char *
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *single_run_post_cleanup () {

/*
* free up stats vars linked list
*/
  if (stats_flag)
    free_vstats();

/*
* close files and tidy up
*/
  DATA_close ();
  //closeUserFiles();

  if (stats_flag)
    fclose(statvar_file);

/*
**  Close the ANIMATION output files
*/
   if (ani_out_flag) {
      for (i = 0; i < num_ani_dims; i++)
         fclose (ani_out_files[i]);

   }

  closeRuntimeGraphs();

  if (!started) {
    return ("Run period outside of data in file.");
  }

/*
* if required, save vars to file
*/
  if (preprocess_on) {
    write_preprocess_params ();
  }

   return (NULL);
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : build_lists
 * COMMENT  :
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define BUILD_LISTS_C
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

#define INPUT  1
#define OUTPUT  2

/*--------------------------------------------------------------------*\
 | FUNCTION     : ALLOC_list
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
LIST *ALLOC_list (char *name, int type, int size) {

	LIST	*list;

	list = (LIST *)malloc (sizeof (LIST));

	if (name)
		list->name = strdup (name);
	else
		list->name = NULL;

	list->size = size;
	list->count = 0;
	list->type = type;
	list->out = FALSE;
	list->user_data = NULL;
	if (size)
		list->itm = (void **)malloc (size * sizeof (void *));
	else 
		list->itm = NULL;

	return (list);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : DELETE_list
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void DELETE_list (LIST *list) {

	int		i;

	if (list->name)
		free (list->name);

	for (i = 0; i < list->count; i++)
		if (list->itm[i])
			free (list->itm[i]);

	free (list->itm);
	free (list);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : RESIZE_list
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void RESIZE_list (LIST *list, int new_size) {
	list->size = new_size;
	list->itm = (void **)realloc (list->itm, new_size * sizeof (void *));
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : ADD_to_list
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void ADD_to_list (LIST *list, void *itm) {
	if (list->count >= list->size)
		RESIZE_list (list, list->size + 100);

	list->itm[list->count++] = itm;
}

/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : check_vars
 * COMMENT  :
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define CHECK_VARS_C
#include <string.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : CHECK_stat_vars
 | COMMENT		: Makes sure that the selected statistic variables
 |                  are valid.
 | PARAMETERS   :
 | RETURN VALUE : 0 - no bad ones are found
 |              : 1 - at least one bad one found
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *CHECK_stat_vars (void) {

	static char err_message[256];

	int		i, status = 0;
	char	**names, **elements, buf[80], *ptr;

	names = (char **) control_var("statVar_names");
	elements = (char **) control_var("statVar_element");

	for (i = 0; i < *((long *)control_var ("nstatVars")); i++) {
		(void)strcpy (buf, names[i]);
		ptr = strchr (buf, '.');
		if (ptr) *ptr = '\0';

		if (CheckIndices (buf, elements[i], M_VARIABLE)) {
			(void)fprintf (stderr, "ERROR - CHECK_stat_vars: %s[%s] is not a valid stat variable.\n", names[i], elements[i]);
			(void)sprintf (err_message, "Set stat variables: %s[%s] is not a valid stat variable.\n", names[i], elements[i]);
			status = 1;
		}
	}

	if (status)
		return (err_message);
	else
		return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : CHECK_disp_vars
 | COMMENT		: Makes sure that the selected display variables
 |                  are valid.
 | PARAMETERS   :
 | RETURN VALUE : error message
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char * CHECK_disp_vars (void) {
       static char	err_message[256];
	int		status = 0;
	int		i, j;
	char	buf[MAXVARLEN], buf0[MAXVARLEN], buf1[MAXVARLEN], buf2[MAXVARLEN];
	char	*dv_name, *dv_index, *ptr;

	for (i = 0; i < *(control_lvar ("ndispGraphs")); i++) {
		(void)sprintf (buf0, "ndispVars%d", i);
		(void)sprintf (buf1, "dispVar_names%d", i);
		(void)sprintf (buf2, "dispVar_element%d", i);

		for (j = 0; j < *(control_lvar (buf0)); j++) {
			dv_name = *((char **)(control_sarray (buf1, j)));
			(void)strcpy (buf, dv_name);
			if ( (ptr = strchr (buf, '.')) )
				*ptr = '\0';
			dv_index = *((char **)(control_sarray (buf2, j)));
			if ((CheckIndices (buf, dv_index, M_VARIABLE))) {
				(void)fprintf (stderr, "ERROR - CHECK_disp_vars: %s[%s] from graph %d is not a valid display variable.\n", buf, dv_index, i+1);
				(void)sprintf (err_message, "Set display variables: %s[%s] from graph %d is not a valid display variable.\n", buf, dv_index, i+1);
				status = 1;
			}
		}
	}

	if (status)
		return (err_message);
	else
		return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : CHECK_ani_vars
 | COMMENT      : Makes sure that the selected ani variables
 |                  are valid.
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *CHECK_ani_vars (void) {
    int     i, status = 0;
    char    **names, buf[80], *ptr;
    PUBVAR  *vaddr;
    static char err_message[256];

    names = (char **) control_var ("aniOutVar_names");

    for (i = 0; i < *((long *)control_var ("naniOutVars")); i++) {
        (void)strcpy (buf, names[i]);
        ptr = strchr (buf, '.');
        if (ptr) *ptr = '\0';

        if (!(vaddr = var_addr (buf))) {
           fprintf (stderr, "ERROR - CHECK_ani_vars: %s is not a valid animation output variable.\n", names[i]);
           sprintf (err_message, "Set animation variables: %s is not a valid stat variable.\n", names[i]);
           status = 1;
        }
    }

    if (status)
        return (err_message);
    else
        return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : CHECK_map_vars
 | COMMENT      : Makes sure that the selected map variables
 |                  are valid.
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *CHECK_map_vars (void) {
    int     i, status = 0;
    char    **names, buf[80], *ptr;
    PUBVAR  *vaddr;
    static char err_message[256];

    names = (char **) control_var ("mapOutVar_names");

    for (i = 0; i < *((long *)control_var ("nmapOutVars")); i++) {
        (void)strcpy (buf, names[i]);
        ptr = strchr (buf, '.');
        if (ptr) *ptr = '\0';

        if (!(vaddr = var_addr (buf))) {
           fprintf (stderr, "ERROR - CHECK_map_vars: %s is not a valid map output variable.\n", names[i]);
           sprintf (err_message, "Set map variables: %s is not a valid stat variable.\n", names[i]);
           status = 1;
        }
    }

    if (status)
        return (err_message);
    else
        return (NULL);
}

/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : control_addr
 * returns a pointer to a CONTROL struct which contains the given key
 * returns NULL if key not found
 * COMMENT  :
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define CONTROL_ADDR_C
#include <stdio.h>
#include <string.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_addr
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
CONTROL *control_addr (char *key) { 
	CONTROL	*cp;
	int		i;

	for (i = 0; i < cont_db->count; i++) {
		cp = (CONTROL *)(cont_db->itm[i]);
		//printf ("control_addr: i = %d comparing %s to %s \n", i, key, cp->key);
		if (!strcmp (cp->key, key))
			return (cp);
	}

	return (NULL);
}

/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : control_array - generic, returns (char *) as a generic pointer
 *            control_larray - returns long *
 *            control_farray - returns float *
 *            control_darray - returns double *
 *            control_sarray - returns char ** - string
 *            These return pointers to particular elements in a control array.
 * COMMENT  : control_array routines
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define CONTROL_ARRAY_C
#include <stdlib.h>
#include "mms.h"

/**************************************************************************
 * control_array.c: 
 *
 * returns a pointer to a particular entry in a CONTROL struct
 *
 * index is 0-based, max size-1
 *
 **************************************************************************/

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_array
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *control_array (char *key, long ind) {
 
  CONTROL *control;

  if ((control = control_addr(key)) == NULL) {
    (void)fprintf(stderr, 
	    "ERROR - control_array - key '%s' not found.\n", key);
    exit(1);
  }

  if (ind >= control->size) {
    (void)fprintf(stderr, 
	    "ERROR - control_array - ind %ld too high for %s.\n", ind, key);
    (void)fprintf(stderr, 
	    "Max ind is %ld.\n", control->size-1);
    exit(1);
  }

	switch (control->type) {
		case M_DOUBLE:
			return (char *) ((double *)(control->start_ptr) + ind * sizeof(double));

		case M_FLOAT:
			return (char *) ((float *)(control->start_ptr) + ind * sizeof(float));

		case M_LONG:
			return (char *) ((long *)(control->start_ptr) + ind * sizeof(long));

		case M_STRING:
			printf ("control_array: key = %s ind = %ld val = %s\n", key, ind, *((char **)control->start_ptr + ind));
//			return (char *) (((char **)(control->start_ptr)) + (ind * sizeof(char *)));
			return *((char **)control->start_ptr + ind);
	}

	return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_larray
 | COMMENT		: returns a pointer to a long entry 
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long *control_larray (char *key, long ind) {
  return ((long *) control_array(key, ind));
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_farray
 | COMMENT		: returns a pointer to a float entry in control array
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
float *control_farray (char *key, long ind) {
  return ((float *) control_array(key, ind));
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_darray
 | COMMENT		: returns a pointer to a double entry in control array
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
double *control_darray (char *key, long ind) {
  return ((double *) control_array(key, ind));
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_sarray
 | COMMENT		: returns a pointer to a string entry in control array
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *control_sarray (char *key, long ind) {
  return control_array(key, ind);
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : control_var - generic, returns (char *) as a generic pointer
 *            control_lvar - returns long *
 *            control_fvar - returns float *
 *            control_dvar - returns double *
 *            control_svar - returns char ** - string
 *            returns pointers to various control array entries
 * COMMENT  :
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define CONTROL_VAR_C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_var
 | COMMENT		: returns a pointer to the start of the variable
 |			( or first element in * the array) in a CONTROL struct
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *control_var (char *key) {
 
  CONTROL *control;

  if ((control = control_addr(key)) == NULL) {
    (void)fprintf(stderr, 
	    "ERROR - control_var - key '%s' not found.\n", key);
    exit(1);
  }
  return (char *) control->start_ptr;

}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_lvar
 | COMMENT		: returns a pointer to a long variable
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long *control_lvar (char *key) {
  return ((long *) control_var(key));
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_fvar
 | COMMENT		: returns a pointer to a float variable
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
float *control_fvar (char *key) {
  return ((float *) control_var(key));
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_dvar
 | COMMENT		: returns a pointer to a double variable
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
double *control_dvar (char *key) {
  return ((double *) control_var(key));
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_svar
 | COMMENT		: returns a pointer to a string variable
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char **control_svar (char *key) {
  return ((char **) control_var(key));
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_string_
 | COMMENT		: called from fortran
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long control_string_ (char *retval, char *tag, ftnlen len, ftnlen tlen) {
	char *foo;

	foo = (char *) umalloc(tlen + 1);
	strncpy(foo, tag, tlen);
	foo[tlen] = '\0';

	memset (retval, ' ', len);
	strncpy (retval, *control_svar(foo), len);
	return 0;
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_string_array_
 | COMMENT		: called from fortran
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long control_string_array_ (char *retval, char *tag, int *index, ftnlen len, ftnlen tlen) {
	char *foo;
    char **strings;
    int i;

	foo = (char *) umalloc(tlen + 1);
	strncpy(foo, tag, tlen);
	foo[tlen] = '\0';

    strings = (char **) control_var(foo);
    i = *index - 1;
	strncpy (retval, *(strings+i), len);
	return 0;
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_integer_
 | COMMENT		: returns a long variable value
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long control_integer_ (int *retval, char *key, ftnlen len) {
	char *foo;

	foo = (char *) umalloc(len + 1);
	strncpy(foo, key, len);
	foo[len] = '\0';

	*retval = *control_var(foo);
	return 0;
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_integer_array_
 | COMMENT		: called from fortran
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long control_integer_array_ (int *retval, int *index, char *key, ftnlen tlen) {
	char *foo;
	long intVal;
    long *longs;
    int i;

	foo = (char *) umalloc(tlen + 1);
	strncpy(foo, key, tlen);
	foo[tlen] = '\0';

    longs = (long *) control_var(foo);
    i = *index - 1;
	intVal = *(longs+i);
	*retval = (int)intVal;
	return 0;
}

/*--------------------------------------------------------------------*\
| FUNCTION     : control_file_name_
| COMMENT		: called from fortran
| PARAMETERS   :
| RETURN VALUE :
| RESTRICTIONS :
\*--------------------------------------------------------------------*/
long control_file_name_(char *retval, ftnlen tlen) {
	char *foo;
	foo = (char *)umalloc(tlen + 1);
	strncpy(foo, MAltContFile, tlen);
	foo[tlen] = '\0';
	memset(retval, ' ', tlen);
	strncpy(retval, foo, tlen);
	return 0;
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : create_vstats
 * COMMENT  : create linked list for stats variables
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define CREATE_VSTATS_C
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : create_vstats
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void create_vstats (void) {

  long *nstatVars, i;
  char **statVar_names;
  char **statVar_element;
  char *ptr;
  STAT_LIST_TYPE *curr_stat_list;
  PUBVAR *var;

  /*
   * get number of statVars
   */

  nstatVars = (long *) control_var("nstatVars");

  /*
   * get address of statVar names array 
   */
  
  statVar_names = (char **) control_var("statVar_names");
  
  /*
   * get address of statVar element  array 
   */
  
  statVar_element = (char **) control_svar("statVar_element");
  
/*
*	Make_linked_list;
*/
	curr_stat_list  = NULL;
	Mfirst_stat_list = NULL;
	for (i = 0; i < *nstatVars; i++) {

		if (curr_stat_list == NULL) {
			curr_stat_list = (STAT_LIST_TYPE *)umalloc(sizeof(STAT_LIST_TYPE));
			curr_stat_list->key = (char *) umalloc(max_data_ln_len * sizeof(char));
			Mfirst_stat_list = curr_stat_list;
		} else {
			curr_stat_list->next =(STAT_LIST_TYPE *)umalloc(sizeof(STAT_LIST_TYPE));
			curr_stat_list->next->key = (char *) umalloc(max_data_ln_len * sizeof(char));
			curr_stat_list = curr_stat_list->next;
		}

		(void)strcpy (curr_stat_list->key, statVar_names[i]);
		ptr = strchr (curr_stat_list->key, '.');
		if (ptr) *ptr = '\0';

		curr_stat_list->element = statVar_element[i];
		curr_stat_list->value = (char *)GetElemAddress (curr_stat_list->key,
					statVar_element[i], M_VARIABLE);

		if ((var = var_addr (curr_stat_list->key)) == NULL ) {
			(void)fprintf(stderr, "ERROR - create_vstats.\n");
			(void)fprintf(stderr, "Getting var_addr for var '%s'.\n",
			statVar_names[i]);
			exit(1);
		}
		curr_stat_list->type = var->type;
		curr_stat_list->next = NULL;
	}
}

/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : decl_control
 * COMMENT  : initializes a module variable entry in the memory database
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define DECL_CONTROL_C
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/**************************************************************************/

/*--------------------------------------------------------------------*\
 | FUNCTION     : add_control
 | COMMENT		: This allocates a control structure and adds it to the
 |                control DB.  It also allocates the space for the variables.
 | PARAMETERS   :
 | RETURN VALUE : None
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
CONTROL *add_control (char *key, long type, long size) {
   CONTROL *cp;

/*
**	check that key does not already exist
*/

   if (control_addr (key)) {
      (void)fprintf (stderr,
         "ERROR - add_control - key '%s' already exists.\n", key);
      exit(1);
   }
// printf ("adding control parameter - key: %s type: %ld size: %ld\n", key, type, size);

/*
**  allocate space for a structure, and store pointer in controls
**  allocate space, and store control variable properties
*/
   cp = (CONTROL *) umalloc (sizeof(CONTROL));
   ADD_to_list (cont_db, (void *)cp);

   cp->key = strdup (key);
   cp->size = size;
   cp->type = type;
   cp->set_in_file = 0;

   if (type == M_STRING) {
      cp->start_ptr = (char *)umalloc (sizeof (char *) * size);
   
   } else if (type == M_LONG) {
      cp->start_ptr = (char *)umalloc (sizeof (long) * size);

   } else if (type == M_FLOAT) {
	   cp->start_ptr = (char *)umalloc (sizeof (float) * size);

   } else if (type == M_DOUBLE) {
	   cp->start_ptr = (char *)umalloc (sizeof (double) * size);

   } else {
      (void)fprintf (stderr,
         "ERROR - add_control - key '%s' don't know what type code %ld is.\n", key, type);
      exit(1);
   }

   return cp;
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : decl_control
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : None
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void decl_control (char *key, long type, long size, void *valstr) {
   CONTROL *cp;

/*
**	check that key does not already exist
*/

   if (control_addr (key)) {
      (void)fprintf (stderr,
         "ERROR - decl_control - key '%s' already exists.\n", key);
      exit(1);
   }

/*
**  allocate space for a structure, and store pointer in controls
**  allocate space, and store control variable properties
*/
   cp = (CONTROL *) umalloc (sizeof(CONTROL));
   ADD_to_list (cont_db, (void *)cp);

   cp->key = key;
   cp->size = size;
   cp->type = type;
   cp->start_ptr = (char *)valstr;
   cp->set_in_file = 0;

}

void decl_control_string (char *key, char *valstr) {
   char **cp;
   cp = (char **)umalloc (sizeof (char *) * 1);
   *cp = strdup (valstr);
   decl_control (strdup (key), M_STRING, 1, cp);
}

void decl_control_string_array (char *key, long size, char *valstr) {
   char **cp;
   int i;

   cp = (char **)umalloc (sizeof (char *) * size);
   for (i = 0; i < size; i++) {
      cp[i] = strdup (valstr);
   }

   decl_control (strdup (key), M_STRING, size, cp);
}

void decl_control_int_array (char *key, long size, long *valstr) {
   long *lp;
   int i;

   lp = (long *)umalloc (sizeof (long) * size);
   for (i = 0; i < size; i++) {
      lp[i] = valstr[i];
   }

   decl_control (strdup (key), M_LONG, size, lp);
}

void decl_control_float_array (char *key, long size, float *valstr) {
   float *fp;
   int i;

   fp = (float *)umalloc (sizeof (float) * size);
   for (i = 0; i < size; i++) {
      fp[i] = (float)(valstr[i]);
   }

   decl_control (strdup (key), M_FLOAT, size, fp);
}

void decl_control_double_array (char *key, long size, double *valstr) {
   double *fp;
   int i;

   fp = (double *)umalloc (sizeof (double) * size);
   for (i = 0; i < size; i++) {
      fp[i] = (double)(valstr[i]);
   }

   decl_control (strdup (key), M_DOUBLE, size, fp);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : decl_control_
 | COMMENT		: decl_control_() is called from Fortran, sorts out args
 |                 and calls decl_control()
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void decl_control_ (char *ckey, ftnint *ctype, ftnint *csize, void *value, ftnlen klen) {
	char *key;
	long type, size;

  /*
   * copy ctype and csize to local long int
   */
	type = *ctype;
	size = *csize;

  /*
   * copy args to new strings, and terminate correctly
   */
	key = (char *) umalloc((unsigned int)(klen + 1));
	strncpy(key, ckey, (int)klen);
	key[klen] = '\0';

	decl_control(key, type, size, value);
	return;
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : decldim() to be called from C
 *            decldim_() to be called from Fortran
 *            declfix() to be called from C
 *            declfix_() to be called from Fortran
 * COMMENT  : initializes an entry in the dimension database
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define DECLDIM_C
#include <stdio.h>
#include <string.h>
#include "mms.h"

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : decldim_
 | COMMENT		: called from Fortran, sorts out args and calls decldim()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long decldim_ (char *dname, ftnint *dval, ftnint *dmax, char *ddescr, ftnlen namelen, ftnlen descrlen) {
	long value, max;
	char *name, *descr;
	long retval;

/*
* copy value & max into local long int
*/

	value = *dval;
	max = *dmax;

/*
* copy args to new strings, and terminate correctly
*/

	name = (char *) umalloc(namelen + 1);
	strncpy(name, dname, namelen);
	name[namelen] = '\0';

	descr = (char *) umalloc(descrlen + 1);
	strncpy(descr, ddescr, descrlen);
	descr[descrlen] = '\0';

/*
* call C version of decldim()
*/

	retval = decldim(name, value, max, descr);

	return(retval);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : decldim
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long decldim (char *name, long value, long max, char *descr) {
	DIMEN *dim;

/*
* check that name does not already exist
*/

	dim = dim_addr(name);
   if (dim != NULL) {
		// This dimension has already been declared. Set the size to the
		// value of the last call.
		dim->value = value;

      return(0);
   }

   if (Mdebuglevel >= M_FULLDEBUG) {
      (void)fprintf(stderr, "Declaring dimension '%s'\n", name);
   }

/*
* check that default value is within limits
*/

   if(value < 0) {
      (void)fprintf(stderr, 
		    "ERROR - decldim - default dimension value negative.\n");
      (void)fprintf(stderr, "Name   :   '%s'\n", name);
      (void)fprintf(stderr, "Default:   %ld\n", value);
      return(1);
   }

   if(value > max) {
      (void)fprintf(stderr, 
        "ERROR - decldim - default dimension value exceeds max. allowed\n");
      (void)fprintf(stderr, "Name   :   '%s'\n", name);
      (void)fprintf(stderr, "Default:   %ld\n", value);
      (void)fprintf(stderr, "Max    :   %ld\n", max);
      return(1);
   }

/*
* allocate space for a structure, and store pointer in dimbase
*/
   dim = (DIMEN *) umalloc (sizeof(DIMEN));
   ADD_to_list (dim_db, (void *)dim);

/*
* allocate space, and store dimension properties
*/
   if (descr) dim->descr = strdup (descr);
   else dim->descr = NULL;

   if (name) dim->name = strdup (name);
   else dim->name = NULL;

   dim->value = value;
   dim->max = max;
   dim->names = NULL;
   dim->notes = NULL;
   dim->files = NULL;
   dim->format = NULL;
   dim->column_width = 10;
   dim->fixed = FALSE;
   dim->got = FALSE;

   sort_dims ();
   return(0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declfix
 | COMMENT		: Called from C to declare a fixed dimension.
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declfix (char *name, long value, long max, char *descr) {
   long ret;

   ret = decldim (name, value, max, descr);
   ((DIMEN *)(dim_db->itm[dim_db->count - 1]))->fixed = TRUE;

   return (ret);
}
/*--------------------------------------------------------------------*\
 | FUNCTION     : declfix_
 | COMMENT		: called from Fortran to declare a fixed dimension.
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declfix_ (char *dname, ftnint *dval, ftnint *dmax, char *ddescr, ftnlen namelen, ftnlen descrlen) {
	long	ret;

	ret = decldim_ (dname, dval, dmax, ddescr, namelen, descrlen);
	((DIMEN *)(dim_db->itm[dim_db->count - 1]))->fixed = TRUE;

	return (ret);
}
/*--------------------------------------------------------------------*\
 | FUNCTION     : getmodule
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
//MODULE_DATA * getmodule (char *key) { 
//	MODULE_DATA *module;
//	long i;
//
//	for (i = 0; i < module_db->count; i++) {
//		module = (MODULE_DATA *)(module_db->itm[i]);
//	   printf ("comparing %s to %s\n", key, module->name);
//		if (!strcmp(module->name, key))
//		return module;
//	}
//
//	/* if no match found, return null */
//	return NULL;
//}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : declparam() to be called from C
 *            declparam_() to be called from Fortran
 *            Returns 0 if successful, 1 otherwise.
 * COMMENT  : initializes a module variable entry in the memory database
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define DECLPARAM_C
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "mms.h"

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/
static int CHECK_param_in_db (char *, char *, char *, int,
 	char *, char *, char *, char *, char *, char *);
static int VAR_type (char *);

/**5*********************** LOCAL VARIABLES ***************************/
static char *types[] = {"long (or integer)", "real (or float)", "double", "string"};

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/

/*--------------------------------------------------------------------*\
 | FUNCTION     : declparam_
 | COMMENT		: declparam_() is called from Fortran, sorts out args
 |                 and calls declparam()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declparam_u_ (char *mname, char *pname, char *pdimen, char *ptype,
	char *pvalstr, char *minstr, char *maxstr, char *dstr, char *hstr,
	char *ustr, char *var, long *update, ftnlen mnamelen,
	ftnlen pnamelen, ftnlen pdimenlen, ftnlen ptypelen,
	ftnlen pvallen, ftnlen minlen, ftnlen maxlen, ftnlen dlen,
	ftnlen hlen, ftnlen ulen, ftnlen varlen, ftnlen uplen) {

	char *module, *name, *dimen, *type, *value;
	char *minimum, *maximum, *descr, *help, *units;
	long retval;

/*
* copy args to new strings, and terminate correctly
*/

	module = (char *) umalloc(mnamelen + 1);
	strncpy(module, mname, mnamelen);
	module[mnamelen] = '\0';

	name = (char *) umalloc(pnamelen + 1);
	strncpy(name, pname, pnamelen);
	name[pnamelen] = '\0';

	dimen = (char *) umalloc(pdimenlen + 1);
	strncpy(dimen, pdimen, pdimenlen);
	dimen[pdimenlen] = '\0';

	type = (char *) umalloc(ptypelen + 1);
	strncpy(type, ptype, ptypelen);
	type[ptypelen] = '\0';

	value = (char *) umalloc(pvallen + 1);
	strncpy(value, pvalstr, pvallen);
	value[pvallen] = '\0';

	minimum = (char *) umalloc(minlen + 1);
	strncpy(minimum, minstr, minlen);
	minimum[minlen] = '\0';

	maximum = (char *) umalloc(maxlen + 1);
	strncpy(maximum, maxstr, maxlen);
	maximum[maxlen] = '\0';

	descr = (char *) umalloc(dlen + 1);
	strncpy(descr, dstr, dlen);
	descr[dlen] = '\0';

	help = (char *) umalloc(hlen + 1);
	strncpy(help, hstr, hlen);
	help[hlen] = '\0';

	units = (char *) umalloc(ulen + 1);
	strncpy(units, ustr, ulen);
	units[ulen] = '\0';

/*
* call C version of declparam_u()
*/

	retval = declparam_u(module, name, dimen, type, value,
	    minimum, maximum, descr, help, units, var, update);

	return(retval);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declparam_u
 | COMMENT		: declparam is called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declparam_u (char *module, char *name, char *dimen, char *type, char *value,
	char *minimum, char *maximum, char *descr, char *help, char *units, char *var,
	long *update) {

	PARAM *param;

	*update = 0;
/*
* get pointer to parameter with key
*/

	param = param_addr(name);

	if (param == NULL) {  // Parameter has not been declared, do so now. Set up array for pointers to local arrays of values.
		declparam (module, name, dimen, type, value, minimum, maximum, descr, help, units);
		param = param_addr(name);
		param->num_references = 0;
		param->size_references = 100;
		param->references = (void **)umalloc (param->size_references * sizeof(void *));
	}

/*
**	 realloc if too large
*/
	if (param->num_references >= param->size_references - 1) {
		param->size_references += 100;
		param->references = (void **) urealloc ((char *)(param->references),
			param->size_references * sizeof(void *));
	}

	param->references[param->num_references++] = var;

	return 0;
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declparam_
 | COMMENT		: declparam_() is called from Fortran, sorts out args
 |                 and calls declparam()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declparam_ (char *mname, char *pname, char *pdimen, char *ptype,
	char *pvalstr, char *minstr, char *maxstr, char *dstr, char *hstr,
	char *ustr, ftnlen mnamelen, ftnlen pnamelen, ftnlen pdimenlen, ftnlen ptypelen,
	ftnlen pvallen, ftnlen minlen, ftnlen maxlen, ftnlen dlen, ftnlen hlen, ftnlen ulen) {

	char *module, *name, *dimen, *type, *value;
	char *minimum, *maximum, *descr, *help, *units;
	long retval;

/*
* copy args to new strings, and terminate correctly
*/

	module = (char *) umalloc(mnamelen + 1);
	strncpy(module, mname, mnamelen);
	module[mnamelen] = '\0';

	name = (char *) umalloc(pnamelen + 1);
	strncpy(name, pname, pnamelen);
	name[pnamelen] = '\0';

	dimen = (char *) umalloc(pdimenlen + 1);
	strncpy(dimen, pdimen, pdimenlen);
	dimen[pdimenlen] = '\0';

	type = (char *) umalloc(ptypelen + 1);
	strncpy(type, ptype, ptypelen);
	type[ptypelen] = '\0';

	value = (char *) umalloc(pvallen + 1);
	strncpy(value, pvalstr, pvallen);
	value[pvallen] = '\0';

	minimum = (char *) umalloc(minlen + 1);
	strncpy(minimum, minstr, minlen);
	minimum[minlen] = '\0';

	maximum = (char *) umalloc(maxlen + 1);
	strncpy(maximum, maxstr, maxlen);
	maximum[maxlen] = '\0';

	descr = (char *) umalloc(dlen + 1);
	strncpy(descr, dstr, dlen);
	descr[dlen] = '\0';

	help = (char *) umalloc(hlen + 1);
	strncpy(help, hstr, hlen);
	help[hlen] = '\0';

	units = (char *) umalloc(ulen + 1);
	strncpy(units, ustr, ulen);
	units[ulen] = '\0';

/*
* call C version of declparam()
*/

	retval = declparam(module, name, dimen, type, value,
	    minimum, maximum, descr, help, units);

	return(retval);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declparam
 | COMMENT		: declparam is called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declparam (char *module, char *name, char *dimen, char *type, char *value,
	char *minimum, char *maximum, char *descr, char *help, char *units) {

	int var_type;
	char *pkey;
	char *token;
	char *tmpdimen;
	long i, retval;

	DIMEN *dim;
	PARAM **params, *param;

/*
**	 realloc if too large
*/
	if (Mnparams >= max_params - 1) {
		max_params += 100;
		Mparambase = (PARAM **) urealloc ((char *)Mparambase,
			max_params * sizeof(PARAM *));
	}

/*
* compute the key
*/
/*
  pkey = (char *) umalloc(strlen(module) + strlen(name) + 2);
  (void)strcpy(pkey, module);
  strcat(strcat(pkey, "."), name);
*/
	pkey = strdup (name);

	if (!(var_type = VAR_type (type)))
		return (0);

	if (CHECK_param_in_db (pkey, module, dimen, var_type, value,
									minimum, maximum, descr, help, units)) {
		return (0);
	}

	if (Mdebuglevel >= M_FULLDEBUG)
		(void)fprintf (stderr, "Declaring param '%s'\n", pkey);
/* 
* get params from Mparambase, the global pointer
*/
	params = Mparambase;
	Mnparams += 1;

/*
* allocate space for a structure, and store pointer in params
*/
	params[Mnparams-1] = param = (PARAM *) umalloc (sizeof(PARAM));
	memset ((char *)param, 0, sizeof(PARAM));

	param->key = pkey;
	param->module = strdup (module);
	param->name = strdup (name);
	param->min_string = strdup (minimum);
	param->max_string = strdup (maximum);
	param->def_string = strdup (value);
	param->value_string = strdup (value);
	param->descr = strdup (descr);
	param->help = strdup (help);
	param->units = strdup (units);
	param->def = strdup (value);
	param->column_width = 4;
	param->type = var_type;
	param->read_in = 0;
	param->preprocess = FALSE;

/*
* determine dimensions
*/
	if (dimen == NULL) { // If dimen is NULL then this is a mapping parameter declared by read_params and not any module.  Most of the information is unknown so return.
		sort_params();
		return(0);
	}

	tmpdimen = strdup (dimen);
	token = strtok (tmpdimen, ",");

	while (token != (char *) NULL) {
		param->ndimen++;
		token = strtok((char *) NULL, ",");
	}

	if (param->ndimen > MAX_NDIMEN) {
		(void)fprintf(stderr, "ERROR - declparam\n");
		(void)fprintf(stderr, "Attempt to use %ld dimensions - this is too many.\n",
		    param->ndimen);
		(void)fprintf(stderr, "Max number of dimensions allowed : %d.\n", MAX_NDIMEN);
		(void)fprintf(stderr, "Key is '%s'.\n", pkey);
		return(1);
	}

	param->dimen = (DIMEN **)umalloc (param->ndimen * sizeof (DIMEN *));

	(void)strcpy (tmpdimen, dimen);
	token = strtok (tmpdimen, ",");

	i = 0;
	while (token != (char *) NULL) {
		param->dimen[i++] = dim_addr (token);
		token = strtok ((char *) NULL, ",");
	}

/*
* check to see if the parameter values are to be bounded by a dimension.
* If so, set the bound and bound_dimen fields, and set the min and max strings
* as applicable. If bounded, the 'minimum' string is set to "bounded",
* and the 'maximum' string contains the name of the bounding dimension.
*/
	if (!strcmp (minimum, "bounded")) {
		param->bound_status = M_BOUNDED;

		if (param->type != M_LONG) {
			(void)fprintf (stderr, "ERROR - declparam\n");
			(void)fprintf (stderr, "Parameter '%s'\n", pkey);
			(void)fprintf (stderr,
			    "Attempt to bound with parameter type '%s'\n",
			    Mtypes[param->type]);
			(void)fprintf(stderr, "Only 'long' parameters may be bounded.\n");
			return(1);
		}

		if (!(param->bound_dimen = dim_addr (maximum))) {
			(void)fprintf(stderr, "ERROR - declparam\n");
			(void)fprintf(stderr, "Parameter '%s'\n", pkey);
			(void)fprintf(stderr, "Attempt to bound with dimension name '%s' %s\n",
			    maximum, "which has not been declared.");
			return(1);
		}
	} else {
		param->bound_status = M_UNBOUNDED;
		param->bound_dimen = NULL;
	}

/*
* get the size of the parameter
*/
	param->size = 1;
	for (i = 0; i < param->ndimen; i++) {
		dim = param->dimen[i];
		if (dim) {
			param->size *= dim->value;
		} else {
			(void)fprintf (stderr, "ERROR - declparam\n");
			(void)fprintf (stderr, "Parameter '%s'\n", pkey);
			(void)fprintf (stderr, "Dimension '%s' not declared.\n", dim->name);
			return (1);
		}
	}

/*
* if size is zero, set size to 1 so that there is at least one
* entry in the data base. This is necesary so that the default, 
* maximum and minimum values will be retained for use when
* the dimension is set to a non-zero value
*/
	if (!param->size)
		param->size = 1;

/*
**	Load the values indicated by the default, minimum, and maximum
**	strings into the arrays.
*/
	retval = load_param (param);
	if (retval) return (retval);

/*
**	Set up the pointers to the description strings.
*/
//	param->value_desc = (char **)umalloc (param->size * sizeof (char *));
//	for (i = 0; i < param->size; i++)
//		param->value_desc[i] = NULL;

	sort_params();
	return(0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declparam_p_
 | COMMENT		: declparam_p() is called from Fortran, sorts out args
 |                 and calls declparam()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declparam_p_ (char *mname, char *pname, char *pdimen, char *ptype,
	char *pvalstr, char *minstr, char *maxstr, char *dstr, char *hstr,
	char *ustr, char *val, ftnlen mnamelen, ftnlen pnamelen,
	ftnlen pdimenlen, ftnlen ptypelen, ftnlen pvallen, ftnlen minlen,
	ftnlen maxlen, ftnlen dlen, ftnlen hlen, ftnlen ulen, ftnlen vallen) {

	char *module, *name, *dimen, *type, *value;
	char *minimum, *maximum, *descr, *help, *units;
	long retval;

/*
* copy args to new strings, and terminate correctly
*/

	module = (char *) umalloc(mnamelen + 1);
	strncpy(module, mname, mnamelen);
	module[mnamelen] = '\0';

	name = (char *) umalloc(pnamelen + 1);
	strncpy(name, pname, pnamelen);
	name[pnamelen] = '\0';

	dimen = (char *) umalloc(pdimenlen + 1);
	strncpy(dimen, pdimen, pdimenlen);
	dimen[pdimenlen] = '\0';

	type = (char *) umalloc(ptypelen + 1);
	strncpy(type, ptype, ptypelen);
	type[ptypelen] = '\0';

	value = (char *) umalloc(pvallen + 1);
	strncpy(value, pvalstr, pvallen);
	value[pvallen] = '\0';

	minimum = (char *) umalloc(minlen + 1);
	strncpy(minimum, minstr, minlen);
	minimum[minlen] = '\0';

	maximum = (char *) umalloc(maxlen + 1);
	strncpy(maximum, maxstr, maxlen);
	maximum[maxlen] = '\0';

	descr = (char *) umalloc(dlen + 1);
	strncpy(descr, dstr, dlen);
	descr[dlen] = '\0';

	help = (char *) umalloc(hlen + 1);
	strncpy(help, hstr, hlen);
	help[hlen] = '\0';

	units = (char *) umalloc(ulen + 1);
	strncpy(units, ustr, ulen);
	units[ulen] = '\0';

/*
* call C version of declparam_p()
*/

	retval = declparam_p(module, name, dimen, type, value,
	    minimum, maximum, descr, help, units, val);

	return(0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declparam_p
 | COMMENT		: declparam is called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declparam_p (char *module, char *name, char *dimen, char *type, char *value,
	char *minimum, char *maximum, char *descr, char *help, char *units, char *var) {
	PARAM *param;

	// If the -preprocess command line arguement is not set, don't allow declaration of any "preprocess parameters."
	if (!preprocess_on) {
		return 0;
	}

/*
* get pointer to parameter with key
*/

	param = param_addr(name);

	if (param == NULL) {  // Parameter has not been declared, do so now. Set up array for pointers to local arrays of values.
		declparam (module, name, dimen, type, value, minimum, maximum, descr, help, units);
		param = param_addr(name);
		param->num_references = 0;
		param->size_references = 100;
		param->references = (void **)umalloc (param->size_references * sizeof(void *));
		param->preprocess = TRUE;
	}

/*
**	 realloc if too large
**  LOOK AT THIS!
*/
	if (param->num_references >= param->size_references - 1) {
		param->size_references += 100;
		param->references = (void **) urealloc ((char *)(param->references),
			param->size_references * sizeof(void *));
	}

	param->references[param->num_references++] = var;

	return 0;
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : CHECK_param_in_db
 | COMMENT      : Check if this parameter is already in the parameter DB.
 | PARAMETERS   : char *pkey -  parameter key
 | RETURN VALUE : 0 = not found; 1 = found
 | RESTRICTIONS : None
\*--------------------------------------------------------------------*/
static int CHECK_param_in_db (char *pkey, char *module, char *dimen, int var_type,
 	char *value, char *minimum, char *maximum, char *descr, char *help,
 	char *units) {

	PARAM *check_param;
	int		inconsistent, i;
	char	buf[1024], buf1[256];
	char dim_names[256];

	check_param = param_addr (pkey);
	if (check_param) {

		inconsistent = FALSE;

		(void)sprintf (buf, "The parameter %s has been declared inconsistently in the modules %s and %s.", pkey, module, check_param->module);

		/*
		 * Get all dimensions of previously declared parameters in the
		 * original format.
		 */
		strcpy(dim_names, check_param->dimen[0]->name);
		for (i = 1; i < check_param->ndimen; i++){
		  sprintf(dim_names,"%s,%s",dim_names,check_param->dimen[i]->name);
		}
		if (strcmp (dimen, dim_names)) {
			inconsistent = TRUE;
			(void)sprintf (buf1, " The dimensions have been declared as %s and %s.", dimen, dim_names);
			strcat (buf, buf1);
		}

		if (var_type != check_param->type) {
			inconsistent = TRUE;
			(void)sprintf (buf1, " The types have been declared as %s and %s.", types[var_type], types[check_param->type]);
			strcat (buf, buf1);
		}

		if (strcmp (value, check_param->value_string)) {
			inconsistent = TRUE;
			(void)sprintf (buf1, " The default values have been declared as %s and %s.", value, check_param->value_string);
			strcat (buf, buf1);
		}

		if (strcmp (minimum, check_param->min_string)) {
			inconsistent = TRUE;
			(void)sprintf (buf1, " The minimum value has been declared as %s and %s.", minimum, check_param->min_string);
			strcat (buf, buf1);
		}

		if (strcmp (maximum, check_param->max_string)) {
			inconsistent = TRUE;
			(void)sprintf (buf1, " The maximum value has been declared as %s and %s.", maximum, check_param->max_string);
			strcat (buf, buf1);
		}

		if (strcmp (descr, check_param->descr)) {
			inconsistent = TRUE;
			(void)sprintf (buf1, " The parameter description has been set as \"%s\"and \"%s.\"", descr, check_param->descr);
			strcat (buf, buf1);
		}

		if (strcmp (units, check_param->units)) {
			inconsistent = TRUE;
			(void)sprintf (buf1, " The units have been set as %s and %s.", help, check_param->help);
			strcat (buf, buf1);
		}

		if (inconsistent)
		    fprintf(stderr, buf);

		return (1);
	}

	return (0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : VAR_type
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
static int VAR_type (char *type) {
	if (!strcmp (type, "integer") || !strcmp (type, "long")) {
		return (M_LONG);
	}

	if (!strcmp (type, "real") || !strcmp (type, "float")) {
	   return (M_FLOAT);
    }

	if (!strcmp (type, "double precision") || !strcmp (type, "double")) {
	   return (M_DOUBLE);
    }

	if (!strcmp (type, "string")) {
	   return (M_STRING);
    }
	(void)fprintf(stderr, "ERROR - declparam - type '%s' is illegal.\n", type);
		return (0);
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : declvar() to be called from C
 *            declvar_() to be called from Fortran
 *            Returns 0 if successful, 1 otherwise.
 * COMMENT  : initializes a module variable entry in the memory database
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define DECLVAR_C
#include <stdio.h>
#include <string.h>
#include "mms.h"

#define LONG 1
#define FLOAT 2
#define DOUBLE 3

/*--------------------------------------------------------------------*\
 | FUNCTION     : declvar_
 | COMMENT		: called from Fortran, sorts out args and calls declvar()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declvar_ (char *mname, char *vname, char *vdimen, ftnint *maxsizeptr,
	char *vtype, char *hstr, char *ustr, char *value, ftnlen mnamelen,
	ftnlen vnamelen, ftnlen vdimenlen, ftnlen vtypelen, ftnlen hlen, ftnlen ulen) {

  char *module, *name, *dimen, *type, *help, *units;
  long maxsize, retval;

  /*
   * copy maxsize to local long int
   */

  maxsize = *maxsizeptr;

  /*
   * copy args to new strings, and terminate correctly
   */

  module = (char *) umalloc((unsigned int)(mnamelen + 1));
  strncpy(module, mname, (int)mnamelen);
  module[mnamelen] = '\0';

  name = (char *) umalloc((unsigned int)(vnamelen + 1));
  strncpy(name, vname, (int)vnamelen);
  name[vnamelen] = '\0';

  dimen = (char *) umalloc((unsigned int)(vdimenlen + 1));
  strncpy(dimen, vdimen, (int)vdimenlen);
  dimen[vdimenlen] = '\0';

  type = (char *) umalloc((unsigned int)(vtypelen + 1));
  strncpy(type, vtype, (int)vtypelen);
  type[vtypelen] = '\0';

  help = (char *) umalloc((unsigned int)(hlen + 1));
  strncpy(help, hstr, (int)hlen);
  help[hlen] = '\0';

  units = (char *) umalloc((unsigned int)(ulen + 1));
  strncpy(units, ustr, (int)ulen);
  units[ulen] = '\0';

  /*
   * call C version of declvar()
   */

  retval = declvar(module, name, dimen, maxsize, type, help, units, value);

  return(retval);

}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declvar()
 | COMMENT		: is called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declvar (char *module, char *name, char *dimen, long maxsize, char *type,
	char *help, char *units, char *value) {
  int var_type;

  char *vkey;
  char *token;
  char *tmpdimen;
  long i, size;

  PUBVAR **vars, *var;

  /*
   * realloc if too large
   */

  if(Mnvars >= max_vars -1) {
	max_vars += 100;
  	Mvarbase = (PUBVAR **)urealloc ((char *)Mvarbase,
		max_vars * sizeof(PUBVAR *));
  }

  /*
   * compute the key
   */

  vkey = strdup (name);

  if (var_addr(vkey) != NULL) {
	  if (print_mode) {
	      return(0);
	  } else {
              fprintf(stderr,
	      "ERROR - declvar - key '%s' already exists.\n", vkey);
              return(1); }
  }

  /*
   * convert fortran types to C equivalents
   */

  var_type = M_LONG;
  if (!strcmp(type, "real") || !strcmp(type, "float"))
    var_type = M_FLOAT;
  else if (!strcmp(type, "double precision") || !strcmp(type, "double"))
    var_type = M_DOUBLE;

  /*
   * check that type is possible
   */

	if((var_type != M_LONG) && (var_type != M_FLOAT) && (var_type != M_DOUBLE))
		{
    	(void)fprintf(stderr,
	    	"ERROR - declvar - type '%s' is illegal.\n", type);
    	(void)fprintf(stderr, "Key is '%s'.\n", vkey);
    	(void)fprintf(stderr, "Type is '%s'.\n", type);
    	return(1);
  		}

  /*  
   * get vars from Mvarbase, the global pointer
   */


  if (Mdebuglevel >= M_FULLDEBUG) {
    (void)fprintf(stderr, "Declaring variable '%s'\n", vkey);
  }

  /*
   * allocate space for a structure, and store pointer in vars
   */
  Mnvars += 1;

  vars = Mvarbase;

  var = (PUBVAR *) umalloc (sizeof(PUBVAR));
  vars[Mnvars-1] = var; /* copy address into vars array */

  /*
   * determine dimensions
   */

  tmpdimen = strdup (dimen);

  var->ndimen = 0;

  token = strtok (tmpdimen, ",");

  while (token != (char *) NULL) {
    var->ndimen++;
    token = strtok((char *) NULL, ",");
  }

  if (var->ndimen > MAX_NDIMEN) {

    (void)fprintf(stderr, "ERROR - declvar\n");
    (void)fprintf(stderr, "Attempt to use %ld dimensions - this is too many.\n",
	    var->ndimen);
    (void)fprintf(stderr, "Max number of dimensions allowed : %d.\n", MAX_NDIMEN);
    (void)fprintf(stderr, "Key is '%s'.\n", vkey);
    return(1);

  }

  var->dimen = (DIMEN **)umalloc (var->ndimen * sizeof(DIMEN *));

  (void)strcpy (tmpdimen, dimen);

  i = 0;
  token = strtok(tmpdimen, ",");

  while (token != (char *) NULL) {
    if (!(var->dimen[i] = dim_addr (token))) {
      (void)fprintf(stderr, "ERROR - declvar\n");
      (void)fprintf(stderr, "Variable '%s'\n", vkey);
      (void)fprintf(stderr, "Dimension '%s' is not declared.\n", token);
      return(1);
    }
    token = strtok ((char *) NULL, ",");
    i++;
  }

  /*
   * get the size of the variable
   */
  
  size = 1;

  for (i = 0; i < var->ndimen; i++) {

    size *= var->dimen[i]->value;

  }

  var->size = size;

  if (size > maxsize) {

    (void)fprintf(stderr,
	    "ERROR - declvar - dimension exceeds space available.\n");
    (void)fprintf(stderr, "Key is '%s'.\n", vkey);
    (void)fprintf(stderr, "Size is %ld.\n", size);
    for (i = 0; i < var->ndimen; i++) {
      (void)fprintf (stderr, "Dimension '%s' is %ld.\n",
	      var->dimen[i]->name, var->dimen[i]->value);
    }
    (void)fprintf(stderr, "Space available is %ld.\n", maxsize);
    return(1);

  }

  /*
   * allocate space, and store variable properties
   */

  var->key = vkey;
  var->module = strdup (module);
  var->name = strdup (name);

  if(var_type == M_DOUBLE)
    var->type = M_DOUBLE;
	else 
		if (var_type == M_FLOAT)
    		var->type = M_FLOAT;
  			else 
				if (var_type == M_LONG) 
    				var->type = M_LONG;
  var->value = value;
  var->help = strdup (help);
  var->units = strdup (units);
  var->private = FALSE;

  sort_vars();

  return(0);
  
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declpri_
 | COMMENT		: called from Fortran, sorts out args and calls declpri()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declpri_ (char *vname, ftnint *maxsizeptr,
	char *vtype, char *value,
	ftnlen vnamelen, ftnlen vtypelen) {

  char *name, *type;
  long maxsize, retval;

  /*
   * copy maxsize to local long int
   */

  maxsize = *maxsizeptr;

  /*
   * copy args to new strings, and terminate correctly
   */

  name = (char *) umalloc((unsigned int)(vnamelen + 1));
  strncpy(name, vname, (int)vnamelen);
  name[vnamelen] = '\0';

  type = (char *) umalloc((unsigned int)(vtypelen + 1));
  strncpy(type, vtype, (int)vtypelen);
  type[vtypelen] = '\0';

  /*
   * call C version of declpri()
   */

  retval = declpri(name, maxsize, type, value);

  /*
   * free up allocated strings
   */

  return(retval);

}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declpri()
 | COMMENT		: is called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declpri (char *name, long size, char *type, char *value) {
  int var_type;

  char *vkey;

  PUBVAR **vars, *var;

  /*
   * realloc if too large
   */

  if(Mnvars >= max_vars -1) {
	max_vars += 100;
  	Mvarbase = (PUBVAR **)urealloc ((char *)Mvarbase,
		max_vars * sizeof(PUBVAR *));
  }

  /*
   * compute the key
   */

  vkey = strdup (name);

  if (var_addr(vkey) != NULL) {
    (void)fprintf(stderr,
	    "ERROR - declvar - key '%s' already exists.\n", vkey);
    return(1);
  }

  /*
   * convert fortran types to C equivalents
   */

  var_type = M_LONG;
  if (!strcmp(type, "real") || !strcmp(type, "float"))
    var_type = M_FLOAT;
  else if (!strcmp(type, "double precision") || !strcmp(type, "double"))
    var_type = M_DOUBLE;

  /*
   * check that type is possible
   */

	if((var_type != M_LONG) && (var_type != M_FLOAT) && (var_type != M_DOUBLE))
		{
    	(void)fprintf(stderr,
	    	"ERROR - declvar - type '%s' is illegal.\n", type);
    	(void)fprintf(stderr, "Key is '%s'.\n", vkey);
    	(void)fprintf(stderr, "Type is '%s'.\n", type);
    	return(1);
  		}

  /*  
   * get vars from Mvarbase, the global pointer
   */


  if (Mdebuglevel >= M_FULLDEBUG) {
    (void)fprintf(stderr, "Declaring private variable '%s'\n", vkey);
  }

  /*
   * allocate space for a structure, and store pointer in vars
   */
  Mnvars += 1;

  vars = Mvarbase;

  var = (PUBVAR *) umalloc (sizeof(PUBVAR));
  vars[Mnvars-1] = var; /* copy address into vars array */

  /*
   * get the size of the variable
   */
  
  var->size = size;

  /*
   * allocate space, and store variable properties
   */

  var->key = vkey;
  var->module = NULL;
  var->name = strdup (name);

   if(var_type == M_DOUBLE) var->type = M_DOUBLE;
   else if (var_type == M_FLOAT) var->type = M_FLOAT;
   else if (var_type == M_LONG) var->type = M_LONG;

   var->value = value;
   var->help = NULL;
   var->units = NULL;
   var->private = TRUE;

   sort_vars();

   return(0);
}

/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : dim_addr
 *            returns a pointer to a DIMEN struct which contains the given name
 *            returns NULL if name not found
 * COMMENT  :
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define DIM_ADDR_C
#include <stdio.h>
#include <string.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : dim_addr
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
DIMEN *dim_addr (char *name) { 
	long i;

	if (!dim_db->count)
		return (NULL);

	for (i = 0; i < dim_db->count; i++)
		if (!strcmp (((DIMEN *)(dim_db->itm[i]))->name, name))
			return ((DIMEN *)(dim_db->itm[i]));

	return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : dim_notes
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *dim_notes (char *ch_ptr) {
	int		i, j;
	DIMEN	*dim;

	for (i = 0; i < dim_db->count; i++) {
		dim = (DIMEN *)(dim_db->itm[i]);
		for (j = 0; j < dim->value; j++)
			if (dim->names && dim->names[j] && (!strcmp (dim->names[j],ch_ptr)))
				return (dim->notes[j]);
	}

	return (NULL);
}

/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : free_vstats
 * COMMENT  : free linked list for stats variables
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define FREE_VSTATS_C
#include <stdio.h>
#include <string.h>
#include "mms.h"

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : free_vstats
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void free_vstats (void) {
  long nstatVars;
  STAT_LIST_TYPE *curr_stat_list, *prev_stat_list;

  nstatVars = *control_lvar("nstatVars");

  if (nstatVars > 0) {

    curr_stat_list  = Mfirst_stat_list;

    while (curr_stat_list->next != NULL) {
      	prev_stat_list = curr_stat_list;
		curr_stat_list = prev_stat_list->next;
    }

	Mfirst_stat_list = NULL;
  }
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : get_elem_add
 * COMMENT  : This file contains utility routines for multiple index arrays.
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define GET_ELEM_ADD_C
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include "mms.h"

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : CheckIndices
 | COMMENT		: Verifies that the number of indices
 |                 passed as an argument is compatible with the indices
 |                 declared for a parameter or a variable.
 | PARAMETERS   :
 |     
 |      key:        is the name of the parameter or variable
 |      elemString: is the string that contains the elements
 |                  separated by commas
 |      type:       = M_PARAMETER for parameters, 
 |                  = M_VARIABLE for variables.
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int CheckIndices (char *key, char *elemString, int type) {
	PARAM	*paddr;
	PUBVAR	*vaddr;
	DIMEN	*dim;
	int      nd;
	char	tsave[80], *temp, *t, *ptr;
	char	**strindx;
	DIMEN	**dimname;
	int		*intindx;
	int		nindex;
	int		i, k, nk;
	int		list_size, list_count;

/*
**	Get the address of the array.
*/
	if (type == M_PARAMETER) {
	    if (!(paddr = param_addr (key)))
   		   	return (1);

		nd = paddr->ndimen;
	    dimname = paddr->dimen;

  	} else {
		if (!(vaddr = var_addr (key)))
			return (1);

		nd = vaddr->ndimen;
		dimname = vaddr->dimen;
	}

/*
**	Parse the index string. First make a local copy
*/
	(void)strcpy (tsave, elemString);

/*
**	Check for '(' and ')', and delete them
*/
	t = strchr (tsave, '(');
	if (t) temp = t;
	else temp = tsave;
  
	t = strchr (temp, ')');
	if (t) *t = '\0';
/*
**	parse the string, build up a list of the multidimensional indices.
*/
	t = temp;
	nindex = 0;

	list_size = 100;
	list_count = 0;
	strindx = (char **)malloc (list_size * sizeof (char *));

	while (t) {
		if (list_count >= list_size) {
			list_size += 100;
			strindx = (char **)realloc (strindx, list_size * sizeof (char *));
		}
		ptr = strchr (t, ',');

		if (ptr)
			*ptr = '\0';

		strindx[list_count] = (char *)malloc (20 * sizeof (char));
		(void)strcpy(strindx[list_count], t);
		list_count++;

		if (ptr)
			t = ptr + 1;
		else
			t = NULL;
	} 

	nindex = list_count;
/*
**	compare number of indices
*/
	if (nd != nindex){
		return (2);
	}
                                /* check if indices are numeric values */
/*
**	ANSI-CHANGE
**  bad assignment type: long * = int *
**	changed declreation from long * to int *
*/
	intindx = (int *)calloc (nindex, sizeof (int));
  
	for (i = 0; i < nindex; i++) {
/*
**	check if all digits are numeric
*/
		t = strindx[i];
		k = 0;
		nk = strlen(strindx[i]);
    
		while (isdigit (t[k]) && (k < nk-1 )) k++;
    
		if (!isdigit (t[k]))
			return(3);
   
		intindx[i] = atoi (strindx[i]);

		if (intindx[i] < 1)
			return(4);

/*
**	get address of ith dimension
*/
		dim = dimname[i];
    
		if (intindx[i] > (int)dim->value)
			return(5);
	}

	return(0);
}
  
/*--------------------------------------------------------------------*\
 | FUNCTION     : GetElemAddress
 | COMMENT		: This function returns a pointer to the memory location
 |    corresponding to elemString. The format of the string should each
 |    such that each component is separated by a comma. For example:
 |
 |    "3" for single-element arrays, or
 |    "2,4" for two-element arrays, or
 |    "4,1,3" for three-element arrays.
 |
 | PARAMETERS   :
 |      key:        is the name of the parameter or variable
 |      elemString: is the string that contains the elements
 |                  separated by commas
 |      type:       = M_PARAMETER for parameters, 
 |                  = M_VARIABLE for variables.
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *GetElemAddress (char *key, char *elemString, int type) {
	PARAM	*paddr;
	PUBVAR	*vaddr;
	DIMEN	*dim;
	char	*addr;
	char	*temp, *t, tkey[80], elmstr[80], *ptr;
	char	**strindx;
	DIMEN	**dimname;
	long     offset;
	long     prod;
	int      nindex;
	int      vtype;
	int      i;
	int		list_size, list_count;

/*
**	first, make a temporary copy of the key
*/
	(void)strcpy (tkey, key);

/*
**	strips leading blanks
*/
	ptr = tkey;
	while (*ptr == ' ')
		ptr++;
/*
**	strips trailing blanks
*/
	if ((temp = strchr (ptr, ' ')))
		*temp = '\0';
	if ((temp = strchr (ptr, '.')))
		*temp = '\0';

	if (type == M_PARAMETER) {
		paddr = param_addr (ptr);
/*
**	check number of dimensions
*/
		vtype = paddr->type;
		dimname = paddr->dimen;
		addr = paddr->value;
	} else {
		vaddr = var_addr(tkey);
/*
**	check number of dimensions
*/
		if (vaddr) {
			dimname = vaddr->dimen;
			vtype = vaddr->type;
			addr = vaddr->value;
		} else {
			(void)fprintf (stderr,"GetElemAddress: %s does not exist\n", ptr);
			return (NULL);
		}
	}

/*
**	parse the string. First make a local copy
*/
	(void)strcpy (elmstr, elemString);
	temp = elmstr; 

/*
**	check for '(' and ')', and delete them
*/
	t = strchr (temp, '(');
	if (t) temp = t;
  
	t = strchr (temp, ')');
	if (t) *t = '\0';
/*
**	parse the string
*/
	t = temp;
	nindex = 0;

	list_size = 100;
	list_count = 0;
	strindx = (char **)malloc (list_size * sizeof (char *));

	while (t) {
		if (list_count >= list_size) {
			list_size += 100;
			strindx = (char **)realloc (strindx, list_size * sizeof (char *));
		}
		ptr = strchr (t, ',');

		if (ptr)
			*ptr = '\0';

		strindx[list_count] = (char *)malloc (20 * sizeof (char));

		(void)strcpy (strindx[list_count], t);
		list_count++;

		if (ptr)
			t = ptr + 1;
		else
			t = NULL;
	} 

	nindex = list_count;
	offset = 0;
	prod   = 1;

	for (i = 0; i < nindex; i++) {
		dim = dimname[i];
		offset += (atol(strindx[i]) - 1) * prod;

		switch (type) {
			case M_PARAMETER:
				prod *= dim->value;
				break;

			case M_VARIABLE:
/*rsr changed next line */
/*				prod *= dim->max; */
				prod *= dim->value;
				break;
		}
	}

	switch (vtype) {
		case M_LONG:
			return (addr += offset * sizeof(long));
/*NOTREACHED*/
			break;

		case M_FLOAT:
			return(addr += offset * sizeof(float));
/*NOTREACHED*/
			break;

		case M_DOUBLE:
			return(addr += offset * sizeof(double));
/*NOTREACHED*/
			break;
	}
    return NULL;
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : get_times
 * COMMENT  : get start and end times from control data base
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define GET_TIMES_C
#include <stdio.h>
#include "mms.h"

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : get_times
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void get_times (void) {
  long *datetime;
  float *newvalue;

  datetime = (long *) control_var("start_time");
  Mstrttime->year = datetime[0];
  Mstrttime->month = datetime[1];
  Mstrttime->day = datetime[2];
  Mstrttime->hour = datetime[3];
  Mstrttime->min = datetime[4];
  Mstrttime->sec = datetime[5];

  datetime = (long *) control_var("end_time");
  Mendtime->year = datetime[0];
  Mendtime->month = datetime[1];
  Mendtime->day = datetime[2];
  Mendtime->hour = datetime[3];
  Mendtime->min = datetime[4];
  Mendtime->sec = datetime[5];

  /* compute julian day for start and end  - this fills in the julian date
     parts of the datetime data structure */

  julday(Mstrttime);
  julday(Mendtime);

  newvalue = (float *) control_var("initial_deltat");
  Mdeltat = (double)(*newvalue / 24.0);
  Mdeltanext = (double)(*newvalue / 24.0);
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : getdim() to be called from C
 *            getdim_() to be called from Fortran
 *            returns it as a long int. Returns -1 if error.
 * COMMENT  : gets the dimension associated with a name, and
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define GETDIM_C
#include <stdio.h>
#include <string.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdim_
 | COMMENT		: called from Fortran, sorts out args and calls getdim()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getdim_ (char *dname, ftnlen namelen) {
  char *name;
  long retval;

  /*
   * copy name and terminate
   */

  name = (char *) umalloc(namelen + 1);
  strncpy(name, dname, namelen);
  name[namelen] = '\0';

  /*
   * call C version of getdim()
   */

  retval =  getdim(name);

  return retval;
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdim
 | COMMENT		: is called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getdim (char *name) {

  DIMEN *dim;

  /*
   * get pointer to dimension with name
   */

  dim = dim_addr(name);

  if (dim == NULL) {
    (void)fprintf(stderr, 
	    "ERROR - getdim - dimension not found.\n");
    (void)fprintf(stderr, "Name:   '%s'\n", name);
    return(-1L);
  }

  /*
   * return the dimension
   */

  dim->got = TRUE;
  return dim->value;
}

/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : getparam() to be called from C
 *            getparam_() to be called from Fortran
 *            Returns 0 if successful, 1 otherwise.
 * COMMENT  : gets the parameter associated with a module and name, and
 *            copies it into the space provided by the calling routine.
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define GETPARAM_C
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/
static long paramcopy (PARAM *, double *, int);

/*--------------------------------------------------------------------*\
 | FUNCTION     : updateparam
 | COMMENT		: This function updates the local parameter value arrays
 |                in the modules with the current values in the parameter
 |                data structure. The local arrays are registered with the
 |                param structures by declaring them with the "declparam_u"
 |                function. This essentually implements a "listener"
 |                pattern, making each module a listener for new parameter
 |                values.
 | PARAMETERS   : name is the name of the parameter to update.
 | RETURN VALUE : integer error code
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long updateparam (char *name) {
	PARAM *param;
	int i;

	param = param_addr(name);

	if (param == NULL) {
		(void)fprintf(stderr, "updateparam: %s not found.\n", name);

	} else {
		for (i = 0; i < param->num_references; i++) {
			paramcopy (param, (double *)(param->references[i]), -1);
		}
	}

	return (0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getparam_
 | COMMENT		: called from Fortran, sorts out args and calls getparam()
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getparam_ (char *mname, char *pname, ftnint *pmaxsize, char *ptype, double *pval,
	       ftnlen mnamelen, ftnlen pnamelen, ftnlen ptypelen) {

	char *module, *name, *type;
	int maxsize;
	long retval;

// copy maxsize to local long int
	maxsize = *pmaxsize;

// copy args to new strings, and terminate
	module = (char *) umalloc(mnamelen + 1);
	strncpy(module, mname, mnamelen);
	module[mnamelen] = '\0';

	name = (char *) umalloc(pnamelen + 1);
	strncpy(name, pname, pnamelen);
	name[pnamelen] = '\0';

	type = (char *) umalloc(ptypelen + 1);
	strncpy(type, ptype, ptypelen);
	type[ptypelen] = '\0';

// call C version of getparam()
	retval = getparam(module, name, maxsize, type, pval);

	return(retval);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getparam
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getparam (char *module, char *name, int maxsize, char *type, double *pval) {
	int var_type;
	PARAM *param;
	char *pkey;

	pkey = strdup (name);

// convert fortran types to C types
	var_type = M_LONG;
	if (!strcmp(type, "real") || !strcmp(type, "float")) {
		var_type = M_FLOAT;
	} else if (!strcmp(type, "double precision") || !strcmp(type, "double")) {
		var_type = M_DOUBLE;
	} else if (!strcmp (type, "string")) {
		var_type = M_STRING;
	}

// check that type is possible
	if((var_type != M_LONG) && (var_type != M_FLOAT) && (var_type != M_DOUBLE) && (var_type != M_STRING)) {
		(void)fprintf(stderr, "\nERROR: data type for parameter %s in module %s has inconsistent uses.\n", pkey, module);
		return(1);
	}

// get pointer to parameter with key
	param = param_addr(pkey);

	if (param == NULL) {
		(void)fprintf(stderr, "\nERROR: getting parameter %s in module %s, but parameter is not found.\n", pkey, module);
		return(1);
	}

//  Check to see if the parameter values were set in the Parameter File
	if (param->read_in == 0) {
		(void)fprintf(stderr,"\nWARNING: parameter %s is used by module %s but values are not\n", pkey, module);
		(void)fprintf(stderr,"         set in the Parameter File. Module default values are being used.\n");
	}

// check that there is enough space allocated in the calling routine
	if (param->size > maxsize) {
		(void)fprintf(stderr, "\nERROR: parameter %s declared array size is not big enough in module %s.\n", pkey, module);
		return(1);
	}

	return paramcopy (param, pval, maxsize);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : paramcopy
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
static long paramcopy (PARAM *param, double *pval, int maxsize) {
	DIMEN *dim1, *dim2;
	char * ptr;
	int max1, max2, val1, val2, nrow;
	int i;
  /*
   * copy the parameter across
   */

	if (param->ndimen == 1) {
		switch (param->type) {

			case M_LONG:
				memcpy ((char *) pval, (char *) param->value, param->size * sizeof(int));
				break;

			case M_FLOAT:
				memcpy ((char *)pval, (char *)param->value, param->size * sizeof(float));
				break;

			case M_DOUBLE:
				memcpy ((char *)pval, (char *)param->value, param->size * sizeof(double));
				break;

			case M_STRING:  // DANGER fortran string size hardwired to 16 characters
 	  			for (i = 0; i < param->size; i++) {
					memcpy ((char *)pval+i, *((char **)param->value+i), 16 * sizeof(char *));
                }
				break;
		}
	} else {
      ptr = (char *) pval;

      dim1 = param->dimen[0];
 	  dim2 = param->dimen[1];

      val1 = dim1->value;
      max1  = dim1->max;

      val2 = dim2->value;
 	  max2 = dim2->max;

 	  if ((max1*max2 == val1*val2) == maxsize ) {
 		  if (max1 == val1 && max2 == val2 ) {
 			  nrow = val1;
 		  } else {
 			  nrow = val1;
 			  (void)fprintf(stderr, "paramcopy: DANGER. Mismatch in array sizes.\n");
			  (void)fprintf(stderr, "Key:   '%s'\n", param->name);
 		  }
 	  } else if (val1*val2 == maxsize) {
 		  nrow = val1;
 	  } else if (max1*max2 == maxsize) {
 		  nrow = max1;
 	  } else {
 		  nrow = val1;
 		  (void)fprintf(stderr, "paramcopy: DANGER 2. Mismatch in array sizes.\n");
		  (void)fprintf(stderr, "Key:   '%s'\n", param->name);
 	  }


 	  for (i = 0; i	< val2;	i++) {
 		  switch (param->type) {

 			case M_LONG:
 				memcpy (ptr, (char *)(param->value + i * val1*sizeof(int)), val1*sizeof(int));
 				ptr	+=	nrow * sizeof(int);
 				break;

 			case M_FLOAT:
 				memcpy (ptr, (char *) (param->value + i * val1*sizeof(float)), val1*sizeof(float));
 				ptr +=  nrow * sizeof(float);
 				break;

 			case M_DOUBLE:
 				memcpy (ptr, (char *) (param->value + i * val1*sizeof(double)), val1*sizeof(double));
 				ptr +=  nrow * sizeof(double);
 				break;

 			case M_STRING:
 				memcpy (ptr, (char *) (param->value + i * val1*sizeof(char *)), val1*sizeof(char *));
 				ptr +=  nrow * sizeof(char *);
 				break;

 		  }
 	  }
	}
	return(0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdatainfo_
 | COMMENT		: called from Fortran
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getdatainfo_ (char *dinfo, ftnlen len) {
	long retval;
	retval = getdatainfo (dinfo, len);
	return(retval);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdatainfo
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getdatainfo (char *dinfo, ftnlen len) {
	strncpy (dinfo, Mdatainfo, len);
	return(0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getoutname_
 | COMMENT		: called from Fortran
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getoutname_ (char *dinfo, char *ext, ftnlen len, ftnlen elen) {
  char *foo;
  long ret;

	foo = (char *) umalloc(elen + 1);
	strncpy(foo, ext, elen);
	foo[elen] = '\0';

	ret = getoutname (dinfo, foo);

    if (strlen (dinfo) >= (size_t)len) {
		printf ("getoutname:  path name is too long for your buffer!\n");
        ret = 1;
	}
    return(ret);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getoutname
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getoutname (char *dinfo, char *ext) {
	sprintf(dinfo, "%s\\%s", *control_svar("model_output_file"), ext);
	return(0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdataname_
 | COMMENT		: called from Fortran
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getdataname_ (char *dinfo, char *ext, ftnlen len, ftnlen elen) {
	char *foo;
	long retval;

	foo = (char *) umalloc(elen + 1);
	strncpy(foo, ext, elen);
	foo[elen] = '\0';

	retval = getdataname (dinfo, foo);
	return(retval);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdataname
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getdataname (char *dinfo, char *ext) {
	sprintf(dinfo, "%s%s", *control_svar("data_file"), ext);
	return(0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getoutdirfile_
 | COMMENT		: called from Fortran
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
/*long getoutdirfile_ (char *dinfo, char *ext, ftnlen len, ftnlen elen) {
	char *foo;
	long retval;

	foo = (char *) umalloc(elen + 1);
	strncpy(foo, ext, elen);
	foo[elen] = '\0';

   retval = getoutdirfile (dinfo, foo);
   return(retval);
}
*/
/*--------------------------------------------------------------------*\
 | FUNCTION     : getoutdirfile
 | COMMENT      : called from C
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
/*long getoutdirfile (char *dinfo, char *foo) {
   sprintf(dinfo, "%s%s", *control_svar("mms_user_out_dir"), foo);
   return(0);
}
*/
/*--------------------------------------------------------------------*\
 | FUNCTION     : getuserdirfile_
 | COMMENT		: called from Fortran
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
/*long getuserdirfile_ (char *dinfo, char *ext, ftnlen len, ftnlen elen) {
	char *foo;
	long retval;

	foo = (char *) umalloc(elen + 1);
	strncpy(foo, ext, elen);
	foo[elen] = '\0';

   retval = getuserdirfile (dinfo, foo);
   return(retval);
}
*/
/*--------------------------------------------------------------------*\
 | FUNCTION     : getuserdirfile
 | COMMENT      : called from C
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
/*long getuserdirfile (char *dinfo, char *foo) {
   sprintf(dinfo, "%s%s", *control_svar("mms_user_dir"), foo);
   return (0);
}
*/
/*--------------------------------------------------------------------*\
 | FUNCTION     : getparamfile
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getparamfile (char *dinfo) {
	sprintf(dinfo, "%s", *control_svar("param_file"));
	return (0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getparamfile_
 | COMMENT		: called from Fortran
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getparamfile_ (char *dinfo, ftnlen len) {
	long retval;
	retval = getparamfile (dinfo);
	return(retval);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getparamstring_
 | COMMENT		: called from Fortran
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/

long getparamstring_ (char *mname, char *pname, ftnint *pmaxsize, char *ptype, ftnint *pindex, char *pstring,
	       ftnlen mnamelen, ftnlen pnamelen, ftnlen ptypelen, ftnlen pslen) {

  char *module, *name, *type;
  int maxsize;
  PARAM *param;

  /*
   * copy maxsize to local long int
   */

  maxsize = *pmaxsize;

  /*
   * copy args to new strings, and terminate
   */

  module = (char *) umalloc(mnamelen + 1);
  strncpy(module, mname, mnamelen);
  module[mnamelen] = '\0';

  name = (char *) umalloc(pnamelen + 1);
  strncpy(name, pname, pnamelen);
  name[pnamelen] = '\0';

  type = (char *) umalloc(ptypelen + 1);
  strncpy(type, ptype, ptypelen);
  type[ptypelen] = '\0';

  param = param_addr(name);

  if (param == NULL) {
    (void)fprintf(stderr,
		"\nERROR: - parameter %s is not found.\n", name);
//    (void)fprintf(stderr, "Key:   '%s'\n", name);
    return(1);
  }

  /*
  **  Check to see if the parameter values were set in the Parameter File
  */
  if (param->read_in == 0) {
		(void)fprintf(stderr,"\nWARNING: parameter %s is used by module %s but values are not\n", name, module);
		(void)fprintf(stderr,"         set in the Parameter File. Module default values are being used.\n");
//	  (void)fprintf(stderr,
//	    "getparamstring - parameter %s is used but values are not set in the Parameter File.  Module default values are being used.\n", name);
  }

//   strncpy (pstring, (char *)(param->value)[*pindex], 80);
   strncpy (pstring, *((char **)param->value + *pindex), pslen);

   return(0);
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : getvar() to be called from C
 *            getvar_() to be called from Fortran
 *            Returns 0 if successful, 1 otherwise.
 * COMMENT  : gets the value associated with a module and name, and copies
 *            it into the variable provided by the calling routine.
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION		: getvar_
 | COMMENT		: called from Fortran, sorts out args and calls getvar()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getvar_ (char *mname, char *vname, ftnint *vmaxsize, char *vtype, double *value, ftnlen mnamelen, ftnlen vnamelen, ftnlen vtypelen) {
	char module[80], name[80], type[80];
	long maxsize, retval;
  
/*
* copy size to local long int
* copy args to new strings, and terminate
*/
	maxsize = *vmaxsize;

	strncpy (module, mname, mnamelen);
	*(module + mnamelen) = '\0';

	strncpy (name, vname, vnamelen);
	*(name + vnamelen) = '\0';

	strncpy (type, vtype, vtypelen);
	*(type + vtypelen) = '\0';
  
/*
* call C version of getvar()
*/
	retval =  getvar (module, name, maxsize, type, value);
  
	return (retval);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getvar
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getvar (char *module, char *name, long maxsize, char *type, double *value) {

	int var_type;
	PUBVAR *var;
//	char *vkey;
	char vkey[128];
	long i;
	long n1, n2;
	char *ptr1;
	char *ptr2;

/*
* compute the key
*/
  
/*
  vkey = (char *) umalloc(strlen(module) + strlen(name) + 2);
  (void)strcpy(vkey, module);
  strcat(strcat(vkey, "."), name);
*/
//  vkey = strdup (name);
   strncpy (vkey, name, 128);
  
/*
* convert fortran types to C types
*/

	var_type = M_LONG;
	if (!strcmp(type, "real") || !strcmp(type, "float"))
	  var_type = M_FLOAT;
	else if (!strcmp(type, "double precision") || !strcmp(type, "double"))
	  var_type = M_DOUBLE;

/*
* check that type is possible
*/
	if((var_type != M_LONG) && (var_type != M_FLOAT) && (var_type != M_DOUBLE)){
		(void)fprintf(stderr,
				"ERROR - getvar - type %s is illegal.\n", type);
		(void)fprintf(stderr, "Key is '%s'.\n", vkey);
		(void)fprintf(stderr, "Type is '%s'.\n", type);
		return(1);
	}
  
/*
* get pointer to variable with key
*/
	if (!(var = var_addr (vkey))) {
		(void)fprintf(stderr, "ERROR - getvar - variable not found.\n");
		(void)fprintf(stderr, "Key:   '%s'\n", vkey);
		return(1);
	}
  
/*
* check that there is enough space allocated in the calling routine
* to accommodate the data
*/
  
	if (var->size > maxsize) {
		(void)fprintf (stderr, 
	    			"ERROR - getvar - insufficient space for data transfer.\n");
		(void)fprintf(stderr, "Key:   '%s'\n", vkey);
		(void)fprintf(stderr, "Actual size in data base: %ld\n", var->size);
		(void)fprintf(stderr, "Available space in calling routine: %ld\n",
						maxsize);
		return(1);
	}

/*
	if (strcmp(Mtypes[var->type], type)) {
		(void)fprintf(stderr, 
				"ERROR - getvar - incorrect data type requested.\n");
		(void)fprintf(stderr, "Key:   '%s'\n", vkey);
		(void)fprintf(stderr, "Requested type: %s\n", type);
		(void)fprintf(stderr, "Actual declared type: %s\n",
							Mtypes[var->type]);
		return(1);
	}
*/
  
/*
* copy the variable across
*/
  
	if (var->ndimen == 1) {
		switch (var->type) {
			case M_LONG:
				memcpy ((char *)value, (char *)var->value,
							var->size * sizeof(long));
				break;
      
			case M_FLOAT:
				memcpy ((char *)value, (char *)var->value,
							var->size * sizeof(float));
				break;
      
			case M_DOUBLE:
				memcpy ((char *)value, (char *)var->value,
							var->size * sizeof(double));
				break;
		}
	} else if (var->ndimen ==2) {
		n1 = var->dimen[0]->value;
		n2 = var->dimen[1]->value;
		if (n1*n2!=maxsize) n1 = var->dimen[0]->max;
/*rsr added next block*/
		if (n1*n2 > maxsize ) {
			n1 = var->dimen[0]->value;
		}
/*rsr end block*/
		ptr1 = var->value;
		ptr2 = (char *)value;

		for (i = 0; i < n2; i++) {

			switch (var->type) {
				case M_LONG:
					memcpy ((char *)ptr2, (char *)ptr1, n1 * sizeof(long));
					ptr1 += n1 * sizeof(long);
					ptr2 += n1 * sizeof(long);
					break;

				case M_FLOAT:
					memcpy ((char *)ptr2, (char *)ptr1, n1 * sizeof(float));
					ptr1 += n1 * sizeof(float);
					ptr2 += n1 * sizeof(float);
					break;

				case M_DOUBLE:
					memcpy ((char *)ptr2, (char *)ptr1, n1 * sizeof(double));
					ptr1 += n1 * sizeof(double);
					ptr2 += n1 * sizeof(double);
					break;
			}
		}
	}

	return (0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION		: getvartype_
 | COMMENT		: called from Fortran, sorts out args and returns the type()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getvartype_ (char *vname, ftnlen vnamelen) {
	char vkey[128];
	PUBVAR *var;
  
    strncpy (vkey, vname, 128);
/*
* get pointer to variable with key
*/
	if (!(var = var_addr (vkey))) {
		(void)fprintf(stderr, "ERROR - getvartype - variable not found.\n");
		(void)fprintf(stderr, "Key:   '%s'\n", vkey);
		return(-1);
	}

	return (var->type);
}

/*--------------------------------------------------------------------*\
 | FUNCTION		: getvarsize_
 | COMMENT		: called from Fortran, sorts out args and returns the variable size()
 | PARAMETERS   :
 | RETURN VALUE : size of the array for input variable
 | RESTRICTIONS : variable must be declared
\*--------------------------------------------------------------------*/
long getvarsize_ (char *vname, ftnlen vnamelen) {
	char vkey[128];
	PUBVAR *var;
  
    strncpy (vkey, vname, 128);
/*
* get pointer to variable with key
*/
	if (!(var = var_addr (vkey))) {
		(void)fprintf(stderr, "ERROR - getvartype - variable not found.\n");
		(void)fprintf(stderr, "Key:   '%s'\n", vkey);
		return(-1);
	}

	return (var->size);
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : graph_single_run
 * COMMENT  : graph routines for mms run
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define GRAPH_SINGLE_RUN_C
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "mms.h"

#define         MAXNUMBEROFGRAPHS               4

/**5*********************** LOCAL VARIABLES ***************************/
long NdispGraphs;
static double zero_time;
PUBVAR **disp_var;
int *disp_ele;
int numDispVars;

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : initializeRuntimeGraphs
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : int
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int initializeRuntimeGraphs (void) {
   CONTROL *control;
   int i;
   //long datetime[6];
   DATETIME starttime_copy;
   char *cptr, *cptr2;

   if (!runtime_graph_on) return (FALSE);

   //dattim("start", datetime);
   //zero_time = getjulday((int)datetime[1],(int)datetime[2],(int)datetime[0],
			//(int)datetime[3], (int)datetime[4],(double)datetime[5]);

      starttime_copy.year =Mstrttime->year;
   starttime_copy.month =Mstrttime->month;
   starttime_copy.day = Mstrttime->day;
   starttime_copy.hour =Mstrttime->hour;
   starttime_copy.min =Mstrttime->min;
   starttime_copy.sec =Mstrttime->sec;

   julday(&starttime_copy);

   //zero_time = zero_time - 1.0;

   zero_time = starttime_copy.jt;

/*
** Get the number of display vars
*/
   cptr = strdup ("dispVar_names");
   control = control_addr(cptr);
   if (control) {
      numDispVars = control->size;

      disp_var = (PUBVAR **)malloc (sizeof(PUBVAR *) * numDispVars);
      memset (disp_var, 0, sizeof (PUBVAR *) * numDispVars);

      disp_ele = (int *)malloc (sizeof (int) * numDispVars);
      memset (disp_ele, 0, sizeof (int) * numDispVars);

      for (i = 0; i < numDispVars; i++) {
/**
** Get address of each display variable for each graph
**/
         cptr = strdup ("dispVar_names");
		 cptr2 = (char *)control_sarray(cptr, i);

         disp_var[i] = var_addr (cptr2);

         cptr = strdup ("dispVar_element");
//         disp_ele[i] =  atoi (*control_sarray(cptr,i)) - 1;
         disp_ele[i] =  atoi (control_sarray(cptr,i)) - 1;
      }
   } else {
	   numDispVars = 0;
	   disp_var = NULL;
	   disp_ele = NULL;
	   runtime_graph_on = 0;
   }
   return (FALSE);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : plotRuntimeGraphValue
 | COMMENT	:
 | PARAMETERS   :
 | RETURN VALUE : int
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int plotRuntimeGraphValue (void) {
   double xval;
   float yval;
   int i;
   //long datetime[6];
   DATETIME nowtime_copy;

   if (!runtime_graph_on) return (FALSE);


   //dattim("now", datetime);
   nowtime_copy.year =Mnowtime->year;
   nowtime_copy.month =Mnowtime->month;
   nowtime_copy.day = Mnowtime->day;
   nowtime_copy.hour =Mnowtime->hour;
   nowtime_copy.min =Mnowtime->min;
   nowtime_copy.sec =Mnowtime->sec;

   julday(&nowtime_copy);
   //xval = getjulday(datetime[1],datetime[2],datetime[0],
	  //               datetime[3], datetime[4],(double)datetime[5]);

   xval = nowtime_copy.jt - zero_time;

   printf ("plotRuntimeGraphValue: xval = %f", xval);

   for (i = 0; i < numDispVars; i++) {
      yval = 0.0;
      switch ((disp_var[i])->type) {
         case M_LONG :
            yval = (float)(*(((long *)((disp_var[i])->value)) + disp_ele[i]));
            break;

         case M_DOUBLE :
            yval = (float)(*(((double *)((disp_var[i])->value)) + disp_ele[i]));
            break;

         case M_FLOAT :
            yval = *(((float *)((disp_var[i])->value)) + disp_ele[i]);
            break;
      }
      printf (" %f", yval);
   }
   printf ("\n");
   fflush (stdout);

   return (FALSE);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : closeRuntimeGraphs
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : int
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int closeRuntimeGraphs (void) {
   if (!runtime_graph_on) return (FALSE);

   printf ("closeRuntimeGraph\n");
   return (FALSE);
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : julconvert
 * COMMENT  : convert Gregorian days to Julian date
 *            Modify as needed for your application.
 *            The Julian day starts at noon of the Gregorian day and extends
 *            to noon the next Gregorian day.
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define JULCONVERT_C
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "mms.h"


//static char *dayofweekstr[] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};

/*
** Takes a date, and returns a Julian day. A Julian day is the number of
** days since some base date  (in the very distant past).
** Handy for getting date of x number of days after a given Julian date
** (use jdate to get that from the Gregorian date).
** Author: Robert G. Tantzen, translator: Nat Howard
** Translated from the algol original in Collected Algorithms of CACM
** (This and jdate are algorithm 199).
*/
double getjulday(int mon, int day, int year, int h, int mi, double se) {
	DATETIME datetime;

	datetime.year = year;
	datetime.month = mon;
	datetime.day = day;
	datetime.hour = h;
	datetime.min = mi;
	datetime.sec = se;
	julday(&datetime);

	return datetime.jt;
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getjulday_
 | COMMENT      : getjulday binding for Fortran
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
double getjulday_(int *mon, int *day, int *year, int *h, int *mi, double *se) {
   return getjulday (*mon, *day, *year, *h, *mi, *se);
}

int dayofweek(double j) {
    j += 0.5;
    return (int) (j + 1) % 7;
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : isleap_
 | COMMENT      : isleap binding for Fortran
 | PARAMETERS   : see below
 | RETURN VALUE : see below
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long isleap_ (ftnint *year) {
   return ((long)isleap((int)(*year)));
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : isleap
 | COMMENT      : Thanks to Rick Webb
 | PARAMETERS   : int year - the year to test
 | RETURN VALUE : Returns 1 if year is a leap year, 0 if not.
 | RESTRICTIONS : Called from C.
\*--------------------------------------------------------------------*/
int isleap (int year) {

   double nptr;

/*
**  Check if leapyear - Start by identifying all years not
**       divisible by 4
*/
   if (modf ((double)year/4.0, &nptr)!=0) {
      return(0);
/*
**  Identify leap years that are not century years
 */
   } else if (modf((double)year/4.0, &nptr)==0 && modf(year/100.0, &nptr)!=0 ) {
      return(1);

/*
**  century years are not leap years unless divisible by 400
*/
   } else if (modf((double)year/400.0, &nptr)!=0 ) {
      return(0);

/*
**  all that's left are century years divisible by 400 which
**         are also leap years
*/
   } else {
      return(1);
   }
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : julday()
 * COMMENT  : computes julian day, puts it into the jd slot in the
 *            datetime structure
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define JULDAY_C
#include <math.h>
#include "mms.h"
#define IGREG (15+31L*(10+12L*1582))

/*--------------------------------------------------------------------*\
 | FUNCTION     : julday
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int julday (DATETIME *datetime) {

  //double foo = getjulday(datetime->month, datetime->day, datetime->year, datetime->hour, datetime->min, datetime->sec);

  //datetime->jd = floor (foo);
  //datetime->jt = foo;

  long jul;
  int ja,jy,jm, iyyy, mm, id;

  iyyy = datetime->year;
  mm = datetime->month;
  id = datetime->day;

  if (iyyy == 0) {
    (void)fprintf(stderr, "JULDAY: there is no year zero.");
    return(1);
  }

  if (iyyy < 0) ++iyyy;
  if (mm > 2) {
    jy=iyyy;
    jm=mm+1;
  } else {
    jy=iyyy-1;
    jm=mm+13;
  }
  jul = (long) (floor(365.25*jy)+floor(30.6001*jm)+id+1720995);
  if (id+31L*(mm+12L*iyyy) >= IGREG) {
    ja=0.01*jy;
    jul += 2-ja+(int) (0.25*ja);
  }
  datetime->jd = jul;
  datetime->jt = (double) jul + (double) datetime->hour / 24.0
                              + (double) datetime->min / 1440.0
			      + (double) datetime->sec / 86400.0;

  return (0);
}
#undef IGREG
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : load_param
 * COMMENT  : Stores the parameter value, minima and maxima at the
 *            required address.  Uses str_to_vals to decode the strings and
 *            store the values. This routine mainly handles the error conditions.
 *            Examples of legal strings for this routine are given in str_to_vals.c
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define LOAD_PARAM_C
#include <stdio.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : load_param
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : Returns 0 if successful, 1 otherwise.
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long load_param (PARAM *param) {

	long i;
	double *dval, *dmin, *dmax, *ddef;
	float *fval, *fmin, *fmax, *fdef;
	int *lval, *lmin, *lmax, *ldef;
	char *sval, *sdef;

	if (param->type == M_DOUBLE) {
		param->value = (char *)umalloc (param->size * sizeof (double));
		param->def = (char *)umalloc (param->size * sizeof (double));
		param->min = (char *)umalloc (param->size * sizeof (double));
		param->max = (char *)umalloc (param->size * sizeof (double));
	} else if (param->type == M_FLOAT) {
		param->value = (char *)umalloc (param->size * sizeof (float));
		param->def = (char *)umalloc (param->size * sizeof (float));
		param->min = (char *)umalloc (param->size * sizeof (float));
		param->max = (char *)umalloc (param->size * sizeof (float));
	} else if (param->type == M_LONG) {
		param->value = (char *)umalloc (param->size * sizeof (int));
		param->def = (char *)umalloc (param->size * sizeof (int));
		param->min = (char *)umalloc (param->size * sizeof (int));
		param->max = (char *)umalloc (param->size * sizeof (int));
	} else if (param->type == M_STRING) {
		param->value = (char *)umalloc (param->size * sizeof (char *));
		param->def = (char *)umalloc (param->size * sizeof (char *));
		param->min = (char *)umalloc (param->size * sizeof (char *));
		param->max = (char *)umalloc (param->size * sizeof (char *));
	}

/*
* decode minima
*/
	if (param->bound_status == M_BOUNDED) {
		lmin = (int *)(param->min);	
		for (i = 0; i < param->size; i++)
			*lmin++ = 0;
	} else {
		if (str_to_vals (param->min_string, param->size,
									param->type, param->min)) {
			(void)fprintf (stderr, "Parameter is '%s'\n", param->key);
			(void)fprintf (stderr, "Decoding minimum values.\n");
			(void)fprintf (stderr, "Encoded string is:\n'%s'\n", param->min_string);
			return (1);
		}
	}

/*
* decode maxima
*/
	if (param->bound_status == M_BOUNDED) {
		lmax = (int *)(param->max);	
		for (i = 0; i < param->size; i++)
			*lmax++ = (long)(param->bound_dimen->value);
	} else {
		if (str_to_vals (param->max_string, param->size,
									param->type, param->max)) {
			(void)fprintf (stderr,"Parameter is '%s'\n", param->key);
			(void)fprintf (stderr,"Decoding maximum values.\n");
			(void)fprintf (stderr,"Encoded string is:\n'%s'\n",param->max_string);
			return (1);
		}
	}

/*
* decode default values
*/
	if (str_to_vals (param->value_string, param->size, param->type,
								param->def)) {
		(void)fprintf(stderr,"Parameter is '%s'\n", param->key);
		(void)fprintf(stderr,"Decoding default values.\n");
		(void)fprintf(stderr,"Encoded string is:\n'%s'\n",param->value_string);
		return(1);
	}

	switch (param->type) {
		case M_DOUBLE:
			dval = (double *)param->value;
			ddef = (double *)param->def;
			for (i = 0; i < param->size; i++)
				*dval++ = *ddef++;
			break;

		case M_FLOAT:
			fval = (float *)param->value;
			fdef = (float *)param->def;
			for (i = 0; i < param->size; i++)
				*fval++ = *fdef++;
			break;

		case M_LONG:
			lval = (int *)param->value;
			ldef = (int *)param->def;
			for (i = 0; i < param->size; i++)
				*lval++ = *ldef++;
			break;

		case M_STRING:
			sval = (char *)param->value;
			sdef = (char *)param->def;
			for (i = 0; i < param->size; i++)
				*sval++ = *sdef++;
			break;
	}

/*
* check that the defaults lie within the min and max range
*/
	switch (param->type) {

	case M_DOUBLE:

		dval = (double *) param->value;
		dmin = (double *) param->min;
		dmax = (double *) param->max;

		for (i = 0; i < param->size; i++) {

			if (dmin[i] > dmax[i]) {
				(void)fprintf(stderr,
					"ERROR: minimum value exceeds maximum value.\n");
				(void)fprintf(stderr, "Parameter is: '%s'\n", param->key);
				(void)fprintf(stderr,
				    "Default minimum and maximum values are:\nMin: '%s'\nMax: '%s'\n",
				    param->min_string, param->max_string);
//				(void)fprintf(stderr, "The problem is with posn no %ld.\n", i+1);
				(void)fprintf(stderr,
				    "Assigned minimum value = %lf, Specified maximum value = %lf\n", dmin[i], dmax[i]);
				return(1);
			}

			if (dval[i] < dmin[i] || dval[i] > dmax[i]) {
				(void)fprintf(stderr,
					"\nERROR: assigned value is out of range for Parameter: '%s'\n", param->key);
				(void)fprintf(stderr,
				    "       Default: '%s'\n       Minimum: '%s'\n       Maximum: '%s'\n",
				    param->value_string, param->min_string, param->max_string);
				(void)fprintf(stderr, "       Assigned values are:\n");
				(void)fprintf(stderr,
				    "       Value = %lf, Minimum = %lf, Maximum = %lf\n",
				    dval[i], dmin[i], dmax[i]);
				return(1);
			}

		}

		break;

	case M_FLOAT:

		fval = (float *) param->value;
		fmin = (float *) param->min;
		fmax = (float *) param->max;

		for (i = 0; i < param->size; i++) {

			if (fmin[i] > fmax[i]) {
				(void)fprintf(stderr,
					"ERROR: minimum value exceeds maximum value.\n");
				(void)fprintf(stderr, "Parameter is: '%s'\n", param->key);
				(void)fprintf(stderr,
				    "Default minimum and maximum values are:\nMin: '%s'\nMax: '%s'\n",
				    param->min_string, param->max_string);
//				(void)fprintf(stderr, "The problem is with posn no %ld.\n", i+1);
				(void)fprintf(stderr,
				    "Assigned minimum = %f, maximum = %f\n", fmin[i], fmax[i]);
				return(1);
			}

			if (fval[i] < fmin[i] || fval[i] > fmax[i]) {
				(void)fprintf(stderr,
					"\nERROR: assigned value is out of range for Parameter: '%s'\n", param->key);
				(void)fprintf(stderr,
				    "       Default: '%s'\n       Minimum: '%s'\n       Maximum: '%s'\n",
				    param->value_string, param->min_string, param->max_string);
				(void)fprintf(stderr, "       Assigned values are:\n");
				(void)fprintf(stderr,
				    "       Value = %f, Minimum = %f, Maximum = %f\n",
				    fval[i], fmin[i], fmax[i]);
				return(1);
			}

		}

		break;

	case M_LONG:

		lval = (int *) param->value;
		lmin = (int *) param->min;
		lmax = (int *) param->max;

		for (i = 0; i < param->size; i++) {

			if (lmin[i] > lmax[i]) {
				(void)fprintf(stderr,
					"ERROR: minimum value exceeds maximum value.\n");
				(void)fprintf(stderr, "Parameter is: '%s'\n", param->key);
				(void)fprintf(stderr,
				    "Default Minimum and maximum values are:\nMin: '%s'\nMax: '%s'\n",
				    param->min_string, param->max_string);
//				(void)fprintf(stderr, "The problem is with posn no %ld.\n", i+1);
				(void)fprintf(stderr,
				    "Assigned minimum = %d, maximum = %d\n", lmin[i], lmax[i]);
				return(1);
			}

			if (lval[i] < lmin[i] || lval[i] > lmax[i]) {
				(void)fprintf(stderr,
					"\nERROR: assigned value is out of range for Parameter: '%s'\n", param->key);
				(void)fprintf(stderr,
				    "       Default: '%s'\n       Minimum: '%s'\n       Maximum: '%s'\n",
				    param->value_string, param->min_string, param->max_string);
				(void)fprintf(stderr, "       Assigned values are:\n");
				(void)fprintf(stderr,
				    "       Value = %d, Minimum = %d, Maximum = %d\n",
				    lval[i], lmin[i], lmax[i]);
				return(1);
			}
		}
		break;

	case M_STRING:
// Nothing to check
		break;

	}
	return(0);
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : param_addr
 *            returns a pointer to a PARAM struct which contains the given key
 *            returns NULL if key not found
 * COMMENT  :
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define PARAM_ADDR_C
#include <string.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : param_addr
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
PARAM * param_addr (char *key) { 
  PARAM **params;
  long i;

  if (Mnparams == 0) return NULL; /* no parameters to locate */

  /*
   * get params from Mparambase, the global pointer
   */

  params = Mparambase;

  /*
   * search between 0 and Mnparams-1
   */

  for (i = 0; i < Mnparams; i++) {
    if (!strcmp(params[i]->key, key))
      return params[i];
  }

  /* if no match found, return null */
  return NULL;
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : parse_args
 * COMMENT  : parses the command line arguments
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define PARSE_ARGS_C
#include <math.h> 
#include <string.h> 
#include <stdlib.h> 
#include "mms.h" 

/*--------------------------------------------------------------------*\
 | FUNCTION     : parse_args
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : void
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void parse_args (int argc, char **argv, int *set_count, char **set_name, char **set_value) {

   int i;
   char *ptr;

   Mdebuglevel = 0;
   MAltContFile = strdup ("control");

/*
**  Get the model name.
*/
   ptr = strrchr (argv[0], '/');
   if (!ptr) ptr = strrchr (argv[0], '\\');
   if (ptr) ++ptr;
   else ptr = argv[0];

   model_name = strdup (ptr);

   executable_model = strdup (argv[0]);
   ptr = strstr (executable_model, ".exe");
   if (ptr) *ptr = '\0';

   if (argc >= 2) {
      for (i = 1; i < argc ; i++) {
		 if (!strcmp(argv[i], "-debug")) {
			 Mdebuglevel = atoi(argv[i+1]);
			 i++;

		 } else if (!strncmp(argv[i],"-C",2)) {
            MAltContFile = (char *)((argv[i]));
            MAltContFile+=2;

         } else if (!strncmp(argv[i],"-batch", 6)){
            batch_run_mode = TRUE;

         } else if (!strncmp(argv[i],"-print", 6)){
            print_mode = TRUE;

         } else if (!strncmp(argv[i],"-por", 4)){
            run_period_of_record = TRUE;

         } else if (!strncmp(argv[i],"-rtg", 4)){
            runtime_graph_on = TRUE;

		 } else if (!strncmp(argv[i],"-preprocess", 11)){
            preprocess_on = TRUE;

         } else if (!strncmp(argv[i],"-set",4)){
            i++;
            *(set_name + *set_count) = strdup ((char *)((argv[i])));
            i++;
            *(set_value + *set_count) = strdup ((char *)((argv[i])));
            (*set_count)++;

		} else if (!strncmp(argv[i],"-MAXDATALNLEN",13)){
            max_data_ln_len = atoi(argv[i+1]);
			i++;

		 } else { // Assume argument with no flag is control file name
			MAltContFile = (char *)((argv[i]));
		 }
      }
   }
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : print_model_info
 * COMMENT  :
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define PRINT_MODEL_INFO_C
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "mms.h"

#define PRINTLEN 77

/*--------------------------------------------------------------------*\
 | FUNCTION     : print_model_info
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int print_model_info (void) {

  char pathname[MAXPATHLEN];
  FILE *model_info_file;
  int i, j;
  MODULE_DATA *module;
  LIST *vlist, *plist;

	(void)sprintf (pathname, "%s.mod_name", MAltContFile);

	if ((model_info_file = fopen (pathname, "w")) == NULL) {
		(void)fprintf(stderr, "ERROR - print_model_info - creating file '%s'\n", pathname);
		perror("");
		return(1);
	}

  /*
   * write header
   */

	(void)fprintf(model_info_file, "PRMS Module Name File\n");
	(void)fprintf(model_info_file, "%s\n", model_name);
	(void)fprintf(model_info_file, "============\n\n");

	(void)fprintf(model_info_file, "Module versions used in the application, listed in computation order.\n\n");

	for (i = 0; i < module_db->count; i++) {
		// print module name
		module = (MODULE_DATA *)(module_db->itm[i]);
		fprintf(model_info_file, "%s,%s\n", module->name, module->version);

		// print the variables
		vlist = module->vars;
		fprintf(model_info_file, "   ");
		for (j = 0; j < vlist->count; j++) {
			fprintf(model_info_file, "%s,", (char *)(vlist->itm[j]));
		}
		fprintf(model_info_file, "\n");

		// print the parameters
		plist = module->params;
		fprintf(model_info_file, "   ");
		for (j = 0; j < plist->count; j++) {
			fprintf(model_info_file, "%s,", (char *)(plist->itm[j]));
		}
		fprintf(model_info_file, "\n");

	}
	//fprintf(model_info_file, "\n\n\n\n\n\n");

  fclose(model_info_file);

  return(0);
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : print_params
 * COMMENT  : prints the param data base to a file
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define PRINT_PARAMS_C
#include <time.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "mms.h"
//
//#define PRINTLEN 77

/*--------------------------------------------------------------------*\
 | FUNCTION     : print_params
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int print_params (void) {

  char pathname[MAXPATHLEN], *infostr;
  FILE *param_file;
  PARAM *param;
  DIMEN *dim;
  long i, j;

  /*
   * get param file path name, open file
   */

  (void)sprintf (pathname, "%s.par_name", MAltContFile);

  if ((param_file = fopen (pathname, "w")) == NULL) {
    (void)fprintf(stderr,
		"ERROR: creating Parameter Name File: '%s'\n", pathname);
    perror("");
    return(1);
  }

  /*
   * write header
   */

  (void)fprintf(param_file, "PRMS\n");
  (void)fprintf(param_file, "============\n\n");

  (void)fprintf(param_file, "Description of dimensions and parameters required in the application.\n\n");

  /*
   * write file name
   */

  (void)fprintf(param_file, "Parameter file: %s\n", *control_svar("param_file"));
  (void)fprintf(param_file, "\n");

  /*
   * write the run info string
   */

  infostr = (char *) umalloc (strlen(Mparaminfo) + 1);
  (void)strcpy(infostr, Mparaminfo);
/*
  (void)fprintf(param_file, "%s\n\n", insert_crs(infostr, PRINTLEN));
*/
  (void)fprintf(param_file, "%s\n\n", infostr);

  /*
   * write out dimensions
   */

  (void)fprintf(param_file, "--------------- DIMENSIONS ---------------\n");

	for (i = 0; i < dim_db->count; i++) {
		dim = (DIMEN *)(dim_db->itm[i]);
//  Only print out dimensions that have calls to "getdim" from the modules
//  markstro -- this didn't work.
		//if (dim->got) {
			(void)fprintf(param_file, "\n");
			(void)fprintf(param_file, "Name  : %s\n", dim->name);
			(void)fprintf(param_file, "Value : %ld\n", dim->value);
			(void)fprintf(param_file, "Desc  : %s\n", dim->descr);
			if (dim->fixed) {
			   (void)fprintf(param_file, "Fixed\n");
			}
		//}
	}

  /*
   * write out parameter values etc
   */

  (void)fprintf(param_file, "\n--------------- PARAMETERS ---------------\n");

  for (i = 0; i < Mnparams; i++) {

    param = Mparambase[i];
	if (param->max != NULL){


		(void)fprintf(param_file, "\n");
		(void)fprintf(param_file, "Name      : %s\n", param->name);
		(void)fprintf(param_file, "Module    : %s\n", param->module);
		(void)fprintf(param_file, "Descr     : %s\n", param->descr);
		(void)fprintf(param_file, "Help      : %s\n", param->help);
		(void)fprintf(param_file, "Ndimen    : %ld\n", param->ndimen);
		(void)fprintf(param_file, "Dimensions: ");

		for (j = 0; j < param->ndimen; j++) {
			(void)fprintf(param_file, "%s - %ld",
				param->dimen[j]->name, param->dimen[j]->value);
			if (j < param->ndimen - 1)
				(void)fprintf(param_file, ", ");
		} /* j */

		(void)fprintf(param_file, "\n");
		(void)fprintf(param_file, "Size      : %ld\n", param->size);
		(void)fprintf(param_file, "Type      : %s\n", Mtypes[param->type]);
		(void)fprintf(param_file, "Units     : %s\n", param->units);
		if (param->format)
			(void)fprintf(param_file, "Format    : %s\n", param->format);
		(void)fprintf(param_file, "Width     : %ld\n", param->column_width);

		switch (param->type) {
		case M_LONG:
			/* DANGER
		   (void)fprintf (param_file, "Max       : %ld\n", *(long *)(param->max));
		   (void)fprintf (param_file, "Min       : %ld\n", *(long *)(param->min));
		   (void)fprintf (param_file, "Default   : %ld\n", *(long *)(param->def));
		   */
			(void)fprintf(param_file, "Max       : %ld\n", *(int *)(param->max));
			(void)fprintf(param_file, "Min       : %ld\n", *(int *)(param->min));
			(void)fprintf(param_file, "Default   : %ld\n", *(int *)(param->def));
			break;

		case M_FLOAT:
			(void)fprintf(param_file, "Max       : %f\n", *(float *)(param->max));
			(void)fprintf(param_file, "Min       : %f\n", *(float *)(param->min));
			(void)fprintf(param_file, "Default   : %f\n", *(float *)(param->def));
			break;

		case M_DOUBLE:
			(void)fprintf(param_file, "Max       : %lf\n", *(double *)(param->max));
			(void)fprintf(param_file, "Min       : %lf\n", *(double *)(param->min));
			(void)fprintf(param_file, "Default   : %lf\n", *(double *)(param->def));
			break;
		}

		if (param->bound_status == M_BOUNDED) {
			(void)fprintf(param_file, "Bounded   : %s\n", (param->bound_dimen)->name);
		}

		/*  DANGER commented out for data dictionary print out */
		/*
			(void)fprintf(param_file, "Value(s):\n");

			if (param->ndimen >= 3) {

			for (j = 0; j < param->dimen[2]->value; j++) {

			(void)fprintf(param_file, "[%ld]\n", j + 1);

			nk = param->dimen[1]->value;

			for (k = 0; k < nk; k++) {

			(void)fprintf(param_file, "%5ld:", k + 1);

			nl = param->dimen[0]->value;

			for (l = 0; l < nl; l++) {

			print_param(param_file, param, l, nl, k, nk, j);

			}

			(void)fprintf(param_file, "\n");

			}

			}

			} else if (param->ndimen == 2) {

			nk = param->dimen[1]->value;

			for (k = 0; k < nk; k++) {

			(void)fprintf(param_file, "%5ld:", k + 1);

			nl = param->dimen[0]->value;

			for (l = 0; l < nl; l++) {

			print_param(param_file, param, l, nl, k,0,0);

			}

			(void)fprintf(param_file, "\n");

			}

			} else {

			nl = param->dimen[0]->value;

			for (l = 0; l < nl; l++) {

			print_param(param_file, param, l,0,0,0,0);

			}

			(void)fprintf(param_file, "\n");

			}
			*/
		/*  end DANGER */
	}
  } /* i */

  fclose(param_file);

  return(0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : print_param
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void print_param (FILE *param_file, PARAM *param, long l, long nl, long k,
	long nk, long j) {
  long ind;

  switch (param->ndimen) {

  case 1:

    ind = l;
    break;

  case 2:

    ind = l + k * nl;
    break;

  default:

    ind = l + k * nl + j * nl * nk;
    break;

  } /* switch (param->ndimen) */

  switch (param->type) {

  case M_DOUBLE:
    (void)fprintf(param_file, " %10lg", *((double *) param->value + ind));
    break;

  case M_FLOAT:
    (void)fprintf(param_file, " %10g", *((float *) param->value + ind));
    break;

  case M_LONG:
    (void)fprintf(param_file, " %10ld", *((long *) param->value + ind));
    break;

  } /* switch (param->type) */
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : print_vars
 * COMMENT  : prints the var data base to a file
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define PRINT_VARS_C
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "mms.h"

#define PRINTLEN 77

/*--------------------------------------------------------------------*\
 | FUNCTION     : print_vars
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int print_vars (void) {

  char pathname[MAXPATHLEN], *infostr;
  FILE *var_file;
  PUBVAR *var;
  long i, j;

  /*
   * get var file path name, open file
   */

  (void)sprintf (pathname, "%s.var_name", MAltContFile);


  if ((var_file = fopen (pathname, "w")) == NULL) {
    (void)fprintf(stderr,
	    "ERROR - print_vars - creating file '%s'\n", pathname);
    perror("");

    return(1);
  }

  /*
   * write header
   */

  (void)fprintf(var_file, "PRMS\n");
  (void)fprintf(var_file, "============\n\n");

  (void)fprintf(var_file, "Description of variables required in the application.\n\n");

  /*
   * write file names
   */

  (void)fprintf(var_file, "Parameter file: %s\n", *control_svar("param_file"));
  (void)fprintf(var_file, "Data file     : %s\n", *control_svar("data_file"));
  (void)fprintf(var_file, "\n");

  /*
   * write the run info string
   */

  infostr = (char *) umalloc (strlen(Mparaminfo) + 1);
  (void)strcpy(infostr, Mparaminfo);
  (void)fprintf(var_file, "%s\n\n", infostr);

  /*
   * write start and end times, and step number
   */

  (void)fprintf(var_file, "Start time        : %02ld:%02ld:%02ld  %02ld/%02ld/%04ld\n",
	  Mstrttime->hour, Mstrttime->min, Mstrttime->sec,
	  Mstrttime->month, Mstrttime->day, Mstrttime->year);
  (void)fprintf(var_file, "End time          : %02ld:%02ld:%02ld  %02ld/%02ld/%04ld\n",
	  Mendtime->hour, Mendtime->min, Mendtime->sec,
	  Mendtime->month, Mendtime->day, Mendtime->year);
  (void)fprintf(var_file, "Current time      : %02ld:%02ld:%02ld  %02ld/%02ld/%04ld\n",
	  Mnowtime->hour, Mnowtime->min, Mnowtime->sec,
	  Mnowtime->month, Mnowtime->day, Mnowtime->year);
  (void)fprintf(var_file, "Current time step : %8ld\n", Mnsteps);

  /*
   * write out variable values
   */

  for (i = 0; i < Mnvars; i++) {

    var = Mvarbase[i];

    if (!(var->private)) {

       (void)fprintf(var_file, "\n");
       (void)fprintf(var_file, "Name: %s\n", var->name);
       (void)fprintf(var_file, "Module: %s\n", var->module);
       (void)fprintf(var_file, "Ndimen: %ld\n", var->ndimen);
       (void)fprintf(var_file, "Dimensions: ");

       for (j = 0; j < var->ndimen; j++) {
         (void)fprintf(var_file, "%s - %ld",
	         var->dimen[j]->name, var->dimen[j]->value);
         if (j < var->ndimen - 1)
	   (void)fprintf(var_file, ", ");
       } /* j */

       (void)fprintf(var_file, "\n");
       (void)fprintf(var_file, "Size: %ld\n", var->size);
       (void)fprintf(var_file, "Type: %s\n", Mtypes[var->type]);
/* DANGER */
       (void)fprintf(var_file, "Desc: %s\n", var->help);
/* DANGER */
       (void)fprintf(var_file, "Units: %s\n", var->units);
       if (var->private)
          (void)fprintf(var_file, "Private \n");
/*
    (void)fprintf(var_file, "Value(s):\n");
    
    if (var->ndimen >= 3) {

      for (j = 0; j < var->dimen[2]->value; j++) {

	(void)fprintf(var_file, "[%ld]\n", j + 1);

	nk = var->dimen[1]->value;
	
	for (k = 0; k < nk; k++) {

	  (void)fprintf(var_file, "%5ld:", k + 1);

	  nl = var->dimen[0]->value;

	  for (l = 0; l < nl; l++) {

	    print_var(var_file, var, l, nl, k, nk, j);

	  }

	  (void)fprintf(var_file, "\n");

	}

      }

    } else if (var->ndimen == 2) {

      nk = var->dimen[1]->value;
	
      for (k = 0; k < nk; k++) {

	(void)fprintf(var_file, "%5ld:", k + 1);

	nl = var->dimen[0]->value;

	for (l = 0; l < nl; l++) {

	  print_var(var_file, var, l, nl, k,0,0);

	}

	(void)fprintf(var_file, "\n");

      }

    } else {

      nl = var->dimen[0]->value;

      for (l = 0; l < nl; l++) {
	print_var(var_file, var, l,0,0,0,0);

      }

      (void)fprintf(var_file, "\n");

    }
*/
     }
    
  } /* i */

  fclose(var_file);
  
  return(0);

}

/*--------------------------------------------------------------------*\
 | FUNCTION     : print_var
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void print_var (FILE *var_file, PUBVAR *var, long l, long nl, long k, long nk,
	long j) {
  
  long ind;

  switch (var->ndimen) {

  case 1:

    ind = l;
    break;

  case 2:

    ind = l + k * nl;
    break;

  default:

    ind = l + k * nl + j * nl * nk;
    break;

  } /* switch (var->ndimen) */

  switch (var->type) {

  case M_DOUBLE:
    (void)fprintf(var_file, " %10lg", *((double *) var->value + ind));
    break;
	
  case M_FLOAT:
    (void)fprintf(var_file, " %10g", *((float *) var->value + ind));
    break;
	
  case M_LONG:
    (void)fprintf(var_file, " %10ld", *((long *) var->value + ind));
    break;
	
  } /* switch (var->type) */

}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : putvar() to be called from C
 *            putvar_() to be called from Fortran
 *            Returns 0 if successful, 1 otherwise.
 * COMMENT  : gets the value associated with a module and name, and copies
 *            it into the variable provided by the calling routine.
 *
* $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define PUTVAR_C
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : putvar_
 | COMMENT		: called from Fortran, sorts out args and calls putvar()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long putvar_ (char *mname, char *vname, ftnint *vmaxsize, char *vtype, double *value, 
	     ftnlen mnamelen, ftnlen vnamelen, ftnlen vtypelen) {

//  char *module, *name, *type;
  char module[80], name[80], type[80];
  long maxsize, retval;

  /*
   * copy size to local long int
   */

  maxsize = *vmaxsize;

  /*
   * copy args to new strings, and terminate
   */

//  module = (char *) umalloc(mnamelen + 1);
//  strncpy(module, mname, mnamelen);
//  module[mnamelen] = '\0';

//  name = (char *) umalloc(vnamelen + 1);
//  strncpy(name, vname, vnamelen);
//  name[vnamelen] = '\0';

//  type = (char *) umalloc(vtypelen + 1);
//  strncpy(type, vtype, vtypelen);
//  type[vtypelen] = '\0';

    strncpy (module, mname, mnamelen);
    *(module + mnamelen) = '\0';

    strncpy (name, vname, vnamelen);
    *(name + vnamelen) = '\0';

    strncpy (type, vtype, vtypelen);
    *(type + vtypelen) = '\0';


  /*
   * call C version of putvar()
   */

  retval =  putvar(module, name, maxsize, type, value);

  return retval;

}

/*--------------------------------------------------------------------*\
 | FUNCTION     : putvar
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long putvar (char *module, char *name, long maxsize, char *type, double *value) {
  int var_type;
  PUBVAR *var;
//  char *vkey;
    char vkey[128];
  long i;
  long n1, n2;
  char *ptr1;
  char *ptr2;

  /*
   * compute the key
   */

/*
  vkey = (char *) umalloc(strlen(module) + strlen(name) + 2);
  (void)strcpy(vkey, module);
  strcat(strcat(vkey, "."), name);
*/
//  vkey = strdup (name);
   strncpy (vkey, name, 128);

  /*
   * convert fortran types to C types
   */
  var_type = M_LONG;
  if (!strcmp(type, "real") || !strcmp(type, "float"))
    var_type = M_FLOAT;
  else if (!strcmp(type, "double precision") || !strcmp(type, "double"))
    var_type = M_DOUBLE;

  /*
   * check that type is possible
   */
  if((var_type != M_LONG) && (var_type != M_FLOAT) && (var_type != M_DOUBLE))
    {
      (void)fprintf(stderr,
	      "ERROR - putvar - type %s is illegal.\n", type);
      (void)fprintf(stderr, "Key is '%s'.\n", vkey);
      (void)fprintf(stderr, "Type is '%s'.\n", type);
      return(1);
    }
  
  /*
   * get pointer to variable with key
   */
  
  var = var_addr(vkey);

  if (var == NULL) {
    (void)fprintf(stderr, 
	    "ERROR - putvar - variable not found.\n");
    (void)fprintf(stderr, "Key:   '%s'\n", vkey);
    return(1);
  }
  
  /*
   * check that there is enough space allocated in the calling routine
   * to accommodate the data
   */
  
  if (var->size > maxsize) {
    (void)fprintf(stderr, 
	    "ERROR - putvar - insufficient space for data transfer.\n");
    (void)fprintf(stderr, "Key:   '%s'\n", vkey);
    (void)fprintf(stderr, "Actual size in data base: %ld\n", var->size);
    (void)fprintf(stderr, "Available space in calling routine: %ld\n", maxsize);
    return(1);
  }
  /*
    if (strcmp(Mtypes[var->type], type)) {
    (void)fprintf(stderr, 
    "ERROR - putvar - incorrect data type requested.\n");
    (void)fprintf(stderr, "Key:   '%s'\n", vkey);
    (void)fprintf(stderr, "Requested type: %s\n", type);
    (void)fprintf(stderr, "Actual type in data base: %s\n", Mtypes[var->type]);
    return(1);
    }
    */
  
  /*
   * copy the variable across
   */
  
  if (var->ndimen == 1) {
    switch (var->type) {
      
    case M_LONG:
      memcpy ((char *) var->value, (char *) value, var->size * sizeof(long));
      break;
      
    case M_FLOAT:
      memcpy ((char *) var->value, (char *) value, var->size * sizeof(float));
      break;
      
    case M_DOUBLE:
      memcpy ((char *) var->value, (char *) value, var->size * sizeof(double));
      break;
      
    }
  }
  else
    if (var->ndimen ==2) {
      n1 = var->dimen[0]->max;
      n2 = var->dimen[1]->value;
/*rsr added next block*/
      if (n1*n2 > maxsize ) {
                n1 = var->dimen[0]->value;
      }
/*rsr end block*/

      ptr1 = (char *)value;
      ptr2 = var->value;
      
      for (i = 0; i < n2; i++)
	{
	  
	  switch (var->type) {
	    
	  case M_LONG:
	    memcpy ((char *) ptr2, (char *) ptr1, n1 * sizeof(long));
	    ptr1 += n1 * sizeof(long);
	    ptr2 += n1 * sizeof(long);
	    break;
	    
	  case M_FLOAT:
	    memcpy ((char *) ptr2, (char *) ptr1, n1 * sizeof(float));
	    ptr1 += n1 * sizeof(float);
	    ptr2 += n1 * sizeof(float);
	    break;
	    
	  case M_DOUBLE:
	    memcpy ((char *) ptr2, (char *) ptr1, n1 * sizeof(double));
	    ptr1 += n1 * sizeof(double);
	    ptr2 += n1 * sizeof(double);
	    break;
	  }
	}
    }
  
  return(0);
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : read_control
 * COMMENT  : reads the control data base from a file
 *            File name is obtained from the environment variable "mms_control_file"
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define READ_CONTROL_C
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/
static char *rc (char *);
char *fgets_rc (char *, int , FILE *);

/*--------------------------------------------------------------------*\
 | FUNCTION     : read_control
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *read_control (char *control_name) {
   static char *foo = NULL;
   char old[256], *cptr;

   if (foo) {
      strncpy (old, foo, 256);
      free (foo);
      foo = strdup (control_name);
   } else {
      strncpy (old, control_name, 256);
      foo = strdup (control_name);
   }

   cptr = rc (control_name);

   if (cptr) {
      rc (old);

      free (foo);
      foo = strdup (old);
   }

   return (cptr);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : rc
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
static char *rc (char *control_name) {
   FILE   *control_file;
   CONTROL *cp;
   long   type, size, i;
   double   *dptr;
   float   *fptr;
   long   *lptr;
   char   line[MAXCTRLLINELEN], *key;
   static char      buf[256];

/*
* compute control path, open file
*/
   if ((control_file = fopen (control_name, "r")) == NULL) {
      (void)sprintf (buf, "read_control: Couldn't open %s", control_name);
      return (buf);
   }

   if (!fgets_rc(line, MAXCTRLLINELEN, control_file)) {
      fclose (control_file);
      (void)sprintf (buf, "read_control: Problems reading %s", control_name);
      return (buf);
   }

/*
**   Read in control variables.
*/
   while (!feof (control_file)) {

/*
**   Space fwd to #### header.
*/
      while (strncmp(line, "####", 4)) {
         if (fgets_rc(line, MAXCTRLLINELEN, control_file) == NULL) {
            fclose(control_file);
            return(NULL);
         }
      }
/*
**   get key
*/
      if (!fgets_rc (line, MAXCTRLLINELEN, control_file)) {
         (void)sprintf (buf, "read_control: reading key; Early end-of-file");
         printf ("read_control: reading key; Early end-of-file\n");
         return (buf);
      }

	  /* Replace the end of line with a null */
      *(line + strlen (line) - 1) = '\0';
	  key = strdup (line);


/*
**   get size
*/
      if (!fgets_rc (line, MAXCTRLLINELEN, control_file)) {
         (void)sprintf (buf,"read_control: reading size; key = %s", key);
         return (buf);
      }

      if ((size = atol(line)) < 0) {
         (void)sprintf (buf, "read_control: negative size; key = %s, line = %s", key, line);
         return (buf);
      }
/*
**   get type
*/
      if (!fgets_rc (line, MAXCTRLLINELEN, control_file)) {
         (void)sprintf (buf, "WARNING: reading type; key = %s", key);
         return (buf);
      }

      if (!(type = atol(line))) {
         (void)sprintf (buf, "WARNING: invalid type; key = %s, line = %s", key, line);
         return (buf);
      }

      cp = control_addr (key);
      if (!cp) {
         cp = add_control (key, type, size); // This is if the control variable was not set in the setupcont function
//	  printf ("   read_control E %s NOT FOUND in SETUPCONT\n", key);
     }
     
	  if (cp->set_in_file > 0) {
		   printf ("\n\nWARNING: %s is duplicated in the control file %s.\n\n", key, control_name);
	  }

//  Set the values to what was just read from the file
      cp->key = strdup(key);
      cp->type = type;
      cp->size = size;
      cp->set_in_file = 1;

      switch (type) {
         case M_DOUBLE:
			dptr = (double *)umalloc (sizeof (double) * size);
            cp->start_ptr = (void *)dptr;
            for (i = 0; i < size; i++) {
               if (fgets_rc(line, MAXCTRLLINELEN, control_file) == NULL) {
                  (void)sprintf (buf, "read_control: key is %s.\n, file: %s", key, control_name);
                  printf ("read_control CRASH reading control file: key is %s.\n, file: %s\n", key, control_name);
                  return (buf);
               }
               dptr[i] = atof(line);
            }
            break;

         case M_FLOAT:
			fptr = (float *)umalloc (sizeof (float) * size);
            cp->start_ptr = (void *)fptr;
            for (i = 0; i < size; i++) {
               if (fgets_rc(line, MAXCTRLLINELEN, control_file) == NULL) {
                  (void)sprintf (buf, "read_control: key is %s.\n, file: %s", key, control_name);
                  printf ("read_control CRASH reading control file: key is %s.\n, file: %s\n", key, control_name);
                  return (buf);
               }
               fptr[i] = (float) atof(line);
            }
            break;

         case M_LONG:
			lptr = (long *)umalloc (sizeof (long) * size);
            cp->start_ptr = (void *)lptr;
            for (i = 0; i < size; i++) {
               if (fgets_rc(line, MAXCTRLLINELEN, control_file) == NULL) {
                  (void)sprintf (buf, "read_control: key is %s.\n, file: %s", key, control_name);
                  printf ("read_control CRASH reading control file: key is %s.\n, file: %s\n", key, control_name);
                  return (buf);
               }
               lptr[i] =  atol(line);
            }
            break;

         case M_STRING:
			cp->start_ptr = umalloc (sizeof (char *) * size);
            for (i = 0; i < size; i++) {
               if (fgets_rc(line, MAXCTRLLINELEN, control_file) == NULL) {
                  (void)sprintf (buf, "read_control: key is %s.\n, file: %s", key, control_name);
                  printf ("read_control CRASH reading control file: key is %s.\n, file: %s\n", key, control_name);
                  return (buf);
               }
               line[strlen(line)-1] = '\0';
               *((char **)cp->start_ptr + i) = strdup (line);

/*			   printf ("read_control just put in string value %s\n", *((char **)cp->start_ptr + i));*/
            }
            break;
      }
   }
   fclose (control_file);
   return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : fgets_rc
 | COMMENT      : replacement in read_control functions for fgets which
 |                stripps off comments.
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *fgets_rc (char *str, int num, FILE *stream) {
   char *ptr, *ptr2;
   // Four situations: (1) Line is blank, (2) line starts with //,
   //   (3) line has info and contains //, (4) line has info and
   //   does not contain //

   ptr = fgets(str, num, stream);
   if (!ptr) return ptr;

/*
**  A line that starts with "//" is a comment (as far as MMS is concerned).
*/
      if ((str[0] == '/') && (str[1] == '/')) {
         return fgets_rc (str, num, stream);

/*
**  Ignore blank lines
*/
      } else if (strlen (str) <= 1) {
         return fgets_rc (str, num, stream);
/*
** Assume anything else is a data line
*/

      } else if (strstr (str, "//")) {
/*
** comment in data line
*/
         ptr2 = strstr (str, "//");
         ptr2--;
         while (*ptr2 != ' ') ptr2--;
         *(ptr2 + 1) = '\0';
         return ptr;

      } else {
         return ptr;
      }
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : read_datainfo
 * COMMENT  : reads the data file and updates the
 *            datainfo string and the data variable names and sizes
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define READ_DATAINFO_C
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : read_datainfo
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *read_datainfo (FILE_DATA *fd) {

   static char   err_buf[512];
   long   count, nline = 0;
   static char   *line = NULL;
   static char *linecopy = NULL;
   PUBVAR   *var;
   char   *key, *countstr, *endptr;

   Mnreads = 0;

   if (line == NULL) {
	   line = (char *) umalloc(max_data_ln_len * sizeof(char));
   }

   if (linecopy == NULL) {
	   linecopy = (char *) umalloc(max_data_ln_len * sizeof(char));
   }

   if (!(fgets (fd->info, max_data_ln_len, fd->fp))) {
      (void)sprintf (err_buf, "Can't read data file info string\n%s", fd->name);
      return (err_buf);
   }

   fd->info[strlen (fd->info) - 1] = '\0';
   nline++;

/*
**  Read the header of the data file.  The end of the header occures
**  when a line starts with at least 4 "#"s.
*/
   (void)strcpy (line, "");
   while (strncmp (line, "####", 4)) {
      if ((fgets (line, max_data_ln_len, fd->fp)) == NULL) {
         (void)sprintf (err_buf,"#### delimiter not found in data file\n%s", fd->name);
         return (err_buf);
      }

      nline++;
      if (strncmp(line, "####", 4)) {

/*
**  A line that starts with "//" is a comment (as far as MMS is concerned).
*/
         if ((line[0] == '/') && (line[1] == '/')) {
/*
            printf ("Comment: %s\n", line);
*/
/*
**  Ignore blank lines
*/
         } else if (strlen (line) <= 1) {

/*
** Assume anything else is a data line
*/
         } else {

/* 
**   Increase size of array pointers
*/
            if (Mnreads >= max_read_vars) {
               max_read_vars += 50;
               Mcheckbase = (READCHECK **)realloc(Mcheckbase, max_read_vars * 
                  sizeof(READCHECK *));
            }
      
/*
**    get key, check the var has been declared
*/

            (void)strcpy(linecopy, line);
            key = strtok(linecopy, " \t");

            if (key == NULL) {
               (void)sprintf (err_buf,"Check format at line number %ld in\n%s\n%s", nline,
                  fd->name, line);
               return (err_buf);
            }

/*
**   get size of var in data file
*/
            countstr = strtok(NULL, " \t");

            if (countstr == NULL) {
               (void)sprintf (err_buf,"Check format at line number %ld in\n%s\n%s", nline,
                  fd->name, line);
               return (err_buf);
            }

            errno = 0;
            count = strtol (countstr, &endptr, 10);

				if (count > 0) {  // Old style Data files have variables with size 0. PRMS should now skip over these, as if they are not there.

					if ((var = var_addr(key)) == NULL) {
						(void)sprintf (err_buf,
							"Variable %s not declared at line number %ld in\n%s\n%s",
							key, nline, fd->name, line);
						return (err_buf);
					}

	/*
	**   make space for data base entry, load pointer to var
	*/

					Mcheckbase[Mnreads] = (READCHECK *) umalloc(sizeof(READCHECK));
					Mcheckbase[Mnreads]->var = var;



					if (errno || (count < 0)) {
						(void)sprintf (err_buf,"Decoding %s at line number %ld in\n%s\n%s",
							countstr, nline, fd->name, line);
						return (err_buf);
					}

					Mcheckbase[Mnreads]->count = count;

	/* 
	**   allocate enough room to read variables in, depending on variable type
	*/

					if (Mcheckbase[Mnreads]->var) {
						switch (Mcheckbase[Mnreads]->var->type) {
							case M_LONG :
								Mcheckbase[Mnreads]->Types.valuel = (long *)umalloc(count * 
									sizeof(long));
								break;
	     
							case M_FLOAT :
								Mcheckbase[Mnreads]->Types.valuef = (float *)umalloc(count *
									sizeof(float));
								break;
	     
							case M_DOUBLE :
								Mcheckbase[Mnreads]->Types.valued=(double *)umalloc(count *
									sizeof(double));
								break;

						}
					}           
					Mnreads++;
				}
			}
      }
   }

/*
**   Read first line of data
*/
   if (!(fgets (fd->line, max_data_ln_len, fd->fp))) {
      (void)sprintf (err_buf,
         "read_datainfo: Data for first timestep not found in file %s\n",
         fd->name);
      return (err_buf);
   }

   return (NULL);
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : read_line
 *            Returns 1l if end of data, 0l if more data to be read,
 *            and 2l if end of file
 * COMMENT  : Reads one line from data file into a string,
 *            decodes date and time, and returns if the time
 *            is within start and end limits.
 *            Otherwise reads lines until within limits or the
 *            end of file is encountered.
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define READ_LINE_C

#include <sys/stat.h>
#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/
static void INSERT_time (char *, DATETIME *);

/**5*********************** LOCAL VARIABLES ***************************/
/*
static double   prevjt = -1.0;
*/

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : read_line
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : void
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long read_line (void) {

   /*static char err[80];*/

   char   *start_point, *end_point;
   float   initial_deltat;
   long   i,j;
   static int   start_of_data;
   static long	data_eof_flag;
   DATETIME   prevtime;
   FILE_DATA   *cur_fd;
   char   *err_ptr;
   static char *line = NULL;
  
   if (line == NULL) {
	   line = (char *) umalloc(max_data_ln_len * sizeof(char));
   }

/*
**   get initial delta-t from control data base
*/
   initial_deltat = *control_fvar("initial_deltat");
   data_eof_flag = *control_lvar ("ignore_data_file_end");

   if (Mnsteps == 0) {
      start_of_data = TRUE;
      Mprevjt = 1.0;
/*
      prevjt = -1.0;
*/
   }

   prevtime = *Mnowtime;

   while (TRUE) {

/*
**  Load cur_fd with the data for the next time step.
*/
      cur_fd = FILE_with_next_ts ();

/*
**  9999 in the year field is the code for EOF. 
*/
      if (cur_fd->time.year == 9999) {
		  (void)fprintf (stderr,"9999  1 \n");
         return ENDOFFILE;
	  }

/*
**   DANGER -- This "if" is a hack to get delta time back after storm mode
**            This is the situation :

cur_fd->time = {year = 1956, month = 2, day = 19, hour = 0, min = 0, sec = 0, 
  jd = 2435523, jt = 2435523}

*Mnowtime = {year = 1956, month = 2, day = 18, hour = 24, min = 0, sec = 0, 
  jd = 2435522, jt = 2435523}

*/

      if ((Mprevjt < 0.0) && (cur_fd->time.jt - Mprevjt <= 0.000001)) {
         Mprevjt = (double)(Mnowtime->jd);
      }

/*
**  End of DANGER
*/

/*
**   Copy time from current file into global time structure
*/
      Mnowtime->year = cur_fd->time.year;
      Mnowtime->month = cur_fd->time.month;
      Mnowtime->day = cur_fd->time.day;
      Mnowtime->hour = cur_fd->time.hour;
      Mnowtime->min = cur_fd->time.min;
      Mnowtime->sec = cur_fd->time.sec;
      Mnowtime->jd = cur_fd->time.jd;
      Mnowtime->jt = cur_fd->time.jt;

/*
**   check if data time is within limits
*/
      if (Mnowtime->jt > Mendtime->jt) {
		  /*
		  (void)fprintf (stderr,"\n\n nowtime = %ld\n", Mnowtime->year);
		 (void)fprintf (stderr,"\n\n endtime = %ld\n", Mendtime->year);
		 (void)fprintf (stderr,"\n\n nowtime = %ld\n", Mnowtime->month);
		 (void)fprintf (stderr,"\n\n endtime = %ld\n", Mendtime->month);
		 (void)fprintf (stderr,"\n\n nowtime = %ld\n", Mnowtime->day);
		 (void)fprintf (stderr,"\n\n endtime = %ld\n", Mendtime->day);
		 (void)fprintf (stderr,"\n\n nowtime = %ld\n", Mnowtime->hour);
		 (void)fprintf (stderr,"\n\n endtime = %ld\n", Mendtime->hour);
		 (void)fprintf (stderr,"\n\n nowtime = %ld\n", Mnowtime->min);
		 (void)fprintf (stderr,"\n\n endtime = %ld\n", Mendtime->min);
		 (void)fprintf (stderr,"\n\n nowtime = %ld\n", Mnowtime->sec);
		 (void)fprintf (stderr,"\n\n endtime = %ld\n", Mendtime->sec);
		 (void)fprintf (stderr,"\n\n nowtime = %ld\n", Mnowtime->jd);
		 (void)fprintf (stderr,"\n\n endtime = %ld\n", Mendtime->jd);
  		 (void)fprintf (stderr,"\n\n nowtime = %f\n", Mnowtime->jt);
		 (void)fprintf (stderr,"\n\n endtime = %f\n", Mendtime->jt); 
		 */
		  *Mnowtime = prevtime;
		 //(void)fprintf (stderr,"nowtime=endtime\n");
		 if (data_eof_flag == 1) {
			Mendtime = Mnowtime;
			return (0); }
		 //(void)fprintf (stderr,"nowtime=endtime 2\n");
         return ENDOFDATA;
      }

      if (Mnowtime->jt >= Mstrttime->jt) {
         if (start_of_data) {
            start_of_data = 0;
            Mprevjt = Mnowtime->jt - (double)(initial_deltat / 24.0);
         }

         (void)strcpy (line, cur_fd->start_of_data);
         Mnsteps++;
/*
**  DANGER -- Mprevjt must be hacked if starting from var init file.
**            It is computed based on current time and Mdeltat (which
**            is read from var init file.
*/

/*
         (void)fprintf (stderr,"\n\n read_line Mprevjt = %f Mnowtime->jt = %f\n", Mprevjt, Mnowtime->jt);
         (void)fprintf (stderr,"     read_line dt = %f\n", Mnowtime->jt - Mprevjt );
         (void)fprintf (stderr,"          year, mon, day = %d %d %d %d %d\n", Mnowtime->year, Mnowtime->month, Mnowtime->day, Mnowtime->hour, Mnowtime->min);
*/

		 if (Mnowtime->jt < Mprevjt) {
			(void)fprintf (stderr,"\n\n read_line Mprevjt = %f Mnowtime->jt = %f\n", Mprevjt, Mnowtime->jt);
			(void)fprintf (stderr,"Current time step is before previous time step.\n");
			(void)fprintf (stderr,"The data file(s) are running backwards.\n");
			return (ERROR_TIME);
		 }

         if ((Mnowtime->jt - Mprevjt) < 0.0000115) {
/*
** DANGER This hack is to come out of the storm
*/
            (void)fprintf (stderr,"read_line:  comming out of storm. dt = 1 day\n");
            Mdeltat = 1.0;
            Mprevjt = Mnowtime->jt - Mdeltat;

         } else {
            if (Mprevjt < 0.0) {
               Mprevjt = Mnowtime->jt - Mdeltat;
            } else {
               Mdeltat = Mnowtime->jt - Mprevjt;
            }
         }
/*
         (void)fprintf (stderr,"read_line Mdeltat = %f\n", Mdeltat );
*/

/*
**  End DANGER
*/

         Minpptr = end_point = line;

/* 
**   Read variables from the line into their respective buffers
*/
         for (i = 0; i < Mnreads; i++) {
            for (j = 0; j < Mcheckbase[i]->count; j++) {
               start_point = NULL;
               if (Mcheckbase[i]->var) {
                  start_point = end_point;
                  errno = 0;
                  switch (Mcheckbase[i]->var->type) {
                     case M_LONG:
                        Mcheckbase[i]->Types.valuel[j] =
                           strtol (start_point, &end_point, 10);
                        break;
      
                     case M_FLOAT:
                        Mcheckbase[i]->Types.valuef[j] =
                           (float)strtod (start_point, &end_point);
                        break;
      
                     case M_DOUBLE:
                        Mcheckbase[i]->Types.valued[j] =
                           strtod (start_point, &end_point);
                        break;
                     }
				  	if (CHECK_data (errno, cur_fd))	 (void)fprintf (stderr,"nowtime=endtime 2\n");
                  if (CHECK_data (errno, cur_fd)) return (ENDOFDATA);

               } else {
                  (void)strtod (start_point, &end_point);
               }
            }
         }

         Mprevjt = Mnowtime->jt;

         if (!(fgets (cur_fd->line, max_data_ln_len, cur_fd->fp))) {
            fclose (cur_fd->fp);
            cur_fd->fp = NULL;
            cur_fd->time.year = 9999;
         } else if (cur_fd->line[0] == '\n') {
            fclose (cur_fd->fp);
            cur_fd->fp = NULL;
            cur_fd->time.year = 9999;
         } else {
            err_ptr = EXTRACT_time (cur_fd);
            if (err_ptr) {
               (void)fprintf (stderr,"%s\n", err_ptr);
               return (ERROR_TIME);
            }
         }

/*
**   Copy time from current file into global next time structure
*/
         if (cur_fd && cur_fd->fp) {
            Mnexttime->year = cur_fd->time.year;
            Mnexttime->month = cur_fd->time.month;
            Mnexttime->day = cur_fd->time.day;
            Mnexttime->hour = cur_fd->time.hour;
            Mnexttime->min = cur_fd->time.min;
            Mnexttime->sec = cur_fd->time.sec;
            Mnexttime->jd = cur_fd->time.jd;
            Mnexttime->jt = cur_fd->time.jt;
            Mdeltanext = Mnexttime->jt - Mnowtime->jt;
         }
         return (0);
      } else {
/*
**   Read throgh the data before the start time.
*/
         Mprevjt = Mnowtime->jt;

         if (!(fgets (cur_fd->line, max_data_ln_len, cur_fd->fp))) {
            fclose (cur_fd->fp);
            cur_fd->fp = NULL;
            cur_fd->time.year = 9999;
         }

         err_ptr = EXTRACT_time (cur_fd);
         if (err_ptr) {
               (void)fprintf (stderr,"%s\n", err_ptr);
            return (ERROR_TIME);
         }
      }
   }
/*
   return (0);
*/
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : DATA_read_init
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : char *
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *DATA_read_init (void) {

   static char err[80];

   int      i;
   static int      num_data_files = 0;
   char   **fname, *err_ptr;
   static char      buf[256];
   static char   *line = NULL;

   if (line == NULL) {
	   line = (char *) umalloc(max_data_ln_len * sizeof(char));
   }

/*
**   Clean up the old files
*/
   if (fd) {
      for (i = 0; i < num_data_files; i++)
         if ((fd[i])->fp) {
               fclose ((fd[i])->fp);
            fd[i]->fp = NULL;
            }
   }

   fname =   control_svar ("data_file");
   num_data_files = control_var_size ("data_file");

   fd = (FILE_DATA **)malloc (num_data_files * sizeof (FILE_DATA *));
    for (i = 0; i < num_data_files; i++) {
      fd[i] = (FILE_DATA *)malloc (sizeof (FILE_DATA));
	  fd[i]->line = (char *) umalloc(max_data_ln_len * sizeof(char));
	  fd[i]->info = (char *) umalloc(max_data_ln_len * sizeof(char));
    }

/*
**   Open the files.
*/
   for (i = 0; i < num_data_files; i++) {
      (fd[i])->name = strdup (fname[i]);
      if (!((fd[i])->fp = fopen (fname[i], "r"))) {
         (void)sprintf (err, "DATA_read_init: can't open data file %s\n",
            fname[i]);
         return (err);
      }

      fgets (line, max_data_ln_len, (fd[i])->fp);
      while (strncmp (line, "####", 4)) {
         if (!(fgets (line, max_data_ln_len, (fd[i])->fp))) {
               (void)sprintf (buf, "DATA_read_init - Spacing fwd to data - Check format of file %s.", fname[i]);
               return (buf);
            }
      }

    /*
     * initialize year
     * PJR 7/10/95
     */
       (fd[i])->time.year = 0;
      fgets ((fd[i])->line, max_data_ln_len, (fd[i])->fp);
/* DANGER
      if (err_ptr = EXTRACT_time (&(fd[i])))
*/
      err_ptr = EXTRACT_time (fd[i]);
      if (err_ptr) return (err_ptr);
    
   }
   return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : READ_data_info
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : char *
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *READ_data_info (void) {

   static char err[80];

   int      i, num_data_files;
   char   **fname, *err_ptr;
   FILE_DATA  lfd;
   struct stat stbuf;

   lfd.info = (char *) umalloc(max_data_ln_len * sizeof(char));
   lfd.line = (char *) umalloc(max_data_ln_len * sizeof(char));

   fname =   (char **) control_var ("data_file");
   num_data_files = control_var_size ("data_file");

/*
**   Check the files
*/
   for (i = 0; i < num_data_files; i++) {
      if (stat (fname[i], &stbuf) == -1) {
         (void)sprintf (err, "Reading Data Info: Can't open data file %s\n",
            fname[i]);
         return (err);
      } else if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
         (void)sprintf (err, "Reading Data Info: Can't open data file %s\n",
            fname[i]);
         return (err);
      }
   }
/*
**   Open the files.
*/
   for (i = 0; i < num_data_files; i++) {
      lfd.name = strdup (fname[i]);
      if (!(lfd.fp = fopen (fname[i], "r"))) {
         (void)sprintf (err, "DATA_read_init: can't open data file %s\n",
            fname[i]);
         return (err);
      }
      err_ptr = read_datainfo (&lfd);
      if (err_ptr) return (err_ptr);

      fclose (lfd.fp);
   }
   return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : DATA_check_start
 | COMMENT      : Check if start time of model is more than a day before
 |                 the start time of the data.
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *DATA_check_start (void) {

   int num_data_files, found, i;

   num_data_files = control_var_size ("data_file");

   found = FALSE;
   for (i = 0; (i < num_data_files) && !found; i++)
      if ((fd[i])->time.jd <= Mstrttime->jd)
         found = TRUE;

   if (!found)
      return ("Start time is before first data.");

   return (NULL);
}
/*--------------------------------------------------------------------*\
 | FUNCTION     : DATA_close
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : void 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void DATA_close (void) {
   int      i, num_data_files;

   num_data_files = control_var_size ("data_file");

   for (i = 0; i < num_data_files; i++) {
        if (((fd[i])->fp) != NULL) {
         fclose ((fd[i])->fp);
         (fd[i])->fp = NULL;
        }
    }

   fd = NULL;
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : control_var_size
 | COMMENT      : returns the size of the array
 | PARAMETERS   :
 | RETURN VALUE : int - returns the size of the array
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int control_var_size (char *key) {
   CONTROL *control;

   if (!(control = control_addr(key))) {
      (void)fprintf (stderr, 
         "control_var_size - key '%s' not found.\n", key);
      return (1);
   }
   return (control->size);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : FILE_with_next_ts
 | COMMENT      : Determine the file with the next time step.
 | PARAMETERS   : None
 | RETURN VALUE : Pointer to file data structure
 | RESTRICTIONS : None
\*--------------------------------------------------------------------*/
FILE_DATA * FILE_with_next_ts (void) {

   FILE_DATA *cur_fd, *fd_ptr;
   int      num_data_files, i;
   char   *err_ptr;

   num_data_files = control_var_size ("data_file");
   cur_fd = fd[0];

   for (i = 1; i < num_data_files; i++) {
        fd_ptr = fd[i];  
      if (fd_ptr->time.year != 9999) {
/*
**   If two files have the same julian day, assume one is a storm file
**   and one is a daily file.  Throw out the daily value.
*/
         if (fd_ptr->time.jd == cur_fd->time.jd) {
            if (fd_ptr->time.jt == cur_fd->time.jt) {
               (void)fprintf (stderr,
                  "FILE_with_next_ts: The files %s and %s both seem to contain the same storm on %ld - %ld - %ld.\n",
                  fd_ptr->name, cur_fd->name, fd_ptr->time.year,
                  fd_ptr->time.month, fd_ptr->time.day);

            } else if (fd_ptr->time.jt < cur_fd->time.jt) {

               Mprevjt = fd_ptr->time.jt;

               if (!(fgets (fd_ptr->line, max_data_ln_len, fd_ptr->fp))) {
                  fclose (fd_ptr->fp);
                      fd_ptr->fp = NULL;
                  fd_ptr->time.year = 9999;
               } else if (fd_ptr->line[0] == '\n') {
                  fclose (fd_ptr->fp);
                      fd_ptr->fp = NULL;
                  fd_ptr->time.year = 9999;
               } else {
                  err_ptr = EXTRACT_time (fd_ptr);
                  if (err_ptr) (void)fprintf (stderr,"%s\n", err_ptr);
               }


            } else {

               Mprevjt = cur_fd->time.jt;

               if (!(fgets (cur_fd->line, max_data_ln_len, cur_fd->fp))) {
                  fclose (cur_fd->fp);
                      cur_fd->fp = NULL;
                  cur_fd->time.year = 9999;
               } else if (cur_fd->line[0] == '\n') {
                  fclose (cur_fd->fp);
                      cur_fd->fp = NULL;
                  cur_fd->time.year = 9999;
               } else {
                  err_ptr = EXTRACT_time (cur_fd);
                  if (err_ptr) (void)fprintf (stderr,"%s\n", err_ptr);
               }

               cur_fd = fd_ptr;
            }

         } else if (fd_ptr->time.jd < cur_fd->time.jd) {
            cur_fd = fd_ptr;
         }
      }
   }

   return (cur_fd);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : EXTRACT_time
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char * EXTRACT_time (FILE_DATA *data) {
   char   *start_point, *end_point;
   static char   *line = NULL;
   static char   *err_buf = NULL;

   if (line == NULL) {
	   line = (char *) umalloc(max_data_ln_len * sizeof(char));
   }

   if (err_buf == NULL) {
	   err_buf = (char *) umalloc(max_data_ln_len * sizeof(char));
   }

   if (data->time.year == 9999) {
	   (void)fprintf (stderr,"9990\n");
      return (NULL);
   }

   (void)strcpy (line, data->line);

   start_point = line;

   errno = 0;
   data->time.year = strtol (start_point, &end_point, 10);

   if (data->time.year < 1800 || data->time.year > 2200) {
      (void)sprintf (err_buf, "EXTRACT_time - year %ld out of range.\nline:%s",
         data->time.year, data->line);
      return (err_buf);
   }

   if (errno == EDOM || errno == ERANGE) {
      (void)sprintf(err_buf, "EXTRACT_time - Decoding year line %s", start_point);
      return (err_buf);
   }

   start_point = end_point;
   errno = 0;
   data->time.month = strtol (start_point, &end_point, 10);

   if (data->time.month < 1 || data->time.month > 12) {
      (void)sprintf (err_buf, "EXTRACT_time - month %ld out of range.\nline:%s",
         data->time.month, data->line);
      return (err_buf);
   }

   if (errno == EDOM || errno == ERANGE) {
      (void)sprintf(err_buf, "EXTRACT_time - Decoding month line %s",start_point);
      return (err_buf);
   }

   start_point = end_point;
   errno = 0;
   data->time.day = strtol (start_point, &end_point, 10);

   if (data->time.day < 1 || data->time.day > 31) {
      (void)sprintf (err_buf, "EXTRACT_time - day %ld out of range.\nline:%s",data->time.day, data->line);
      return (err_buf);
   }

   if (errno == EDOM || errno == ERANGE) {
      (void)sprintf (err_buf, "Decoding day line %s", start_point);
      return (err_buf);
   }

   start_point = end_point;
   errno = 0;
   data->time.hour = strtol(start_point, &end_point, 10);

   if (data->time.hour < 0 || data->time.hour > 24) {
      (void)sprintf(err_buf,"EXTRACT_time - hour %ld out of range.\nline:%s",data->time.hour, data->line);
      return  (err_buf);
   }

   if (errno == EDOM || errno == ERANGE)   {
      (void)sprintf (err_buf, "Decoding hour line %s", start_point);
      return (err_buf);
   }

   start_point = end_point;
   errno = 0;
   data->time.min = strtol(start_point, &end_point, 10);


    if (data->time.min < 0 || data->time.min > 59) {
      (void)sprintf (err_buf, "EXTRACT_time - minute %ld out of range.\nline:%s",
         data->time.min, data->line);
      return (err_buf);
   }

   if (errno == EDOM || errno == ERANGE) {
      (void)sprintf (err_buf, "Decoding minute line %s", start_point);
      return (err_buf);
   }

   start_point = end_point;
   errno = 0;
   data->time.sec = (int)(strtod(start_point, &end_point));
/*
   data->time.sec = strtod(start_point, &end_point, 10);
*/

   if (data->time.sec < 0 || data->time.min > 59) {
      (void)sprintf (err_buf, "EXTRACT_time - second %ld out of range.\nline:%s",
         data->time.sec, data->line);
      return (err_buf);
   }

   if (errno == EDOM || errno == ERANGE) {
      (void)sprintf (err_buf, "Decoding second line %s\n", start_point);
      return (err_buf);
   }

   data->start_of_data = data->line + (end_point - line);

   julday (&(data->time));

   return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : CHECK_data
 | COMMENT      : See if there is an error with the data value.
 | PARAMETERS   :
 | RETURN VALUE : Error code
 | RESTRICTIONS : None
\*--------------------------------------------------------------------*/
int CHECK_data (int en, FILE_DATA *cur_fd) {
   if (en == EDOM || en == ERANGE) {
      (void)fprintf (stderr,"read_line");
      perror (" ");
      (void)fprintf (stderr, "Reading line %s\n", cur_fd->line);
      return (ENDOFDATA);
   }
   return (0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : DATA_find_end
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void DATA_find_end (DATETIME *start_of_data, DATETIME *end_of_data) {
  FILE   *f_ptr;
  int      i, num_data_files;
  DATETIME check;
  static char *line = NULL;

   if (line == NULL) {
	   line = (char *) umalloc(max_data_ln_len * sizeof(char));
   }

  num_data_files = control_var_size ("data_file");

  /*
  **  Get start and end of first file.
  */
  f_ptr = fopen ((fd[0])->name, "r");
  
  fgets (line, max_data_ln_len, f_ptr);
  while (strncmp (line, "####", 4))
    fgets (line, max_data_ln_len, f_ptr);

  fgets (line, max_data_ln_len, f_ptr);
  INSERT_time (line, start_of_data);

  while (fgets (line, max_data_ln_len, f_ptr));

  INSERT_time (line, end_of_data);

  fclose (f_ptr);

  /*
  **  Loop through the other ones.
  */
  for (i = 1; i < num_data_files; i++) {
    f_ptr = fopen ((fd[i])->name, "r");

    fgets (line, max_data_ln_len, f_ptr);
    while (strncmp (line, "####", 4))
      fgets (line, max_data_ln_len, f_ptr);

    fgets (line, max_data_ln_len, f_ptr);
    INSERT_time (line, &check);

    if (check.jt < start_of_data->jt) {
      start_of_data->year = check.year;
      start_of_data->month = check.month;
      start_of_data->day = check.day;
      start_of_data->hour = check.hour;
      start_of_data->min = check.min;
      start_of_data->sec = check.sec;
      start_of_data->jd = check.jd;
      start_of_data->jt = check.jt;
    }

    while (fgets (line, max_data_ln_len, f_ptr));
    INSERT_time (line, &check);

    if (check.jt > end_of_data->jt) {
      end_of_data->year = check.year;
      end_of_data->month = check.month;
      end_of_data->day = check.day;
      end_of_data->hour = check.hour;
      end_of_data->min = check.min;
      end_of_data->sec = check.sec;
      end_of_data->jd = check.jd;
      end_of_data->jt = check.jt;
    }
    fclose (f_ptr);
  }
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : INSERT_time
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
static void INSERT_time (char *line, DATETIME *ptr) {
   char   *start_point, *end_point;

   start_point = line;
   ptr->year = strtol (start_point, &end_point, 10);

   start_point = end_point;
   ptr->month = strtol (start_point, &end_point, 10);

   start_point = end_point;
   ptr->day = strtol (start_point, &end_point, 10);

   start_point = end_point;
   ptr->hour = strtol(start_point, &end_point, 10);

   start_point = end_point;
   ptr->min = strtol(start_point, &end_point, 10);

   start_point = end_point;
   ptr->sec = strtol(start_point, &end_point, 10);

   julday (ptr);
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : read_params
 * COMMENT  : reads the params data base from a file
 *            File name is passed in as an argument
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define READ_PARAMS_C
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>
#include "mms.h"

static char *READ_param_head (PARAM **, FILE **, char *, char[], int);
static char *READ_param_values (long, long, char *, char *, FILE *, char[]);
static char *rp (char *, int, int);
static int checkForValidDimensions (PARAM *);
static int isDimensionIncompatable (char *, char *);
static void oneToAnySizedArray(PARAM *, char *);
static int getParamFileParamSize (PARAM *);
static char *getMapParamName(char *);
static void subbasinTo1DArray (PARAM *, PARAM *, char *);

static char* dimNames[] = {"nhru", "nsegment",
	"nrain", "ntemp", "nobs", "ngw",
	"nssr"
};

static char* mapParamNames[] = {"hru_subbasin", "segment_subbasin",
	"rain_subbasin", "temp_subbasin", "obs_subbasin", "gw_subbasin",
	"ssr_subbasin"
};

int nComments;
char **Comments;

/*--------------------------------------------------------------------*\
 | FUNCTION     : read_params
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *read_params (char *param_file_name, int index, int mapping_flag) {
  	static char *foo = NULL;
  	char old[256], *cptr;

	if (foo) {
		strncpy (old, foo, 256);
		free (foo);
		foo = strdup (param_file_name);
	} else {
		strncpy (old, param_file_name, 256);
		foo = strdup (param_file_name);
	}

	cptr = rp (param_file_name, index, mapping_flag);

	if (cptr) {
		rp (old, index, 0);

		free (foo);
		foo = strdup (old);
	}

	return (cptr);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : read_dims
 | COMMENT	:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *read_dims (char *param_file_name) {
  FILE *param_file;
  DIMEN *dim;
  int dim_size, i, j;

  static char *line = NULL;
  static char *key = NULL;
  static char buf[256];
  char *endptr;
  char *nch;
  int		done;
  
   if (line == NULL) {
	   line = (char *) umalloc(max_data_ln_len * sizeof(char));
   }

   if (key == NULL) {
	   key = (char *) umalloc(max_data_ln_len * sizeof(char));
   }

/*
* get param name, open file
*/
	if ((param_file = fopen (param_file_name, "r")) == NULL) {
		if (param_file_name) {
			(void)sprintf (buf, "ERROR: cannot open Parameter File: %s", param_file_name);
		} else {
			(void)sprintf (buf, "ERROR: cannot open Parameter File");
		}
		return (buf);
	}

/*
* read in run info string
*/
	if (!fgets (line, max_data_ln_len, param_file)) {
		if (param_file != NULL) {
		   fclose (param_file);
		   param_file = NULL;
		}

		(void)sprintf (buf, "ERROR: problems reading info line in Parameter File");
		return (buf);
	}

	if (Mparaminfo) {
		free (Mparaminfo);
	}
	Mparaminfo = strdup (line);

/*
**	See if version number is set
*/
	if (!fgets (line, max_data_ln_len, param_file)) {
		if (param_file != NULL) {
		   fclose (param_file);
		   param_file = NULL;
		}

		(void)sprintf (buf, "ERROR: problems reading version number in Parameter File");
		return (buf);
	}

	if (!fgets (line, max_data_ln_len, param_file)) {
		if (param_file != NULL) {
		   fclose (param_file);
		   param_file = NULL;
		}
		(void)sprintf (buf, "ERROR: problems reading dimension label in Parameter File");
		return (buf);
	}

/*
 *  Read in comments -- everything between version line and
 *  "** Dimensions **" line is a comment
 */

	Comments = (char **)malloc (1000 * sizeof (char *));
	nComments = 0;

	while (strncmp (line, "** Dimensions **", 16)) {
		if (!fgets (line, max_data_ln_len, param_file)) {
		   if (param_file != NULL) {
		      fclose (param_file);
		      param_file = NULL;
		   }
			(void)sprintf (buf, "ERROR: problems skipping comments in Parameter File");
			return (buf);
		}

		if (strncmp (line, "** Dimensions **", 16)) {
			printf ("Comment line = %s\n", line);
			Comments[nComments++] = strdup (line);
		}
	}
	//}

/*
**	Check dimension label
*/
	if (strncmp (line, "** Dimensions **", 16)) {
		if (param_file != NULL) {
		   fclose (param_file);
		   param_file = NULL;
		}
		(void)sprintf (buf, "ERROR: ** Dimensions ** label not found in Parameter File %s.",
		param_file_name);
		return (buf);
	}
  
	if (!fgets (line, max_data_ln_len, param_file)) {
		if (param_file != NULL) {
		   fclose (param_file);
		   param_file = NULL;
		}
		(void)sprintf (buf, "ERROR: unexpected end of Parameter File");
		return (buf);
	}

/*
* read in dimensions
*/
	while (strncmp (line, "** Parameters **", 16)) {

		if (strncmp (line, "####", 4)) {
		   if (param_file != NULL) {
		      fclose (param_file);
		      param_file = NULL;
		   }
			(void)sprintf (buf, "ERROR: expecting '####' found %s in Parameter File %s", line, param_file_name);
			return (buf);
		}

/*
**	Read dimension name from parameter file.
*/
		if (fgets (key, max_data_ln_len, param_file) == NULL) {
		   if (param_file != NULL) {
		      fclose (param_file);
		      param_file = NULL;
	       }
			(void)sprintf (buf, "ERROR: trying to read dimension name %s in Parameter File %s.", key, param_file_name);
			return (buf);
		}

		key[strlen(key)-1] = '\0';

		dim = dim_addr (key);
		if (dim) {
/*
**	Read dimension size from parameter file.
*/
			if (fgets (line, max_data_ln_len, param_file) == NULL) {
		       if (param_file != NULL) {
		          fclose (param_file);
		          param_file = NULL;
	           }
				(void)sprintf (buf, "ERROR: can't read dimension size for %s in Parameter File %s.", key, param_file_name);
				return (buf);
			}

			errno = 0;
			dim_size = strtol(line, &endptr, 10);
			if (errno != 0) {
		       if (param_file != NULL) {
		          fclose (param_file);
		          param_file = NULL;
		       }
				(void)sprintf (buf, "ERROR: size problem with %s in Parameter File %s", key, param_file_name);
				return (buf);
			}

/*
**	If necessary, reset dimension to value read from file.
*/
			if (dim->value != dim_size) {
				//reset_dim (dim, dim_size);
				dim->value = dim_size;
			}

/*
* check if there are index names below
*/
			if (fgets (line, max_data_ln_len, param_file)) {
				if (strncmp (line, "** Parameters **", 16)) {
					if (dim->names) {
				//        free (dim->names);
						dim->names = NULL;
					}

					if (dim->notes) {
					//        free (dim->notes);
						dim->notes = NULL;
					}

					if (strncmp (line, "####", 4)) {
						dim->names = (char **)calloc (dim_size, sizeof (char *));
						dim->notes = (char **)calloc (dim_size, sizeof (char *));

						done = FALSE;
						i = 0;
						while (!done) {
							if (!strncmp (line, "####", 4)) {
								for (j = i; j < dim_size; j++) {
									dim->names[j] = NULL;
									dim->notes[j] = NULL;
								}
								done = TRUE;

							} else if (line[0] == '@') {
								i--;
								nch = (char *)strchr (line, '\n');
								if (nch) {
									*nch = '\0';
								}
								dim->notes[i] = strdup (&(line[1]));
								fgets (line, max_data_ln_len, param_file);
								i++;

							} else {
								nch = (char *)strchr (line, '\n');
								if (nch) {
									*nch = '\0';
								}
								dim->names[i] = strdup (line);
								fgets (line, max_data_ln_len, param_file);
								i++;
							}

							if ((i > dim_size) || ((i == dim_size) && (line[0] != '@'))) {
								done = TRUE;
							}
						}
					} else {
						dim->names = NULL;
						dim->files = NULL;
						dim->notes = NULL;
					}
				}
			} else {
		       if (param_file != NULL) {
		          fclose (param_file);  // EOL was returned -- done reading dimensions from this file;
		          param_file = NULL;
		       }
				return (NULL);
			}
		} else {
			(void)fprintf (stderr,"\nWARNING: dimension '%s' is not required; set in parameter file:\n         %s\n", key, param_file_name);
			fgets (line, max_data_ln_len, param_file);
			fgets (line, max_data_ln_len, param_file);
		}
	}

	if (param_file != NULL) {
	   fclose (param_file);
	   param_file = NULL;
	}
	return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : rp
 | COMMENT	:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
static char *rp (char *param_file_name, int index, int map_flag) {

  FILE *param_file;
  PARAM *param;

  static char *line = NULL;
  static char buf[256], *buf_ptr;
  char *pf_value, *mapParamName;
  int i, j, k;
  PARAM *mapping_param;
//  int *mapping;

  if (line == NULL) {
	  line = (char *) umalloc(max_data_ln_len * sizeof(char));
  }

/*
* get param name, open file
*/
	if ((param_file = fopen (param_file_name, "r")) == NULL) {
		if (param_file_name)
			(void)sprintf (buf, "ERROR: cannot open Parameter File: %s", param_file_name);
		else
			(void)sprintf (buf, "ERROR: cannot open Parameter File");

		return (buf);
	}

	fgets (line, max_data_ln_len, param_file);
	if (index == 0) {  // if index equals zero, than this parameter file has dimension stuff and we need to skip over it.
		while (strncmp (line, "** Parameters **", 16)) {
			if (!fgets (line, max_data_ln_len, param_file)) {  // return if hits eol
		       if (param_file != NULL) {
		          fclose (param_file);
		          param_file = NULL;
		       }
			   return (NULL);
			}
		}
		fgets (line, max_data_ln_len, param_file);
	}

/*
**	Read in parameters.
*/
	while (!feof (param_file)) {
		buf_ptr = READ_param_head (&param, &param_file, param_file_name, line, map_flag);
		if (buf_ptr) {
			if (buf_ptr == (char *)-1) {
		      if (param_file != NULL) {
		         fclose (param_file);
		         param_file = NULL;
		      }
			  return (NULL);
			} else {
		       if (param_file != NULL) {
		          fclose (param_file);
		          param_file = NULL;
		       }
			   return (buf_ptr);
			}
		}

		if (param != NULL) {

			// If param->size (array size as defined by module) is the
			// same as param->pf_size (array size as defined by parameter
			// file, then read the values for this parameter from the
			// parameter file directly into param->value because the 
			// dimensions match.  If the sizes do not match, read the
			// values into a temporary array that is used to remap the
			// values into the correct size and shape for param->value.
			if (param->pf_size == param->size) {
				buf_ptr = READ_param_values (param->size, param->type, param->name, param->value, param_file, line);

			} else {

//  Make sure that this resizing code is in sync with the ParamToolExpandor code in the oui4 code base.

				if (param->type == M_DOUBLE) {
					pf_value = (char *)umalloc (param->pf_size * sizeof (double));
				} else if (param->type == M_FLOAT) {
					pf_value = (char *)umalloc (param->pf_size * sizeof (float));
				} else if (param->type == M_LONG) {
					pf_value = (char *)umalloc (param->pf_size * sizeof (int));
				} else if (param->type == M_STRING) {
					pf_value = (char *)umalloc (param->pf_size * sizeof (char *));
				} else {
					pf_value = NULL;
				}

				buf_ptr = READ_param_values (param->pf_size, param->type, param->name, pf_value, param_file, line);

				// The values read from the parameter file need to be resized to fit into the size
				// of the module array for this parameter.

				// It's easy when the size is 1.  Tested this works for floats
				if (param->pf_size == 1) {
					oneToAnySizedArray(param, pf_value);

				} else if (param->pf_ndimen == 1 && !strncmp(param->pf_dimNames[0], "nsub", 4) &&
						(!strncmp(param->dimen[0]->name, "nhru", 4) || !strncmp(param->dimen[0]->name, "nsegment", 8) ||
						!strncmp(param->dimen[0]->name, "nrain", 5) || !strncmp(param->dimen[0]->name, "ntemp", 5) ||
						!strncmp(param->dimen[0]->name, "nobs", 4) || !strncmp(param->dimen[0]->name, "ngw", 3) ||
						!strncmp(param->dimen[0]->name, "nssr", 4))) {  // subbasin to one mappable dimension

					mapParamName = getMapParamName(param->dimen[0]->name);

					mapping_param = param_addr (mapParamName);

					if (!mapping_param || !(mapping_param->read_in)) {
						sprintf (buf, "\nERROR: mapping parameter %s must be set in parameter file before parameter %s\n",
							mapParamName, param->name);
						return (buf);
					}

					subbasinTo1DArray (param, mapping_param, pf_value);

				} else if (param->pf_ndimen == 1 && param->ndimen == 2) { // 1D in parameter file to 2D in module

   					// convert "nmonths" to "nhru,nmonths"
					if (!strncmp(param->pf_dimNames[0], "nmonths", 7)
                                && !strncmp(param->dimen[0]->name, "nhru", 4) 
                                && !strncmp(param->dimen[1]->name, "nmonths", 7)) {

						if (param->type == M_DOUBLE) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((double *)(param->value))[j + (i*param->dimen[0]->value)] = ((double *)pf_value)[i];
								}
							}

						} else if (param->type == M_FLOAT) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((float *)(param->value))[j + (i*param->dimen[0]->value)] = ((float *)pf_value)[i];
								}
							}

						} else if (param->type == M_LONG) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((int *)(param->value))[j + (i*param->dimen[0]->value)] = ((int *)pf_value)[i];
								}
							}

						} else if (param->type == M_STRING) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									*((char **)param->value + (i*param->dimen[0]->value)) = strdup (pf_value + i);
								}
							}
						}

            // convert "nhru" to "nhru,nmonths"
					} else if (!strncmp(param->pf_dimNames[0], "nhru", 4)
                                && !strncmp(param->dimen[0]->name, "nhru", 4) 
                                && !strncmp(param->dimen[1]->name, "nmonths", 7)) {

						if (param->type == M_DOUBLE) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((double *)(param->value))[k++] = ((double *)pf_value)[j];
								}
							}

						} else if (param->type == M_FLOAT) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((float *)(param->value))[k++] = ((float *)pf_value)[j];
								}
							}

						} else if (param->type == M_LONG) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((int *)(param->value))[k++] = ((int *)pf_value)[j];
								}
							}

						} else if (param->type == M_STRING) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									*((char **)param->value + (k++)) = strdup (pf_value + j);
								}
							}
						}

            // convert "nrain" to "nrain,nmonths"
					} else if (!strncmp(param->pf_dimNames[0], "nrain", 5)
                                && !strncmp(param->dimen[0]->name, "nrain", 5) 
                                && !strncmp(param->dimen[1]->name, "nmonths", 7)) {

						if (param->type == M_DOUBLE) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((double *)(param->value))[k++] = ((double *)pf_value)[j];
								}
							}

						} else if (param->type == M_FLOAT) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((float *)(param->value))[k++] = ((float *)pf_value)[j];
								}
							}

						} else if (param->type == M_LONG) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((int *)(param->value))[k++] = ((int *)pf_value)[j];
								}
							}

						} else if (param->type == M_STRING) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									*((char **)param->value + (k++)) = strdup (pf_value + j);
								}
							}
						}

            // convert "ntemp" to "ntemp,nmonths"
					} else if (!strncmp(param->pf_dimNames[0], "ntemp", 5)
                                && !strncmp(param->dimen[0]->name, "ntemp", 5) 
                                && !strncmp(param->dimen[1]->name, "nmonths", 7)) {

						if (param->type == M_DOUBLE) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((double *)(param->value))[k++] = ((double *)pf_value)[j];
								}
							}

						} else if (param->type == M_FLOAT) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((float *)(param->value))[k++] = ((float *)pf_value)[j];
								}
							}

						} else if (param->type == M_LONG) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((int *)(param->value))[k++] = ((int *)pf_value)[j];
								}
							}

						} else if (param->type == M_STRING) {
							k = 0;
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									*((char **)param->value + (k++)) = strdup (pf_value + j);
								}
							}
						}

            // convert "nmonths" to "nrain,nmonths"
					} else if (!strncmp(param->pf_dimNames[0], "nmonths", 7)
                                && !strncmp(param->dimen[0]->name, "nrain", 5) 
                                && !strncmp(param->dimen[1]->name, "nmonths", 7)) {

						if (param->type == M_DOUBLE) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((double *)(param->value))[j + (i*param->dimen[0]->value)] = ((double *)pf_value)[i];
								}
							}

						} else if (param->type == M_FLOAT) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((float *)(param->value))[j + (i*param->dimen[0]->value)] = ((float *)pf_value)[i];
								}
							}

						} else if (param->type == M_LONG) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((int *)(param->value))[j + (i*param->dimen[0]->value)] = ((int *)pf_value)[i];
								}
							}

						} else if (param->type == M_STRING) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									*((char **)param->value + (i*param->dimen[0]->value)) = strdup (pf_value + i);
								}
							}
						}

            // convert "nmonths" to "ntemp,nmonths"
					} else if (!strncmp(param->pf_dimNames[0], "nmonths", 7)
                                && !strncmp(param->dimen[0]->name, "ntemp", 5) 
                                && !strncmp(param->dimen[1]->name, "nmonths", 7)) {

						if (param->type == M_DOUBLE) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((double *)(param->value))[j + (i*param->dimen[0]->value)] = ((double *)pf_value)[i];
								}
							}

						} else if (param->type == M_FLOAT) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((float *)(param->value))[j + (i*param->dimen[0]->value)] = ((float *)pf_value)[i];
								}
							}

						} else if (param->type == M_LONG) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									((int *)(param->value))[j + (i*param->dimen[0]->value)] = ((int *)pf_value)[i];
								}
							}

						} else if (param->type == M_STRING) {
							for (i = 0; i < param->dimen[1]->value; i++) {
								for (j = 0; j < param->dimen[0]->value; j++) {
									*((char **)param->value + (i*param->dimen[0]->value)) = strdup (pf_value + i);
								}
							}
						}







                    }  // end of 1D to 2D conversion code
				}
			}

			if (buf_ptr) {
		        if (param_file != NULL) {
		           fclose (param_file);
		           param_file = NULL;
		        }
				return (buf_ptr);
			}

			// This function copies the parameter values from the param structure
			// to the arrays in the modules.
			updateparam (param->name);
		}
	}

    if (param_file != NULL) {
	   fclose (param_file);
	   param_file = NULL;
    }

	return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : READ_param_head
 | COMMENT		: Read the preliminary stuff for the parameter.  This is
 |                 the stuff between the ####s and where the data actually
 |                 starts.
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
static char *READ_param_head (PARAM **param_ptr, FILE **param_file, char *param_file_name, char line[], int map_flag) {
  static char *key = NULL;
  static char *dimen = NULL;
  static char buf[256];
  char *temp, *npos, *tempfmt;
  int tempwidth, i, param_size, type;

  int badFlag;
  
  if (key == NULL) {
	  key = (char *) umalloc(max_data_ln_len * sizeof(char));
  }

   if (dimen == NULL) {
	  dimen = (char *) umalloc(max_data_ln_len * sizeof(char));
  }


/*
* space fwd to #### header
*/
  while (strncmp (line, "####", 4))
    if (!fgets (line, max_data_ln_len, *param_file)) {
		if (*param_file != NULL) {
		   fclose (*param_file);
		   *param_file = NULL;
		}
      return ((char *)-1);
    }
  
/*
* get key, column width and format
*/
  if (fgets (line, max_data_ln_len, *param_file) == NULL) {
	  (void)sprintf (buf, "\nERROR: Early end of Parameter File: %s", param_file_name);
    return (buf);
  }

/*
**	get the key
*/
  temp = (char *)strtok(line," ");


  npos = strchr(temp,'\n');
  if (npos) *npos = '\0';

  (void)strcpy(key,temp);
  key[strlen(temp)] = '\0';
	
/*
**	get the column width
*/
  temp = (char *)strtok(NULL," ");
  if (temp) {
    tempwidth = atoi(temp);
  } else {
    tempwidth = 0;
  }

/*
**	get the format
*/
  tempfmt = (char *)strtok(NULL," ");

/*
** markstro -- this check is added so that if there is just a space
**             after the width the parameter will not have a blank
**             format.
*/
  if (tempfmt && (strlen (tempfmt) < 2)) {
     tempfmt = NULL;
  }

/*
**  param is allocated by calls from the modules to declparam.
*/
	if (!map_flag) {
		*param_ptr = param_addr(key);

	} else {
		if (!strncmp (key, mapParamNames[0], strlen(key)) ||
			!strncmp (key, mapParamNames[1], strlen(key)) ||
			!strncmp (key, mapParamNames[2], strlen(key)) ||
			!strncmp (key, mapParamNames[3], strlen(key)) ||
			!strncmp (key, mapParamNames[4], strlen(key)) ||
			!strncmp (key, mapParamNames[5], strlen(key)) ||
			!strncmp (key, mapParamNames[6], strlen(key))) {
			*param_ptr = param_addr (key);

			if (*param_ptr == NULL) {  // Didn't find this mapping parameter in the parameter DB so declare one
				declparam ("read_params", key, NULL, "integer", NULL, NULL, NULL, NULL, NULL, NULL);
				*param_ptr = param_addr(key);
			}

		} else {
			*param_ptr = NULL;
		}
	}

	if (*param_ptr) {
	  /*
	  **  Set the read_in flag to true
	  */
		(*param_ptr)->read_in = 1;
/*
* save format and column width
*/
		(*param_ptr)->column_width = tempwidth;
		if (tempfmt) {
			tempfmt[strlen(tempfmt)-1] = '\0';
			if(!(*param_ptr)->format) {
				(*param_ptr)->format = (char *)(malloc(strlen(tempfmt)+1));
			} else {
				(*param_ptr)->format = (char *)(realloc((*param_ptr)->format, strlen(tempfmt) + 1));
			}   
			(void)strcpy((*param_ptr)->format, tempfmt);
		} else {
			(*param_ptr)->format = NULL;
		}
/*
* get number of dimensions
*/
		if(fgets(line, max_data_ln_len, *param_file) == NULL) {
			(void)sprintf (buf,"ERROR: reading param number of dimensions for %s in Parameter File %s", key, param_file_name);
			return buf;
		}

		if (isdigit(*line)) {
			//if ((*param_ptr)->ndimen != atol(line)) {
			//	sprintf (buf, "\nERROR: number of dimensions for parameter %s doesn't match parameter declaration.\nParameter File: %s\n", key, param_file_name);
			//	return buf;
			//}

			(*param_ptr)->pf_ndimen = atol(line);

			//if((*param_ptr)->ndimen == 0) {
			//	(void)sprintf (buf, "\nERROR: number of dimensions is 0 for %s in Parameter File %s", key, param_file_name);
			//	return (buf);
			//}

			if((*param_ptr)->pf_ndimen == 0) {
				(void)sprintf (buf, "\nERROR: number of dimensions is 0 for %s in Parameter File %s", key, param_file_name);
				return (buf);
			}
/*
* get number of dimensions if file format supports 2D arrays. Otherwise
* get dimension name.
*/
/*			for (i = 0; i < (*param_ptr)->ndimen; i++) {
				if(fgets(dimen, max_data_ln_len, *param_file) == NULL) {
					(void)sprintf (buf, "\nERROR: number of dimensions is wrong for %s in Parameter File %s", key, param_file_name);
					return (buf);
				}

				dimen[strlen(dimen) - 1] = '\0';
				if (strcmp(dimen, (*param_ptr)->dimen[i]->name)) {
					(void)sprintf (buf, "\nERROR: dimension specification is wrong for %s in Parameter File %s", key, param_file_name);
					return (buf);
				}
			}*/ /* i */

			(*param_ptr)->pf_dimNames = (char **)malloc ((*param_ptr)->pf_ndimen * sizeof (char *));

			for (i = 0; i < (*param_ptr)->pf_ndimen; i++) {
				if(fgets(dimen, max_data_ln_len, *param_file) == NULL) {
					(void)sprintf (buf, "\nERROR: number of dimensions is wrong for %s in Parameter File %s", key, param_file_name);
					return (buf);
				}

				dimen[strlen(dimen) - 1] = '\0';
				(*param_ptr)->pf_dimNames[i] = strdup(dimen);
				//if (strcmp(dimen, (*param_ptr)->dimen[i]->name)) {
				//	(void)sprintf (buf, "\nERROR: dimension specification is wrong for %s in Parameter File %s", key, param_file_name);
				//	return (buf);
				//}
			}

			if (map_flag) { // Need to set some values in the param structure for mapping parameter
				(*param_ptr)->ndimen = 1;
				(*param_ptr)->dimen = (DIMEN **)umalloc ((*param_ptr)->ndimen * sizeof (DIMEN *));
				(*param_ptr)->dimen[0] = dim_addr((*param_ptr)->pf_dimNames[0]);
			}

			badFlag = checkForValidDimensions (*param_ptr);  // 0 = good;  1 = bad

			if (badFlag) {
				(void)sprintf (buf, "ERROR: dimensions for %s in Parameter File %s are incompatable with declaration in module", key, param_file_name);
				return (buf);
			}

			(*param_ptr)->pf_size = getParamFileParamSize(*param_ptr);

			if (map_flag) { // Need to set some values in the param structure for mapping parameter
				(*param_ptr)->size = (*param_ptr)->pf_size;
				(*param_ptr)->value = (char *)umalloc ((*param_ptr)->size * sizeof (int)); // Mapping parameters are always integers
			}
/*
* get param size
*/
			fgets(line, max_data_ln_len, *param_file);
			if(line == NULL) {
				(void)sprintf (buf, "ERROR: incorrect parameter size for %s in Parameter File %s", key, param_file_name);
				return (buf);
			}

			if((param_size = atol(line)) == 0) {
				(void)sprintf (buf, "\nERROR: incorrect parameter size for %s in Parameter File %s", key, param_file_name);
				return (buf);
			}

			//if(param_size != (*param_ptr)->size) {
			if(param_size != (*param_ptr)->pf_size) {
				(void)sprintf (buf, "\nERROR: incorrect parameter size for %s in Parameter File %s", key, param_file_name);
				return (buf);
			}

		} else {  //  number of dimensions not a digit
			(*param_ptr)->ndimen = 1;
			strncpy(dimen, line, strlen(line));
			dimen[strlen(line)-1] = '\0';

			if (strcmp(dimen, (*param_ptr)->dimen[0]->name)) {
				(void)sprintf (buf, "\nERROR: incorrect dimension specified for parameter %s in Parameter File %s",
				  key, param_file_name);
				return (buf);
			}
			(*param_ptr)->size = getdim(dimen);
			param_size = (*param_ptr)->size;
		}
/*
* get type
*/
		fgets(line, max_data_ln_len, *param_file);
		if(line == NULL) {
			(void)sprintf (buf, "\nERROR: incorrect data type specified for parameter %s in Parameter File %s", key, param_file_name);
			return (buf);
		}

		if((type = atol(line)) == 0) {
			sprintf (buf, "\nERROR: incorrect data type specified for parameter %s in Parameter File %s", key, param_file_name);
			return (buf);
		}

		if(type != (*param_ptr)->type) {
			sprintf (buf, "\nERROR: incorrect data type specified for parameter %s in Parameter File %s", key, param_file_name);
			return (buf);
		}
  
	} else {
		if (!map_flag) {
			(void)printf ("\nWARNING: parameter %s is ignored as it is not required.\n", key);
			(void)printf ("         Read from Parameter File: %s\n", param_file_name);
		}
	}

	return (NULL);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : READ_param_values
 | COMMENT		: Read the values and comments for the parameter.
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
static char *READ_param_values (long size, long type, char *name, char *value,
								FILE *param_file, char *line) {
	int i, j;
//	char *nch;
//	int l1, l2
    int  done;
	int	desc_count = 0;
	int repeat_count;
	char delims[] = ",";
	char *result = NULL;
	char *comp_ptr = NULL;
	static char *crap = NULL;
	static char *crap2 = NULL;
	static char buf[256];
	float foo;
	double d;
	char *endp;
	long l;
	
	if (crap == NULL) {
		crap = (char *) umalloc(max_data_ln_len * sizeof(char));
	}

	if (crap2 == NULL) {
		crap2 = (char *) umalloc(max_data_ln_len * sizeof(char));
	}

/*
**  Space for the values and value_desc are allocated in declparam
*/
	done = FALSE;
	i = 0;
	while (!done) {
		fgets (line, max_data_ln_len, param_file);
		if (!line) {
			done = TRUE;

		} else if (!strncmp (line, "####", 4)) {
			done = TRUE;

		//} else if (!param) {
		//	;

		//} else if (line[0] == '@') {
		//	i--;

		//	nch = (char *)strchr (line, '\n');
		//	if (nch) *nch = '\0';

		//	if (desc_count) {
		//		if (param->value_desc[i]) {
		//			l1 = strlen (param->value_desc[i]);
		//			l2 = strlen (line);
		//			param->value_desc[i] = (char *)realloc
		//			    (param->value_desc[i],
		//			    (l1 + l2 + 2) * sizeof (char));
		//			strcat (param->value_desc[i], "\n");
		//			strcat (param->value_desc[i], &(line[1]));
		//		} else {
		//			param->value_desc[i] = strdup (&(line[1]));
		//		}
		//	} else {

		//		param->value_desc[i] = strdup (&(line[1]));
		//	}
		//	i++;

		} else {
			desc_count = 0;
			result = NULL;
			//printf ("READ_param_values: line is %s\n", line);
			strncpy (crap, line, max_data_ln_len);
			//printf ("crap is %s\n", crap);

			result = strtok (crap, delims);
			while (result != NULL && !done) {
				//printf ("   READ_param_values: result is |%s|\n", result);

				strncpy (crap2, result, max_data_ln_len);
				//printf ("crap2 is %s\n", crap2);
				comp_ptr = strchr (crap2, '*');
				//printf ("comp_ptr is %s\n", comp_ptr);
				if (comp_ptr == NULL){
					repeat_count = 1;
					comp_ptr = crap2;
					//printf ("comp_ptr is %s\n", comp_ptr);
				} else {
					*comp_ptr = '\0';
					repeat_count = atol(crap2);
					comp_ptr++;
					//printf ("comp_ptr is %s\n", comp_ptr);
					foo = (float) atof(comp_ptr);
				}

				for (j = 0; j < repeat_count && !done; j++) {
					if (i < size) {
						switch (type) {

							case M_STRING:
                                comp_ptr[strlen(comp_ptr)-1] = '\0';
                                //*((char **)param->value + i) = strdup (comp_ptr);
								*((char **)value + i) = strdup (comp_ptr);
                                i++;

								//if (comp_ptr != endp && *endp == '\n') {

								//} else {
								//	sprintf (buf, "There is a parameter format error. Parameter name: %s Index = %d\n   The data type should be a character string or there could be white spaces after the values on the line.", param->name, (i+1));
									//printf ("%s", buf);
								//	return (buf);
								//}

								//((double *)(param->value))[i++] = atof(comp_ptr);
								break;

							case M_DOUBLE:
								d = strtod(comp_ptr, &endp);
								if (comp_ptr != endp && *endp == '\n') {
									//((double *)(param->value))[i++] = d;
									((double *)value)[i++] = d;
								} else {
									sprintf (buf, "\nERROR: parameter format error. Parameter name: %s Index = %d\n   The data type should be a double precision float or there could be white spaces after the values on the line.", name, (i+1));
									return (buf);
								}
								break;

							case M_FLOAT:
								d = strtod(comp_ptr, &endp);
								if (comp_ptr != endp && *endp == '\n') {
									//((float *)(param->value))[i++] = (float)d;
									((float *)value)[i++] = (float)d;
								} else {
									sprintf (buf, "\nERROR: parameter format error. Parameter name: %s Index = %d\n   The data type should be a float or there could be white spaces after the values on the line.", name, (i+1));
									return (buf);
								}
								break;

							case M_LONG:
								l = strtol(comp_ptr, &endp, 0);
								if (comp_ptr != endp && *endp == '\n') {
									//((int *)(param->value))[i++] = (int)l;
									((int *)value)[i++] = (int)l;
								} else {
									sprintf (buf, "\nERROR: parameter format error. Parameter name: %s Index = %d\n   The data type should be an integer or there could be white spaces after the values on the line.", name, (i+1));
									return (buf);
								}
								break;
						} // switch
				 
					} else { // if (i < size)
						done = TRUE;
						i++;
					} // if (i < size)
				}
				result = strtok(NULL, delims);
			} // while
		}
	}

	if (i < size) {
		sprintf (buf, "\nERROR: too few values read for paramter %s in Parameter File", name);
		return (buf);
	} else if (i > size && !done) {
		sprintf (buf, "\nERROR: too many values read for paramter %s in Parameter File", name);
		return (buf);
	}
	return (NULL);
}

// returns
// 0 = good;  1 = bad
static int checkForValidDimensions (PARAM *param_ptr) {
	int i, badFlag;

//	printf ("checkForValidDimensions name = %s\n", param_ptr->name);
//	printf ("   pf_ndimen = %d; module_ndimen = %d\n", (int)(param_ptr->pf_ndimen), (int)(param_ptr->ndimen));

	if (param_ptr->pf_ndimen > param_ptr->ndimen ) { // more dimensions in the parameter file is always invalid
		return 1;

	} else if (param_ptr->pf_ndimen == param_ptr->ndimen ) {
		for (i = 0; i < param_ptr->pf_ndimen; i++) {  // check each dimension for compatiblilty
//printf ("   1 comparing %s to %s\n", param_ptr->pf_dimNames[i], param_ptr->dimen[i]->name);
			badFlag = isDimensionIncompatable (param_ptr->pf_dimNames[i], param_ptr->dimen[i]->name); // 0 = good;  1 = bad
		}
		if (badFlag == 1) {
			return 1;
		}

	} else { // less dimensions in the parameter file than declared in the module.
		badFlag = 1;
//printf ("   2 parameter file has %d dimensions\n", param_ptr->pf_ndimen);
		for (i = 0; i < param_ptr->ndimen; i++) {  // check each dimension for compatiblilty; only need to find one that is compatable
//printf ("   2 comparing %s to %s\n", param_ptr->pf_dimNames[0], param_ptr->dimen[i]->name);
			if (badFlag == 1) {
				badFlag = isDimensionIncompatable (param_ptr->pf_dimNames[0], param_ptr->dimen[i]->name); // 0 = good;  1 = bad
			}
		}
		if (badFlag == 1) {
			return 1;
		}
	}

	//param_ptr->ndimen
	return 0;
}

// returns
// 0 = good;  1 = bad
static int isDimensionIncompatable (char *pfDimName, char *modDimName) {
//	char *dimNames[] ={"one",
//		"ncascade",
//		"ncascdgw",
//		"nsegment",
//		"npoigages",
//		"nsub",
//		"nhrucell",
//		"ngw",
//		"nhru",
//		"nssr",
//		"nsfres",
//		"nlake",
//		"nrain",
//		"nsol",
//		"ntemp",
//		"nratetbl",
//		"nwateruse",
//		"ndepl",
//		"ndeplval",
//		"ndays",
//		"nmonths",
//		"nlapse",
//		"nobs",
//		"nsnow",
//		"nform",
//		"nevap",
//		"nsfelev",
//		"nlakeelev",
//		"nwind",
//		"nhumid",
//		"ngate",
//		"nstage",
//		"ngate2",
//		"nstage2",
//		"ngate3",
//		"nstage3",
//		"ngate4",
//		"nstage4",
//		"mxnsos",
//	};

	if (!strncmp (pfDimName, modDimName, 10)) {  // a dimension is compatable with itself
		return 0; 
	}

	if (!strncmp (pfDimName, "one", 3)) {  // "one" in the parameter file is compatable with everything
		return 0; 
	}

	// Subbasin (nsub) can be mapped to these dimensions with mapping parameter
	// "nhru" "hru_subbasin";
    // "nsegment" "segment_subbasin";
    // "nrain" "rain_subbasin";
    // "ntemp" "temp_subbasin";
    // "nobs"  "obs_subbasin";
    // "ngw" "gw_subbasin";
    // "nssr" "ssr_subbasin";
	if (!strncmp (pfDimName, "nsub", 4)) {
		if (!strncmp (modDimName, "nhru", 4)) {
			return 0;

		} else if (!strncmp (modDimName, "nsegment", 8)) {
			return 0;

		} else if (!strncmp (modDimName, "nrain", 5)) {
			return 0;

		} else if (!strncmp (modDimName, "ntemp", 5)) {
			return 0;

		} else if (!strncmp (modDimName, "nobs", 4)) {
			return 0;

		} else if (!strncmp (modDimName, "ngw", 3)) {
			return 0;

		} else if (!strncmp (modDimName, "nssr", 4)) {
			return 0;
		}
	}
	

	return 1;
}

static int getParamFileParamSize (PARAM *param) {
	int i, size;

	size = 1;
	for (i = 0; i < param->pf_ndimen; i++) {  // check each dimension for size
		size = size * getdim(param->pf_dimNames[i]);
	}
	return size;
}

static void oneToAnySizedArray(PARAM *param, char *pf_value) {
	int i;

	if (param->type == M_DOUBLE) {
		for (i = 0; i < param->size; i++) {
			((double *)(param->value))[i] = *((double *)pf_value);
		}
	} else if (param->type == M_FLOAT) {
		for (i = 0; i < param->size; i++) {
			((float *)(param->value))[i] = *((float *)pf_value);
		}
	} else if (param->type == M_LONG) {
		for (i = 0; i < param->size; i++) {
			((int *)(param->value))[i] = *((int *)pf_value);
		}
	} else if (param->type == M_STRING) {
		for (i = 0; i < param->size; i++) {
			*((char **)param->value + i) = strdup (pf_value);
		}
	}
}

static char *getMapParamName(char *name) {
	char *mapParamName;
	int i;

	mapParamName = NULL;
	for (i = 0; i < (sizeof (dimNames) / sizeof (dimNames[0])); i++) {
		if (!strncmp (name, dimNames[i], strlen(dimNames[i]))) {
			mapParamName = mapParamNames[i];
		} 
	}

    return mapParamName;
}

static void subbasinTo1DArray (PARAM *param, PARAM *mapping_param, char *pf_value) {
	int i, map;

	if (param->type == M_DOUBLE) {
		for (i = 0; i < param->size; i++) {
			map = ((int *)(mapping_param->value))[i];
			((double *)(param->value))[i] = ((double *)pf_value)[map - 1];
		}

	} else if (param->type == M_FLOAT) {
		for (i = 0; i < param->size; i++) {
			map = ((int *)(mapping_param->value))[i];
			((float *)(param->value))[i] = ((float *)pf_value)[map - 1];
		}

	} else if (param->type == M_LONG) {
		for (i = 0; i < param->size; i++) {
			map = ((int *)(mapping_param->value))[i];
			((int *)(param->value))[i] = ((int *)pf_value)[map - 1];
		}

	} else if (param->type == M_STRING) {
		for (i = 0; i < param->size; i++) {
			map = ((int *)(mapping_param->value))[i];
//			*((char **)param->value + i) = strdup (*pf_value + map - 1);
			*((char **)param->value + i) = strdup (pf_value + map - 1);
		}
	}
}

/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : read_vars
 * COMMENT  : reads the vars data base from a file.
 *            File name is passed in as an argument
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define READ_VARS_C
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include "mms.h"

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/
static int read_var_line (char *, char *, FILE *, char *);

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : read_vars
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int read_vars (char *var_file_name) {

	FILE *var_file;
	PUBVAR *var;
	DIMEN *dim;
	long dim_size, var_size, type, i;
	double *dvalptr;
	float *fvalptr;
	long *lvalptr;
	char line[MAXVARLEN], key[MAXVARLEN];
	char dimen[MAXVARLEN];
	char *pathname;
	char *endptr;

/*
* get var name, open file
*/
      pathname = strdup (var_file_name);

   if ((var_file = fopen (pathname, "r")) == NULL) {
      (void)fprintf(stderr, "WARNING - read_vars - cannot open file '%s'\n",
                    pathname);
      return(0);
   }

/*
* read in run info string
*/
   if (fgets(line, MAXVARLEN, var_file) == NULL) {
      fclose(var_file);
      return(0);
   }
   Mparaminfo = strdup (line);

/*
* read in last nstep
*/
   if (fgets(line, MAXVARLEN, var_file) == NULL) {
      fclose(var_file);
      return(0);
   }

   Mnsteps = strtol(&(line[11]), &endptr, 10);

/*
* read in last time step
*/
   if (fgets(line, MAXVARLEN, var_file) == NULL) {
      fclose(var_file);
      return(0);
   }

/*
   (void)fprintf (stderr,"read_vars Mnowtime->jt stirng = %s\n", &(line[17]));
   Mnowtime->jt = strtod(&(line[17]), &endptr);
   (void)fprintf (stderr,"read_vars Mnowtime->jt = %d\n", Mnowtime->jt);
*/

/*
* read in last delta time
*/
   if (fgets(line, MAXVARLEN, var_file) == NULL) {
      fclose(var_file);
      return(0);
   }

   Mdeltat = strtod(&(line[16]), &endptr);
   Mdeltanext = strtod(&(line[16]), &endptr);

/*
* read in dimensions
*/
   while (!feof(var_file)) {

/*
* space fwd to #### header
*/
   (void)strcpy(line, " ");
   while (strncmp(line, "####", 4)) {
      if (fgets(line, MAXVARLEN, var_file) == NULL) {
         fclose(var_file);
         return(0);
      }

/*
* break if variable list starts
*/
      if(!strncmp(line, "** Variables **", strlen("** Variables **")))
         goto variables;
      }

/*
* get dimen name
*/

      if(fgets(key, MAXVARLEN, var_file) == NULL) {
         (void)fprintf(stderr, "ERROR - read_var, reading dimen name.\n");
         (void)fprintf(stderr, "Early end-of-file, file '%s'\n", var_file_name);
         return(1);
      }
      key[strlen(key)-1] = '\0';

      if ((dim = dim_addr(key)) == NULL) {
         (void)fprintf(stderr, "WARNING - read_vars.\n");
         (void)fprintf(stderr, "Using var file '%s'\n", pathname);
         (void)fprintf(stderr, "Dimension '%s' not declared.\n", key);
         (void)fprintf(stderr, "Variables not read from file.\n");
         fclose(var_file);
         return(0);
      } else {

/*
* get dimen size
*/
         if(fgets(line, MAXVARLEN, var_file) == NULL) {
            (void)fprintf(stderr, "ERROR - read_var, reading dimen size.\n");
            fprintf(stderr,"Early end-of-file, file '%s'\n",var_file_name);
            return(1);
         }

         errno = 0;
         dim_size = strtol(line, &endptr, 10);
         if(errno != 0) {
            (void)fprintf(stderr,
                        "ERROR - read_var, decoding size from '%s'.\n", line);
            (void)fprintf(stderr, "Var file '%s'.\n", var_file_name);
            perror(" ");
            return(1);
         }
/*
* check dimension size
*/
         if (dim->value != dim_size) {
            (void)fprintf(stderr, "WARNING - read_vars.\n");
            (void)fprintf(stderr, "Using var file '%s'\n", pathname);
            (void)fprintf(stderr, "Dimension '%s' has size %ld.\n", key, dim->value);
            (void)fprintf(stderr, "Size in var file is %ld.\n", dim_size);
            (void)fprintf(stderr, "Variables not read from file.\n");
            fclose(var_file);
            return(0);
         }
      }
   } /* while */

/*
* read in variables
*/

variables:
   while (!feof(var_file)) {

/*
* space fwd to #### header
*/
      (void)strcpy(line, " ");
      while (strncmp(line, "####", 4)) {
         if (fgets(line, MAXVARLEN, var_file) == NULL) {
            fclose(var_file);
            return(0);
         }
      }

/*
* get key
*/
      if(fgets(key, MAXVARLEN, var_file) == NULL) {
         (void)fprintf(stderr, "ERROR - read_var, reading var key.\n");
         (void)fprintf(stderr, "Early end-of-file, file '%s'\n", var_file_name);
         return(1);
      }
      key[strlen(key) - 1] = '\0';

      if ((var = var_addr(key)) != NULL) {
/*
* get number of dimensions
*/
         if(fgets(line, MAXVARLEN, var_file) == NULL) {
            (void)fprintf(stderr, "ERROR - read_var, reading var ndimen.\n");
            fprintf(stderr, "Early end-of-file, file '%s'\n", var_file_name);
            return(1);
         }

         if((var->ndimen = atol(line)) == 0) {
            (void)fprintf(stderr,
                  "ERROR - read_var, decoding var ndimen from '%s'.\n", line);
            (void)fprintf(stderr, "Key is '%s'\n", key);
            (void)fprintf(stderr, "Var file '%s'.\n", var_file_name);
            return(1);
         }

/*
* get dimens
*/

         for (i = 0; i < var->ndimen; i++) {
            if(fgets(dimen, MAXVARLEN, var_file) == NULL) {
               (void)fprintf(stderr, "ERROR - read_var, reading var dimen.\n");
               (void)fprintf(stderr, "Early end-of-file, file '%s'\n", var_file_name);
               return(1);
            }
            dimen[strlen(dimen) - 1] = '\0';

            if (strcmp(dimen, "PRIVATE")) {
               if (strcmp(dimen, var->dimen[i]->name)) {
                  (void)fprintf(stderr, "ERROR - read_var, reading var dimen.\n");
                  (void)fprintf(stderr, "Expecting dimension '%s'\n", var->dimen[i]->name);
                  (void)fprintf(stderr, "Read dimension '%s'\n", dimen);
                  (void)fprintf(stderr, "Key is '%s'\n", key);
                  (void)fprintf(stderr, "File '%s'\n", var_file_name);
                  return(1);
               }
            }
         } /* i */

/*
* get var size
*/

         if(fgets(line, MAXVARLEN, var_file) == NULL) {
            (void)fprintf(stderr, "ERROR - read_var, reading var size.\n");
            (void)fprintf(stderr, "Early end-of-file, file '%s'\n", var_file_name);
            return(1);
         }

         errno = 0;
         var_size = strtol(line, &endptr, 10);
         if(errno != 0) {
            (void)fprintf(stderr,
                     "ERROR - read_var, decoding var size from '%s'.\n", line);
            (void)fprintf(stderr, "Key is '%s'\n", key);
            (void)fprintf(stderr, "Var file '%s'.\n", var_file_name);
            return(1);
         }
         if(var_size != var->size) {
            (void)fprintf(stderr, "ERROR - read_var, size incorrect.\n");
            (void)fprintf(stderr, "Key is '%s'\n", key);
            (void)fprintf(stderr, "Var file '%s'.\n", var_file_name);
            return(1);
         }

/*
* get type
*/
         if(fgets(line, MAXVARLEN, var_file) == NULL) {
            (void)fprintf(stderr, "ERROR - read_var, reading var type.\n");
            (void)fprintf(stderr, "Early end-of-file, file '%s'\n", var_file_name);
            return(1);
         }
         if((type = atol(line)) == 0) {
            (void)fprintf(stderr,
                  "ERROR - read_var, decoding var type from '%s'.\n", line);
            (void)fprintf(stderr, "Key is '%s'\n", key);
            (void)fprintf(stderr, "Var file '%s'.\n", var_file_name);
            return(1);
         }
         if(type != var->type) {
            (void)fprintf(stderr, "ERROR - read_var, type incorrect.\n");
            (void)fprintf(stderr, "Key is '%s'\n", key);
            (void)fprintf(stderr, "Var file '%s'.\n", var_file_name);
            return(1);
         }

/*
* read in and store the file data
*/

         switch (type) {
            case M_DOUBLE:
               dvalptr = (double *) var->value;
               for (i = 0; i < var_size; i++) {
                  if(read_var_line(key, line, var_file, var_file_name))
                     return(1);
                  dvalptr[i] = atof(line);
               }
               break;

            case M_FLOAT:
               fvalptr = (float *) var->value;
               for (i = 0; i < var_size; i++) {
                  if(read_var_line(key, line, var_file, var_file_name))
                     return(1);
                  fvalptr[i] = (float) atof(line);
               }
               break;

         case M_LONG:
            lvalptr = (long *) var->value;
            for (i = 0; i < var_size; i++) {
               if(read_var_line(key, line, var_file, var_file_name))
                  return(1);
               lvalptr[i] =  atol(line);
            }
            break;
         }
      } /* if (var ... */

   } /* while */

   fclose(var_file);

   return(0);
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : read_var_line
 | COMMENT		: gets a line from the variable file
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
static int read_var_line (char *key, char *line, FILE *var_file, char *var_file_name) {

	if (fgets(line, MAXVARLEN, var_file) == NULL) {
		(void)fprintf(stderr,
		    "ERROR - read_var, reading data.\n");
		(void)fprintf(stderr,
		    "Early end-of-file, file '%s'\n", var_file_name);
		(void)fprintf(stderr, "Key is '%s'\n", key);
		return(1);
	}

	return(0);

}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : readvar() to be called from C
 *            readvar_() to be called from Fortran
 *            returns 0 if success, 1 if failure
 * COMMENT  : reads the values associated with a key from an input file,
 *            and stores it in the data base
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define READVAR_C
#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <string.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/
#define MISSING_VAR -999

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : readvar_
 | COMMENT		: called from Fortran, sorts out args and calls readvar()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long readvar_ (char *mname, char *vname, ftnlen mnamelen, ftnlen vnamelen) {
	char module[80], name[80];
	long retval;

/*
* copy args to new strings, and terminate
*/
	strncpy (module, mname, mnamelen);
	*(module + mnamelen) = '\0';

	strncpy (name, vname, vnamelen);
	*(name + vnamelen) = '\0';

/*
* call C version of readvar()
*/
	retval = readvar (module, name);

	return (retval);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : readvar
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long readvar (char *module, char *name) {

	PUBVAR *var;
	long i, found;
	char vkey[80];
	long *long_var;
	float *float_var;
	double *double_var;

/*
* compute the key
*/

/*
  vkey = (char *) umalloc(strlen(module) + strlen(name) + 2);
  (void)strcpy(vkey, module);
  strcat(strcat(vkey, "."), name);
*/
	strcpy (vkey, name);

/*
* get pointer to variable with key
*/
	if (!(var = var_addr (vkey))) {
		(void)fprintf(stderr, "ERROR - readvar - variable not found.\n");
		(void)fprintf(stderr, "Key:   %s.\n", vkey);
		return(1);
	}

/*
* check that this is the correct variable, and that the size is
* set to the expected read count
*/
	found = -1;
	for (i = 0; i < Mnreads; i++) {
		if (var == Mcheckbase[i]->var) {
			found = i;
			break;
		}
	}

/*
	if (!(var->size))
		return (0);
*/

	if (found == -1) {
		(void)fprintf(stderr, "\nERROR: Attempting to read variable %s, which is not in Data File\n", vkey);
		return (1);
	}

/*
* data is present in file
*/

  if(var->size != Mcheckbase[found]->count) {
    (void)fprintf(stderr, "ERROR - readvar\n");
    (void)fprintf(stderr, "Reading var '%s'\n", vkey);
    (void)fprintf(stderr, "Attempting to read %ld items\n", var->size);
    (void)fprintf(stderr, "Data file has %ld items for this variable.\n",
	    Mcheckbase[found]->count);
    return(1);

  }
    
  /*
   * copy the variable from the input line into the data base,
   * according to the type, if size > 0
   */

  if (var->size > 0) {
  
    switch (var->type) {
      
    case M_LONG:
      long_var = (long *) var->value;
      for (i = 0; i < var->size; i++) {
	long_var[i] = Mcheckbase[found]->Types.valuel[i];
      }
      break;
      
    case M_FLOAT:
      float_var = (float *) var->value;
      for (i = 0; i < var->size; i++) {
	float_var[i] = Mcheckbase[found]->Types.valuef[i];
      }
      break;
      
    case M_DOUBLE:
      double_var = (double *) var->value;
      for (i = 0; i < var->size; i++) {
	double_var[i] = Mcheckbase[found]->Types.valued[i];
      }
      break;
      
    }
    

  }  /* if(var->size > 0) */


  return(0);
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : reset_dim
 * COMMENT  :
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define RESET_DIM_C 0
#define VALUE_CASE 0
#define MIN_CASE 1
#define MAX_CASE 2
#define NCASES 3

#include <stdio.h>
#include <stdlib.h>
#include "mms.h"

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/
static void resize_param (PARAM *, long, long, long, long);

/*--------------------------------------------------------------------*\
 | FUNCTION     : reset_dim
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void reset_dim (DIMEN *dim, long nnew) {
	int dimen_used;
	long iparam, ivar, idimen, i, j;
	long size_new, nold;
	long dimen_num;

	long *lptr_max, *lptr_value;

	PARAM *param;
	PUBVAR *var;

	if (nnew == dim->value) return;

/*
**	reset the dimension
*/
	nold = dim->value;
	dim->value = nnew;

/*
**	if existing entry had any index names, free the excess ones, if new size is
**	smaller than previous size. Otherwise, fill the new ones with null strings.
*/

	if (nnew > nold) {
		if (dim->names) {
			dim->names = (char **)realloc ((char **)dim->names,
				nnew * (sizeof(char *)));
			for (i = nold ; i < nnew; i++)
				dim->names[i] = NULL;
		}

		if (dim->notes) {
			dim->notes = (char **)realloc ((char **)dim->notes,
				nnew * (sizeof(char *)));
			for (i = nold ; i < nnew; i++)
				dim->notes[i] = NULL;
		}	

	} else {
		for (i = nnew + 1; i < nold; i++) {
			if (dim->names && dim->names[i]) {
				dim->names[i] = NULL;
			}

			if (dim->notes && dim->notes[i]) {
				dim->notes[i] = NULL;
			}
		}

		if (nnew) {
			if (dim->names)
				dim->names = (char **)realloc ((char **)dim->names,
					nnew * sizeof (char *));
			if (dim->notes)
				dim->notes = (char **)realloc ((char **)dim->notes,
					nnew * sizeof (char *));
		} else {
			dim->names = NULL;
			dim->notes = NULL;
		}
	}

/*
* search through params for parameters which use this dimension
* and resize
*/

	for (iparam = 0; iparam < Mnparams; iparam++) {
		param = Mparambase[iparam];
		dimen_used = FALSE;
		size_new = 1;
		dimen_num = 1;

		for (idimen = 0; idimen < param->ndimen; idimen++) {
			size_new *= param->dimen[idimen]->value;
			if (dim == param->dimen[idimen]) {
				dimen_num = idimen;
				dimen_used = TRUE;
			}
		}
/*
* if this dimension is used by this parameter, resize the parameter
* array
*/
		if (dimen_used) {

/*
* if size_new is zero, set size_new to 1 so that there is at least one
* entry in the parameters data base. This is necesary so that the
* default, maximum and minimum values will be retained for use when
* the size is set to a non-zero value
*/
			if (size_new == 0)
				size_new = 1;

			resize_param (param, dimen_num, nold, nnew, size_new);
			param->size = size_new;
		}
	}

/*
* if a param is bounded by this dimension,
* reset the maximum values accordingly, and the set the current
* values to the maximum if they exceed it
*/
	for (iparam = 0; iparam < Mnparams; iparam++) {
		param = Mparambase[iparam];
		if((param->bound_status == M_BOUNDED) &&
		 					(param->bound_dimen == dim)) {
/*
 (void)fprintf (stderr,"check bound max for %s\n", param->name);
 (void)fprintf (stderr,"   dim = %s;   bound dim = %s\n", dim->name, param->bound_dimen->name);
*/
			lptr_value = (long *) param->value;
			lptr_max = (long *) param->max;

			for (j = 0; j < param->size; j++) {
				lptr_max[j] = dim->value;
/*
 (void)fprintf (stderr,"   j = %d\n", j);
*/
				if (lptr_value[j] > lptr_max[j])
					lptr_value[j] = lptr_max[j];
			}
		}
	}

/*
* search through vars for variables which use this dimension
* and reset size
*/
   for (ivar = 0; ivar < Mnvars; ivar++) {
      var = Mvarbase[ivar];
      if (!(var->private)) {
         dimen_used = FALSE;
         size_new = 1;

         for (idimen = 0; idimen < var->ndimen; idimen++) {
            size_new *= var->dimen[idimen]->value;
            if (dim == var->dimen[idimen]) {
               dimen_num = idimen;
               dimen_used = TRUE;
            }
         }
/*
* if this dimension is used by this variable, resize the variable
*/
         if (dimen_used) {
            var->size = size_new;
         }
      }
   }
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : resize_param
 | COMMENT		: resizes and repacks param array to take account of
 |                  a change in the value of a dimension
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
static void resize_param (PARAM *param, long dimen_num, long nold, long nnew, long size_new) {

	char *aptr_prev, *aptr_new;

	long i, j, iframe, inew, iold, icase;
	long nframes;
	long blocksize;
	long new_framesize, old_framesize;
	long new_index, old_index;
	long *lptr_prev, *lptr_new;

	float *fptr_prev, *fptr_new;

	double *dptr_prev, *dptr_new;

/*
* compute the number of frames
*/

	nframes = 1;

	for (i = dimen_num + 1; i < param->ndimen; i++)
		nframes *= param->dimen[i]->value;

/*
* compute the block size
*/

	blocksize = 1;

	for (i = 0; i < dimen_num; i++)
		blocksize *= param->dimen[i]->value;

/*
* compute the old and new frame sizes
*/

	old_framesize = blocksize * nold;
	new_framesize = blocksize * nnew;

/*
**	resize the value_desc
*/
//	if (size_new)
//		param->value_desc = (char **) realloc (param->value_desc,
//			size_new * sizeof (char *));
//
//	for (i = param->size; i < size_new; i++)
//		param->value_desc[i] = NULL;

/*
* copy the data
*/
	for (icase = 0; icase < NCASES; icase++) {
		switch (icase) {
			case VALUE_CASE:
				aptr_prev = param->value;
				break;

			case MIN_CASE:
				aptr_prev = param->min;
				break;

			case MAX_CASE:
				aptr_prev = param->max;
				break;
		}

		switch (param->type) {
			case M_LONG:
				lptr_prev = (long *) aptr_prev;
				aptr_new = (char *) umalloc (size_new * sizeof(long));
				lptr_new = (long *) aptr_new;
				break;

			case M_FLOAT:
				fptr_prev = (float *) aptr_prev;
				aptr_new = (char *) umalloc (size_new * sizeof(float));
				fptr_new = (float *) aptr_new;
				break;

			case M_DOUBLE:
				dptr_prev = (double *) aptr_prev;
				aptr_new = (char *) umalloc (size_new * sizeof(double));
				dptr_new = (double *) aptr_new;
				break;

		} /* switch (param->type) */

		for (iframe = 0; iframe < nframes; iframe++) {
			for (inew = 0; inew < nnew; inew++) {
				if (inew < nold)
					iold = inew;
				else
					iold = nold - 1;

				for (j = 0; j < blocksize; j++) {
					new_index = j + inew * blocksize + iframe * new_framesize;
					old_index = j + iold * blocksize + iframe * old_framesize;

					switch (param->type) {
						case M_LONG:
							lptr_new[new_index] = lptr_prev[old_index];
							break;

						case M_FLOAT:
							fptr_new[new_index] = fptr_prev[old_index];
							break;

						case M_DOUBLE:
							dptr_new[new_index] = dptr_prev[old_index];
							break;

					} /* switch (param->type) */
				} /* j */
			} /* inew */
		} /* iframe */

		switch (icase) {
			case VALUE_CASE:
				param->value = aptr_new;
				break;

			case MIN_CASE:
				param->min = aptr_new;
				break;

			case MAX_CASE:
				param->max = aptr_new;
				break;

		} /* switch (icase) */
	} /* icase */
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : save_params
 * COMMENT  : saves the param data base to a file. File name is passed in.
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define SAVE_PARAMS_C
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "mms.h"

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/
static void write_parameters (FILE *, int);
static void write_dimensions (FILE *);
static void write_header (FILE *, char *);

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : save_params
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE :
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
int save_params (char *param_file_name) {
	FILE *param_file;
	//PARAM *param;
	//DIMEN *dim;
	//char *ptr;
	//long i,j;
	//double	*dvalptr;
	//float	*fvalptr;
	//long	*lvalptr;

	if ((param_file = fopen (param_file_name, "w")) == NULL) {
		(void)fprintf(stderr, "ERROR - save_params - creating file '%s'\n", param_file_name);
		return(1);
	}

	write_header (param_file, "Default Parameter File generated based on active modules and any specified Parameter File(s)\n");
	write_dimensions (param_file);
	write_parameters (param_file, TRUE);
	
	fclose(param_file);
	return(0);
}

int write_preprocess_params () {
	FILE *param_file;
	char param_file_name[512];
	char   **fname;
	/*char *extension, *ptr, *ptr1;*/
	char *ptr, *ptr1;

	fname =   control_svar ("param_file");
	strcpy (param_file_name, fname[0]);

// Isolate the file name from the path
	ptr1 = strrchr (param_file_name, '/');

// Find the last "." in the file name
	if (!ptr1) {
		ptr = NULL;
	} else {
		ptr = strrchr (ptr1, '.');
	}

	if (!ptr) {
		ptr = param_file_name + strlen(param_file_name);
	}
	strcpy (ptr, "_preprocess.params");


	printf ("NOTICE: preprocessed parameters are being written to file: %s\n", param_file_name);

	if ((param_file = fopen (param_file_name, "w")) == NULL) {
		(void)fprintf(stderr, "ERROR - save_params - creating file '%s'\n", param_file_name);
		return(1);
	}

	write_parameters (param_file, FALSE);
	return(0);
}

static void write_header (FILE *param_file, char *desc) {
    (void)fprintf (param_file, desc);
	(void)fprintf (param_file, "PRMS version 4\n");
}

static void write_dimensions (FILE *param_file) {
	DIMEN *dim;
	long i,j;
	(void)fprintf(param_file, "** Dimensions **\n");

	for (i = 0; i < dim_db->count; i++) {

		dim = (DIMEN *)(dim_db->itm[i]);

		(void)fprintf(param_file, "####\n");
		(void)fprintf(param_file, "%s\n", dim->name);
		(void)fprintf(param_file, "%ld\n", dim->value);
		for (j = 0; j < dim->value; j++) {
			if (dim->names && dim->names[j])
				(void)fprintf (param_file, "%s\n", dim->names[j]);
			if (dim->notes && dim->notes[j])
				(void)fprintf (param_file, "@%s\n", dim->notes[j]);
		}
	}
}


static void write_parameters (FILE *param_file, int writeAllParams) {
	PARAM *param;
//	char *ptr;
	long i,j;
	double	*dvalptr;
	float	*fvalptr;
//	long	*lvalptr;
	int	*lvalptr;
/*
* Write out parameter values and description if any.
*/
	if (writeAllParams) {
		(void)fprintf(param_file, "** Parameters **\n");
	}

	for (i = 0; i < Mnparams; i++) {
		param = Mparambase[i];

		if (writeAllParams || param->preprocess ) {

			(void)fprintf(param_file, "####\n");
			(void)fprintf(param_file, "%s %ld", param->key, param->column_width);
			if (param->format)
				(void)fprintf(param_file, " %s\n", param->format);
			else
				(void)fprintf (param_file, "\n");
			(void)fprintf (param_file, "%ld\n", param->ndimen);
			for (j = 0; j < param->ndimen; j++)
				(void)fprintf(param_file, "%s\n", param->dimen[j]->name);

			(void)fprintf(param_file, "%ld\n", param->size);
			(void)fprintf(param_file, "%ld\n", param->type);

			switch (param->type) {
				case M_DOUBLE:
					if (writeAllParams) {
						dvalptr = (double *) param->value;
					} else {
						dvalptr = (double *) (param->references[0]);
					}

					for (j = 0; j < param->size; j++) {
						(void)fprintf(param_file, "%.20le\n", *dvalptr);
						dvalptr++;
						//if (param->value_desc[j]) {
						 // while ((ptr = strchr (param->value_desc[j], '\n'))) {
							//*ptr = '\0';
							//(void)fprintf (param_file, "@%s\n", param->value_desc[j]);
							//param->value_desc[j] = ptr + 1;
						 // }
						 // if (param->value_desc[j] && strlen (param->value_desc[j]))
							//(void)fprintf (param_file, "@%s\n", param->value_desc[j]);
						//}
					}
					break;

				case M_FLOAT:
					if (writeAllParams) {
						fvalptr = (float *) param->value;
					} else {
						fvalptr = (float *) (param->references[0]);
					}

					for (j = 0; j < param->size; j++) {
						(void)fprintf(param_file, "%.12e\n", *fvalptr);
						fvalptr++;
						//if (param->value_desc[j]) {
						//  while ((ptr = strchr (param->value_desc[j], '\n'))) {
						//	*ptr = '\0';
						//	(void)fprintf (param_file, "@%s\n", param->value_desc[j]);
						//	param->value_desc[j] = ptr + 1;
						//  }
						//  if (param->value_desc[j] && strlen (param->value_desc[j]))
						//	(void)fprintf (param_file, "@%s\n", param->value_desc[j]);
						//}
					}
					break;

				case M_LONG:
					if (writeAllParams) {
//						lvalptr = (long *) param->value;
						lvalptr = (int *) param->value;
					} else {
//						lvalptr = (long *) (param->references[0]);
						lvalptr = (int *) (param->references[0]);
					}

					for (j = 0; j < param->size; j++) {
//						(void)fprintf(param_file, "%ld\n", *lvalptr);
						(void)fprintf(param_file, "%d\n", *lvalptr);
						lvalptr++;
						//if (param->value_desc[j]) {
						//  while ((ptr = strchr (param->value_desc[j], '\n'))) {
						//	*ptr = '\0';
						//	(void)fprintf (param_file, "@%s\n", param->value_desc[j]);
						//	param->value_desc[j] = ptr + 1;
						//  }
						//  if (param->value_desc[j] && strlen (param->value_desc[j]))
						//	(void)fprintf (param_file, "@%s\n", param->value_desc[j]);
						//}
					}
					break;
			}
		}
	}
}

/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : setup_cont
 * COMMENT  :
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define SETUP_CONT_C

#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/**5*********************** LOCAL VARIABLES ***************************/
extern void decl_control_string (char *key, char *valstr);
extern void decl_control_int_array (char *key, long size, long *valstr);
extern void decl_control_float_array (char *key, long size, float *valstr);
extern void decl_control_string_array (char *key, long size, char *valstr);

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : setup_cont
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : void
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void setup_cont (void) {
        long *lval;
        float *fval;

        static long start_date[] = {2000,10,1,0,0,0};
        static long end_date[] = {2001,9,30,0,0,0};
		static long modflow0_date[] = {-999,9,30,0,0,0};

/*
**	GSFLOW control variables
*/
        decl_control_string ("model_mode", "GSFLOW");
        decl_control_string ("modflow_name", "modflow.nam");
        decl_control_string ("precip_module", "precip_1sta");
        decl_control_string ("temp_module", "temp_1sta");
        decl_control_string ("et_module", "potet_jh");
        decl_control_string ("srunoff_module", "srunoff_smidx");
        decl_control_string ("solrad_module", "ddsolrad");
		decl_control_string ("soilzone_module", "soilzone");
		decl_control_string ("capillary_module", "soilzone");
		decl_control_string ("strmflow_module", "strmflow");
        decl_control_string ("transp_module", "transp_tindex");
        decl_control_string ("gsflow_output_file", "gsflow.out");
        decl_control_string ("gsflow_csv_file", "gsflow.csv");

/*
        cval = (char *)umalloc (sizeof (long));
        cval[0] = "recharge";
        decl_control_string_array ("mapOutVar_names", 20, cval);
*/

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 7;
        decl_control_int_array ("rpt_days", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 1;
        decl_control_int_array ("gsf_rpt", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("print_debug", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 1;
		decl_control_int_array ("cascade_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 1;
		decl_control_int_array ("cascadegw_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 1;
		decl_control_int_array ("subbasin_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("frozen_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dprst_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 1;
		decl_control_int_array ("parameter_check_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 1;
		decl_control_int_array ("cbh_check_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("cbh_binary_flag", 1, lval);		

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_imperv_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_intcp_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_covden_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_sro2dprst_perv_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_sro2dprst_imperv_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_covtype_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_transp_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_fallfrost_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_springfrost_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_potet_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_soil_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_radtrncf_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_sro_to_dprst_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_sro_to_imperv_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dyn_dprst_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("stream_temp_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("segment_transferON_OFF", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("gwr_transferON_OFF", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("external_transferON_OFF", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("consumed_transferON_OFF", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("lake_transferON_OFF", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("dprst_transferON_OFF", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("soilzone_transferON_OFF", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("canopy_transferON_OFF", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("seg2hru_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("glacier_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("mbInit_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("musroute_flag", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("orad_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("snow_cbh_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("gwflow_cbh_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("humidity_cbh_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("windspeed_cbh_flag", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
		decl_control_int_array ("segmentOutON_OFF", 1, lval);

		lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("ignore_data_file_end", 1, lval);
/*
**	file names
*/
        decl_control_string ("executable_desc", "MOWS executable");
        decl_control_string ("executable_model", "prmsIV");
        decl_control_string ("data_file", "prms.data");
        decl_control_string ("param_file", "prms.params");
        decl_control_string ("var_save_file", "prms_ic.out");
        decl_control_string ("var_init_file", "prms_ic.in");
        decl_control_string ("stat_var_file", "statvar.out");
        decl_control_string ("ani_output_file", "animation.out");
        decl_control_string ("model_output_file", "prms.out");
        decl_control_string ("stats_output_file", "stats.out");
		decl_control_string ("tmax_day", "tmax.day");
        decl_control_string ("tmin_day", "tmin.day");
        decl_control_string ("precip_day", "precip.day");
        decl_control_string ("swrad_day", "swrad.day");
        decl_control_string ("potet_day", "potet.day");
        decl_control_string ("transp_day", "transp.day");
        decl_control_string ("windspeed_day", "windspeed.day");
        decl_control_string ("humidity_day", "humidity.day");
		decl_control_string ("pkwater_equiv_day", "pkwater_equiv.day");
        decl_control_string ("pk_depth_day", "pk_depth.day");
        decl_control_string ("snow_evap_day", "snow_evap.day");
        decl_control_string ("snowcov_area_day", "snowcov_area.day");
        decl_control_string ("snowmelt_day", "snowmelt.day");
        decl_control_string ("gwres_flow_day", "gwres_flow.day");
        decl_control_string ("dprst_area_dynamic", "dyndprst_area");
        decl_control_string ("dprst_depth_dynamic", "dyndprst_depth");
        decl_control_string ("dprst_frac_dynamic", "dyndprst_frac");
		decl_control_string ("snow_intcp_dynamic", "dynsnowintcp");
		decl_control_string ("srain_intcp_dynamic", "dynsrainintcp");
		decl_control_string ("wrain_intcp_dynamic", "dynwrainintcp");
		decl_control_string ("imperv_frac_dynamic", "dynimperv");
		decl_control_string ("imperv_stor_dynamic", "dynimperv");
		decl_control_string ("covtype_dynamic", "dyncovtype");
		decl_control_string ("covden_sum_dynamic", "dyncovden_sum");
		decl_control_string ("covden_win_dynamic", "dyncovden_win");
		decl_control_string ("jhcoef_dynamic", "dynjhcoef");
		decl_control_string ("potet_coef_dynamic", "dynpotetcoef");
		decl_control_string ("transpbeg_dynamic", "dyntranspbeg");
		decl_control_string ("transpend_dynamic", "dyntranspend");
		decl_control_string ("fallfrost_dynamic", "dynfallfrost");
		decl_control_string ("springfrost_dynamic", "dynspringfrost");
		decl_control_string ("soilrechr_dynamic", "dynsoilrechr");
		decl_control_string ("soilmoist_dynamic", "dynsoilmoist");
		decl_control_string ("radtrncf_dynamic", "dynradtrncf");
		decl_control_string ("sro2dprst_perv_dynamic", "dynsro2dprst_perv");
		decl_control_string ("sro2dprst_imperv_dynamic", "dynsro2dprst_imperv");
		decl_control_string ("csv_output_file", "gsflow.csv");
        decl_control_string ("nhruOutBaseFileName", "nhruout_path");
/*
**	run start and end times
*/
        decl_control_int_array("start_time", 6, start_date);
        decl_control_int_array("end_time", 6, end_date);
        decl_control_int_array("modflow_time_zero", 6, modflow0_date);

/*
**	flag for initializing vars from file
*/
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("init_vars_from_file", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("save_vars_to_file", 1, lval);

/*
**	initial delta-t - hours
*/
        fval = (float *)umalloc (sizeof (float));
		fval[0] = 24.0;
        decl_control_float_array ("initial_deltat", 1, fval);

/*
**	stats analysis
*/

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("statsON_OFF", 1, lval);
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("nstatVars", 1, lval);

/*
**	animation output
*/
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("aniOutON_OFF", 1, lval);
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("naniOutVars", 1, lval);

/*
**	map output
*/
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("mapOutON_OFF", 1, lval);
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("nmapOutVars", 1, lval);

/*
**	nhru_summary
*/
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("nhruOutON_OFF", 1, lval);
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("nhruOutVars", 1, lval);
		lval = (long *)umalloc(sizeof (long));
		lval[0] = 1;
		decl_control_int_array("nhruOut_freq", 1, lval);
		
/*
**	graphics display
*/
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("ndispGraphs", 1, lval);

        lval = (long *)umalloc (sizeof (long));
		lval[0] = 50;
        decl_control_int_array ("dispGraphsBuffSize", 1, lval);

// CSV output
        lval = (long *)umalloc (sizeof (long));
		lval[0] = 0;
        decl_control_int_array ("csvON_OFF", 1, lval);

}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : sort_dims
 * COMMENT  : sorts the dimen array so that the key for each
 *            structure is in increasing alphabetical order
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define SORT_DIMS_C
#include <stdio.h>
#include <string.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : sort_dims
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void sort_dims (void) {
	int		i, j;
	DIMEN	*tmpdim, **dims;

	dims = (DIMEN **)(dim_db->itm);

	for (i = dim_db->count - 2; i >= 0; i--) {
		for (j =  0; j <= i; j++) {
			if (strcmp (dims[j]->name, dims[j+1]->name) > 0) {
				tmpdim = dims[j];
				dims[j] = dims[j+1];
				dims[j+1] = tmpdim;
			}
		}
	}
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : sort_params
 * COMMENT  : sorts the param array so that the key for each structure
 *            is in increasing alphabetical order
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define SORT_PARAMS_C
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : sort_params
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void sort_params (void) {

	PARAM **params;
	PARAM *tmpparam;
	int i, j;

	params =  Mparambase;

/*
**	Make a array of the unsorted parameter order.
*/
	if (!unsort_params) {
		unsort_params = (PARAM **)malloc (Mnparams * sizeof (PARAM *));
		for (i = 0; i < Mnparams; i++)
			unsort_params[i] = params[i];
	}

/*
**	Sort the parameter data base
*/
	for (i = Mnparams-2; i >= 0; i--) {
		for (j =  0; j <= i; j++) {
			if(strcmp(params[j]->key,params[j+1]->key) > 0) {
				tmpparam = params[j];
				params[j] = params[j+1];
				params[j+1] = tmpparam;
			}
		}
	}
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : sort_vars
 * COMMENT  : sorts the pubvar array so that the key for each
 *            structure is in increasing alphabetical order
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#ifdef MALLOC_FUNC_CHECK
#include <malloc_dbg.h>
#endif

#define SORT_VARS_C
#include <stdio.h>
#include <string.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : sort_vars
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void sort_vars (void) {

  PUBVAR **vars;
  PUBVAR *tmpvar;
  int i, j;


  /*
   * get vars from varbase, the global pointer
   */

  vars =  Mvarbase;

  for (i = Mnvars-2; i >= 0; i--) {

    for (j =  0; j <= i; j++) {

      if(strcmp(vars[j]->key,vars[j+1]->key) > 0) {

	tmpvar = vars[j];
	vars[j] = vars[j+1];
	vars[j+1] = tmpvar;

      }

    }

  }
/*
  printf("sort_vars\n");
  for (i = 0; i < Mnvars; i++) {
          printf("I: %ld %s\n",i,vars[i]->key);
      }
*/

}

/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : str_to_vals
 * COMMENT  : decodes a string into values, and loads memory addresses
 *            Examples of legal strings for this routine:
 *
 *            "1 2 3 4 5"
 *            "1.0, 2.2, 19e9"
 *            "1*23.5, 7*1 13 12*3"
 *
 * Blanks, commas, tabs and newlines may delimit the values.
 * The repeat count is optional, but must be greater than 0 if included.
 * If the total number of entries is less than required, the sequence
 * is repeated.
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define STR_TO_VALS_C
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include <stdlib.h>
#include "mms.h"

#define S2V_ERROR 1l
#define S2V_SUCCESS 0l

/*--------------------------------------------------------------------*\
 | FUNCTION     : str_to_vals
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long str_to_vals (char *encoded_string, long size, long type, char *store_addr) {
  long i, isource;
  long ndecoded, repeat;
  char *scopy, *token, *valstr, *asterisk, *end_point;
  static char *tcopy = NULL;
  double dvalue, *dval;
  float fvalue, *fval;
  int lvalue, *lval;
  
  if (tcopy == NULL) {
	  tcopy = (char *) umalloc(max_data_ln_len * sizeof(char));
  }

  /*
   * set up pointer for data type
   */

  dval = NULL;
  fval = NULL;
  lval = NULL;
  switch (type) {
  case M_DOUBLE:
    dval = (double *) store_addr;
    break;
  case M_FLOAT:
    fval = (float *) store_addr;
    break;
  case M_LONG:
    lval = (int *) store_addr;
    break;
  }

  /*
   * copy encoded_string before tokenizing
   */

  scopy = strdup (encoded_string);

  token = strtok (scopy, " ,\t\n");

  ndecoded = 0;

  while (token != NULL) {

    (void)strcpy(tcopy, token);
    asterisk = strrchr(tcopy, '*'); /* search for '*' */

    if (asterisk == NULL ) {        /* no repeat count */

      valstr = tcopy;
      repeat = 1;

    } else {

      valstr = asterisk + 1;
      *asterisk = '\0';             /* terminate repeat count str */
      repeat = strtol(tcopy, &end_point, 10l);

      if (repeat <= 0 || *end_point != '\0') {
	(void)fprintf(stderr,
		"ERROR - str_to_vals - decoding string into values.\n");
	(void)fprintf(stderr, "Illegal repeat count.\n");
	return S2V_ERROR;
      }

    }

    /*
     * set errno to 0 so that previous errors are cancelled
     */

    errno = 0;

    dvalue = 0.0;
    fvalue = 0.0;
    lvalue = 0;
    switch (type) {

    case M_DOUBLE:
      dvalue = strtod(valstr, &end_point);
      break;
    case M_FLOAT:
      fvalue = (float) strtod(valstr, &end_point);
      break;
    case M_LONG:
      lvalue = (int)strtol(valstr, &end_point, 10);
      break;
    }

    if (errno == EDOM) {
      (void)fprintf(stderr,
	      "ERROR - str_to_vals - decoding string into values.\n");
      (void)fprintf(stderr, "Illegal value.\n");
      return S2V_ERROR;
    }

    if (errno == ERANGE) {
      (void)fprintf(stderr,
	      "ERROR - str_to_vals - decoding string into values.\n");
      (void)fprintf(stderr, "Value out of range.\n");
      return S2V_ERROR;
    }

    if (ndecoded + repeat > size) {
      repeat = size - ndecoded;
    }

    switch (type) {

    case M_DOUBLE:
      for (i = 0; i < repeat; i++) {
	dval[ndecoded] = dvalue;
	ndecoded++;
      }
      break;

    case M_FLOAT:
      for (i = 0; i < repeat; i++) {
	fval[ndecoded] = fvalue;
	ndecoded++;
      }
      break;

    case M_LONG:
      for (i = 0; i < repeat; i++) {
	lval[ndecoded] = lvalue;
	ndecoded++;
      }
      break;
    }

    token = strtok(NULL, " ,\n\t");

  }

  /*
   * If too few elements decoded, repeat the sequence
   */

  if (ndecoded < size) {

    isource = 0;

    switch (type) {

    case M_DOUBLE:
      for (i = ndecoded; i < size; i++) {
	dval[i] = dval[isource];
	isource++;
	if (isource == ndecoded)
	  isource = 0;
      }
      break;

    case M_FLOAT:
      for (i = ndecoded; i < size; i++) {
	fval[i] = fval[isource];
	isource++;
	if (isource == ndecoded)
	  isource = 0;
      }
      break;

    case M_LONG:
      for (i = ndecoded; i < size; i++) {
	lval[i] = lval[isource];
	isource++;
	if (isource == ndecoded)
	  isource = 0;
      }
      break;
    }

  }

  return S2V_SUCCESS;
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : timing
 * COMMENT  : timing functions
 *            The routines with a _ suffix are called from Fortran
 *            The routines without the suffix are called from C
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define TIMING_C
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/**************************************************************************
 * dattim and dattim_ : get start, end or current data date and time
 *
 * args - dwhen : string, "start", "end", and "now"
 *        timearray: integer or long array which accepts the time
 *                   and date
 * 
 */

/*--------------------------------------------------------------------*\
 | FUNCTION     : dattim_
 | COMMENT		: called from Fortran, sorts out args and calls dattim()
 |                 get start, end or current data date and time
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void dattim_ (char *dwhen, ftnint *timearray, ftnlen dwhenlen) {

//  char *when;
  char when[80];
  long ta[6];

  /*
   * copy when and terminate
   */

//  when = (char *) umalloc(dwhenlen + 1);
//  strncpy(when, dwhen, dwhenlen);
//  when[dwhenlen] = '\0';

    strncpy (when, dwhen, dwhenlen);
    *(when + dwhenlen) = '\0';


  /*
   * call C version of dattim()
   */

  dattim(when, ta);
  timearray[0] = ta[0];
  timearray[1] = ta[1];
  timearray[2] = ta[2];
  timearray[3] = ta[3];
  timearray[4] = ta[4];
  timearray[5] = ta[5];

}

/**************************************************************************
 */


/*--------------------------------------------------------------------*\
 | FUNCTION     : dattim
 | COMMENT		: called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void dattim (char *when, long *timearray) {

  DATETIME *time;

  /*
   * set time according to when argument
   */

  if (!strcmp(when, "start"))
    time = Mstrttime;
  else if(!strcmp(when, "end"))
    time = Mendtime;
  else if (!strcmp(when, "now"))
    time = Mnowtime;
  else {
    (void)fprintf(stderr,
	    "ERROR - dattim - illegal argument '%s'.\n", when);
    exit(1);
  }

  /*
   * load up time array
   */

  timearray[0] = time->year;
  timearray[1] = time->month;
  timearray[2] = time->day;
  timearray[3] = time->hour;
  timearray[4] = time->min;
  timearray[5] = time->sec;

}

/*--------------------------------------------------------------------*\
 | FUNCTION     : nowjt_
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
double nowjt_ () {
   return Mnowtime->jt;
}

/**************************************************************************
 * julian_ and julian : returns the julian date of the data stream relative
 *                      to calendar, solar and water year start dates.
 *                         (1 JAN)   (22 DEC)  (1 OCT)
 *
 * args - when : string, "start", "end", "now"
 *        type : string, "calendar", "solar", "water", "absolute"
 * 
 * julian_() is called from Fortran, sorts out args and calls julian()
 */

/*--------------------------------------------------------------------*\
 | FUNCTION     : julian_
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long julian_ (char *jwhen, char *jtype, ftnlen jwhenlen, ftnlen jtypelen) {

//  char *when, *type;
  char when[80], type[80];
  long retval;

  /*
   * copy strings and terminate
   */

//  when = (char *) umalloc(jwhenlen + 1);
//  strncpy(when, jwhen, jwhenlen);
//  when[jwhenlen] = '\0';
    strncpy (when, jwhen, jwhenlen);
    *(when + jwhenlen) = '\0';

//  type = (char *) umalloc(jtypelen + 1);
//  strncpy(type, jtype, jtypelen);
//  type[jtypelen] = '\0';
    strncpy (type, jtype, jtypelen);
    *(type + jtypelen) = '\0';

  /*
   * call C version of julian()
   */

  retval = julian(when, type);

  return retval;

}

/**************************************************************************
 * julian() is called from C
 */

/*--------------------------------------------------------------------*\
 | FUNCTION     : julian
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long julian (char *when, char *type) {

  DATETIME *time, reftime;

  /*
   * set time according to when argument
   */

  if (!strcmp(when, "start"))
    time = Mstrttime;
  else if(!strcmp(when, "end"))
    time = Mendtime;
  else if (!strcmp(when, "now"))
    time = Mnowtime;
  else {
    (void)fprintf(stderr,
	    "ERROR - julian - illegal argument '%s'.\n", when);
    exit(1);
  }

  /*
   * set reftime depending on type arg
   */

  if (!strcmp(type, "calendar")) {
    reftime.year = time->year - 1;
    reftime.month = 12;
    reftime.day = 31;
  } else if(!strcmp(type, "solar")) {
    if ((time->month == 12) && (time->day > 21))
      reftime.year = time->year;
    else
      reftime.year = time->year - 1;
    reftime.month = 12;
    reftime.day = 21;
  } else if(!strcmp(type, "spring")) {
	  if ((time->month > 3) || (time->month == 3 && time->day > 20)) {
		reftime.year = time->year;
	  } else {
		reftime.year = time->year - 1;
	  }
	reftime.month = 3;
	reftime.day = 20;
  } else if (!strcmp(type, "water")) {
    if (time->month > 9)
      reftime.year = time->year;
    else
      reftime.year = time->year - 1;
    reftime.month = 9;
    reftime.day = 30;
  } else if(!strcmp(type, "absolute")) {
    julday(time);
    return (time->jd);
  } else {
    (void)fprintf(stderr,
	    "ERROR - julian - illegal argument '%s'.\n", type);
    exit(1);
  }

  reftime.hour = 0;
  reftime.min = 0;
  reftime.sec = 0;

  /*
   * compute the julian dates
   */

  julday(time);
  julday(&reftime);

  return (time->jd - reftime.jd);

}

/**************************************************************************
 * deltim_() is called from Fortran, deltim()
 */

double deltim_(void) {

/* printf ("from deltim:  %f\n", deltim()); */
  return deltim();

}

/**************************************************************************
 * deltim() is called from C
 */
/*--------------------------------------------------------------------*\
 | FUNCTION     : deltim
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
double deltim (void) {
  return (double) Mdeltat * 24.0;
}

/**************************************************************************
 * getstep_() is called from Fortran, getstep()
 */

/*--------------------------------------------------------------------*\
 | FUNCTION     : getstep_
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getstep_ (void) {
  return getstep();
}

/**************************************************************************
 * getstep() is called from C
 */
/*--------------------------------------------------------------------*\
 | FUNCTION     : getstep
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long getstep (void) {
  return Mnsteps;
}

/**************************************************************************
 * djulian_ and djulian : returns the double julian date of the data stream
 *                      relative to calendar, solar and water year start dates.
 *                         (1 JAN)   (22 DEC)  (1 OCT)
 *
 * args - when : string, "start", "end", "now"
 *        type : string, "calendar", "solar", "water", "absolute"
 * 
 * julian_() is called from Fortran, sorts out args and calls julian()
 */

/*--------------------------------------------------------------------*\
 | FUNCTION     : djulian_
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
double djulian_ (char *jwhen, char *jtype, ftnlen jwhenlen, ftnlen jtypelen) {

  char *when, *type;
  double retval;

  /*
   * copy strings and terminate
   */

  when = (char *) umalloc(jwhenlen + 1);
  strncpy(when, jwhen, jwhenlen);
  when[jwhenlen] = '\0';

  type = (char *) umalloc(jtypelen + 1);
  strncpy(type, jtype, jtypelen);
  type[jtypelen] = '\0';

  /*
   * call C version of djulian()
   */

  retval = djulian(when, type);

  /*
   * free up arrays
   */

//ufree(when);
//ufree(type);

  return retval;

}

/**************************************************************************
 * julian() is called from C
 */

/*--------------------------------------------------------------------*\
 | FUNCTION     : djulian
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
double djulian (char *when, char *type) {

  DATETIME *time, reftime;

  /*
   * set time according to when argument
   */

  if (!strcmp(when, "start"))
    time = Mstrttime;
  else if(!strcmp(when, "end"))
    time = Mendtime;
  else if (!strcmp(when, "now"))
    time = Mnowtime;
  else {
    (void)fprintf(stderr,
	    "ERROR - julian - illegal argument '%s'.\n", when);
    exit(1);
  }

  /*
   * set reftime depending on type arg
   */

  if (!strcmp(type, "calendar")) {
    reftime.year = time->year - 1;
    reftime.month = 12;
    reftime.day = 31;
  } else if(!strcmp(type, "solar")) {
    if ((time->month == 12) && (time->day > 21))
      reftime.year = time->year;
    else
      reftime.year = time->year - 1;
    reftime.month = 12;
    reftime.day = 21;
  } else if (!strcmp(type, "water")) {
    if (time->month > 9)
      reftime.year = time->year;
    else
      reftime.year = time->year - 1;
    reftime.month = 9;
    reftime.day = 30;
  } else if(!strcmp(type, "absolute")) {
    julday(time);
    return (time->jt);
  } else {
    (void)fprintf(stderr,
	    "ERROR - julian - illegal argument '%s'.\n", type);
    exit(1);
  }

  reftime.hour = 0;
  reftime.min = 0;
  reftime.sec = 0;

  /*
   * compute the julian dates
   */

  julday(time);
  julday(&reftime);

  return (time->jt - reftime.jt);

}

/**************************************************************************
 * delnex_() is called from Fortran, delnex()
 */

double delnex_(void) {

/* printf ("from deltim:  %f\n", deltim()); */
  return delnex();

}

/**************************************************************************
 * delnex() is called from C
 */
/*--------------------------------------------------------------------*\
 | FUNCTION     : delnex
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
double delnex (void) {
  return (double) Mdeltanext * 24.0;
}

/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : umalloc_etc
 * COMMENT  : memory allocation routines with error handling
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define UMALLOC_ETC_C
#include <stdlib.h>
#include <stdio.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : umalloc
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *umalloc (unsigned size) {
  char *ptr;

  if (!size)
    return (NULL);

  if ((ptr = (char *)malloc(size)) == NULL)
    if (size != 0) {
      (void)fprintf(stderr, "Cannot perform malloc, size = %d\n",size);
      exit(1);
    }
  return(ptr);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : urealloc
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *urealloc (char *ptr, unsigned size) {
  if (ptr == NULL) return(umalloc(size));
  if ((ptr = (char *)realloc(ptr, size)) == NULL)
    if (size != 0) {
      (void)fprintf(stderr, "Cannot perform realloc, size = %d\n",size);
      exit(1);
    }
  return(ptr);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : ucalloc
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
char *ucalloc (unsigned num, unsigned size) {
  char *ptr;
  if ((ptr = (char *)calloc(num, size)) == NULL) 
    if ((size != 0) && (num != 0))
      (void)fprintf(stderr, "Cannot perform calloc, num, size = %d,%d\n",num,size);
      exit(1);
  return(ptr);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : ufree
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void ufree (char *ptr) {
/*
   if (ptr != NULL) free(ptr);
*/
}

/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : var_addr
 * COMMENT  : returns a pointer to a PUBVAR struct which contains the given key
 *            returns NULL if key not found
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define VAR_ADDR_C
#include <stdio.h>
#include <string.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : var_addr
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
PUBVAR *var_addr (char *key) { 
  PUBVAR **vars;
  int lowcomp, midcomp, highcomp;
  long low, mid, high;

  if (Mnvars == 0) return NULL; /* no variables to locate */

  /*
   * get vars from Mvarbase, the global pointer
   */

  vars = Mvarbase;

  /*
   * search between 0 and Mnvars-1
   */

  low = 0;
  high = Mnvars-1;

   lowcomp = strcmp(vars[low]->key, key);
   if (!lowcomp) {
      return vars[low];
   }

   if (lowcomp > 0) return NULL; /* key out of limits */

   highcomp = strcmp(vars[high]->key, key);

   if (!highcomp) {
      return vars[high];
  }

  if (highcomp < 0) return NULL; /* key out of limits */

  /*
   * the basic search uses bisection
   */

  while (low != high) {

    mid = (low + high) / 2;
    midcomp = strcmp(vars[mid]->key, key);

    if (!midcomp) {
       return vars[mid];
    } else {
      if ((mid == low) || (mid == high)) {   /* the search has closed to */
	return NULL;                         /* width 1 without success  */
      } else {
	if (midcomp < 0) {   /* reset low or high as appropriate */
	  low = mid;
	} else {
	  high = mid;
	}
      }
    }

  }

  /* if no match found, return null */

  return NULL;

}

/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : write_vstats
 * COMMENT  : saves values of stat variables into a temporary file.
 *            The temporary file was open in user_input
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define WRITE_VSTATS_C
#include <stdio.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : write_vstats
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : void
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void write_vstats (FILE *statvar_file) { 

  STAT_LIST_TYPE *stat_list;

  /*
   * writes first record number, date and time info
   */

  (void)fprintf(statvar_file, "%ld %ld %ld %ld %ld %ld %ld ",
	  Mnsteps, Mnowtime->year,
	  Mnowtime->month, Mnowtime->day, Mnowtime->hour,
	  Mnowtime->min, Mnowtime->sec);

  /*
   * Initializes linked list to first pointer
   */

  stat_list = Mfirst_stat_list;

  /*
   * The list is NULL-terminated
   */

  while (stat_list)  {
    
    /*
     * Gets variable value
     */

    switch (stat_list->type) {

    case M_FLOAT:

      (void)fprintf(statvar_file,"%f ", *(float *)stat_list->value);
      break;

    case M_DOUBLE:

      (void)fprintf(statvar_file,"%lf ", *(double *)stat_list->value);
      break;

    case M_LONG:
	// markstro test
    //  (void)fprintf(statvar_file,"%ld ", *(long *)stat_list->value);
	(void)fprintf(statvar_file,"%ld ", *(int *)stat_list->value);
      break;

    }

    /*
     * Updates pointer
     */

    stat_list = stat_list->next;

  }

  (void)fprintf(statvar_file,"\n");

}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : call_setdims
 * COMMENT  :
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#include <stdio.h>
#include "mms.h"

extern long setdims_();

int call_setdims()

{

  long retval;

  retval = setdims_();
  if (retval) {
    fprintf(stderr,"ERROR in 'setdims' routine.\n");
    fprintf(stderr,"Return val = %ld\n", retval);
    return(1);
  }
  return(0);
}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : getdimname
 * COMMENT  : The following are two routines to obtain the "ith" index name 
 *            of a dimension variable from either Fortran or C modules.
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define GETDIMNAME_C
#include <string.h>
#include <stdlib.h>
#include "mms.h"

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdimname_
 | COMMENT		: called from fortran
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void getdimname_ (char *name, ftnint *i, char *idxname, ftnlen namelen, ftnlen idxlen) {
  /*
   * local copies
   */

  char * lname;

  lname = (char *)malloc(namelen+1);
  strncpy(lname, name, namelen);
  lname[namelen] = '\0';

  /*
   * call c version
   */
 getdimname(lname, (*i) - 1, idxname);
  
  idxlen = strlen(idxname);

}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdimdesc_
 | COMMENT		: called from fortran
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void getdimdesc_ (char *name, ftnint *i, char *desc, ftnlen namelen, ftnlen desclen) {
  /*
   * local copies
   */

  char * lname;

  lname = (char *)malloc(namelen+1);
  strncpy(lname, name, namelen);
  lname[namelen] = '\0';

/*
**	call c version
*/
	getdimdesc (lname, (*i) - 1, desc);
	desclen = strlen (desc);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdimnameint_
 | COMMENT		: called from fortran
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void getdimnameint_ (char *name, ftnint *i, ftnint *idx, ftnlen namelen) {
  /*
   * local copies
   */

  char * lname;
  char idxname[80];

  lname = (char *)malloc(namelen+1);
  strncpy(lname, name, namelen);
  lname[namelen] = '\0';

  /*
   * call c version
   */
 getdimname(lname, (*i) - 1, idxname);
  
  *idx = atoi(idxname);

}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdimname
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void getdimname (char *name, long i, char *idxname) {
  DIMEN *dim;

  dim = dim_addr(name);
  if (!dim) {
      (void)fprintf(stderr, "ERROR - getdimname, Can't find dimension named %s\n",name);
      return;
	}
  
  if (!dim->names) {
      (void)fprintf(stderr, "ERROR - getdimname. Dimension %s has no named indices\n",name);
      return;
    }
  (void)strcpy(idxname, dim->names[i]);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : getdimdesc
 | COMMENT		:
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
void getdimdesc (char *name, long i, char *descname) {
	DIMEN *dim;

	dim = dim_addr(name);
	if (!dim) {
		(void)fprintf (stderr,
			"ERROR - getdimname, Can't find dimension named %s\n", name);
		(void)strcpy (descname, "");
		return;
	}
  
	if (!dim->notes) {
		(void)strcpy (descname, "");
		return;
	}

	if (dim->notes[i])
		(void)strcpy (descname, dim->notes[i]);
	else
		(void)strcpy (descname, "");

}
/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * FUNCTION : call_modules
 * COMMENT  : used to call a Fortran version
 *
 * $Id: mms_util.c 7588 2015-08-18 22:58:42Z rsregan $
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#include <stdlib.h>
#include <string.h>
#include "mms.h"

extern long call_modules_ (char *, ftnlen);

int call_modules(char *arg) {
	 long retval;
	 ftnlen len;

	 len = (ftnlen)strlen(arg);
	 retval = call_modules_ (arg, len);
	 return((int)retval);
}

