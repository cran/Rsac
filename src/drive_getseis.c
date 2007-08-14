#include <stdio.h>
#include <rpc/rpc.h>

#include <assert.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <unistd.h>
#include <stddef.h>
#include <math.h>
#include <sys/stat.h>

#include <malloc.h>

#include "seis.h"

#include "sac.h"

#include "ahhead.h"

void  get_one_segy(char *fn, TIME_SERIES *ts);
void  get_one_sac(char *fn, TIME_SERIES *ts);
void  get_one_ah(char *fn, TIME_SERIES *ts);

int main(int argc, char **argv)
{

   TIME_SERIES ts;
   char fn;

   int i, j, i1, i2, k, kind;
   double dt, tt;
   double rms;
   double day, sec2;
   
   int n, m;

   double wt;  /* half window time in seconds */
   double skipt; 

   wt = 2*60;
   skipt = 3*60;

   kind = atoi(argv[1]);




   for(j=2; j<argc; j++)
   {


   switch(kind)
   {
      case 1:
	 get_one_segy(argv[j],  &ts);
	 break;
      case 2:
	 get_one_sac(argv[j],  &ts);
	 break;
      case 3:
	 get_one_ah(argv[j],  &ts);
	 break;
      default:
	 get_one_segy(argv[j],  &ts);
	 break;
   }
   


   /* get_one_segy(argv[j],  &ts); */

   n = ts.numsamp;
   dt = ts.deltat;
   tt = n*dt;

   i1 = (int)(wt/dt);
   i2 = (int)(skipt/dt);

   fprintf(stderr, "%s %s %d %d %d %d %f %f\n",  ts.id.staname,  ts.id.comp ,  ts.year,  ts.jday, ts.hour,  ts.minute,   ts.sec, ts.deltat);
   for(i=i1; i<n; i+=i2)
   {
      rms = 0;
      m=0;
      sec2 = i*dt;
      for(k=i-i1; k<i+i1; k++)
      {
	 if(k>n) break;
	 rms += ts.amp[k]*ts.amp[k];
	 m++;
      }   
      if(m>0)
      {
      rms /=m;
      }
      fprintf(stdout, "%d %d %d %d %f %f %g\n",  ts.year,  ts.jday, ts.hour,  ts.minute,   ts.sec, sec2, rms);
   }
   
   	       free(ts.amp);
	       free(ts.ampbak);
   

   
   }
   return(0);


}
