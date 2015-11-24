
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Description:                                                              //
//     The Runge-Kutta-Prince-Dormand method is an adaptive procedure for    //
//     approximating the solution of the differential equation y'(x) = f(x,y) //
//     with initial condition y(x0) = c.  This implementation evaluates       //
//     f(x,y) six times per step using embedded fourth order and fifth order  //
//     Runge-Kutta estimates to estimate the not only the solution but also   //
//     the error.                                                             //
//     The next step size is then calculated using the preassigned tolerance  //
//     and error estimate.                                                    //
//     For step i+1,                                                          //
//        y[i+1] = y[i] +  h * ( 31 / 540 * k1 + 190 / 297 * k3               //
//                           - 145 / 108 * k4 + 351 / 220 * k5 + 1/20 * k6 )  //
//     where                                                                  //
//     k1 = f( x[i],y[i] ),                                                   //
//     k2 = f( x[i]+h/5, y[i] + h*k1/5 ),                                     //
//     k3 = f( x[i]+3h/10, y[i]+(h/40)*(3 k1 + 9 k2) ),                       //
//     k4 = f( x[i]+3h/5, y[i]+(h/10)*(3 k1 - 9 k2 + 12 k3) ),                //
//     k5 = f( x[i]+2h/3, y[i]+(h/729)*(226 k1 - 675 k2 + 880 k3 + 55 k4) )   //
//     k6 = f( x[i]+h, y[i]+(h/2970)*(-1991 k1 + 7425 k2 - 2660 k3            //
//                                                  - 10010 k4 + 10206 k5) )  //
//     x[i+1] = x[i] + h.                                                     //
//                                                                            //
//     The error is estimated to be                                           //
//        err = h*( 77 k1 - 400 k3 + 1925 k4 - 1701 k5 + 99 k6 ) / 2520       //
//     The step size h is then scaled by the scale factor                     //
//         scale = 0.8 * | epsilon * y[i] / [err * (xmax - x[0])] | ^ 1/4     //
//     The scale factor is further constrained 0.125 < scale < 4.0.           //
//     The new step size is h := scale * h.                                   //
////////////////////////////////////////////////////////////////////////////////

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "export.h"

#define max(x,y) ( (x) < (y) ? (y) : (x) )
#define min(x,y) ( (x) < (y) ? (x) : (y) )

#define ATTEMPTS 12
#define MIN_SCALE_FACTOR 0.125
#define MAX_SCALE_FACTOR 4.0

void vector_sum (int n, double *x, double *y, double *result) 
{
  int i;
  for (i = 0; i<n; i++) 
    {
      result[i] = x[i] + y[i];
    }
}

void vector_scale (int n, double k, double *x, double *result)
{
  int i;
  for (i = 0; i<n; i++) 
    {
      result[i] = k * x[i];
    }
}

int Dormand_Prince_5_4(int, int (*f)(double,double*,double*), double *y, double *yout, double x, double h, double *err,
                       double *k1, double *k2, double *k3, double *k4, double *k5, double *k6, 
                       double *t1, double *t2, double *t3, double *t4, double *t5, double *t6, double *t7, double *t8, double *t9, double *t10, double *t11, double *t12);


////////////////////////////////////////////////////////////////////////////////
//  static double Dormand_Prince_5_4(int (*f)(double,double*), double *y,          //
//                                                       double x0, double h) //
//                                                                            //
//  Description:                                                              //
//     This routine uses Prince-Dormand's embedded 4th and 5th order methods  //
//     to approximate the solution of the differential equation y'=f(x,y)     //
//     with the initial condition y = y[0] at x = x0.  The value at x + h is  //
//     returned in y[1].  The function returns err / h ( the absolute error   //
//     per step size ).                                                       //
//                                                                            //
//  Arguments:                                                                //
//     double *f  Pointer to the function which returns the slope at (x,y) of //
//                integral curve of the differential equation y' = f(x,y)     //
//                which passes through the point (x0,y[0]).                   //
//     double y[] On input y[0] is the initial value of y at x, on output     //
//                y[1] is the solution at x + h.                              //
//     double x   Initial value of x.                                         //
//     double h   Step size                                                   //
//                                                                            //
//  Return Values:                                                            //
//     This routine returns the err / h.  The solution of y(x) at x + h is    //
//     returned in y[1].                                                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

int Dormand_Prince_5_4(int n, int (*f)(double,double *,double *), double *y, double *yout, double x0, double h, double *err,
                       double *k1, double *k2, double *k3, double *k4, double *k5, double *k6, 
                       double *t1, double *t2, double *t3, double *t4, double *t5, double *t6, double *t7, double *t8, double *t9, double *t10, double *t11, double *t12)
{

  int status;

  static const double two_thirds = 2.0 / 3.0;
  static const double one_seventwoninths = 1.0 / 729.0;
  static const double one_twoninesevenzero = 1.0 / 2970.0;
  static const double one_twofivetwozero = 1.0 / 2520.0;
  static const double one_fiveninefourzero = 1.0 / 5940.0;
  
  double h5 = 0.2 * h;
  
  printf("c: y[0] = %g\n", y[0]);

  assert ((*f)(x0, y, k1) == 0);
  printf("c: k1[0] = %g\n", k1[0]);
  
  vector_scale(n, h5, k1, t1); 
  printf("c: t1[0] = %g\n", t1[0]);
  vector_sum(n, y, t1, t2);
  printf("c: t2[0] = %g\n", t2[0]);
  assert ((*f)(x0+h5, t2, k2) == 0);
  printf("c: k2[0] = %g\n", k2[0]);
  
  vector_scale(n, 0.075, k1, t1); vector_scale(n, 0.225, k2, t2); 
  vector_sum(n, t1, t2, t3); vector_scale(n, h, t3, t4); vector_sum(n, y, t4, t5); 
  assert ((*f)(x0+0.3*h, t5, k3) == 0);
  printf("c: k3[0] = %g\n", k3[0]);

  vector_scale(n, 0.3, k1, t1); vector_scale(n, -0.9, k2, t2); vector_scale(n, 1.2, k3, t3); 
  vector_sum(n, t1, t2, t4); vector_sum(n, t3, t4, t5); vector_scale(n, h, t5, t6); vector_sum(n, y, t6, t7); 
  assert ((*f)(x0+0.6*h, t7, k4) == 0);
  printf("c: k4[0] = %g\n", k4[0]);
  
  vector_scale(n, 226.0, k1, t1); vector_scale(n, -675.0, k2, t2); vector_scale(n, 880.0, k3, t3); vector_scale(n, 55.0, k4, t4); 
  vector_sum(n, t1, t2, t5); vector_sum(n, t3, t4, t6); vector_sum(n, t5, t6, t7); vector_scale(n, one_seventwoninths * h, t7, t8);  
  vector_sum(n, y, t8, t9); 
  assert ((*f)(x0+two_thirds * h, t9, k5) == 0);
  printf("c: k5[0] = %g\n", k5[0]);
  
  vector_scale(n, -1991.0, k1, t1); vector_scale(n, 7425.0, k2, t2); vector_scale(n, -2660.0, k3, t3); 
  vector_scale(n, 10010.0, k4, t4); vector_scale(n, 10206.0, k5, t5); 
  vector_sum(n, t1, t2, t6); vector_sum(n, t3, t4, t7); vector_sum(n, t5, t6, t8); vector_sum(n, t7, t8, t9); 
  vector_scale(n, one_twoninesevenzero * h, t9, t10);  
  vector_sum(n, y, t10, t11); 
  assert ((*f)(x0+h, t11, k6 ) == 0);
  printf("c: k6[0] = %g\n", k6[0]);
  
  vector_scale(n, 341.0, k1, t1); vector_scale(n, 3800.0, k3, t3); 
  vector_scale(n, -7975.0, k4, t4); vector_scale(n, 9477.0, k5, t5); vector_scale(n, 297.0, k6, t6); 
  vector_sum(n, t1, t3, t7); vector_sum(n, t4, t5, t8); vector_sum(n, t6, t7, t9); vector_sum(n, t8, t9, t10); 
  vector_scale(n, one_fiveninefourzero * h, t10, t11);  
  vector_sum(n, y, t11, t12); 
  assert ((*f)(x0+h, t12, yout) == 0);
  printf("c: yout[0] = %g\n", yout[0]);
  
  vector_scale(n, 77.0, k1, t1); vector_scale(n, -400.0, k3, t3); 
  vector_scale(n, 1925.0, k4, t4); vector_scale(n, -1701.0, k5, t5); 
  vector_scale(n, 99.0, k6, t6); 
  vector_sum(n, t1, t3, t7); vector_sum(n, t4, t5, t8); vector_sum(n, t6, t7, t9); vector_sum(n, t8, t9, t10); 
  vector_scale(n, one_twofivetwozero, t10, err);  
  
  return 0;
}
