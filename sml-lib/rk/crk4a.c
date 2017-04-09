
// Runge-Kutta's 4th order method:
// cs:     [0,0.5,0.5,1]
// as:     <1, []>
//         <2, [1]>
//         <2, [0,1]>
//         <1, [0,0,1]>
// bs:     <6, [1,2,2,1]>

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "crk_common.h"

int Runge_Kutta_4a(int n, void (*f)(double,double *,double *,void **), 
                   void **clos,
                   double *y, double x0, double h, double *yout, 
                   double *k1, double *k2, double *k3, double *k4, 
                   double *t1, double *t2, double *t3, double *t4, double *t5, double *t6,
                   double *t7, double *t8);

////////////////////////////////////////////////////////////////////////////////
//  static double Runge_Kutta_4a(int (*f)(double,double*), double *y,          //
//                                                       double x0, double h) //
//                                                                            //
//  Description:                                                              //
//     This routine uses Runge-Kutta's 4th order method                       //
//     to approximate the solution of the differential equation y'=f(x,y)     //
//     with the initial condition y = y[0] at x = x0.  The value at x + h is  //
//     returned in yout.  
//                                                                            //
//  Arguments:                                                                //
//     double *f  Pointer to the function which returns the slope at (x,y) of //
//                integral curve of the differential equation y' = f(x,y)     //
//                which passes through the point (x0,y[0]).                   //
//     double y[] On input y[0] is the initial value of y at x
//     double x   Initial value of x.                                         //
//     double h   Step size                                                   //
//                                                                            //
//  Return Values:                                                            //
//     The solution of y(x) at x + h is returned in yout                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

int Runge_Kutta_4a(int n, void (*f)(double,double *,double *,void **), 
                   void **clos,
                   double *y, double x0, double h, double *yout, 
                   double *k1, double *k2, double *k3, double *k4, 
                   double *t1, double *t2, double *t3, double *t4, double *t5, double *t6,
                   double *t7, double *t8)
{
  double h2, h3;

  if (n == 0)
    {
      return 0;
    }
// cs:     [0,0.5,0.5,1]
// as:     <1, []>
//         <2, [1]>
//         <2, [0,1]>
//         <1, [0,0,1]>
// bs:     <6, [1,2,2,1]>

  h2 = 0.5 * h;
  h3 = 0.5 * h;

  (*f)(x0, y, k1, clos);
  // printf("c: k1[0] = %g\n", k1[0]);
  
  vector_scale(n, 1.0, k1, t1); 
  vector_scale(n, h/2.0, t1, t2); 
  vector_sum(n, y, t2, t3);
  (*f)(x0+h2, t3, k2, clos);
  // printf("c: k2[0] = %g\n", k2[0]);
  
  vector_scale(n, 1.0, k2, t1);
  vector_scale(n, h/2.0, t1, t2); vector_sum(n, y, t2, t3); 
  (*f)(x0+h3, t3, k3, clos);
  // printf("c: k3[0] = %g\n", k3[0]);

  vector_scale(n, 1.0, k3, t1);
  vector_scale(n, h, t1, t2); vector_sum(n, y, t2, t3); 
  (*f)(x0+h, t3, k4, clos);
  // printf("c: k3[0] = %g\n", k3[0]);

  vector_scale(n, 1.0, k1, t1); 
  vector_scale(n, 2.0, k2, t2); 
  vector_scale(n, 2.0, k3, t3); 
  vector_scale(n, 1.0, k4, t4); 
  vector_sum(n, t1, t2, t5); vector_sum(n, t3, t4, t6);
  vector_sum(n, t5, t6, t7); 
  vector_scale(n, h/6.0, t7, t8); 
  // printf("c: t6[0] = %g\n", t6[0]);
  vector_sum(n, y, t8, yout); 
  
  return 0;
}
