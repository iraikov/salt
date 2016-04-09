
// Kutta's 3rd order method:
// cs:     [0,0.5,1]
// as:    <1, []>
//        <2, [1]>
//        <1, [~1,2]>
// bs:    <6, [1,4,1]>

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

static int vector_zerop (int n, double *x)
{
  int i; int result;
  result = 1;
  for (i = 0; i<n; i++) 
    {
      result = result && (x[i] == 0.0);
    }
  return result;
}

static void vector_sum (int n, double *x, double *y, double *result) 
{
  int i;
  for (i = 0; i<n; i++) 
    {
      result[i] = x[i] + y[i];
    }
}

static void vector_scale (int n, double k, double *x, double *result)
{
  int i;
  for (i = 0; i<n; i++) 
    {
      result[i] = k * x[i];
    }
}

int Runge_Kutta_3(int, void (*f)(double,double*,double*,double*,double*,double*,double *), 
                  double *p, double *fld, double *ext, double *extev, 
                  double *y, double x, double h, double *yout, 
                  double *k1, double *k2, double *k3, 
                  double *t1, double *t2, double *t3, double *t4, double *t5, double *t6);

int Runge_Kutta_3_regime(int, void (*f)(double,double*,double*,double*,int*,double*,double*,double*,double *), 
                         double *p, double *fld, double *d, int *r, double *ext, double *extev, 
                         double *y, double x, double h, double *yout,
                         double *k1, double *k2, double *k3, 
                         double *t1, double *t2, double *t3, double *t4, double *t5, double *t6);


////////////////////////////////////////////////////////////////////////////////
//  static double Runge_Kutta_3(int (*f)(double,double*), double *y,          //
//                                                       double x0, double h) //
//                                                                            //
//  Description:                                                              //
//     This routine uses Kutta's 3rd order method                             //
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

int Runge_Kutta_3(int n, void (*f)(double,double *,double *,double *,double *,double *,double *), 
                  double *p, double *fld, double *ext, double *extev, 
                  double *y, double x0, double h, double *yout, 
                  double *k1, double *k2, double *k3, 
                  double *t1, double *t2, double *t3, double *t4, double *t5, double *t6)
{
  double h2;

  if (n == 0)
    {
      return 0;
    }

  h2 = 0.5 * h;

  (*f)(x0, p, fld, ext, extev, y, k1);
  // printf("c: k1[0] = %g\n", k1[0]);
  
  vector_scale(n, 1.0, k1, t1); 
  vector_scale(n, h/2.0, t1, t2); 
  vector_sum(n, y, t2, t3);
  (*f)(x0+h2, p, fld, ext, extev, t3, k2);
  // printf("c: k2[0] = %g\n", k2[0]);
  
  vector_scale(n, -1.0, k1, t1); vector_scale(n, 2.0, k2, t2); 
  vector_sum(n, t1, t2, t3); 
  vector_scale(n, h, t3, t4); vector_sum(n, y, t4, t5); 
  (*f)(x0+h, p, fld, ext, extev, t5, k3);
  // printf("c: k3[0] = %g\n", k3[0]);

  vector_scale(n, 1.0, k1, t1); 
  vector_scale(n, 4.0, k2, t2); 
  vector_scale(n, 1.0, k3, t3); 
  vector_sum(n, t1, t2, t4); vector_sum(n, t3, t4, t5); 
  vector_scale(n, h/6.0, t5, t6); 
  // printf("c: t6[0] = %g\n", t6[0]);
  vector_sum(n, y, t6, yout); 
  
  return 0;
}

int Runge_Kutta_3_regime(int n, void (*f)(double,double *,double *,double *,int *,double *,double *,double *,double *), 
                         double *p, double *fld, double *d, int *r, double *ext, double *extev, 
                         double *y, double x0, double h, double *yout, 
                         double *k1, double *k2, double *k3, 
                         double *t1, double *t2, double *t3, double *t4, double *t5, double *t6)
{
  double h2;

  if (n == 0)
    {
      return 0;
    }

  h2 = 0.5 * h;

  (*f)(x0, p, fld, d, r, ext, extev, y, k1);
  
  vector_scale(n, 1.0, k1, t1); 
  vector_scale(n, h/2.0, t1, t2); 
  vector_sum(n, y, t2, t3);
  (*f)(x0+h2, p, fld, d, r, ext, extev, t3, k2);
  
  vector_scale(n, -1.0, k1, t1); vector_scale(n, 2.0, k2, t2); 
  vector_sum(n, t1, t2, t3); 
  vector_scale(n, h, t3, t4); vector_sum(n, y, t4, t5); 
  (*f)(x0+h, p, fld, d, r, ext, extev, t5, k3);

  vector_scale(n, 1.0, k1, t1); 
  vector_scale(n, 4.0, k2, t2); 
  vector_scale(n, 1.0, k3, t3); 
  vector_sum(n, t1, t2, t4); vector_sum(n, t3, t4, t5); 
  vector_scale(n, h/6.0, t5, t6); 
  vector_sum(n, y, t6, yout); 
  
  return 0;
}
