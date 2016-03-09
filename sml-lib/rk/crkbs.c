
// Bogacki-Shampine 3(2):
// ds:     <72, [~5,6,8,~9]>
// cs:     [0,0.5,0.75,1]
// bs:     <9, [2,3,4]>
// as:     <1, []>
//         <2, [1]>
//         <4, [0,3]>
//         <9, [2,3,4]>


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

int Bogacki_Shampine_3_2(int, void (*f)(double,double*,double*,double*,double*,double*), 
                         double *p, double *ext, double *extev, 
                         double *y, double x, double h, double *yout, double *err,
                         double *k1, double *k2, double *k3, double *k4, 
                         double *t1, double *t2, double *t3, double *t4, double *t5, double *t6, double *t7);

int Bogacki_Shampine_3_2_regime(int, void (*f)(double,double*,double*,int*,double*,double*,double*,double*), 
                                double *p, double *d, int *r, double *ext, double *extev, 
                                double *y, double x, double h, double *yout, double *err,
                                double *k1, double *k2, double *k3, double *k4, 
                                double *t1, double *t2, double *t3, double *t4, double *t5, double *t6, double *t7);


////////////////////////////////////////////////////////////////////////////////
//  static double Bogacki_Shampine_3_2(int (*f)(double,double*), double *y,   //
//                                                       double x0, double h) //
//                                                                            //
//  Description:                                                              //
//     This routine uses Bogacki-Shampine's embedded 2nd and 3rd order methods//
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

int Bogacki_Shampine_3_2(int n, void (*f)(double,double *,double *,double *,double *,double *), 
                         double *p, double *ext, double *extev, 
                         double *y, double x0, double h, double *yout, double *err,
                         double *k1, double *k2, double *k3, double *k4, 
                         double *t1, double *t2, double *t3, double *t4, double *t5, double *t6, double *t7)
{
  double h2, h3; 

  if (n == 0)
    {
      return 0;
    }

  h2 = 0.5 * h;
  h3 = 0.75 * h;

  //printf("c: h = %g\n", h);
  //printf("c: y[0] = %g\n", y[0]);
  (*f)(x0, p, ext, extev, y, k1);
  //  printf("c: k1[0] = %g\n", k1[0]);
  
  vector_scale(n, 1.0, k1, t1); 
  //  printf("c: t1[0] = %g\n", t1[0]);
  vector_scale(n, h/2.0, t1, t2); 
  vector_sum(n, y, t2, t3);
  //  printf("c: t2[0] = %g\n", t2[0]);
  //  printf("c: k2 = %p\n", k2);
  //  printf("c: k2[0] = %g\n", k2[0]);
  (*f)(x0+h2, p, ext, extev, t3, k2);
  //  printf("c: k2[0] = %g\n", k2[0]);
  // printf("c: k2[0] = %g\n", k2[0]);
  
  vector_scale(n, 3.0, k2, t2); 
  vector_scale(n, h/4.0, t2, t3); vector_sum(n, y, t3, t4); 
  (*f)(x0+h3, p, ext, extev, t4, k3);
  //  printf("c: k3[0] = %g\n", k3[0]);

  vector_scale(n, 2.0, k1, t1); 
  vector_scale(n, 3.0, k2, t2); 
  vector_scale(n, 4.0, k3, t3); 
  //  printf("c: t1[0] = %g\n", t1[0]);
  //  printf("c: t2[0] = %g\n", t2[0]);
  //  printf("c: t3[0] = %g\n", t3[0]);
  vector_sum(n, t1, t2, t4); vector_sum(n, t3, t4, t5); 
  vector_scale(n, h/9.0, t5, t6); vector_sum(n, y, t6, t7); 
  //  printf("c: t4[0] = %g\n", t4[0]);
  //  printf("c: t5[0] = %g\n", t5[0]);
  //  printf("c: t6[0] = %g\n", t6[0]);
  //  printf("c: t7[0] = %g\n", t7[0]);
  (*f)(x0+h, p, ext, extev, t7, k4);
  //  printf("c: k4 = %p\n", k4);
  //  printf("c: k4[0] = %g\n", k4[0]);
  
  vector_sum(n, y, t6, yout); 
  //  printf("c: yout[0] = %g\n", yout[0]);

  vector_scale(n, -5.0, k1, t1); 
  vector_scale(n, 6.0, k2, t2); 
  vector_scale(n, 8.0, k3, t3); 
  vector_scale(n, -9.0, k4, t4); 

  vector_sum(n, t1, t2, t5); vector_sum(n, t3, t4, t6); 
  vector_sum(n, t5, t6, t7); 
  vector_scale(n, h/72.0, t7, err);  

  //printf("c: xn = %g err[0] = %g\n", x0+h, err[0]);
  
  return 0;
}

int Bogacki_Shampine_3_2_regime(int n, void (*f)(double,double *,double *,int *,double *,double *,double *,double *), 
                                double *p, double *d, int *r, double *ext, double *extev, 
                                double *y, double x0, double h, double *yout, double *err,
                                double *k1, double *k2, double *k3, double *k4, 
                                double *t1, double *t2, double *t3, double *t4, double *t5, double *t6, double *t7)
{
  double h2, h3;

  if (n == 0)
    {
      return 0;
    }

  h2 = 0.5 * h;
  h3 = 0.75 * h;

  //printf("c: h = %g\n", h);
  //printf("c: y[0] = %g\n", y[0]);
  (*f)(x0, p, d, r, ext, extev, y, k1);
  //  printf("c: k1[0] = %g\n", k1[0]);
  
  vector_scale(n, 1.0, k1, t1); 
  //  printf("c: t1[0] = %g\n", t1[0]);
  vector_scale(n, h/2.0, t1, t2); 
  vector_sum(n, y, t2, t3);
  //  printf("c: t2[0] = %g\n", t2[0]);
  //  printf("c: k2 = %p\n", k2);
  //  printf("c: k2[0] = %g\n", k2[0]);
  (*f)(x0+h2, p, d, r, ext, extev, t3, k2);
  //  printf("c: k2[0] = %g\n", k2[0]);
  // printf("c: k2[0] = %g\n", k2[0]);
  
  vector_scale(n, 3.0, k2, t2); 
  vector_scale(n, h/4.0, t2, t3); vector_sum(n, y, t3, t4); 
  (*f)(x0+h3, p, d, r, ext, extev, t4, k3);
  //  printf("c: k3[0] = %g\n", k3[0]);

  vector_scale(n, 2.0, k1, t1); 
  vector_scale(n, 3.0, k2, t2); 
  vector_scale(n, 4.0, k3, t3); 
  //  printf("c: t1[0] = %g\n", t1[0]);
  //  printf("c: t2[0] = %g\n", t2[0]);
  //  printf("c: t3[0] = %g\n", t3[0]);
  vector_sum(n, t1, t2, t4); vector_sum(n, t3, t4, t5); 
  vector_scale(n, h/9.0, t5, t6); vector_sum(n, y, t6, t7); 
  //  printf("c: t4[0] = %g\n", t4[0]);
  //  printf("c: t5[0] = %g\n", t5[0]);
  //  printf("c: t6[0] = %g\n", t6[0]);
  //  printf("c: t7[0] = %g\n", t7[0]);
  (*f)(x0+h, p, d, r, ext, extev, t7, k4);
  //  printf("c: k4 = %p\n", k4);
  //  printf("c: k4[0] = %g\n", k4[0]);
  
  vector_sum(n, y, t6, yout); 
  //  printf("c: yout[0] = %g\n", yout[0]);

  vector_scale(n, -5.0, k1, t1); 
  vector_scale(n, 6.0, k2, t2); 
  vector_scale(n, 8.0, k3, t3); 
  vector_scale(n, -9.0, k4, t4); 

  vector_sum(n, t1, t2, t5); vector_sum(n, t3, t4, t6); 
  vector_sum(n, t5, t6, t7); 
  vector_scale(n, h/72.0, t7, err);  

  //printf("c: xn = %g err[0] = %g\n", x0+h, err[0]);
  
  return 0;
}
