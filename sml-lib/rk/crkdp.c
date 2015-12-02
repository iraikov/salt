
// Dormand-Prince 5(4):
//
// ds:     <21369600, [26341,0,~90880,790230,~1086939,895488,~534240]>
// cs:     [0,0.2,0.3,0.8,0.888888888889,1,1]
// bs:     <142464, [12985,0,64000,92750,~45927,18656]>
// as:     <1, []>
//         <5, [1]>
//         <40, [3,9]>
//         <45, [44,~168,160]>
//         <6561, [19372,~76080,64448,~1908]>
//         <167904, [477901,~1806240,1495424,46746,~45927]>
//         <142464, [12985,0,64000,92750,~45927,18656]>


#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

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

int Dormand_Prince_5_4(int, int (*f)(double,double*,double*,double*), double *p, double *y, double x, double h, double *yout, double *err,
                       double *k1, double *k2, double *k3, double *k4, double *k5, double *k6, 
                       double *t1, double *t2, double *t3, double *t4, double *t5, double *t6, double *t7, double *t8, double *t9, double *t10, double *t11, double *t12);


////////////////////////////////////////////////////////////////////////////////
//  static double Dormand_Prince_5_4(int (*f)(double,double*), double *y,     //
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

int Dormand_Prince_5_4(int n, int (*f)(double,double *,double *,double *), double *p, double *y, double x0, double h, double *yout, double *err,
                       double *k1, double *k2, double *k3, double *k4, double *k5, double *k6, 
                       double *t1, double *t2, double *t3, double *t4, double *t5, double *t6, double *t7, double *t8, double *t9, double *t10, double *t11, double *t12)
{
  double h2 = 0.2 * h;
  double h3 = 0.3 * h;
  double h4 = 0.8 * h;
  double h5 = 0.888888888889 * h;

  //printf("c: y[0] = %g\n", y[0]);
  assert ((*f)(x0, p, y, k1) == 0);
  //  printf("c: k1[0] = %g\n", k1[0]);
  
  vector_scale(n, 1.0, k1, t1); 
  //  printf("c: t1[0] = %g\n", t1[0]);
  vector_scale(n, h/5.0, t1, t2); 
  vector_sum(n, y, t2, t3);
  //  printf("c: t2[0] = %g\n", t2[0]);
  //  printf("c: k2 = %p\n", k2);
  //  printf("c: k2[0] = %g\n", k2[0]);
  assert ((*f)(x0+h2, p, t3, k2) == 0);
  //  printf("c: k2[0] = %g\n", k2[0]);
  // printf("c: k2[0] = %g\n", k2[0]);
  
  vector_scale(n, 3.0, k1, t1); 
  vector_scale(n, 9.0, k2, t2); 
  vector_sum(n, t1, t2, t3); vector_scale(n, h/40.0, t3, t4); vector_sum(n, y, t4, t5); 
  assert ((*f)(x0+h3, p, t5, k3) == 0);
  //  printf("c: k3[0] = %g\n", k3[0]);

  vector_scale(n, 44.0, k1, t1); 
  vector_scale(n, -168.0, k2, t2); 
  vector_scale(n, 160.0, k3, t3); 
  //  printf("c: t1[0] = %g\n", t1[0]);
  //  printf("c: t2[0] = %g\n", t2[0]);
  //  printf("c: t3[0] = %g\n", t3[0]);
  vector_sum(n, t1, t2, t4); vector_sum(n, t3, t4, t5); 
  vector_scale(n, h/45.0, t5, t6); vector_sum(n, y, t6, t7); 
  //  printf("c: t4[0] = %g\n", t4[0]);
  //  printf("c: t5[0] = %g\n", t5[0]);
  //  printf("c: t6[0] = %g\n", t6[0]);
  //  printf("c: t7[0] = %g\n", t7[0]);
  assert ((*f)(x0+h4, p, t7, k4) == 0);
  //  printf("c: k4 = %p\n", k4);
  //  printf("c: k4[0] = %g\n", k4[0]);
  
  vector_scale(n, 19372.0, k1, t1); 
  vector_scale(n, -76080.0, k2, t2); 
  vector_scale(n, 64448.0, k3, t3); 
  vector_scale(n, -1908.0, k4, t4); 
  vector_sum(n, t1, t2, t5); vector_sum(n, t3, t4, t6); vector_sum(n, t5, t6, t7); 
  vector_scale(n, h/6561.0, t7, t8);  
  //  printf("c: t8[0] = %g\n", t8[0]);
  vector_sum(n, y, t8, t9); 
  //  printf("c: t9[0] = %g\n", t9[0]);
  assert ((*f)(x0+h5, p, t9, k5) == 0);
  //  printf("c: k5[0] = %g\n", k5[0]);

  vector_scale(n, 477901.0, k1, t1); 
  vector_scale(n, -1806240.0, k2, t2); 
  vector_scale(n, 1495424.0, k3, t3); 
  vector_scale(n, 46746.0, k4, t4); 
  vector_scale(n, -45927.0, k5, t5); 
  vector_sum(n, t1, t2, t6); vector_sum(n, t3, t4, t7); vector_sum(n, t5, t6, t8); vector_sum(n, t7, t8, t9); 
  vector_scale(n, h/167904.0, t9, t10);  
  vector_sum(n, y, t10, t11); 
  assert ((*f)(x0+h, p, t11, k6 ) == 0);
  //  printf("c: k6[0] = %g\n", k6[0]);

  vector_scale(n, 12985.0, k1, t1); 
  vector_scale(n, 64000.0, k3, t3); 
  vector_scale(n, 92750.0, k4, t4); 
  vector_scale(n, -45927.0, k5, t5); 
  vector_scale(n, 18656.0, k6, t6); 
  vector_sum(n, t1, t3, t7); vector_sum(n, t4, t5, t8); vector_sum(n, t6, t7, t9); vector_sum(n, t8, t9, t10); 
  vector_scale(n, h/142464.0, t10, t11);  
  vector_sum(n, y, t11, yout); 
  //  printf("c: yout[0] = %g\n", yout[0]);

  vector_scale(n, 26341.0, k1, t1); 
  vector_scale(n, -90880.0, k2, t2); 
  vector_scale(n, 790230.0, k3, t3); 
  vector_scale(n, -1086939.0, k4, t4); 
  vector_scale(n, 895488.0, k5, t5); 
  vector_scale(n, -534240.0, k6, t6); 
  vector_sum(n, t1, t2, t7); vector_sum(n, t3, t4, t8); vector_sum(n, t5, t6, t9); vector_sum(n, t7, t8, t10); vector_sum(n, t9, t10, t11); 
  vector_scale(n, 1.0/21369600.0, t11, err);  

  //  printf("c: err[0] = %g\n", err[0]);
  
  return 0;
}
