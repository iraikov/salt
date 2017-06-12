int rhsfun (double t, double *y, double *dy_out, void **clos) {
  double *p = ((double **)clos)[0];
  dy_out[0] = p[0] * y[0];
  //printf ("t = %g y[0] = %g dy_out[0] = %g\n", t, y[0], dy_out[0]);
  return 0;
}
