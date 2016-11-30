int rhsfun (double t, double *p, double *fld, double *ext, double *extev, double *y, double *dy_out) {
  dy_out[0] = p[0] * y[0];
  //printf ("t = %g y[0] = %g dy_out[0] = %g\n", t, y[0], dy_out[0]);
  return 0;
}
