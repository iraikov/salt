
typedef unsigned long long uint64_t;

void **alloc_rhs_closure(int n)
{
  void **p;
  p = malloc(n * sizeof(void*));
  return p;
}


void free_rhs_closure(void **p)
{
  free(p);
}

void **update_closure_regime_rs (double *p, double *fld, double *ext, double *extev,
                                 double *d, int *r, int *rs, uint64_t *ki, uint64_t *ke,
                                 double *wi, double *fi, double *we, double *fe,
                                 void **clos)
{
  clos[0] = p;
  clos[1] = fld;
  clos[2] = ext;
  clos[3] = extev;
  clos[4] = d;
  clos[5] = r;
  clos[6] = rs;
  clos[7] = ki;
  clos[8] = ke;
  clos[9] = wi;
  clos[10] = fi;
  clos[11] = we;
  clos[12] = fe;
  return clos;
}

void **update_closure_cont_rs (double *p, double *fld, double *ext, double *extev,
                               int *rs, uint64_t *ki, uint64_t *ke,
                               double *wi, double *fi, double *we, double *fe,
                               void **clos)
{
  clos[0] = p;
  clos[1] = fld;
  clos[2] = ext;
  clos[3] = extev;
  clos[4] = rs;
  clos[5] = ki;
  clos[6] = ke;
  clos[7] = wi;
  clos[8] = fi;
  clos[9] = we;
  clos[10] = fe;
  return clos;
}

void **update_closure_regime (double *p, double *fld, double *ext, double *extev,
                              double *d, int *r, 
                              void **clos)
{
  clos[0] = p;
  clos[1] = fld;
  clos[2] = ext;
  clos[3] = extev;
  clos[4] = d;
  clos[5] = r;
  return clos;
}

void **update_closure_cont (double *p, double *fld, double *ext, double *extev,
                            void **clos)
{
  clos[0] = p;
  clos[1] = fld;
  clos[2] = ext;
  clos[3] = extev;
  return clos;
}
  
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
