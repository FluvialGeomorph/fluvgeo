#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

extern SEXP fg_priority_create(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP fg_priority_load(SEXP, SEXP, SEXP, SEXP);
extern SEXP fg_priority_fill(SEXP, SEXP);
extern SEXP fg_priority_values(SEXP, SEXP, SEXP);
extern SEXP fg_priority_load_directions(SEXP, SEXP, SEXP, SEXP);
extern SEXP fg_priority_assign_d8(SEXP, SEXP, SEXP, SEXP);
extern SEXP fg_priority_resolve_flats(SEXP, SEXP);
extern SEXP fg_priority_direction_values(SEXP, SEXP, SEXP);
extern SEXP fg_priority_accumulate(SEXP);
extern SEXP fg_priority_accumulation_values(SEXP, SEXP, SEXP);

static const R_CallMethodDef call_methods[] = {
  {"fg_priority_create", (DL_FUNC) &fg_priority_create, 7},
  {"fg_priority_load", (DL_FUNC) &fg_priority_load, 4},
  {"fg_priority_fill", (DL_FUNC) &fg_priority_fill, 2},
  {"fg_priority_values", (DL_FUNC) &fg_priority_values, 3},
  {"fg_priority_load_directions", (DL_FUNC) &fg_priority_load_directions, 4},
  {"fg_priority_assign_d8", (DL_FUNC) &fg_priority_assign_d8, 4},
  {"fg_priority_resolve_flats", (DL_FUNC) &fg_priority_resolve_flats, 2},
  {"fg_priority_direction_values", (DL_FUNC) &fg_priority_direction_values, 3},
  {"fg_priority_accumulate", (DL_FUNC) &fg_priority_accumulate, 1},
  {"fg_priority_accumulation_values", (DL_FUNC) &fg_priority_accumulation_values, 3},
  {NULL, NULL, 0}
};

void attribute_visible R_init_fluvgeo(DllInfo *dll) {
  R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, FALSE);
}
