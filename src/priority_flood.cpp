#include <R.h>
#include <Rinternals.h>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <new>
#include <queue>
#include <stdexcept>
#include <vector>

struct Run {
  int row;
  int first_col;
  int last_col;
  std::uint64_t first_index;
};

struct Cell {
  float height;
  std::uint32_t index;
};

struct HigherCell {
  bool operator()(const Cell& a, const Cell& b) const {
    if (a.height != b.height) return a.height > b.height;
    return a.index > b.index;
  }
};

struct RoutingState {
  int rows;
  int cols;
  std::uint64_t valid_count;
  std::vector<Run> runs;
  std::vector<std::size_t> row_offsets;
  std::vector<float> elevation;
  std::vector<unsigned char> direction;
  std::vector<double> accumulation;

  RoutingState(int nrow, int ncol, std::uint64_t valid)
    : rows(nrow), cols(ncol), valid_count(valid), row_offsets(nrow + 1, 0),
      elevation(valid, std::numeric_limits<float>::quiet_NaN()) {}

  std::uint64_t compact_index(int row, int col) const {
    if (row < 0 || row >= rows || col < 0 || col >= cols)
      return std::numeric_limits<std::uint64_t>::max();
    for (std::size_t i = row_offsets[row]; i < row_offsets[row + 1]; ++i) {
      const Run& run = runs[i];
      if (col < run.first_col) break;
      if (col <= run.last_col)
        return run.first_index + static_cast<std::uint64_t>(col - run.first_col);
    }
    return std::numeric_limits<std::uint64_t>::max();
  }

  bool grid_position(std::uint64_t index, int& row, int& col) const {
    auto hit = std::upper_bound(runs.begin(), runs.end(), index,
      [](std::uint64_t value, const Run& run) { return value < run.first_index; });
    if (hit == runs.begin()) return false;
    --hit;
    std::uint64_t length = static_cast<std::uint64_t>(hit->last_col-hit->first_col+1);
    if (index < hit->first_index || index >= hit->first_index+length) return false;
    row = hit->row;
    col = hit->first_col + static_cast<int>(index-hit->first_index);
    return true;
  }
};

static RoutingState* state_from(SEXP pointer) {
  if (TYPEOF(pointer) != EXTPTRSXP) Rf_error("Invalid routing state.");
  RoutingState* state = static_cast<RoutingState*>(R_ExternalPtrAddr(pointer));
  if (!state) Rf_error("Routing state is no longer available.");
  return state;
}

static void routing_state_finalizer(SEXP pointer) {
  RoutingState* state = static_cast<RoutingState*>(R_ExternalPtrAddr(pointer));
  if (state) delete state;
  R_ClearExternalPtr(pointer);
}

extern "C" SEXP fg_priority_create(SEXP nrow_s, SEXP ncol_s, SEXP valid_s,
                                    SEXP row_s, SEXP first_s, SEXP last_s,
                                    SEXP index_s) {
  int nrow = Rf_asInteger(nrow_s);
  int ncol = Rf_asInteger(ncol_s);
  double valid_d = Rf_asReal(valid_s);
  if (nrow <= 0 || ncol <= 0 || !R_FINITE(valid_d) || valid_d < 1 ||
      valid_d > 4294967295.0)
    Rf_error("Unsupported routing dimensions or valid-cell count.");
  R_xlen_t count = XLENGTH(row_s);
  if (TYPEOF(row_s) != INTSXP || TYPEOF(first_s) != INTSXP ||
      TYPEOF(last_s) != INTSXP || TYPEOF(index_s) != REALSXP ||
      XLENGTH(first_s) != count || XLENGTH(last_s) != count ||
      XLENGTH(index_s) != count || count < 1)
    Rf_error("Invalid compact run index.");

  RoutingState* state = nullptr;
  try {
    state = new RoutingState(nrow, ncol, static_cast<std::uint64_t>(valid_d));
    state->runs.reserve(static_cast<std::size_t>(count));
    int prior_row = -1;
    int prior_last = -1;
    for (R_xlen_t i = 0; i < count; ++i) {
      int row = INTEGER(row_s)[i] - 1;
      int first = INTEGER(first_s)[i] - 1;
      int last = INTEGER(last_s)[i] - 1;
      double index = REAL(index_s)[i] - 1;
      if (row < 0 || row >= nrow || first < 0 || last < first || last >= ncol ||
          !R_FINITE(index) || index < 0 || index >= valid_d ||
          row < prior_row || (row == prior_row && first <= prior_last))
        throw std::runtime_error("Invalid or unsorted compact run index.");
      state->runs.push_back(Run{row, first, last, static_cast<std::uint64_t>(index)});
      state->row_offsets[row + 1] += 1;
      prior_row = row;
      prior_last = last;
    }
    for (int row = 0; row < nrow; ++row)
      state->row_offsets[row + 1] += state->row_offsets[row];
  } catch (const std::bad_alloc&) {
    delete state;
    Rf_error("Routing state allocation failed after preflight.");
  } catch (const std::exception& e) {
    delete state;
    Rf_error("%s", e.what());
  }

  SEXP pointer = PROTECT(R_MakeExternalPtr(state, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(pointer, routing_state_finalizer, TRUE);
  UNPROTECT(1);
  return pointer;
}

extern "C" SEXP fg_priority_load(SEXP pointer, SEXP first_row_s, SEXP nrows_s,
                                  SEXP values_s) {
  RoutingState* state = state_from(pointer);
  int first_row = Rf_asInteger(first_row_s) - 1;
  int nrows = Rf_asInteger(nrows_s);
  if (TYPEOF(values_s) != REALSXP || first_row < 0 || nrows < 1 ||
      first_row + nrows > state->rows ||
      XLENGTH(values_s) != static_cast<R_xlen_t>(nrows) * state->cols)
    Rf_error("Invalid routing input block.");
  const double* values = REAL(values_s);
  for (int local_row = 0; local_row < nrows; ++local_row) {
    int row = first_row + local_row;
    for (std::size_t j = state->row_offsets[row]; j < state->row_offsets[row + 1]; ++j) {
      const Run& run = state->runs[j];
      for (int col = run.first_col; col <= run.last_col; ++col) {
        double value = values[static_cast<R_xlen_t>(local_row) * state->cols + col];
        if (!R_FINITE(value)) Rf_error("A preflight-valid DEM cell is missing while loading.");
        state->elevation[run.first_index + static_cast<std::uint64_t>(col - run.first_col)] =
          static_cast<float>(value);
      }
    }
  }
  return R_NilValue;
}

extern "C" SEXP fg_priority_fill(SEXP pointer, SEXP outlet_s) {
  RoutingState* state = state_from(pointer);
  if (TYPEOF(outlet_s) != REALSXP || XLENGTH(outlet_s) < 1)
    Rf_error("Supply at least one outlet cell.");
  for (std::uint64_t i = 0; i < state->valid_count; ++i)
    if (!std::isfinite(state->elevation[i])) Rf_error("Routing state was not fully loaded.");

  std::vector<unsigned char> closed;
  std::vector<std::uint32_t> fifo;
  std::vector<Cell> heap_storage;
  try {
    closed.assign(state->valid_count, 0);
    fifo.reserve(state->valid_count);
    heap_storage.reserve(state->valid_count);
  } catch (const std::bad_alloc&) {
    Rf_error("Priority-Flood allocation failed after preflight.");
  }
  std::priority_queue<Cell, std::vector<Cell>, HigherCell> heap(
    HigherCell(), std::move(heap_storage));

  std::uint64_t visited = 0;
  for (R_xlen_t i = 0; i < XLENGTH(outlet_s); ++i) {
    double supplied = REAL(outlet_s)[i];
    if (!R_FINITE(supplied) || supplied < 1 || supplied >
        static_cast<double>(state->rows) * state->cols)
      Rf_error("Outlet cell is outside the DEM.");
    std::uint64_t cell = static_cast<std::uint64_t>(supplied - 1);
    int row = static_cast<int>(cell / state->cols);
    int col = static_cast<int>(cell % state->cols);
    std::uint64_t index = state->compact_index(row, col);
    if (index == std::numeric_limits<std::uint64_t>::max())
      Rf_error("Outlet cell is NoData.");
    if (!closed[index]) {
      closed[index] = 1;
      ++visited;
      heap.push(Cell{state->elevation[index],static_cast<std::uint32_t>(index)});
    }
  }

  const int dr[8] = {-1,-1,-1,0,0,1,1,1};
  const int dc[8] = {-1,0,1,-1,1,-1,0,1};
  std::size_t fifo_head = 0;
  std::uint64_t changed = 0;
  double fill_sum = 0;
  double fill_max = 0;
  std::uint64_t cycles = 0;
  while (!heap.empty() || fifo_head < fifo.size()) {
    Cell current;
    if (fifo_head < fifo.size()) {
      current.index = fifo[fifo_head++];
      current.height = state->elevation[current.index];
    } else {
      current = heap.top();
      heap.pop();
    }
    int row = 0;
    int col = 0;
    if (!state->grid_position(current.index,row,col))
      Rf_error("Priority-Flood compact index is invalid.");
    for (int k = 0; k < 8; ++k) {
      int next_row = row + dr[k];
      int next_col = col + dc[k];
      std::uint64_t index = state->compact_index(next_row, next_col);
      if (index == std::numeric_limits<std::uint64_t>::max() || closed[index]) continue;
      closed[index] = 1;
      ++visited;
      float original = state->elevation[index];
      if (original <= current.height) {
        if (original < current.height) {
          double amount = static_cast<double>(current.height) - original;
          state->elevation[index] = current.height;
          ++changed;
          fill_sum += amount;
          fill_max = std::max(fill_max, amount);
        }
        fifo.push_back(static_cast<std::uint32_t>(index));
      } else {
        heap.push(Cell{original,static_cast<std::uint32_t>(index)});
      }
    }
    if ((++cycles & 1048575ULL) == 0) R_CheckUserInterrupt();
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, 5));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 5));
  const char* labels[5] = {"visited_cells","changed_cells","fill_sum","maximum_fill","outlet_cells"};
  for (int i = 0; i < 5; ++i) SET_STRING_ELT(names,i,Rf_mkChar(labels[i]));
  SET_VECTOR_ELT(result,0,Rf_ScalarReal(static_cast<double>(visited)));
  SET_VECTOR_ELT(result,1,Rf_ScalarReal(static_cast<double>(changed)));
  SET_VECTOR_ELT(result,2,Rf_ScalarReal(fill_sum));
  SET_VECTOR_ELT(result,3,Rf_ScalarReal(fill_max));
  SET_VECTOR_ELT(result,4,Rf_ScalarInteger(static_cast<int>(XLENGTH(outlet_s))));
  Rf_setAttrib(result,R_NamesSymbol,names);
  UNPROTECT(2);
  return result;
}

extern "C" SEXP fg_priority_values(SEXP pointer, SEXP first_row_s, SEXP nrows_s) {
  RoutingState* state = state_from(pointer);
  int first_row = Rf_asInteger(first_row_s) - 1;
  int nrows = Rf_asInteger(nrows_s);
  if (first_row < 0 || nrows < 1 || first_row + nrows > state->rows)
    Rf_error("Invalid routing output block.");
  R_xlen_t length = static_cast<R_xlen_t>(nrows) * state->cols;
  SEXP result = PROTECT(Rf_allocVector(REALSXP,length));
  std::fill(REAL(result),REAL(result)+length,NA_REAL);
  for (int local_row = 0; local_row < nrows; ++local_row) {
    int row = first_row + local_row;
    for (std::size_t j = state->row_offsets[row]; j < state->row_offsets[row + 1]; ++j) {
      const Run& run = state->runs[j];
      for (int col = run.first_col; col <= run.last_col; ++col)
        REAL(result)[static_cast<R_xlen_t>(local_row) * state->cols + col] =
          state->elevation[run.first_index + static_cast<std::uint64_t>(col - run.first_col)];
    }
  }
  UNPROTECT(1);
  return result;
}

extern "C" SEXP fg_priority_load_directions(SEXP pointer, SEXP first_row_s,
                                               SEXP nrows_s, SEXP values_s) {
  RoutingState* state = state_from(pointer);
  int first_row = Rf_asInteger(first_row_s) - 1;
  int nrows = Rf_asInteger(nrows_s);
  if (TYPEOF(values_s) != REALSXP || first_row < 0 || nrows < 1 ||
      first_row + nrows > state->rows ||
      XLENGTH(values_s) != static_cast<R_xlen_t>(nrows) * state->cols)
    Rf_error("Invalid flow-direction input block.");
  if (state->direction.empty()) state->direction.assign(state->valid_count, 0);
  const double* values = REAL(values_s);
  for (int local_row = 0; local_row < nrows; ++local_row) {
    int row = first_row + local_row;
    for (std::size_t j = state->row_offsets[row]; j < state->row_offsets[row + 1]; ++j) {
      const Run& run = state->runs[j];
      for (int col = run.first_col; col <= run.last_col; ++col) {
        double value = values[static_cast<R_xlen_t>(local_row) * state->cols + col];
        int direction = static_cast<int>(value);
        if (!R_FINITE(value) || value != direction ||
            !(direction == 0 || direction == 1 || direction == 2 ||
              direction == 4 || direction == 8 || direction == 16 ||
              direction == 32 || direction == 64 || direction == 128))
          Rf_error("Flow directions must use terra D8 codes on every valid DEM cell.");
        state->direction[run.first_index + static_cast<std::uint64_t>(col - run.first_col)] =
          static_cast<unsigned char>(direction);
      }
    }
  }
  return R_NilValue;
}

extern "C" SEXP fg_priority_assign_d8(SEXP pointer, SEXP xres_s, SEXP yres_s,
                                         SEXP outlet_s) {
  RoutingState* state = state_from(pointer);
  double xres = Rf_asReal(xres_s);
  double yres = Rf_asReal(yres_s);
  if (!R_FINITE(xres) || !R_FINITE(yres) || xres <= 0 || yres <= 0)
    Rf_error("Grid resolution must be finite and positive.");
  if (TYPEOF(outlet_s) != REALSXP || XLENGTH(outlet_s) < 1)
    Rf_error("Supply at least one outlet cell.");

  const int dr[8] = {-1,-1,-1,0,0,1,1,1};
  const int dc[8] = {-1,0,1,-1,1,-1,0,1};
  const unsigned char d8[8] = {32,64,128,16,1,8,4,2};
  const double diagonal = std::sqrt(xres*xres+yres*yres);
  const double distance[8] = {diagonal,yres,diagonal,xres,xres,diagonal,yres,diagonal};
  state->direction.assign(state->valid_count,0);
  std::uint64_t directed = 0;
  std::uint64_t flat_or_pit = 0;

  for (std::uint64_t index = 0; index < state->valid_count; ++index) {
    int row = 0;
    int col = 0;
    if (!state->grid_position(index,row,col))
      Rf_error("Compact routing index is invalid.");
    double best_slope = 0;
    unsigned char selected = 0;
    for (int k = 0; k < 8; ++k) {
      std::uint64_t neighbor = state->compact_index(row+dr[k],col+dc[k]);
      if (neighbor == std::numeric_limits<std::uint64_t>::max()) continue;
      double drop = static_cast<double>(state->elevation[index])-
        static_cast<double>(state->elevation[neighbor]);
      double slope = drop/distance[k];
      if (slope > best_slope) {
        best_slope = slope;
        selected = d8[k];
      }
    }
    state->direction[index] = selected;
    if (selected == 0) ++flat_or_pit; else ++directed;
  }

  for (R_xlen_t i = 0; i < XLENGTH(outlet_s); ++i) {
    double supplied = REAL(outlet_s)[i];
    if (!R_FINITE(supplied) || supplied < 1 || supplied >
        static_cast<double>(state->rows)*state->cols || supplied != std::floor(supplied))
      Rf_error("Outlet cell is outside the DEM.");
    std::uint64_t cell = static_cast<std::uint64_t>(supplied-1);
    std::uint64_t index = state->compact_index(static_cast<int>(cell/state->cols),
      static_cast<int>(cell%state->cols));
    if (index == std::numeric_limits<std::uint64_t>::max())
      Rf_error("Outlet cell is NoData.");
    if (state->direction[index] != 0) {
      state->direction[index] = 0;
      --directed;
      ++flat_or_pit;
    }
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP,2));
  SEXP names = PROTECT(Rf_allocVector(STRSXP,2));
  SET_STRING_ELT(names,0,Rf_mkChar("directed_cells"));
  SET_STRING_ELT(names,1,Rf_mkChar("flat_or_pit_cells"));
  SET_VECTOR_ELT(result,0,Rf_ScalarReal(static_cast<double>(directed)));
  SET_VECTOR_ELT(result,1,Rf_ScalarReal(static_cast<double>(flat_or_pit)));
  Rf_setAttrib(result,R_NamesSymbol,names);
  UNPROTECT(2);
  return result;
}

extern "C" SEXP fg_priority_resolve_flats(SEXP pointer, SEXP outlet_s) {
  RoutingState* state = state_from(pointer);
  if (state->direction.size() != state->valid_count)
    Rf_error("Routing directions were not loaded.");
  if (TYPEOF(outlet_s) != REALSXP || XLENGTH(outlet_s) < 1)
    Rf_error("Supply at least one outlet cell.");
  if (state->valid_count > std::numeric_limits<std::uint32_t>::max())
    Rf_error("Flat resolution exceeds compact-index capacity.");

  const int dr[8] = {-1,-1,-1,0,0,1,1,1};
  const int dc[8] = {-1,0,1,-1,1,-1,0,1};
  const unsigned char d8[8] = {32,64,128,16,1,8,4,2};
  std::vector<std::uint32_t> outlets;
  outlets.reserve(static_cast<std::size_t>(XLENGTH(outlet_s)));
  for (R_xlen_t i = 0; i < XLENGTH(outlet_s); ++i) {
    double supplied = REAL(outlet_s)[i];
    if (!R_FINITE(supplied) || supplied < 1 || supplied >
        static_cast<double>(state->rows) * state->cols)
      Rf_error("Outlet cell is outside the DEM.");
    std::uint64_t cell = static_cast<std::uint64_t>(supplied - 1);
    int row = static_cast<int>(cell / state->cols);
    int col = static_cast<int>(cell % state->cols);
    std::uint64_t index = state->compact_index(row, col);
    if (index == std::numeric_limits<std::uint64_t>::max())
      Rf_error("Outlet cell is NoData.");
    outlets.push_back(static_cast<std::uint32_t>(index));
    state->direction[index] = 255; // Defined terminal while resolving its flat.
  }

  std::vector<std::uint32_t> labels(state->valid_count, 0);
  std::vector<std::int32_t> mask(state->valid_count, 0);
  std::vector<std::uint32_t> queue;
  try {
    queue.reserve(state->valid_count);
  } catch (const std::bad_alloc&) {
    Rf_error("Flat-resolution allocation failed after preflight.");
  }

  auto neighbor_index = [&](std::uint32_t index, int k) -> std::uint64_t {
    const Run* containing = nullptr;
    int row = 0;
    int col = 0;
    // Compact indices are ordered by row; locate the full-grid cell through runs.
    auto hit = std::upper_bound(state->runs.begin(), state->runs.end(), index,
      [](std::uint32_t value, const Run& run) { return value < run.first_index; });
    if (hit != state->runs.begin()) {
      --hit;
      std::uint64_t length = static_cast<std::uint64_t>(hit->last_col-hit->first_col+1);
      if (index >= hit->first_index && index < hit->first_index+length) containing = &*hit;
    }
    if (!containing) return std::numeric_limits<std::uint64_t>::max();
    row = containing->row;
    col = containing->first_col + static_cast<int>(index-containing->first_index);
    return state->compact_index(row+dr[k], col+dc[k]);
  };

  auto is_low_edge = [&](std::uint32_t index) {
    if (state->direction[index] == 0) return false;
    for (int k = 0; k < 8; ++k) {
      std::uint64_t n = neighbor_index(index,k);
      if (n != std::numeric_limits<std::uint64_t>::max() &&
          state->direction[n] == 0 && state->elevation[n] == state->elevation[index])
        return true;
    }
    return false;
  };
  auto is_high_edge = [&](std::uint32_t index) {
    if (state->direction[index] != 0) return false;
    for (int k = 0; k < 8; ++k) {
      std::uint64_t n = neighbor_index(index,k);
      if (n != std::numeric_limits<std::uint64_t>::max() &&
          state->elevation[n] > state->elevation[index]) return true;
    }
    return false;
  };

  std::uint32_t label_count = 0;
  for (std::uint64_t raw = 0; raw < state->valid_count; ++raw) {
    std::uint32_t seed = static_cast<std::uint32_t>(raw);
    if (!is_low_edge(seed) || labels[seed] != 0) continue;
    ++label_count;
    queue.clear();
    queue.push_back(seed);
    float elevation = state->elevation[seed];
    for (std::size_t head = 0; head < queue.size(); ++head) {
      std::uint32_t cell = queue[head];
      if (labels[cell] != 0 || state->elevation[cell] != elevation) continue;
      labels[cell] = label_count;
      for (int k = 0; k < 8; ++k) {
        std::uint64_t n = neighbor_index(cell,k);
        if (n != std::numeric_limits<std::uint64_t>::max() && labels[n] == 0 &&
            state->elevation[n] == elevation) queue.push_back(static_cast<std::uint32_t>(n));
      }
    }
  }

  std::vector<std::uint32_t> flat_height(label_count+1,0);
  queue.clear();
  for (std::uint64_t raw = 0; raw < state->valid_count; ++raw) {
    std::uint32_t cell = static_cast<std::uint32_t>(raw);
    if (labels[cell] != 0 && is_high_edge(cell)) queue.push_back(cell);
  }
  std::size_t head = 0;
  std::uint32_t level = 1;
  while (head < queue.size()) {
    std::size_t end = queue.size();
    while (head < end) {
      std::uint32_t cell = queue[head++];
      if (mask[cell] > 0) continue;
      mask[cell] = static_cast<std::int32_t>(level);
      flat_height[labels[cell]] = level;
      for (int k = 0; k < 8; ++k) {
        std::uint64_t n = neighbor_index(cell,k);
        if (n != std::numeric_limits<std::uint64_t>::max() &&
            labels[n] == labels[cell] && state->direction[n] == 0 && mask[n] == 0)
          queue.push_back(static_cast<std::uint32_t>(n));
      }
    }
    ++level;
  }

  for (std::uint64_t i = 0; i < state->valid_count; ++i) mask[i] = -mask[i];
  queue.clear();
  for (std::uint64_t raw = 0; raw < state->valid_count; ++raw) {
    std::uint32_t cell = static_cast<std::uint32_t>(raw);
    if (labels[cell] != 0 && is_low_edge(cell)) queue.push_back(cell);
  }
  head = 0;
  level = 1;
  while (head < queue.size()) {
    std::size_t end = queue.size();
    while (head < end) {
      std::uint32_t cell = queue[head++];
      if (mask[cell] > 0) continue;
      if (mask[cell] < 0)
        mask[cell] = static_cast<std::int32_t>(flat_height[labels[cell]]) + mask[cell] +
          static_cast<std::int32_t>(2*level);
      else mask[cell] = static_cast<std::int32_t>(2*level);
      for (int k = 0; k < 8; ++k) {
        std::uint64_t n = neighbor_index(cell,k);
        if (n != std::numeric_limits<std::uint64_t>::max() &&
            labels[n] == labels[cell] && state->direction[n] == 0 && mask[n] <= 0)
          queue.push_back(static_cast<std::uint32_t>(n));
      }
    }
    ++level;
  }

  std::uint64_t resolved = 0;
  for (std::uint64_t raw = 0; raw < state->valid_count; ++raw) {
    std::uint32_t cell = static_cast<std::uint32_t>(raw);
    if (state->direction[cell] != 0 || labels[cell] == 0) continue;
    std::int32_t best = mask[cell];
    unsigned char selected = 0;
    for (int k = 0; k < 8; ++k) {
      std::uint64_t n = neighbor_index(cell,k);
      if (n != std::numeric_limits<std::uint64_t>::max() &&
          labels[n] == labels[cell] && mask[n] < best) {
        best = mask[n];
        selected = d8[k];
      }
    }
    if (selected != 0) {
      state->direction[cell] = selected;
      ++resolved;
    }
  }
  for (std::uint32_t outlet : outlets) state->direction[outlet] = 0;
  std::uint64_t unresolved = 0;
  for (std::uint64_t i = 0; i < state->valid_count; ++i)
    if (state->direction[i] == 0 &&
        std::find(outlets.begin(),outlets.end(),static_cast<std::uint32_t>(i)) == outlets.end())
      ++unresolved;

  SEXP result = PROTECT(Rf_allocVector(VECSXP,4));
  SEXP names = PROTECT(Rf_allocVector(STRSXP,4));
  const char* labels_out[4] = {"flat_labels","resolved_cells","unresolved_cells","maximum_mask"};
  for (int i = 0; i < 4; ++i) SET_STRING_ELT(names,i,Rf_mkChar(labels_out[i]));
  std::int32_t maximum_mask = 0;
  for (std::int32_t value : mask) maximum_mask = std::max(maximum_mask,value);
  SET_VECTOR_ELT(result,0,Rf_ScalarReal(label_count));
  SET_VECTOR_ELT(result,1,Rf_ScalarReal(static_cast<double>(resolved)));
  SET_VECTOR_ELT(result,2,Rf_ScalarReal(static_cast<double>(unresolved)));
  SET_VECTOR_ELT(result,3,Rf_ScalarReal(maximum_mask));
  Rf_setAttrib(result,R_NamesSymbol,names);
  UNPROTECT(2);
  return result;
}

extern "C" SEXP fg_priority_direction_values(SEXP pointer, SEXP first_row_s,
                                                SEXP nrows_s) {
  RoutingState* state = state_from(pointer);
  int first_row = Rf_asInteger(first_row_s) - 1;
  int nrows = Rf_asInteger(nrows_s);
  if (state->direction.size() != state->valid_count || first_row < 0 || nrows < 1 ||
      first_row + nrows > state->rows) Rf_error("Invalid flow-direction output block.");
  R_xlen_t length = static_cast<R_xlen_t>(nrows) * state->cols;
  SEXP result = PROTECT(Rf_allocVector(REALSXP,length));
  std::fill(REAL(result),REAL(result)+length,NA_REAL);
  for (int local_row = 0; local_row < nrows; ++local_row) {
    int row = first_row + local_row;
    for (std::size_t j = state->row_offsets[row]; j < state->row_offsets[row + 1]; ++j) {
      const Run& run = state->runs[j];
      for (int col = run.first_col; col <= run.last_col; ++col)
        REAL(result)[static_cast<R_xlen_t>(local_row) * state->cols + col] =
          state->direction[run.first_index + static_cast<std::uint64_t>(col-run.first_col)];
    }
  }
  UNPROTECT(1);
  return result;
}

extern "C" SEXP fg_priority_accumulate(SEXP pointer) {
  RoutingState* state = state_from(pointer);
  if (state->direction.size() != state->valid_count)
    Rf_error("Routing directions were not assigned.");
  if (state->valid_count > std::numeric_limits<std::uint32_t>::max())
    Rf_error("Flow accumulation exceeds compact-index capacity.");

  std::vector<std::uint32_t> indegree;
  std::vector<std::uint32_t> queue;
  try {
    state->accumulation.assign(state->valid_count,1.0);
    indegree.assign(state->valid_count,0);
    queue.reserve(state->valid_count);
  } catch (const std::bad_alloc&) {
    state->accumulation.clear();
    Rf_error("Flow-accumulation allocation failed after preflight.");
  }

  auto downstream = [&](std::uint64_t index) -> std::uint64_t {
    int row = 0;
    int col = 0;
    if (!state->grid_position(index,row,col))
      return std::numeric_limits<std::uint64_t>::max();
    int next_row = row;
    int next_col = col;
    switch (state->direction[index]) {
      case 1: next_col += 1; break;
      case 2: next_row += 1; next_col += 1; break;
      case 4: next_row += 1; break;
      case 8: next_row += 1; next_col -= 1; break;
      case 16: next_col -= 1; break;
      case 32: next_row -= 1; next_col -= 1; break;
      case 64: next_row -= 1; break;
      case 128: next_row -= 1; next_col += 1; break;
      case 0: return std::numeric_limits<std::uint64_t>::max();
      default: Rf_error("Flow directions contain an unsupported value.");
    }
    return state->compact_index(next_row,next_col);
  };

  std::uint64_t terminals = 0;
  for (std::uint64_t index = 0; index < state->valid_count; ++index) {
    if (state->direction[index] == 0) {
      ++terminals;
      continue;
    }
    std::uint64_t next = downstream(index);
    if (next == std::numeric_limits<std::uint64_t>::max())
      Rf_error("A flow direction leaves the valid terrain domain.");
    if (indegree[next] == std::numeric_limits<std::uint32_t>::max())
      Rf_error("Flow-accumulation indegree overflow.");
    ++indegree[next];
  }
  for (std::uint64_t index = 0; index < state->valid_count; ++index)
    if (indegree[index] == 0) queue.push_back(static_cast<std::uint32_t>(index));

  std::uint64_t processed = 0;
  double maximum = 1;
  for (std::size_t head = 0; head < queue.size(); ++head) {
    std::uint32_t index = queue[head];
    ++processed;
    maximum = std::max(maximum,state->accumulation[index]);
    std::uint64_t next = downstream(index);
    if (next != std::numeric_limits<std::uint64_t>::max()) {
      state->accumulation[next] += state->accumulation[index];
      if (--indegree[next] == 0) queue.push_back(static_cast<std::uint32_t>(next));
    }
    if ((processed & 1048575ULL) == 0) R_CheckUserInterrupt();
  }
  if (processed != state->valid_count) {
    state->accumulation.clear();
    Rf_error("Flow directions contain a cycle.");
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP,3));
  SEXP names = PROTECT(Rf_allocVector(STRSXP,3));
  SET_STRING_ELT(names,0,Rf_mkChar("processed_cells"));
  SET_STRING_ELT(names,1,Rf_mkChar("terminal_cells"));
  SET_STRING_ELT(names,2,Rf_mkChar("maximum_accumulation"));
  SET_VECTOR_ELT(result,0,Rf_ScalarReal(static_cast<double>(processed)));
  SET_VECTOR_ELT(result,1,Rf_ScalarReal(static_cast<double>(terminals)));
  SET_VECTOR_ELT(result,2,Rf_ScalarReal(maximum));
  Rf_setAttrib(result,R_NamesSymbol,names);
  UNPROTECT(2);
  return result;
}

extern "C" SEXP fg_priority_accumulation_values(SEXP pointer, SEXP first_row_s,
                                                   SEXP nrows_s) {
  RoutingState* state = state_from(pointer);
  int first_row = Rf_asInteger(first_row_s)-1;
  int nrows = Rf_asInteger(nrows_s);
  if (state->accumulation.size() != state->valid_count || first_row < 0 || nrows < 1 ||
      first_row+nrows > state->rows) Rf_error("Invalid flow-accumulation output block.");
  R_xlen_t length = static_cast<R_xlen_t>(nrows)*state->cols;
  SEXP result = PROTECT(Rf_allocVector(REALSXP,length));
  std::fill(REAL(result),REAL(result)+length,NA_REAL);
  for (int local_row = 0; local_row < nrows; ++local_row) {
    int row = first_row+local_row;
    for (std::size_t j = state->row_offsets[row]; j < state->row_offsets[row+1]; ++j) {
      const Run& run = state->runs[j];
      for (int col = run.first_col; col <= run.last_col; ++col)
        REAL(result)[static_cast<R_xlen_t>(local_row)*state->cols+col] =
          state->accumulation[run.first_index+static_cast<std::uint64_t>(col-run.first_col)];
    }
  }
  UNPROTECT(1);
  return result;
}
