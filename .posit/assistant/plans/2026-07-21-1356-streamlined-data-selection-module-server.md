# Plan: Streamlined Data Selection Module Server

## Goal

Simplify `mod_data_select_server` so that:
- Filters control what is shown in **both** the map and the DT
- Selection can be made via the **map popup button** or a **DT row click**
- Both methods stay in sync with each other
- A single reactive is the source of truth for selection state

---

## Core Principle: One Source of Truth

`selected_ids` (a `reactiveVal`) is the **only** authoritative record of which
entries are currently selected. Nothing reads selection state from the map or
the DT directly — they are purely **outputs** of `selected_ids`.

---

## Reactive Data Flow

```
metadata_tbl + filter inputs (species / method / season)
                      │
                      ▼
              filtered_data  ◄────── single reactive, used everywhere
                 │        │
                 ▼        ▼
            Map markers   DT table   ◄── both re-render when filters change
                 │        │
       map popup │        │ row click
      (Add Entry)│        │ (input$all_metadata_rows_selected)
                 │        │
                 └──┬─────┘
                    ▼
              selected_ids  (reactiveVal — single source of truth)
                 │        │
                 ▼        ▼
         Map marker     DT row
         colours        highlight
         (leafletProxy) (DT proxy selectRows)
```

---

## Step-by-Step Implementation Plan

### 1. `filtered_data` reactive

A single reactive that applies whichever filters are active. Because the filters
use `multiple = TRUE` and may be empty (meaning "show all"), the logic is:

```r
filtered_data <- reactive({
  data <- metadata_tbl
  if (length(input$species) > 0) data <- filter(data, species_id %in% input$species)
  if (length(input$method)  > 0) data <- filter(data, method     %in% input$method)
  if (length(input$season)  > 0) data <- filter(data, season     %in% input$season)
  data
})
```

Empty selection = no filter applied for that dimension (the "All" behaviour,
without needing a sentinel value).

### 2. Map rendering

`renderLeaflet` renders the **base map only once** (tiles, view, options). It
does **not** add markers — that is done entirely by `leafletProxy`.

An `observe` on `filtered_data()` uses `leafletProxy` to:
1. `clearMarkers()` — remove previous markers
2. `addCircleMarkers()` — re-add markers for the filtered set, coloured by
   whether their `fhd_id` is in `selected_ids()`

This means the map always reflects both the current filter and the current
selection simultaneously.

### 3. Map → `selected_ids` (popup button)

No change from the current approach — `Shiny.setInputValue` in the popup
`onclick` fires `input$map_add_btn`. The server toggles the ID in/out of
`selected_ids`:

```r
observeEvent(input$map_add_btn, {
  id      <- input$map_add_btn
  current <- selected_ids()
  selected_ids(if (id %in% current) setdiff(current, id) else c(current, id))
})
```

### 4. DT rendering

`renderDT` renders the table from `filtered_data()`, with `selection = "multiple"`
so rows can be highlighted. The `scrollX` option and hidden columns (`fhd_id`,
`lon`, `lat`, `sf_obj`) remain as-is.

### 5. DT row click → `selected_ids`

```r
observeEvent(input$all_metadata_rows_selected, {
  rows    <- input$all_metadata_rows_selected
  new_ids <- filtered_data()$fhd_id[rows]
  # Preserve selections that are outside the current filter view
  out_of_view <- setdiff(selected_ids(), filtered_data()$fhd_id)
  selected_ids(c(out_of_view, new_ids))
}, ignoreNULL = FALSE)
```

The `out_of_view` step is important: if a user selects entry A under filter X,
then changes the filter so A is no longer visible, A should remain selected.

### 6. `selected_ids` → DT row highlight (sync back)

A DT proxy observes `selected_ids()` and selects the rows corresponding to the
currently selected IDs that are **within** the current filtered view:

```r
dt_proxy <- DT::dataTableProxy("all_metadata")

observe({
  in_view_rows <- which(filtered_data()$fhd_id %in% selected_ids())
  DT::selectRows(dt_proxy, in_view_rows)
})
```

### 7. `selected_ids` → map marker colours (sync back)

The same `observe` on `filtered_data()` that draws markers (step 2) also
colours them, because it already has access to `selected_ids()`. No additional
observer needed.

### 8. Breaking update cycles

The main loop risk is:

> DT row click → `selected_ids` → `selectRows(proxy)` → `input$*_rows_selected`
> fires again → `selected_ids` updates → ...

`DT::selectRows()` called via proxy **does** re-trigger
`input$*_rows_selected`. The guard is in step 5: the observer computes
`new_ids` from the current row selection, and `selected_ids` is only updated
if the derived IDs differ from what is already stored. Because `reactiveVal`
notifies all dependents on **every** assignment (even to the same value), we
add an explicit equality check:

```r
observeEvent(input$all_metadata_rows_selected, {
  rows    <- input$all_metadata_rows_selected %||% integer(0)
  new_ids <- filtered_data()$fhd_id[rows]
  out_of_view <- setdiff(selected_ids(), filtered_data()$fhd_id)
  candidate <- sort(c(out_of_view, new_ids))
  if (!identical(candidate, sort(selected_ids()))) selected_ids(candidate)
}, ignoreNULL = FALSE)
```

Similarly, the `selectRows` observer (step 6) guards with `isolate`:

```r
observe({
  in_view_rows <- which(isolate(filtered_data())$fhd_id %in% selected_ids())
  DT::selectRows(dt_proxy, in_view_rows)
})
```

---

## Things to Remove

- The old per-filter `dplyr::filter` blocks inside `renderDT` (replaced by `filtered_data`)
- The old `all_metadata` output (renamed to match the DT `outputId` already in the UI: keep `all_metadata` but rebuild it cleanly)
- The top-level `observe` that calls `updateSelectizeInput` for season (season choices are static; only species and method need updating from data)

---

## What Stays the Same

- `selected_ids` reactiveVal
- The "Selected Data" card on the right (`show_selected` DT) — continues to
  show `metadata_tbl[fhd_id %in% selected_ids(), ]` unfiltered (all selections
  regardless of current filter view)
- The `go_analysis` button and nav logic
- The module's return value (`selected_data` reactive)
- The popup HTML structure (only the button's onclick attribute)
