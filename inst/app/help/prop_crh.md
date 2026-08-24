## Proportion at Collision Risk Height (CRH)

This section summarises how much of each Flight Height Distribution (FHD) falls
within the **Collision Risk Height (CRH) envelope** - the vertical area swept by
a turbine's rotor.

The CRH envelope is set via the **Turbine Parameters** button in the
top-right of this card (see below), and applies to both tabs.

---

### Turbine Parameters

Click the **Turbine Parameters** button to configure the rotor geometry:

- **Air Gap (m)** - Tip clearance gap: the distance between the minimum blade tip height and the highest astronomical tide at the site
- **Rotor Radius (m)** - The distance from the axis of rotation to blade tip

The CRH envelope spans **air gap → air gap + 2 × rotor radius**. Both tabs
update automatically when these values change.

---

### Summaries

Displays the **median, 25th, and 75th percentile** of the proportion of birds at
CRH, computed across all posterior/bootstrap draws for each selected FHD. Wider
interquartile ranges indicate greater uncertainty in the FHD estimate.

---

### Air Gap Sensitivity

Shows how the proportion at CRH would change if the turbine were placed at
different heights, stepping in ± fixed increments given the configured air gap.

Use the controls to adjust the output:

- **Metric** - *% Change*: relative change from the configured position;
  *Proportion*: absolute proportion at CRH at each shifted position.
- **Increments** - step size for the height shift (1 m or 5 m).
- **View Mode** - toggle between table and plot views.

In the *% Change* table, positive values (red) indicate more birds at risk when
the turbine is raised; negative values (green) indicate fewer.
