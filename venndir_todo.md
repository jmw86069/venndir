
# todo on venndir

## textvenn

* label positions are hardcoded, difficult to use with directional counts
* need method to indicate positions that allows directional counts

* Design idea:

   * colored data.frame format
   * one data.frame with labels
   * one data.frame with colors per cell
   * one data.frame indicating whether to inverse the color
   * one data.frame indicating justification per cell

* Next step:

   * arrange signed Venn counts for each label
