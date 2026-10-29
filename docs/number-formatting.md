# Number formatting

Use comma grouping for counts of 1,000 or more in manuscript and README prose,
tables, captions, and other reader-facing summaries: 999; 1,000; 8,818; 16,437.
Keep years and year ranges (2017; 2018–2020), identifiers, DOIs, page numbers,
software versions, and random seeds in their original form. A seed such as 31415
is an identifier, not a count of replications; 9,999 bootstrap draws is a count.

Use a decimal point and retain the stated precision for estimates, percentages,
means, medians, and confidence limits. A mean of 3.46 is not rounded to an integer
because its underlying outcome is a count. Fractional medians are valid. Where
numeric quantities exceed 1,000, group their integer part as well: 1,234.5.
Intervals retain both endpoints at the same precision. Label their units.

Apply formatting when generating presentation artifacts. Keep analytical CSVs,
numeric JSON fields, identifiers, and source records machine-readable. JSON files
explicitly containing presentation macros may contain formatted strings.

Shared helpers are `format_count()` in `scripts/reporting.py` and `R/reporting.R`,
and `format_number()` in `R/reporting.R`. Count helpers reject fractional and
nonfinite values rather than silently truncating them. Use the count helper only
for known count fields; do not infer a number's meaning from its magnitude.
For graph count axes, use `scales::label_comma()`; year axes retain year labels.
The standard Python equivalent for a numeric estimate is `f"{value:,.2f}"`, with
the intended decimal precision. Edit generating code, then rebuild the artifact.
