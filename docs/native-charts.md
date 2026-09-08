# Native chart document protocol, version 1

`render_chart` presents data in a capable frontend. It does not run Swift,
read files, fetch URLs, or export files. The macOS application offers native
interaction and user-initiated PNG export. Tool availability must be enabled
explicitly by the frontend; the operating system alone is not authorization.

Native hosts opt in with `ha_engine_set_chart_rendering_enabled(engine, 1)`.
New engines default to disabled; changes affect subsequently started turns,
not an already running turn. The ordinary CLI does not register this tool.

## Example tool arguments

```json
{
  "version": 1,
  "kind": "line",
  "title": "Monthly revenue",
  "subtitle": "Actual booked revenue, excluding tax",
  "x_axis": {"type": "timestamp", "label": "Month"},
  "y_axis": {"type": "number", "label": "Revenue", "unit": "EUR"},
  "series": [
    {
      "name": "Subscriptions",
      "points": [
        {"x": "2026-07-01T00:00:00Z", "y": 12000},
        {"x": "2026-08-01T00:00:00Z", "y": 14500},
        {"x": "2026-09-01T00:00:00Z", "y": 13900}
      ]
    }
  ]
}
```

Use `line` or `area` for trends, `bar` for comparisons, and `scatter` for
relationships. Give axes meaningful labels and units. Category x values are
strings; numeric x values are JSON numbers; timestamp x values are UTC strings
of the form `YYYY-MM-DDTHH:mm:ss[.fraction]Z`, with 1–3 fractional digits
when present (millisecond precision).
Years range from 0001 through 9999; leap seconds are not supported.
All y values are finite JSON numbers. Numeric and timestamp x coordinates in
each line/area series must be strictly increasing and unique. Aggregate timestamps
more precise than milliseconds before rendering. Category order
is input order. No null/missing observations are supported: do not invent
replacements, and disclose omitted data or aggregation in the subtitle.

There must be 1–8 uniquely named nonempty series and no more than 2,000 total
points. The UTF-8 argument document is limited to 256 KiB. Every text field is
limited to 200 Unicode scalar values. Titles, series names and category values
must not be blank. Optional subtitle, label, unit, and numeric y-axis type may
be omitted or null. Unknown object fields and unsupported versions are errors.

## Persistent result

The successful tool result is:

```json
{"type":"chart","chart":{"version":1,"...":"..."},"summary":"Rendered Monthly revenue — line chart; 1 series, 3 points."}
```

The `chart` field contains the complete validated argument document. This
versioned envelope is durable tool output, not an executable request or a
native-language boundary convenience. Native projections extract a distinct
complete chart attachment before truncating diagnostic output previews.
They must never reconstruct a chart from tool arguments or truncated output.
The summary provides a readable fallback for unsupported consumers. Failed,
malformed, and unknown-version results do not become charts.

HAEV v1 tool-finish frames set flag bit 3 and append the complete chart
document as a third UTF-8 field after call ID and readable output. The field
is independently bounded and never preview-truncated. Native session history
projects the same document as `chart`; load-around consumers read the durable
result envelope from full response items. Both paths associate results with
the originating `render_chart` call ID, not with similarly shaped output
from unrelated tools.
