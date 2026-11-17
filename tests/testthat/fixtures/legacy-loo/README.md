# Legacy Leave-One-Out cache fixtures

These protobuf fixtures retain the combined `outHtmlLeave1Out` result type
and visibility metadata from synthetic 13-study analyses generated before
the independent plot and table controls. The old HTML is replaced with a
constant technical placeholder; scripts, figures, other results and logs
are removed. Tests exercise both legacy selection modes across the six
models and require the current backend to regenerate the selected views
and 13 omission estimates. Tests must not depend on a local `md` directory.
