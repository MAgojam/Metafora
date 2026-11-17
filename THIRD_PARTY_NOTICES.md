# Third-party notices

## D3 7.9.0

`inst/extdata/d3.min.js` includes D3 7.9.0, Copyright 2010–2023 Mike Bostock.
It is distributed under the ISC license. The complete copyright and permission
notice is retained in `inst/licenses/D3-LICENSE.txt` and distributed in the module.

## R dependencies

The module uses the R packages declared in DESCRIPTION, including metafor,
TOSTER, puniform, poibin, BiasedUrn, gsl, ggplot2, jmvcore and their dependencies.
Each dependency retains its own authorship and license metadata. Platform JMO
builds are produced by the jamovi compiler; inspect their included dependency
metadata rather than treating all components as licensed solely by Metafora.

## Metafora runtime assets

The `metaforad3.min.js`, `metaforad3.min.css` and `metaforahtml.min.js` assets
are supplied as part of this module. Their editable sources remain maintained
separately; the included runtime bundles are sufficient for module compilation.
The module's GPL-2-or-later terms do not replace D3's ISC notice or the licenses
of the R dependencies.


## Native scientific libraries in macOS packages

When a macOS R dependency links to GNU GSL libraries not supplied by jamovi,
the build includes the required native libraries beside that dependency and
relocates their loader paths before packaging. The GNU GPL version 3 text is
retained as `GSL-COPYING` in that library directory. This does not replace
the separate R-package license metadata. GNU GSL source is available from
https://www.gnu.org/software/gsl/.
