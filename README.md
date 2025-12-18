# juntar-core

This is a collection of code and documentation for preparation and quality assurance of Jornada's core, long-term datasets. Scripts here assemble data files from various sources, check data quality with various diagnostics, and produce standardized datasets for publication. Contact the Jornada data managers with questions (jornada.data@nmsu.edu)

## Directory Tree

```
|-- analysis                 - Analysis notebooks and scripts
|-- build_scripts            - Build scripts for data files
|   |-- jrn011_npp           -   JRN Study 011:  NPP study
|   |   |-- ds002_quadmeas.R -     Build dataset 002
|   |   `-- (...)            -     ... more scripts
|   |-- jrn262_ecotone       -   JRN Study 262: Ecotone
|   |   `-- ds008_rodent.R   -     Build dataset 008
|   |-- jrn413_csis          -   JRN Study 413: Cross-scale
|   `-- (...)                -   Many more projects...
|-- config                   - Some configurations
|-- py                       - Common Python files
|-- R                        - Common R files
`-- README.md                - This file
```