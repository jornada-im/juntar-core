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
|-- config.R                 - Some configurations
|-- py                       - Common Python files
|-- R                        - Common R files
`-- README.md                - This file
```

## Preparing and publishing a dataset

1. Run the appropriate build script for the dataset. Be sure to check unique categorical or missing values, substitutions, diagnostic figures, and other QA/QC features of the script.
2. Make any necessary changes to the metadata in `jrn_metabase` (ask Greg if you need an account)
3. Do a dry-run of publishing the dataset using the `publish_dataset` function:

    ```r
    source("publish.R")
    publish_dataset(id, env, data_path, cred_path, dry_run=TRUE, s3_upload=TRUE)
    ```
    Where `id` is the numerical identifier for the dataset (210001001), `env` is the name of the EDI environment to publish to ("development", "staging" or "production"), `data_path` is the path to the data (usually set as a variable in your build script), and `cred_path` is the path to your credentials R file. This will create and validate the EML, but not publish it. If you encounter errors here, you probably have issues in the metadata.
4. Publish the dataset to EDI's staging environment (as appropriate):

    ```r
    publish_dataset(210000001, "staging", output.path, "~/Desktop", dry_run=FALSE, s3_upload=TRUE)
    ```
    Note that `dry_run` is set to `FALSE` now.
5. When you are satisfied with the dataset as it is in the staging environment you can change the EDI environment to "production" and publish there.