# dinoSoil-toolbox: A tool for optimizing Digital Soil Mapping at the Subdirectorate of Agrology in Colombia

The toolbox (`src/dinoSOILtoolbox.R`) incoporates multiple information technologies incl. Machine learning for Digital Soil Mapping (DSM).

If you use this code, please cite it as follows:
[![DOI](https://zenodo.org/badge/294735247.svg)](https://zenodo.org/badge/latestdoi/294735247)

Download a demo project via:

```bash
bash download.sh
```

Alternatively, it can be downloaded from:
```bash
https://github.com/acocac/dinoSOIL-toolbox/blob/master/proyecto_cesarmagdalena.zip
```
## Libraries
The tool uses R version 4.0.2. The `pacman` library is used to import/install required libraries.

## Documentatipn
A user manual related to version 0.1 (in Spanish) is provided (see `ManualUsuario_V01.pdf`). 

## Acknowledgements
The authors thank the Sub-Directorate of Agrology of the Geographic Institute Agustín Codazzi (IGAC), for their support and technical feedback in the development of this work.

## Contributions
Contributions via pull requests are welcome. Please make sure that changes pass the unit tests. Any bugs and problems can be reported on the repo's issues page.

## Future work
- Incorporate improvements as those added as TODO at the end of the main script (see `src/dinoSOILtoolbox.R`);
- Generate a more user-friendly interface for non-experts;
- Allow reproducibility of the code via Binder.

## Key features of the toolbox
Scheme of the main steps and outputs:
<p align="center">
<img src="doc/flowchart_tool.png" width="900" />
</p>

Scheme of the data selection and integration component:
<p align="center">
<img src="doc/datapreparation.png" width="900" />
</p>