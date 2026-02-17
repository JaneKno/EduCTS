# EduCTS - Educational Clinical Trial Simulation

A comprehensive platform for exploring validated pharmacokinetic-pharmacodynamic (PKPD) and pharmacokinetic (PK) models of antisense oligonucleotide (ASO) therapeutics. EduCTS provides an interactive Shiny-based application for clinical trial simulation and model exploration across multiple therapeutic areas.

## Overview

EduCTS is designed as an educational tool to understand what role model-informed drug development plays for clinical study design. It includes:

- **Interactive Model Library**: Browse and filter a collection of PK and PKPD models
- **Clinical Trial Simulation**: Conduct simulations using validated models with clinical trial data
- **Model Validation**: Access to historical clinical study data and model validation results
- **Educational Resource**: Learn about pharmacokinetics, pharmacodynamics, and clinical trial simulation concepts
- **Multiple Therapeutic Areas**: Models covering cardiovascular disease, metabolic disorders, and other indications

## Project Structure

```
EduCTS/
├── models/                          # Model definitions and metadata
│   ├── ASO_PK_model.cpp            # Generic PK model for ASOs
│   ├── ASO_PK_model.json           # Model metadata and configuration
│   ├── Olezarsen_PKPD_model.cpp    # Compound-specific PKPD model
│   ├── Olezarsen_PKPD_model.json   # Compound-specific metadata
│   └── [other model files...]       # Additional therapeutic models
│
├── data/                            # Clinical trial data
│   ├── source/                      # Raw clinical trial data files
│   │   ├── Olezarsen_PKPD_model/
│   │   │   ├── Alexander2019/       # Phase 1 trial data
│   │   │   ├── Bergmark2024/        # Additional studies
│   │   │   └── Stroes2024/          # Phase 3 trial data
│   │   ├── Plozasiran_KPD_model/    # KPD (kinetic-pharmacodynamic) models
│   │   │   ├── Gaudet2024/          # Phase 2b data
│   │   │   ├── Shi2023/             # Simulated data
│   │   │   └── Watts2025/           # Phase 3 data
│   │   └── [other compound data...]
│   │
│   └── derived/                     # Processed validation results
│       └── validation/              # Validation summaries by study/arm
│           ├── Olezarsen_PKPD_model/
│           │   ├── Alexander2019/   # Validation summaries (summaries.rds)
│           │   └── Stroes2024/      # Configuration and metadata
│           └── Plozasiran_KPD_model/
│
├── scripts/                         # R/Shiny application code
│   ├── ModelLibrary.R              # Main model library component
│   └── subdir/
│       ├── CTS_app.R               # Shiny app wrapper
│       ├── CTS_ui.R                # User interface definitions
│       ├── CTS_server.R            # Server-side logic
│       └── global.R                # Global configuration
│
├── results/                         # Output directory for simulations
│
└── README.md                        # This file
```

## Key Features

### Model Library
- **Browsable Catalog**: Explore models with filtering by:
  - Model type (PK, PKPD, KPD)
  - File type (mrgsolve format)
  - Therapeutic area and indication
  - Publication year
  - Validation status
- **Detailed Model Information**: View descriptions, authors, parameters, and applications
- **Direct Access**: Quick links to model equations (C++ files) 

### Supported Compounds & Indications

#### Metabolic/Cardiovascular Focus
- **Olezarsen**: ApoC-III lowering agent for familial chylomicronemia syndrome (FCS), hypertriglyceridemia (HTG), cardiovascular disease
- **Plozasiran**: GalNAc-conjugated ASO for lipoprotein(a) reduction
- **ASO Generic PK Model**: Template model for general antisense drug development

#### Additional Models
- Eplontersen (Hereditary ATTR amyloidosis)
- Fitusiran (Rare bleeding disorders)
- Inclisiran (Hypercholesterolemia)
- Inotersen (Amyloid polyneuropathy)
- Nusinersen (Spinal muscular atrophy)
- Patisiran (Hereditary ATTR amyloidosis)

### Clinical Trial Data

The project includes validation datasets from:
- **Phase 1, 2, and 3 clinical trials**
- **Multiple study sponsors** (Stroes2024, Alexander2019, Gaudet2024, etc.)
- **Processed summaries** in R objects for rapid analysis
- **Study configurations** documenting study design and arms

## Technologies & Dependencies

### Core Framework
- **Shiny**: Interactive web-based user interface
- **R**: Statistical computing and graphics

### Modeling & Simulation
- **mrgsolve**: Pharmacokinetic/pharmacodynamic modeling using C++ with R integration
- **jsonlite**: JSON configuration file parsing

### Data Analysis & Visualization
- **ggplot2**: Static graphics
- **plotly**: Interactive visualizations
- **DT**: Interactive data tables

### Theming & UX
- **shinythemes**: Bootstrap theme support (Flatly theme)
- **shinyjs**: JavaScript interactivity

## Getting Started

### Installation

1. **Install R** (>= 4.0.0) - [r-project.org](https://www.r-project.org/)

2. **Install required packages**:
   ```r
   install.packages(c("shiny", "jsonlite", "mrgsolve", "DT", "ggplot2", 
                      "plotly", "shinythemes", "shinyjs"))
   ```

3. **Clone the repository**:
   ```bash
   git clone https://github.com/JaneKno/EduCTS.git
   cd EduCTS
   ```

### Running the Application

1. **Navigate to the scripts directory**:
   ```bash
   cd scripts/subdir
   ```

2. **Launch the Shiny application in R**:
   ```r
   shiny::runApp(".")
   ```

3. **Access the application**: Open your browser to the displayed local address (typically `http://localhost:3838`)

Alternatively, run directly from command line:
```bash
Rscript -e "shiny::runApp('scripts/subdir')"
```

## File Formats

### Model Definitions (`.cpp`)
C++ files containing differential equations and model structure using mrgsolve syntax.

**Structure**:
- Parameter definitions
- Differential equations (ODE system)
- Output calculations
- Table statements for result extraction

### Model Metadata (`.json`)
Configuration files containing:
- `display_name`: User-friendly model name
- `description`: Model purpose and scope
- `author`: Model developer
- `model_type`: "PK", "PKPD", or "KPD"
- `compound`: Drug molecule name
- `dose_unit`: Standard dose unit (mg, etc.)
- `output_label`: Description of model outputs
- `therapeutic_area`: Disease category
- `indication`: Specific use case
- `validation_status`: "Validated", "Fully Validated", etc.
- `internal_validation_data`: Reference studies and data locations
- `external_validation_data` : Reference studies and data locations

### Clinical Trial Data (`.csv`)
Meta data from clinical trials with columns for:
- Study ID
- study period and time points
- Dosing information
- Observed values (concentrations, biomarkers)
- Study arm/cohort assignments

### Validation Results (`.rds`)
R serialized objects containing:
- Simulation results
- Goodness-of-fit diagnostics
- Posterior distributions from Bayesian fits
- Study-specific summary statistics

## Model Validation

Each model includes validation against clinical trial data:

1. **Validation data**: Associated clinical study datasets in `data/source/<model_name>/`
2. **Configuration Tracking**: `config_hash.txt` and `study_config.json` document study setup
3. **Arm-Specific Results**: `summaries.rds` contains processed validation metrics per study arm

Browse validation results in the application's Model Details view.

## Workflow

### For Researchers/Modelers
1. **Explore**: Use the Model Library to browse available models
2. **Evaluate**: Review validation against clinical data
3. **Adapt**: Export models and modify for your population/compound

### For Clinical Trial Planning
1. **Select**: Choose an appropriate model from the library
2. **Simulate**: Use the CTS application to simulate multiple scenarios
3. **Design**: Evaluate dose regimens, sample timing, and statistical power
4. **Plan**: Inform study design and population selection

## Contributing

To add a new model:

1. **Create the mrgsolve C++ model** (`models/YourModel_PKPD_model.cpp`)
2. **Create the metadata JSON** (`models/YourModel_PKPD_model.json`) with:
   - Model information and parameters
   - Reference to clinical validation data
   - Study arms and data locations
3. **Add clinical data** to `data/source/YourModel_PKPD_model/` as CSV files
4. **Generate validation results** automatically placed in `data/validation/` as `.rds` files
5. **Test** by running the Shiny application and verifying your model appears

## References & Resources

### Key Publications
- Models reference peer-reviewed pharmacology and clinical trial literature
- See individual model JSON files for DOI links and source citations
- FDA Integrated Reviews for approved compounds available via links in metadata

### Educational Resources
The application includes embedded descriptions of:
- Pharmacokinetics and pharmacodynamics concepts
- Clinical trial simulation methodology
- Population pharmacokinetics
- Study design principles

## License

[Specify license - e.g., MIT, GPL, proprietary]

## Contact & Support

**Project Lead**: Jane Knochel  
**Institution**: University of Copenhagen

For questions, issues, or contributions, please refer to project documentation or contact the development team.

## Changelog

### Version 0.1 (Current)
- Initial comprehensive model library with 8+ major ASO therapeutic models
- Interactive Shiny-based model exploration interface
- Support for PK, PKPD, and KPD model types
- Clinical trial validation data for multiple studies
- Filtering and visualization by therapeutic area, indication, and publication year

---

**Last Updated**: February 2026  
**Project Repository**: [GitHub - JaneKno/EduCTS](https://github.com/JaneKno/EduCTS)
