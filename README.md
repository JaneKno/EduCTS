# EduCTS - Educational Clinical Trial Simulation

A comprehensive platform for exploring validated pharmacokinetic-pharmacodynamic (PKPD) and pharmacokinetic (PK) models of antisense oligonucleotide (ASO) therapeutics. EduCTS provides an interactive Shiny-based application for clinical trial simulation and model exploration across multiple therapeutic areas.

## Overview

EduCTS is designed as an educational tool to understand what role model-informed drug development plays for clinical study design. It includes:

- **Question-First Workflow**: Start with clinical questions relevant to your work, then optionally explore underlying models
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

### Question-First Workflow
Start your exploration with clinical questions relevant to your work:
- **Browse Questions**: Navigate by therapeutic area, drug class, or clinical context
- **Explore Models**: Dive into specific models for deeper understanding when needed
- **Seamless Integration**: Transition smoothly between clinical questions and technical details
- **No Model Background Required**: Accessible to clinicians, students, and researchers new to pharmacokinetics

### Interactive Model Library
- **Browsable Catalog**: Explore models with filtering by:
  - Model type (PK, PKPD, KPD)
  - File type (mrgsolve format)
  - Therapeutic area and indication
  - Publication year
  - Validation status
- **Detailed Model Information**: View descriptions, authors, parameters, and applications
- **Direct Access**: Quick links to model equations (C++ files)
- **Question Context**: Use the Question-First tab for clinical context before diving into models 

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
- **googlesheets4**: Integration for data collection and feedback

## Getting Started

### Installation

1. **Install R** (>= 4.0.0) - [r-project.org](https://www.r-project.org/)

2. **Install required packages**:
   ```r
   install.packages(c("shiny", "jsonlite", "mrgsolve", "DT", "ggplot2", 
                      "plotly", "shinythemes", "shinyjs", "googlesheets4"))
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

**Getting Started with the App**: The application launches with a Question-First tab for intuitive exploration. First-time users can start with clinical questions or browse the Model Library directly based on their preference.

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

### New: Start with Clinical Questions
1. **Question-First Tab**: Launch the app and browse clinical questions by therapeutic area
2. **Find Relevant Models**: Discover models that address your specific question
3. **Explore When Ready**: Dive into model details, validation data, and technical parameters as needed
4. **No Model Background Required**: Clinical context provided for all questions

### For Researchers/Clinicians with Clinical Questions
1. **Ask**: Use the Question-First tab to find models addressing your clinical question
2. **Understand**: Review model context and how it relates to clinical practice
3. **Explore**: Compare models or dive deeper into pharmacokinetics/pharmacodynamics
4. **Apply**: Use validated models to inform decision-making

### For Modelers Exploring Model Validation
1. **Explore**: Use the Model Library to browse available models
2. **Evaluate**: Review validation against clinical data
3. **Adapt**: Export models and modify for your population/compound

### For Clinical Trial Planning
1. **Select**: Choose an appropriate model from the library or question context
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

This work is licensed under the Creative Commons Attribution 4.0 International License. See https://creativecommons.org/licenses/by/4.0/legalcode for details.

## Contact & Support

**Project Lead**: Jane Knochel  
**Institution**: University of Copenhagen

For questions, issues, or contributions, please refer to project documentation or contact the development team.

## UI & User Experience

- **Full-Width Content Views**: Model details, documentation, and evidence tabs offer focused, distraction-free reading
- **Responsive Sidebar**: Intelligently adapts based on content type and user goals
- **Multiple Entry Points**: Question-first workflow or traditional model-first exploration
- **Optimized Performance**: Production-ready codebase with streamlined user interactions

## Changelog

### Version 1.0 (Current)
- **Question-First Workflow**: New primary entry point for users starting with clinical questions
- **Enhanced Navigation**: Intuitive pathways from questions to model exploration
- **UI/UX Improvements**: Full-width content views, responsive sidebar, optimized layout
- **Production-Ready**: Comprehensive model library with 8+ major ASO therapeutic models
- **Interactive Shiny-Based Interface**: Support for PK, PKPD, and KPD model types
- **Clinical Trial Validation**: Extensive datasets from Phase 1, 2, and 3 studies
- **Performance Optimization**: Streamlined user tracking and interaction logging

### Version 0.1 (Archived)
- Initial comprehensive model library with 8+ major ASO therapeutic models
- Interactive Shiny-based model exploration interface
- Support for PK, PKPD, and KPD model types
- Clinical trial validation data for multiple studies
- Filtering and visualization by therapeutic area, indication, and publication year

---

**Last Updated**: April 2026  
**Project Repository**: [GitHub - JaneKno/EduCTS](https://github.com/JaneKno/EduCTS)
