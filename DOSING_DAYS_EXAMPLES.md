# Example: Using dosing_days in Validation Studies

This document provides practical examples of how to use the `dosing_days` feature in your validation studies.

## Example 1: Simple Case - JSON Configuration

### JSON Configuration (models/Example_PKPD_model.json)

```json
{
  "model_information": {
    "display_name": "Example PKPD Model",
    "compound": "ExampleDrug",
    "model_type": "PKPD"
  },
  "internal_validation_data": {
    "studies": [
      {
        "study_id": "STUDY_001",
        "study_name": "Phase 3 - Custom Dosing Schedule",
        "study_length_days": 504,
        "arms": [
          {
            "arm_name": "Placebo",
            "dose": 0,
            "dosing_days": [1, 90, 270, 450],
            "n_subjects": 50,
            "data_location": "data/source/Example_PKPD_model/Phase3/Phase3_placebo.csv"
          },
          {
            "arm_name": "300 mg Custom Schedule",
            "dose": 300,
            "dosing_days": [1, 90, 270, 450],
            "n_subjects": 100,
            "data_location": "data/source/Example_PKPD_model/Phase3/Phase3_300mg.csv"
          },
          {
            "arm_name": "600 mg Custom Schedule",
            "dose": 600,
            "dosing_days": [1, 90, 270, 450],
            "n_subjects": 100,
            "data_location": "data/source/Example_PKPD_model/Phase3/Phase3_600mg.csv"
          }
        ]
      }
    ]
  }
}
```

### Resulting Input Dataset

For each arm, the system will create dosing records like this (example for 300 mg arm):

```
ID  time  amt  rate  cmt  evid  ii   addl  ss  SEQ  Period  GroupName
1   1     300  0     1    1     NA   0     0   1    1       300 mg Custom Schedule
1   90    300  0     1    1     NA   0     0   1    1       300 mg Custom Schedule
1   270   300  0     1    1     NA   0     0   1    1       300 mg Custom Schedule
1   450   300  0     1    1     NA   0     0   1    1       300 mg Custom Schedule
2   1     300  0     1    1     NA   0     0   1    1       300 mg Custom Schedule
2   90    300  0     1    1     NA   0     0   1    1       300 mg Custom Schedule
... (repeated for all 100 subjects)
```

## Example 2: Data-Driven Approach - Extracting from CSV

### CSV Data File (data/source/Example_PKPD_model/Phase3/Phase3_300mg.csv)

```csv
Patient_ID;Time;Days;Observation;Conc_ng_mL;Dose_mg;dosing_days;N_subjects;Frequency
101;0;1;baseline;0.0;300;"1,90,270,450";100;"custom"
101;0.5;1;0.5h;2.5;300;"1,90,270,450";100;"custom"
101;1;1;1h;3.2;300;"1,90,270,450";100;"custom"
101;2;1;2h;4.1;300;"1,90,270,450";100;"custom"
101;4;1;4h;5.0;300;"1,90,270,450";100;"custom"
101;8;1;8h;4.2;300;"1,90,270,450";100;"custom"
101;24;1;24h;2.1;300;"1,90,270,450";100;"custom"
101;48;1;48h;1.5;300;"1,90,270,450";100;"custom"
...continues with observations from other time points...
102;0;1;baseline;0.0;300;"1,90,270,450";100;"custom"
...repeated for each patient...
```

### Minimal JSON Configuration (when dosing_days is extracted from CSV)

```json
{
  "model_information": {
    "display_name": "Example PKPD Model",
    "compound": "ExampleDrug"
  },
  "internal_validation_data": {
    "studies": [
      {
        "study_id": "STUDY_002",
        "study_name": "Phase 3 - Data-Driven",
        "study_length_days": 504,
        "arms": [
          {
            "arm_name": "300 mg from CSV",
            "dose": 300,
            "data_location": "data/source/Example_PKPD_model/Phase3/Phase3_300mg.csv"
            // Note: No frequency or dosing_days specified here
            // System will extract dosing_days from CSV automatically
          }
        ]
      }
    ]
  }
}
```

The system will:
1. Read Phase3_300mg.csv
2. Filter data by dose (300 mg)
3. Extract the `dosing_days` value: "1,90,270,450"
4. Extract n_subjects: 100
5. Create the input dataset with 4 dose events per subject

## Example 3: Non-Uniform Dosing Schedule

### Use Case: Titration Study with Varied Dosing

```json
{
  "study_id": "TITRATION_STUDY",
  "study_name": "Dose Titration Study",
  "study_length_days": 112,
  "arms": [
    {
      "arm_name": "Titrated Dosing (100->200->400 mg)",
      "dose": 400,  // final dose
      "dosing_days": [1, 15, 29],  // Different doses on each day
      "n_subjects": 60,
      "data_location": "data/source/Model/Titration/titration_data.csv"
    }
  ]
}
```

**Note:** In this example, the same `dose` (400 mg) is given on each `dosing_day`. 
If you need different doses on different days, currently you would need to create separate arms with different dose values and combine them during analysis, or modify the approach to track specific dosing schedules.

## Example 4: Long-term Study with Sparse Dosing

### Use Case: Quarterly Injection Study

```json
{
  "study_id": "QUARTERLY_STUDY",
  "study_name": "Quarterly Dosing Study",
  "study_length_days": 365,
  "arms": [
    {
      "arm_name": "Quarterly 100 mg",
      "dose": 100,
      "dosing_days": [1, 91, 181, 271],  // Approximately every 3 months
      "n_subjects": 150,
      "data_location": "data/source/Model/Quarterly/quarterly_data.csv"
    },
    {
      "arm_name": "Quarterly 200 mg",
      "dose": 200,
      "dosing_days": [1, 91, 181, 271],
      "n_subjects": 150,
      "data_location": "data/source/Model/Quarterly/quarterly_data.csv"
    }
  ]
}
```

**Input Dataset Output:**
- 2 dose events per subject for Q3 months (4 per year)
- Exact timing: days 1, 91, 181, 271
- Much more efficient than traditional ii/addl method (which would require very small intervals)

## Example 5: Real-World Dosing Variability

### Use Case: Study with Observed Dosing Compliance

```json
{
  "study_id": "COMPLIANCE_STUDY",
  "study_name": "Real-World Compliance",
  "study_length_days": 365,
  "arms": [
    {
      "arm_name": "Weekly - Perfect Compliance",
      "dose": 50,
      "dosing_days": [1, 8, 15, 22, 29, 36, 43, 50, 57, 64, 71, 78, 85, 92, 99, 106, 113, 120, 127, 134, 141, 148, 155, 162, 169, 176, 183, 190, 197, 204, 211, 218, 225, 232, 239, 246, 253, 260, 267, 274, 281, 288, 295, 302, 309, 316, 323, 330, 337, 344, 351, 358],
      "n_subjects": 75,
      "data_location": "data/source/Model/Compliance/perfect_compliance.csv"
    },
    {
      "arm_name": "Weekly - 80% Compliance",
      "dose": 50,
      "dosing_days": [1, 8, 15, 22, 29, 36, 50, 57, 71, 78, 92, 99, 113, 120, 134, 148, 155, 169, 176, 190, 204, 211, 225, 232, 246, 260, 267, 281, 295, 302, 316, 323, 337, 351],  // Missed ~20% of doses
      "n_subjects": 75,
      "data_location": "data/source/Model/Compliance/partial_compliance.csv"
    }
  ]
}
```

This allows modeling real-world compliance variations with exact dosing schedules.

## CSV Format Flexibility

The system accepts various column name formats. All of these will work:

```csv
# Format 1: Lowercase with underscores
dosing_days

# Format 2: CamelCase
DosingDays

# Format 3: Sentence case
Dosing_Days

# Format 4: Alternative names (all recognized)
dosing_times
Dosing_Times
dose_days
Dose_Days
```

## Tips and Best Practices

1. **Keep dosing_days values in days**: The system expects days and will convert to model time units automatically.

2. **Use JSON for controlled studies**: When dosing is pre-defined (clinical trials), use JSON configuration for clarity and version control.

3. **Use CSV for observational data**: When extracting from actual study data, use CSV extraction for traceability.

4. **Document the source**: Add comments in JSON explaining where the dosing schedule came from.

5. **Validate extraction**: Check the debug output to confirm dosing_days were extracted correctly:
   ```
   DEBUG: Arm 1 - extracted dosing_days from data column 'dosing_days': 1,90,270,450
   ```

6. **Priority matters**: Remember JSON takes priority over CSV if both are specified. Use this intentionally to override extracted values if needed.

## Validation

To verify your dosing_days are being processed correctly:

1. Look for these debug messages in the console:
   - `"extracted dosing_days from data column"` (CSV extraction)
   - `"using dosing_days"` (JSON usage)
   - `"Added DosingDays column"` (successful setup)

2. Check the input dataset output:
   - Verify correct number of dose records per subject
   - Confirm time values match your dosing_days

3. Review simulation output:
   - Peak concentrations should align with dose timing
   - Multiple peaks if appropriate for your dosing schedule
