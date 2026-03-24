# Dosing Days Feature - Usage Guide

## Overview
The validation studies simulation now supports a new `dosing_days` field that allows you to specify exact times for dose administration instead of using frequency (`ii`) and number of doses (`addl`) parameters.

## When to Use Dosing Days

Use `dosing_days` when:
- Doses are administered at specific, non-uniform intervals (e.g., day 1, 90, 270, 450)
- The dosing schedule doesn't follow a regular frequency pattern
- You want precise control over the exact timing of each dose
- Extracting this information directly from study data

## How to Use

### Option 1: In JSON Configuration

Add a `dosing_days` field to your arm definition:

```json
{
  "study_id": "STUDY_001",
  "study_name": "Phase 3 Trial",
  "study_length_days": 504,
  "arms": [
    {
      "arm_name": "Treatment A",
      "dose": 300,
      "dosing_days": [1, 90, 270, 450],
      "n_subjects": 100,
      "data_location": "data/source/Model/Study/Study_data.csv"
    }
  ]
}
```

The `dosing_days` field accepts:
- **Array of numbers**: `[1, 90, 270, 450]`
- **Comma-separated string**: `"1,90,270,450"`

### Option 2: In CSV Data File

Add a `dosing_days` column to your source CSV file:

```csv
Patient_ID;Time;Observation;Dose_mg;dosing_days;N_subjects
101;0;baseline;300;1,90,270,450;100
101;1;...;...;...;...
```

The system will automatically extract the `dosing_days` value from the CSV for each arm.

### Priority Order

When `dosing_days` is available from multiple sources, the system uses the following priority:

1. **JSON configuration** (highest priority) - Explicit `dosing_days` field in arm definition
2. **CSV data file** - `dosing_days` column in the source data
3. **Frequency-based dosing** (default) - Uses traditional `frequency`, `ii`, and `addl` parameters

## How It Works

When `dosing_days` is provided:

1. Each value in `dosing_days` is treated as a separate dose administration event
2. The value is interpreted as **days** and automatically converted to the model's time unit
3. Each dose record is created with:
   - `time` = the converted dosing day
   - `amt` = the specified dose amount
   - `ii` = NA (not applicable)
   - `addl` = 0 (not applicable)

Example: For `dosing_days: [1, 90, 270, 450]` with dose 300 mg:

| ID | time | amt | ii | addl | evid | cmt |
|---|---|---|----|------|------|-----|
| 1 | 1 | 300 | NA | 0 | 1 | 1 |
| 1 | 90 | 300 | NA | 0 | 1 | 1 |
| 1 | 270 | 300 | NA | 0 | 1 | 1 |
| 1 | 450 | 300 | NA | 0 | 1 | 1 |

## Code Changes Summary

### Updated Functions

1. **`generate_input_dataset()`**
   - Added support for `DosingDays` column in treatment_groups dataframe
   - Detects presence of `DosingDays` column
   - When present: Creates separate dose records for each specified day
   - When absent: Falls back to traditional frequency-based dosing

2. **Validation Studies Section**
   - Extracts `dosing_days` from CSV data
   - Extracts `dosing_days` from JSON configuration
   - Adds `DosingDays` column to treatment_groups_df when available
   - Supports multiple column name variations for flexibility

### Supported Column Names

The system recognizes these column names in CSV files (case-insensitive):
- `dosing_days`, `DosingDays`, `Dosing_Days`
- `dosing_times`, `Dosing_Times`
- `dose_days`, `Dose_Days`

## Example Workflow

### Step 1: Prepare CSV Data
```csv
Patient_ID;Time;Observation;Dose_mg;dosing_days;N_subjects;Frequency
101;0;baseline;300;"1,90,270,450";50;"custom"
101;1;PK;...;...;...;...
```

### Step 2: Configure JSON (optional - data will be extracted if not specified)
```json
{
  "arms": [
    {
      "arm_name": "300 mg Custom Schedule",
      "dose": 300,
      "data_location": "data/source/Model/Study/Study_data.csv",
      "n_subjects": 50
      // Note: frequency and dosing_days are both optional here
      // System will extract them from the CSV or JSON as needed
    }
  ]
}
```

### Step 3: Run Validation Studies
The simulation will:
1. Read the arm configuration
2. Extract `dosing_days` from JSON or CSV
3. Create 4 separate dose records (one for each dosing day)
4. Run the model simulation with these specific dosing times

## Technical Details

### Time Unit Conversion
- `dosing_days` values are assumed to be in **days**
- They are automatically converted to the model's time unit using the `convert_time()` function
- Example: If model uses hours, `dosing_days: 90` becomes `90 * 24 = 2160` hours

### Data Structure
When `dosing_days` is used, the input dataset structure remains identical to traditional dosing:
- All standard NONMEM/mrgsolve columns are present
- Only the `time`, `ii`, and `addl` columns differ
- The `rate` column is always 0 (not applicable for discrete dosing)

### Compatibility
- Works with all model types (PK, PKPD, KPD)
- Compatible with all input compartments
- Works with unit conversions (dose units and time units)
- Supports multiple subjects automatically

## Troubleshooting

### Issue: Dosing days not recognized
**Solution:** Ensure the CSV column or JSON field name matches one of the supported variations (see "Supported Column Names" above)

### Issue: Wrong time values in simulation
**Solution:** Verify that dosing_days values are in days and check the model's time unit setting

### Issue: Multiple dosing schedules conflicting
**Remember:** JSON configuration takes priority over CSV data. If both are present, JSON will be used.

## Migration from Frequency-Based Dosing

If you have existing studies using frequency, you can migrate to dosing_days:

**Before (Frequency-based):**
```json
{
  "arm_name": "Weekly dosing",
  "dose": 300,
  "frequency": "Weekly",
  "no_doses": 20
}
```

**After (Dosing_days-based):**
```json
{
  "arm_name": "Weekly dosing",
  "dose": 300,
  "dosing_days": [1, 8, 15, 22, 29, 36, 43, 50, 57, 64, 71, 78, 85, 92, 99, 106, 113, 120, 127, 134]
}
```

Both approaches are fully supported and can coexist in the same study.
