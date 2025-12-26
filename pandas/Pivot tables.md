# Creating pivot tables in pandas

`pd.pivot_table()` function.

This function enables you to summarize and aggregate data in a spreadsheet-style format.

## Key Parameters

The primary arguments for `pd.pivot_table()` are:

- `data`: The DataFrame you want to use.
- `values`: The column(s) to aggregate (e.g., 'Sales', 'Quantity').
- `index`: The column(s) to use as row labels.
- `columns`: The column(s) to use as column labels.
- `aggfunc`: The aggregation function(s) to apply (e.g., `mean`, `sum`, `count`,).
  The default is `mean`.
- `fill_value`: A value to replace missing data (NaNs) in the resulting table.
- `margins`: If `True`, adds row and column totals (subtotals).

## Step-by-Step Example

Here is how to create a basic pivot table.

First, create a sample DataFrame:

```py
import pandas as pd
import numpy as np

data = {
    'Product': ['A', 'B', 'A', 'B', 'A', 'B', 'A', 'B'],
    'Region': ['North', 'North', 'South', 'South', 'North', 'North', 'South', 'South'],
    'Sales': [100, 150, 80, 120, 110, 140, 90, 130],
    'Units': [10, 15, 8, 12, 11, 14, 9, 13]
}
df = pd.DataFrame(data)
print("       Original DataFrame:")
print(df)
print("-" * 30)
```

To create a pivot table summarizing total sales by product and region:

```py
# Create a pivot table
pivot_table = pd.pivot_table(
    data=df,
    values='Sales',
    index='Product',
    columns='Region',
    aggfunc='sum' # Use 'sum' to get total sales
)

print("Pivot Table (Total Sales):")
print(pivot_table)
```

Output:

```sh
       Original DataFrame:
  Product Region  Sales  Units
0       A  North    100     10
1       B  North    150     15
2       A  South     80      8
3       B  South    120     12
4       A  North    110     11
5       B  North    140     14
6       A  South     90      9
7       B  South    130     13
------------------------------

Pivot Table (Total Sales):
Region  North  South
Product              
A         210    170
B         290    250
```

## Advanced Functionality

- Multiple Aggregation Functions: You can apply a list of functions to a single
  value column, or a dictionary to apply different functions to different value columns.

  ```py
  multi_agg = pd.pivot_table(
      df,
      values=['Sales', 'Units'],
      index='Product',
      columns='Region',
      aggfunc={'Sales': 'sum', 'Units': 'mean'} # Different functions for each column
  )
  print("\nPivot Table (Multiple Aggregations):")
  print(multi_agg)
  ```

- **Handling Missing Values**
  Use fill_value to replace NaN entries in the result.
  
  ```py
  filled_table = pd.pivot_table(
      df,
      values='Sales',
      index='Product',
      columns='Region',
      aggfunc='sum',
      fill_value=0
  )
  print("\nPivot Table (Fill NaNs with 0):")
  print(filled_table)
  ```

- **Adding Totals**
  Set margins=True to include row/column totals.
  
  ```py
  totals_table = pd.pivot_table(
      df,
      values='Sales',
      index='Product',
      columns='Region',
      aggfunc='sum',
      margins=True,
      margins_name='Grand Total' # Customize the total label
  )
  print("\nPivot Table (With Totals):")
  print(totals_table)
  ```

For detailed documentation, refer to the official pandas documentation.
