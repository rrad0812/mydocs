
# Seamless Data Migration from MySQL to PostgreSQL with Python: Full Pipeline

Migrating data from MySQL to PostgreSQL can be a challenging task, especially when handling large tables, data type differences, null values, and ensuring the auto-increment sequences remain intact. This comprehensive guide walks through the entire migration pipeline with Python scripts that tackle common problems such as:

- Initial bulk migration for empty tables.
- Handling incremental updates for new data.
- Parallel migration for massive tables.
- Synchronizing primary key sequences in PostgreSQL.

We’ll explore each step of the migration with detailed code examples and explanations. Let’s dive into the solution.

## Key Components of the Migration

### Common Utilities

Before diving into the individual migration scripts, let’s set up some essential functions that streamline the process of moving data from MySQL to PostgreSQL. These utilities will be used across various stages of the migration.

#### Database Connections

To begin, we need functions that handle MySQL and PostgreSQL database connections. These will be used across all migration tasks.

```py
import pymysql
import psycopg2
import pandas as pd
import logging
from psycopg2.extras import execute_values

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Configuration
MYSQL_CONFIG = { … } # your config here
POSTGRES_CONFIG = { … }

# Constants for parallel processing
BATCH_SIZE = 1000
THREADS = 4

def create_mysql_connection():
    """Create and return a MySQL connection."""
    return pymysql.connect(**MYSQL_CONFIG)

def create_postgres_connection():
    """Create and return a PostgreSQL connection."""
    return psycopg2.connect(**POSTGRES_CONFIG)
```

#### Fetch Table List from MySQL

Before migrating data, we need to know which tables exist in MySQL. This function retrieves the list of tables:

```py
def get_table_list(mysql_conn):
    """Fetch the list of tables from MySQL."""
    with mysql_conn.cursor() as cursor:
        cursor.execute("SHOW TABLES;")
        return [row[0] for row in cursor.fetchall()]
```

#### Data Type Transformations

Data types in MySQL and PostgreSQL may differ, so we’ll need to ensure compatibility during the migration. This function handles common transformations for MySQL types to PostgreSQL-friendly formats:

```py
def transform_data_types(data, column_types):
    """Transform MySQL column types to PostgreSQL-compatible formats."""
    for column, mysql_type in column_types.items():
        if data[column].isnull().all():
            continue
        if "tinyint" in mysql_type.lower():
            data[column] = data[column].apply(lambda x: bool(x) if pd.notnull(x) else None)
        elif "int" in mysql_type.lower():
            max_value = data[column].max(skipna=True)
            if max_value > 2_147_483_647:
                data[column] = data[column].astype("Int64")
            else:
                data[column] = data[column].astype("Int32").where(pd.notna(data[column]), None)
        elif "bigint" in mysql_type.lower():
            data[column] = data[column].astype("Int64").where(pd.notna(data[column]), None)
        elif "float" in mysql_type.lower() or "double" in mysql_type.lower() or "decimal" in mysql_type.lower():
            data[column] = data[column].astype(float).where(pd.notna(data[column]), None)
        elif "datetime" in mysql_type.lower() or "timestamp" in mysql_type.lower():
            data[column] = pd.to_datetime(data[column], errors="coerce")
            data[column] = data[column].apply(lambda x: None if pd.isna(x) else (x if x.year >= 1000 else pd.Timestamp("1000-01-01 00:00:00")))
        elif "varchar" in mysql_type.lower() or "text" in mysql_type.lower() or "char" in mysql_type.lower():
            data[column] = data[column].apply(lambda x: str(x) if pd.notnull(x) else None)
    return data
```

#### Helper Functions for Data Fetching

These utility functions handle the actual data fetching and manipulation operations:

```py
def get_total_rows(mysql_conn, table_name):
    """Get the total number of rows in a MySQL table."""
    with mysql_conn.cursor() as cursor:
        cursor.execute(f"SELECT COUNT(*) FROM {table_name};")
        return cursor.fetchone()[0]

def fetch_data_in_batch(mysql_conn, table_name, offset, batch_size):
    """Fetch a batch of data from MySQL using LIMIT and OFFSET."""
    with mysql_conn.cursor() as cursor:
        query = f"SELECT * FROM {table_name} LIMIT {batch_size} OFFSET {offset};"
        cursor.execute(query)
        return cursor.fetchall()

def get_columns(mysql_conn, table_name):
    """Retrieve column names and types from a MySQL table."""
    with mysql_conn.cursor() as cursor:
        cursor.execute(f"DESCRIBE {table_name};")
        columns = cursor.fetchall()
        column_names = [col[0] for col in columns]
        column_types = {col[0]: col[1] for col in columns}
        return column_names, column_types

def insert_into_postgres(df, postgres_conn, table_name):
    """Insert DataFrame into PostgreSQL using execute_values for efficiency."""
    if df.empty:
        logger.info(f"No data to insert for {table_name}")
        return
    
    with postgres_conn.cursor() as cursor:
        # Prepare column names and values
        columns = ",".join(df.columns)
        values = [tuple(row) for row in df.values]
        
        # Create insert query
        insert_query = f"INSERT INTO {table_name} ({columns}) VALUES %s ON CONFLICT DO NOTHING;"
        
        try:
            execute_values(cursor, insert_query, values)
            postgres_conn.commit()
            logger.info(f"Inserted {len(df)} rows into {table_name}")
        except Exception as e:
            postgres_conn.rollback()
            logger.error(f"Error inserting into {table_name}: {e}")
            raise
```

#### Functions for Synchronization

These functions help identify and migrate missing rows between databases:

```py
def get_missing_ids(mysql_conn, postgres_conn, table_name, id_column="id"):
    """Find IDs present in MySQL but missing in PostgreSQL."""
    # Get all IDs from MySQL
    with mysql_conn.cursor() as cursor:
        cursor.execute(f"SELECT {id_column} FROM {table_name};")
        mysql_ids = set(row[0] for row in cursor.fetchall())
    
    # Get all IDs from PostgreSQL
    with postgres_conn.cursor() as cursor:
        cursor.execute(f"SELECT {id_column} FROM {table_name};")
        postgres_ids = set(row[0] for row in cursor.fetchall())
    
    # Find the difference
    missing_ids = list(mysql_ids - postgres_ids)
    logger.info(f"Found {len(missing_ids)} missing IDs in {table_name}")
    return sorted(missing_ids)

def fetch_missing_rows(mysql_conn, table_name, id_list, id_column="id"):
    """Fetch specific rows from MySQL by their IDs."""
    if not id_list:
        return []
    
    with mysql_conn.cursor() as cursor:
        placeholders = ",".join(["%s"] * len(id_list))
        query = f"SELECT * FROM {table_name} WHERE {id_column} IN ({placeholders});"
        cursor.execute(query, id_list)
        return cursor.fetchall()

def migrate_batch(table_name, batch_ids, id_column="id"):
    """Migrate a batch of rows identified by their IDs."""
    mysql_conn = create_mysql_connection()
    postgres_conn = create_postgres_connection()
    
    try:
        # Fetch rows from MySQL
        rows = fetch_missing_rows(mysql_conn, table_name, batch_ids, id_column)
        column_names, column_types = get_columns(mysql_conn, table_name)
        
        # Transform and insert
        df = pd.DataFrame(rows, columns=column_names)
        df = transform_data_types(df, column_types)
        insert_into_postgres(df, postgres_conn, table_name)
        
        return f"Migrated {len(batch_ids)} rows from {table_name}"
    except Exception as e:
        logger.error(f"Error migrating batch for {table_name}: {e}")
        return f"Failed to migrate batch: {e}"
    finally:
        mysql_conn.close()
        postgres_conn.close()

def get_primary_key(cursor, table_name):
    """Get the primary key column(s) for a PostgreSQL table."""
    query = """
        SELECT a.attname
        FROM pg_index i
        JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey)
        WHERE i.indrelid = %s::regclass AND i.indisprimary;
    """
    cursor.execute(query, (table_name,))
    return [row[0] for row in cursor.fetchall()]
```

### Initial Bulk Migration for Empty Tables

When you’re starting with empty PostgreSQL tables, the simplest approach is a bulk insert from MySQL to PostgreSQL. Here’s how to do it:

**The Problem**:  
You need to move all the data from MySQL to PostgreSQL, and you want to do it efficiently without overloading memory or the network.

**The Solution**:

- **Batch inserts**: Break the data into smaller chunks to avoid memory issues.
- **Type transformations**: Convert MySQL types into PostgreSQL-compatible types before inserting.
- **Error handling**: Ensure that errors in any row don’t stop the entire migration.

**The Code**:

```py
def migrate_table(mysql_conn, postgres_conn, table_name):
    """Migrate a full table from MySQL to PostgreSQL."""
    offset = 0
    batch_size = 10000  # Example batch size
    total_rows = get_total_rows(mysql_conn, table_name)

    while offset < total_rows:
        # Fetch the batch of data
        rows = fetch_data_in_batch(mysql_conn, table_name, offset, batch_size)
        column_names, column_types = get_columns(mysql_conn, table_name)

        # Transform and clean the data
        df = pd.DataFrame(rows, columns=column_names)
        df = transform_data_types(df, column_types)

        # Insert into PostgreSQL
        insert_into_postgres(df, postgres_conn, table_name)

        offset += batch_size
```

**Key points**:

- We break the data into manageable batches to prevent performance issues.
- We transform MySQL data types into PostgreSQL-compatible ones before insertion.
- Errors are handled gracefully to prevent batch failure.

### Parallel Migration for Large Tables

Handling tables with millions of rows requires parallelization for efficiency. As the size of your dataset grows, it becomes impractical to migrate everything sequentially.

The Problem:
When migrating large tables with millions of rows, you can run into performance bottlenecks due to the sheer volume of data. Using traditional pagination or offsets can also slow down as you go deeper into the dataset.

The Solution:
Instead of relying on offset pagination, we compare primary keys between MySQL and PostgreSQL to detect missing rows and then migrate them in parallel.

The Code:

```py
from concurrent.futures import ThreadPoolExecutor

def migrate_missing_rows_parallel(table_name, id_column="id"):
    """Find and migrate missing rows from MySQL to PostgreSQL in parallel."""
    mysql_conn = create_mysql_connection()
    postgres_conn = create_postgres_connection()

    # Fetch missing IDs
    missing_ids = get_missing_ids(mysql_conn, postgres_conn, table_name, id_column)

    # Process missing rows in parallel
    with ThreadPoolExecutor(max_workers=THREADS) as executor:
        futures = []
        for i in range(0, len(missing_ids), BATCH_SIZE):
            batch = missing_ids[i:i + BATCH_SIZE]
            futures.append(executor.submit(migrate_batch, table_name, batch))

        for future in futures:
            logger.info(future.result())
```

Key points:

- We use ThreadPoolExecutor for parallel processing.
- We batch the IDs and submit them as tasks to parallelize the migration.
- This greatly improves performance when migrating large datasets.

### Synchronizing Missing Rows After Initial Migration

What happens if new data gets added to MySQL after the initial migration? You need to keep your PostgreSQL database in sync with missing rows.

The Problem:
After the initial migration, you need a method to sync only the rows that were added after the migration was completed.

The Solution:
You can compare the primary key columns in both databases and identify the missing rows, then migrate those rows in smaller batches.

The Code:

```py
def migrate_missing_rows(mysql_conn, postgres_conn, table_name, id_column="id"):
    """Migrate missing rows from MySQL to PostgreSQL."""
    missing_ids = get_missing_ids(mysql_conn, postgres_conn, table_name, id_column)
    
    for i in range(0, len(missing_ids), BATCH_SIZE):
        batch_ids = missing_ids[i:i + BATCH_SIZE]
        
        # Fetch the batch of missing rows
        rows = fetch_missing_rows(mysql_conn, table_name, batch_ids)
        column_names, column_types = get_columns(mysql_conn, table_name)

        # Clean and transform data
        df = pd.DataFrame(rows, columns=column_names)
        df = transform_data_types(df, column_types)

        # Insert into PostgreSQL
        insert_into_postgres(df, postgres_conn, table_name)
```

Key points:

- This function compares the primary keys and migrates only the missing rows.
- It’s ideal for keeping your databases in sync without migrating the entire dataset again.

### Fixing Primary Key Sequences in PostgreSQL

After migrating data, PostgreSQL might encounter issues with auto-incrementing primary keys. This happens because the sequence that controls the auto-incrementing behavior doesn’t automatically adjust to the new data.

The Problem:
If the sequence is not updated, you might run into duplicate key errors when inserting new rows into PostgreSQL.

The Solution:
We dynamically fetch the primary key for each table and synchronize the sequence to ensure it aligns with the maximum `id` value.

The Code:

```py
def update_sequence(cursor, table_name):
    """Fix the primary key sequence in PostgreSQL after data migration."""
    primary_keys = get_primary_key(cursor, table_name)
    if primary_keys:
        pk_column = primary_keys[0]  # Assuming a single primary key

        cursor.execute(f"SELECT setval(pg_get_serial_sequence('{table_name}', '{pk_column}'), "
                       f"COALESCE((SELECT MAX({pk_column}) FROM {table_name}), 1), true);")
        print(f"Sequence updated for {table_name}.{pk_column}")
```

Key points:

- This function dynamically fetches the primary key column and updates the sequence to match the latest `id` value.
- This ensures auto-increment consistency in PostgreSQL.

## Final Thoughts

By using the Python scripts outlined in this guide, you can effectively handle the migration of data from MySQL to PostgreSQL:

- **Parallelization**: Migrates large tables more efficiently.
- **Error handling**: Logs row-level failures without halting the migration.
- **Data type compatibility**: Ensures MySQL data is compatible with PostgreSQL.
- **Sequence synchronization**: Keeps auto-incrementing primary keys in sync.

This pipeline offers a robust, scalable, and production-ready solution for migrating data across databases. It also lays the groundwork for incremental sync, making it easier to handle ongoing data changes.
