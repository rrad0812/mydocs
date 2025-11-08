
# Developer Cheat Sheet (Extended)

## PostgreSQL

**1. psql Basics**:

```sh
psql -U user -d db
\q                      -- quit
\h                      -- help SQL
?                      -- help psql
\i file.sql             -- run SQL script
```

**2. Object Inspection**:

```sql
\dt                   -- tables
\di                   -- indexes
\dv                   -- views
\df                   -- functions
\d table              -- describe table
\dx                   -- extensions
```

**3. CREATE Essentials**:

```sql
CREATE TABLE t (id SERIAL PRIMARY KEY, ...);
CREATE VIEW v AS SELECT ...;
CREATE MATERIALIZED VIEW mv AS SELECT ...;
CREATE INDEX idx ON t(col);
CREATE UNIQUE INDEX idx_u ON t(col);
CREATE FUNCTION f(...) RETURNS ... AS $$ ... $$ LANGUAGE plpgsql;
```

**4. Data Manipulation**:

```sql
INSERT INTO t(col) VALUES (...), (...), (...);   -- batch insert
UPDATE t SET col = ... WHERE ...;
DELETE FROM t WHERE ...;
TRUNCATE t;
```

**5. Transactions**:

```sql
BEGIN;
UPDATE ...;
SAVEPOINT sp;
ROLLBACK TO sp;
COMMIT;
```

**6. JSON / JSONB**:

```sql
SELECT data->>'name' FROM t;
UPDATE t SET data = jsonb_set(data, '{a,b}', '100'::jsonb);
SELECT jsonb_build_object('a',1,'b',2);
SELECT jsonb_agg(col) FROM t;
```

**7. Indexing**:

```sql
CREATE INDEX idx ON t(col);
CREATE INDEX idx ON t((lower(col)));             -- expression index
CREATE INDEX idx ON t(col1, col2);               -- multi-column
CREATE UNIQUE INDEX idx ON t(col);
CREATE INDEX idx_gin ON t USING gin(jsonb_col);  -- JSONB
CREATE INDEX idx_trgm ON t USING gin (col gin_trgm_ops); -- LIKE/ILIKE
```

**8. Index Inspection**:

```sql
SELECT * FROM pg_indexes WHERE tablename='t';
\d t                    -- shows indexes
EXPLAIN SELECT ...      -- check index usage
```

**9. EXPLAIN / Query Debugging**:

```sql
EXPLAIN SELECT ...;
EXPLAIN ANALYZE SELECT ...;
EXPLAIN (ANALYZE, BUFFERS) SELECT ...;
```

Common nodes:

```sql
Seq Scan      -- no index used
Index Scan    -- good
Bitmap Heap Scan -- semi-indexed
Nested Loop   -- ok for small rows
Hash Join     -- good for large joins
```

**10. Locking / Blocking**:

```sql
SELECT * FROM pg_stat_activity;
SELECT * FROM pg_locks;
SELECT pg_terminate_backend(pid);
```

**11. Performance / Settings**:

```sql
SHOW work_mem;
SET work_mem = '64MB';
SET enable_seqscan = off;
SET statement_timeout = '5s';
```

**12. UPSERT**:

```sql
INSERT INTO t(id,val)
VALUES (1,'x')
ON CONFLICT (id) DO UPDATE SET val = EXCLUDED.val;
```

**13. Partitioning**:

```sql
CREATE TABLE logs (
  id SERIAL,
  ts TIMESTAMP,
  data TEXT
) PARTITION BY RANGE (ts);

CREATE TABLE logs_2024 PARTITION OF logs
  FOR VALUES FROM ('2024-01-01') TO ('2025-01-01');
```

**14. COPY (Fast Import/Export)**:

```sql
COPY t FROM '/file.csv' CSV HEADER;
COPY (SELECT ...) TO '/file.csv' CSV HEADER;
```

**15. Extensions Worth Knowing**:

```sql
CREATE EXTENSION IF NOT EXISTS pgcrypto;     -- hashing/uuid
CREATE EXTENSION IF NOT EXISTS uuid-ossp;    -- uuid_generate_v4()
CREATE EXTENSION IF NOT EXISTS citext;       -- case-insensitive text
CREATE EXTENSION IF NOT EXISTS pg_trgm;      -- fuzzy search
CREATE EXTENSION IF NOT EXISTS hstore;       -- key/value pairs
```

**16. UUID / Security / Hash**:

```sql
SELECT gen_random_uuid();
SELECT crypt('pass', gen_salt('bf'));
SELECT pgp_sym_encrypt('text','key');
```

**17. Temp Tables / CTE**:

```sql
WITH tmp AS (SELECT ... )
SELECT ... FROM tmp;
CREATE TEMP TABLE tmp (...);
```

**18. Time / Date**:

```sql
SELECT now(), current_date, current_timestamp;
SELECT age(now(), '2021-01-01');
SELECT extract(year FROM now());
```

**19. Useful System Views**:

```sql
pg_stat_activity      -- running queries
pg_stat_user_tables   -- seq/index scans
pg_matviews           -- materialized views
pg_available_extensions
```

**20. PL/pgSQL Quick Template**:

```sql
CREATE OR REPLACE FUNCTION f(x int)
RETURNS int AS $$
DECLARE y int;
BEGIN
  y := x * 2;
  RETURN y;
END;
$$ LANGUAGE plpgsql;
```

**21. Debug Function Execution**:

```sql
SELECT * FROM f(10);
RAISE NOTICE 'value: %', var;
```

**22. Search / Replace Text**:

```sql
UPDATE t SET col = regexp_replace(col, 'old', 'new', 'g');
```

**23. Safe Delete Pattern**:

```sql
DELETE FROM t WHERE id IN (SELECT id FROM t LIMIT 5000);
```

**24. Find Table Size**:

```sql
SELECT pg_size_pretty(pg_total_relation_size('t'));
SELECT pg_size_pretty(pg_database_size('dbname'));
```

**25. Materialized View Refresh**:

```sql
REFRESH MATERIALIZED VIEW mv;
REFRESH MATERIALIZED VIEW CONCURRENTLY mv;
```

## Dockering

**26. Docker - Basic Commands**:

```sh
docker ps                          -- running containers
docker ps -a                       -- all containers
docker images                      -- list images
docker logs pg                     -- show logs
docker exec -it pg bash            -- shell into container
docker stop pg
docker start pg
docker rm -f pg
docker volume rm vol
```

**27. Run PostgreSQL Container**:

```sh
docker run -d 
--name pg 
-e POSTGRES_PASSWORD=pass 
-e POSTGRES_USER=user 
-e POSTGRES_DB=appdb 
-v pgdata:/var/lib/postgresql 
postgres:16
```

PG 18+ (ICU collation):

```sh
docker run -d 
--name pg 
-e POSTGRES_PASSWORD=pass 
-e POSTGRES_USER=user 
-e POSTGRES_DB=appdb 
-e POSTGRES_INITDB_ARGS="--locale-provider=icu --icu-locale=sr-Latn-RS --encoding=UTF8" 
-v pgdata:/var/lib/postgresql 
postgres:18
```

**28. Connect to psql from container**:

```sh
docker exec -it pg psql -U user -d appdb
docker exec -it pg psql -U postgres
```

Run SQL file:

```sh
docker exec -i pg sh -c "psql -U user -d appdb" < script.sql
```

Copy file into container:

```sh
docker cp script.sql pg:/tmp/
```

**29. docker-compose.yml (basic)**:

```yaml
services:
  db:
    image: postgres:16
    container_name: pg
    restart: unless-stopped
    environment:
      POSTGRES_USER: user
      POSTGRES_PASSWORD: pass
      POSTGRES_DB: appdb
    volumes:
      - pgdata:/var/lib/postgresql
volumes:
  pgdata:
```

**30. docker-compose with pgAdmin**:

```yaml
services:
  db:
    image: postgres:16
    container_name: pg
    environment:
      POSTGRES_USER: user
      POSTGRES_PASSWORD: pass
      POSTGRES_DB: appdb
    volumes:
      - pgdata:/var/lib/postgresql

  pgadmin:
    image: dpage/pgadmin4
    container_name: pgadmin
    environment:
      PGADMIN_DEFAULT_EMAIL: admin@mail.com
      PGADMIN_DEFAULT_PASSWORD: admin
    ports:
      - "5050:80"
    depends_on:
      - db

volumes:
  pgdata:
```

**31. Clean Reset (delete container + data)**:

```sh
docker rm -f pg
docker volume rm pgdata
```

**32. Export Dump / Import Dump via Docker**:

Dump:

```sh
docker exec -i pg pg_dump -U user appdb > dump.sql
```

Restore:

```sh
cat dump.sql | docker exec -i pg psql -U user -d appdb
```

**33. Check DB Status Inside Container**:

```sh
docker exec -it pg pg_isready -U user
docker exec -it pg psql -U user -c "SELECT version();"
```

**34. Change Postgres Config at Runtime**:

```sh
docker exec -it pg bash
vi /var/lib/postgresql/data/postgresql.conf
docker restart pg
```

(Or via ALTER SYSTEM)

```sh
ALTER SYSTEM SET work_mem='64MB';
SELECT pg_reload_conf();
```
