## Updating SQL tables

Whenever a change in either PBS table structure or initial data, 
update the local XDB database in MFE:

*MFE->Tools->SQL For DB2->SQL Wizard* and run the sql's in order:

```
drop_tbls.sql
create_tbls.sql
insert_tbls.sql
```
