## Project PBS

### An Exercise in COBOL and Embedded SQL

This is the repository for a project with the objective to create an invoice application
based on Micro Focus COBOL and the built-in XDB relational database.

The COBOL environment used here is based on [Main Frame Express](http://www.microfocus.com/products/enterprise/mfeee.aspx) (MFE).

MFE starts to show its age, and a more modern environment, the [Enterprise Developer Personal Edition](http://online.microfocus.com/enterprise-developer-pe) is available for *free* (but without debug capability) for students.

The development team use Atlassians [SourceTree Git client](https://www.atlassian.com/software/sourcetree/overview ) for GIT management on Windows developer machines.
 

### Version Information

Windows - v7

Main Frame Express - v3.1

Source Tree - v1.4.1


### Directory Structure.

The directory structure for the system development team:

```
.
├── COPYLIB
├── data
├── doc
├── sql
└── tex
```

COPYLIB contains DLCGEN generated table structures and two error routines used in the project. One for SQL-errors and the other routine logs database errors to file with a timestamp. The sql directory contains the SQL DDL/DML statements to create and populate an initial database. The tex directory is for latex file for invoice pdf creation.
Cobol source files are kept at root.

 