-- ---------------------------------------------------------------------- 
-- Tables for PBS application
-- ---------------------------------------------------------------------- 
-- Run drop_tbls.sql before running this file
-- Add tables. Observe order of creation i.e. FKs must exist. 
-- For MFE/XDB, indexes must exist and created explicitly.                                                          
-- ---------------------------------------------------------------------- 

-- ---------------------------------------------------------------------- 
-- Add table "WORKTBL"                                                    
-- ---------------------------------------------------------------------- 

CREATE TABLE WORKTBL (
    WRK_ID INTEGER NOT NULL,
    CUSTNO INTEGER NOT NULL,
    PRODNO INTEGER NOT NULL,
    ORDNO INTEGER,
    ORDDATE DATE NOT NULL,
    DISCOUNT CHAR(2),
    PRICE INTEGER,
    QUANTITY SMALLINT,
    ORDSUM INTEGER,
    /* keys */
    PRIMARY KEY (WRK_ID)
);

CREATE UNIQUE INDEX idx_worktbl
	ON WORKTBL(WRK_ID);

-- ---------------------------------------------------------------------- 
-- Add table "DEBTOR"                                                     
-- ---------------------------------------------------------------------- 

CREATE TABLE DEBTOR (
    DEBT_ID INTEGER NOT NULL,
    NAME VARCHAR(100) NOT NULL,
    ADDRESS VARCHAR(100) NOT NULL,
    POSTNO CHAR(5) NOT NULL,
    PLACE VARCHAR(40) NOT NULL,
    CONTACT VARCHAR(254),
    NOTES VARCHAR(254),
    DISCOUNT CHAR(2),
    STATUS CHAR(1),
    ORGNO CHAR(11),
    TAXREGNO CHAR(20),
    BANKREQ VARCHAR(254),
    /* keys */
    PRIMARY KEY (DEBT_ID)
);

CREATE UNIQUE INDEX idx_debtor
	ON DEBTOR(DEBT_ID);

-- ---------------------------------------------------------------------- 
-- Add table "CUSTOMER"                                                   
-- ---------------------------------------------------------------------- 

CREATE TABLE CUSTOMER (
    CUS_ID INTEGER NOT NULL,
    NAME VARCHAR(254) NOT NULL,
    ADDRESS VARCHAR(100) NOT NULL,
    POSTNO CHAR(5) NOT NULL,
    PLACE VARCHAR(40) NOT NULL,
    CONTACT VARCHAR(254),
    NOTES VARCHAR(254),
    DISCOUNT CHAR(2),
    STATUS CHAR(1),
    ORGNO CHAR(11),
    TAXREGNO CHAR(20),
    BANKREQ CHAR(9),
    /* keys */
    PRIMARY KEY (CUS_ID)
);

CREATE UNIQUE INDEX idx_customer
	ON CUSTOMER(CUS_ID);

-- ---------------------------------------------------------------------- 
-- Add table "COMPANY"                                                    
-- ---------------------------------------------------------------------- 

CREATE TABLE COMPANY (
    CMP_ID SMALLINT NOT NULL,
    NAME VARCHAR(100),
    ADDRESS VARCHAR(100),
    POSTNO CHARACTER(5),
    PLACE VARCHAR(40),
    BOARDPLACE VARCHAR(40),
    TEL VARCHAR(40),
    EMAIL VARCHAR(40),
    BANKGIRO CHAR(9),
    ORGNO CHAR(11),
    TAXREGNO VARCHAR(40),
    TAXCLASS VARCHAR(40),
    /* keys */
    PRIMARY KEY (CMP_ID)
);

CREATE UNIQUE INDEX idx_company
	ON COMPANY(CMP_ID);

-- ---------------------------------------------------------------------- 
-- Add table "ERRDATA"                                                    
-- ---------------------------------------------------------------------- 

CREATE TABLE ERRDATA (
    ERR_ID INTEGER NOT NULL,
    CUS_ID INTEGER,
    FILE_NR VARCHAR(40),
    DATE DATE,
    ORDNO INTEGER,
    ERRCODE VARCHAR(254),
    /* keys */
    PRIMARY KEY (ERR_ID),
    /* foreign keys */
    FOREIGN KEY(CUS_ID)
	REFERENCES CUSTOMER(CUS_ID)
);

CREATE UNIQUE INDEX idx_errdata
	ON ERRDATA(ERR_ID);

-- ---------------------------------------------------------------------- 
-- Add table "SERVICE"                                                    
-- ---------------------------------------------------------------------- 

CREATE TABLE SERVICE (
    SRV_ID SMALLINT NOT NULL,
    DESCRIPTION VARCHAR(40) NOT NULL,
    CHARGE DECIMAL(3,2) NOT NULL,
    CUS_ID INTEGER,
    /* keys */
    PRIMARY KEY (SRV_ID),
    /* foreign keys */
    FOREIGN KEY(CUS_ID)
	REFERENCES CUSTOMER(CUS_ID)
);

CREATE UNIQUE INDEX idx_service
	ON SERVICE(SRV_ID);

-- ---------------------------------------------------------------------- 
-- Add table "INMETA"                                                     
-- ---------------------------------------------------------------------- 

CREATE TABLE INMETA (
    FILE_ID INTEGER NOT NULL,
    CMP_ID SMALLINT,
    PROCESSED CHAR(1),
    /* keys */
    PRIMARY KEY (FILE_ID),
    /* foreign keys */
    FOREIGN KEY(CMP_ID)
	REFERENCES COMPANY(CMP_ID)
);

CREATE UNIQUE INDEX idx_inmeta
	ON INMETA(FILE_ID);


-- ---------------------------------------------------------------------- 
-- Add table "ARCHINV"                                                  
-- ---------------------------------------------------------------------- 

CREATE TABLE ARCHINV (
    ARCH_ID INTEGER NOT NULL,
    CUS_ID INTEGER NOT NULL,
    DEBT_ID INTEGER NOT NULL,
    STATUS SMALLINT,
    INVDATE DATE,
    INVSUM DECIMAL(9,2),
    VAT DECIMAL(3,2),
    /* keys */
    PRIMARY KEY (ARCH_ID),
    /* foreign keys */
    FOREIGN KEY(CUS_ID)
	REFERENCES CUSTOMER(CUS_ID),
    FOREIGN KEY(DEBT_ID)
	REFERENCES DEBTOR(DEBT_ID)
);

CREATE UNIQUE INDEX idx_archinv
	ON ARCHINV(ARCH_ID);


-- ---------------------------------------------------------------------- 
-- Add table "INVOICE"                                                  
-- ---------------------------------------------------------------------- 

CREATE TABLE INVOICE (
    INV_ID INTEGER NOT NULL,
    CUS_ID INTEGER NOT NULL,
    DEBT_ID INTEGER NOT NULL,
    STATUS SMALLINT,
    INVDATE DATE,
    INVSUM DECIMAL(9,2),
    VAT DECIMAL(3,2) NOT NULL,
    /* keys */
    PRIMARY KEY (INV_ID),
    /* foreign keys */
    FOREIGN KEY(CUS_ID)
	REFERENCES CUSTOMER(CUS_ID),
    FOREIGN KEY(DEBT_ID)
	REFERENCES DEBTOR(DEBT_ID)
);

CREATE UNIQUE INDEX idx_invoice
	ON INVOICE(INV_ID);


-- ---------------------------------------------------------------------- 
-- Add table "ITEM"                                                      
-- ---------------------------------------------------------------------- 

CREATE TABLE ITEM (
    ITEM_ID INTEGER NOT NULL,
    DESCRIPTION VARCHAR(254),
    PARTNO VARCHAR(40),
    UNIT VARCHAR(25),
    ITEMQTY DECIMAL(7,2),
    UNITPRICE DECIMAL(9,2),
    /* keys */
    PRIMARY KEY (ITEM_ID)
);

CREATE UNIQUE INDEX idx_item
	ON ITEM(ITEM_ID);

-- ---------------------------------------------------------------------- 
-- Add table "INVITEM"                                                    
-- ---------------------------------------------------------------------- 

CREATE TABLE INVITEM (
    INV_ID INTEGER NOT NULL,
    ITEM_ID INTEGER NOT NULL,
    /* keys */
    PRIMARY KEY (INV_ID, ITEM_ID),
    /* foreign keys */
    FOREIGN KEY(INV_ID)
	REFERENCES INVOICE(INV_ID),
    FOREIGN KEY(ITEM_ID)
	REFERENCES ITEM(ITEM_ID)
);

CREATE UNIQUE INDEX idx_invitem
	ON INVITEM(INV_ID,ITEM_ID);

-- ---------------------------------------------------------------------- 
-- Add table "CUSPROD"   -  
-- Internal development validation table                                          
-- ---------------------------------------------------------------------- 

CREATE TABLE CUSPROD (
    PROD_ID INTEGER NOT NULL,
    NAME VARCHAR(254) NOT NULL,
    ARTICLE_ID VARCHAR(30) NOT NULL,
    PRICE INTEGER NOT NULL,
    VAT DECIMAL(3,2) NOT NULL,
    /* keys */
    PRIMARY KEY (PROD_ID)
);

CREATE UNIQUE INDEX idx_cusprod
	ON CUSPROD(PROD_ID);
