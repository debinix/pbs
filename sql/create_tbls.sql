- ---------------------------------------------------------------------- 
-- Tables for PBS application
-- ---------------------------------------------------------------------- 
-- Run drop_tbls.sql before running this file
-- Add tables. Observe order of creation i.e. FKs must exist. 
-- For MFE/XDB, indexes must exist and created explicitly.                                                          
-- ---------------------------------------------------------------------- 

-- ---------------------------------------------------------------------- 
-- Add table "ADDR"                                                    
-- ---------------------------------------------------------------------- 

CREATE TABLE ADDR (
    ADDR_ID INTEGER NOT NULL,
    STREET VARCHAR(254) NOT NULL,
    POSTNO CHAR(5) NOT NULL,
    PLACE VARCHAR(254) NOT NULL,
    /* keys */
    PRIMARY KEY (ADDR_ID)
);

CREATE UNIQUE INDEX idx_addr
	ON ADDR(ADDR_ID);

-- ---------------------------------------------------------------------- 
-- Add table "FINDATA"                                                    
-- ---------------------------------------------------------------------- 

CREATE TABLE FINDATA (
    FIN_ID INTEGER NOT NULL,
    VATREGNO VARCHAR(254) NOT NULL,
    BANKGIRO CHAR(9) NOT NULL,
    POSTGIRO CHAR(8) NOT NULL,
    /* keys */
    PRIMARY KEY (FIN_ID)
);

CREATE UNIQUE INDEX idx_fin
	ON FINDATA(FIN_ID);

-- ---------------------------------------------------------------------- 
-- Add table "ITEM"                                                      
-- ---------------------------------------------------------------------- 

CREATE TABLE ITEM (
    ITEM_ID INTEGER NOT NULL,
    DESCRIPTION VARCHAR(254) NOT NULL,
    ARTNO CHAR(10) NOT NULL,
    UNITDESC VARCHAR(254),
    QTY DECIMAL(7,2) NOT NULL,
    PRICE DECIMAL(9,2) NOT NULL,
    /* keys */
    PRIMARY KEY (ITEM_ID)
);

CREATE UNIQUE INDEX idx_item
	ON ITEM(ITEM_ID);

-- ---------------------------------------------------------------------- 
-- Add table "DEBTOR"                                                     
-- ---------------------------------------------------------------------- 

CREATE TABLE DEBTOR (
    DEBT_ID INTEGER NOT NULL,
    NAME VARCHAR(254) NOT NULL,
    CONTACT VARCHAR(254) NOT NULL,
    NOTE VARCHAR(254),
    ADDR_ID INTEGER NOT NULL,
    /* keys */
    PRIMARY KEY (DEBT_ID),
    /* foreign keys */
    FOREIGN KEY(ADDR_ID)
	REFERENCES ADDR(ADDR_ID)
);

CREATE UNIQUE INDEX idx_debtor
	ON DEBTOR(DEBT_ID);

-- ---------------------------------------------------------------------- 
-- Add table "CUSTOMER"                                                   
-- ---------------------------------------------------------------------- 

CREATE TABLE CUSTOMER (
    CUST_ID INTEGER NOT NULL,
    CUSTNO INTEGER NOT NULL,
    NAME VARCHAR(254) NOT NULL,
    BOARDPLACE VARCHAR(254) NOT NULL,
    OURCONTACT VARCHAR(254) NOT NULL,
    OURNOTE VARCHAR(254),
    TEL VARCHAR(254) NOT NULL,
    EMAIL VARCHAR(254) NOT NULL,
    WEB VARCHAR(254),
    ORGNO CHAR(11) NOT NULL,
    ACTIVE CHAR(1) NOT NULL,
    ADDR_ID INTEGER NOT NULL,
    FIN_ID INTEGER NOT NULL,
    /* keys */
    PRIMARY KEY (CUST_ID),
    /* foreign keys */
    FOREIGN KEY(ADDR_ID)
	REFERENCES ADDR(ADDR_ID),
    FOREIGN KEY(FIN_ID)
	REFERENCES FINDATA(FIN_ID)
);

CREATE UNIQUE INDEX idx_customer
	ON CUSTOMER(CUST_ID);


-- ---------------------------------------------------------------------- 
-- Add table "INLOG"                                                    
-- ---------------------------------------------------------------------- 

CREATE TABLE INLOG (
    INLOG_ID INTEGER NOT NULL,
    CUST_ID INTEGER NOT NULL,
    FILENO INTEGER NOT NULL,
    PROCDATE DATE NOT NULL,
/* what was the reason for ORDNO  */
    ORDNO INTEGER,
    RESULTCODE SMALLINT NOT NULL,
    /* keys */
    PRIMARY KEY (INLOG_ID),
    /* foreign keys */
    FOREIGN KEY(CUST_ID)
	REFERENCES CUSTOMER(CUST_ID)
);

CREATE UNIQUE INDEX idx_errdata
	ON INLOG(INLOG_ID);

-- ---------------------------------------------------------------------- 
-- Add table "SRV"                                                    
-- ---------------------------------------------------------------------- 

CREATE TABLE SRV (
    SRV_ID INTEGER NOT NULL,
    ARTNO VARCHAR(254),
    DESCRIPTION VARCHAR(254) NOT NULL,
    CHARGE DECIMAL(5,2) NOT NULL,
    CUST_ID INTEGER NOT NULL,
    DELRATE DECIMAL(3,2) NOT NULL,
    DUEDAYS SMALLINT NOT NULL,
    /* keys */
    PRIMARY KEY (SRV_ID),
    /* foreign keys */
    FOREIGN KEY(CUST_ID)
	REFERENCES CUSTOMER(CUST_ID)
);

CREATE UNIQUE INDEX idx_SRV
	ON SRV(SRV_ID);


-- ---------------------------------------------------------------------- 
-- Add table "INVOICE"
--  INVSTATE: 0=received-and-accepted, 1=printed, 2=remainded, 
--                   3=transfer-to-collect-closed, 9=closed-with-customer
-- ---------------------------------------------------------------------- 

CREATE TABLE INVOICE (
    INV_ID INTEGER NOT NULL,
    CUST_ID INTEGER NOT NULL,
    DEBT_ID INTEGER NOT NULL,
    INVSTATE SMALLINT NOT NULL,  
    INVNO VARCHAR (254) NOT NULL,  
    CUSTNO VARCHAR (254) NOT NULL,
    INVDATE DATE,
    VAT DECIMAL(3,2) NOT NULL,
    /* keys */
    PRIMARY KEY (INV_ID),
    /* foreign keys */
    FOREIGN KEY(CUST_ID)
	REFERENCES CUSTOMER(CUST_ID),
    FOREIGN KEY(DEBT_ID)
	REFERENCES DEBTOR(DEBT_ID)
);

CREATE UNIQUE INDEX idx_invoice
	ON INVOICE(INV_ID);

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


