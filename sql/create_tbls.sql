-- ---------------------------------------------------------------------- 
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
    STREET CHAR(30) NOT NULL,
    POSTNO CHAR(5) NOT NULL,
    PLACE CHAR(30) NOT NULL,
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
    VATREGNO CHAR(14) NOT NULL,
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
    DESCRIPTION CHAR(30) NOT NULL,
    ARTNO CHAR(10) NOT NULL,
    UNITDESC CHAR(10),
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
    NAME CHAR(40) NOT NULL,
    CONTACT CHAR(40) NOT NULL,
    NOTE CHAR(60),
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
    CUSTNO CHAR(10) NOT NULL,
    NAME CHAR(40) NOT NULL,
    BOARDPLACE CHAR(30) NOT NULL,
    OURCONTACT CHAR(40) NOT NULL,
    OURNOTE CHAR(60),
    TEL CHAR(20) NOT NULL,
    EMAIL CHAR(40) NOT NULL,
    WEB CHAR(40),
    ORGNO CHAR(11) NOT NULL,
    ACTIVE CHAR(1) NOT NULL,
    ADDR_ID INTEGER NOT NULL,
    FIN_ID INTEGER NOT NULL,
    DELRATE DECIMAL(3,2) NOT NULL,
    DUEDAYS SMALLINT NOT NULL,
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
-- RESULTCODE: 0 is success, > 0 some error                                                   
-- ---------------------------------------------------------------------- 

CREATE TABLE INLOG (
    INLOG_ID INTEGER NOT NULL,
    CUST_ID INTEGER NOT NULL,
    FILENO INTEGER NOT NULL,
    PROCDATE DATE NOT NULL,
    INVNO CHAR(16) NOT NULL,
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
-- Add table "INERROR"                                                 
-- ---------------------------------------------------------------------- 

CREATE TABLE INERROR (
    INERR_ID INTEGER NOT NULL,
    DESCRIPTION CHAR(60) NOT NULL,
    INLOG_ID INTEGER NOT NULL,
    /* keys */
    PRIMARY KEY (INERR_ID),
    /* foreign keys */
    FOREIGN KEY(INLOG_ID)
	REFERENCES INLOG(INLOG_ID)
);

CREATE UNIQUE INDEX idx_inerror
	ON INERROR(INERR_ID);

-- ---------------------------------------------------------------------- 
-- Add table "SRV"                                                    
-- ---------------------------------------------------------------------- 

CREATE TABLE SRV (
    SRV_ID INTEGER NOT NULL,
    ARTNO CHAR(10) NOT NULL,
    DESCRIPTION CHAR(40) NOT NULL,
    CHARGE DECIMAL(5,2) NOT NULL,
    /* unique */
   UNIQUE (ARTNO),
    /* keys */
    PRIMARY KEY (SRV_ID)
);

CREATE UNIQUE INDEX idx_srv
	ON SRV(SRV_ID);

CREATE UNIQUE INDEX idx_srvartno
	ON SRV(ARTNO);


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
    INVNO CHAR (16) NOT NULL,  
    CUSTNO CHAR (10) NOT NULL,
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

CREATE UNIQUE INDEX idx_initem
	ON INVITEM(INV_ID,ITEM_ID);

-- ---------------------------------------------------------------------- 
-- Add table "SRVCUST"                                                    
-- ---------------------------------------------------------------------- 

CREATE TABLE SRVCUST (
    SRV_ID INTEGER NOT NULL,
    CUST_ID INTEGER NOT NULL,
    /* keys */
    PRIMARY KEY (SRV_ID, CUST_ID),
    /* foreign keys */
    FOREIGN KEY(SRV_ID)
	REFERENCES SRV(SRV_ID),
    FOREIGN KEY(CUST_ID)
	REFERENCES CUSTOMER(CUST_ID)
);

CREATE UNIQUE INDEX idx_srvcust
	ON SRVCUST(SRV_ID,CUST_ID);


