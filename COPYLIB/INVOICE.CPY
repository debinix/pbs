
           EXEC SQL DECLARE TUTORIAL.INVOICE TABLE
           (
              INV_ID                         INT
                                             NOT NULL,
              CUST_ID                        INT
                                             NOT NULL,
              DEBT_ID                        INT
                                             NOT NULL,
              INVSTATE                       SMALLINT
                                             NOT NULL,
              INVNO                          CHAR (16)
                                             NOT NULL,
              CUSTNO                         CHAR (10)
                                             NOT NULL,
              INVDATE                        DATE,
              VAT                            DECIMAL (3, 2)
                                             NOT NULL
           )
           END-EXEC.


       01  INVOICE.
           03 INVOICE-INV-ID                 PIC S9(9) COMP.
           03 INVOICE-CUST-ID                PIC S9(9) COMP.
           03 INVOICE-DEBT-ID                PIC S9(9) COMP.
           03 INVOICE-INVSTATE               PIC S9(4) COMP.
           03 INVOICE-INVNO                  PIC X(16).
           03 INVOICE-CUSTNO                 PIC X(10).
           03 INVOICE-INVDATE                PIC X(10).
           03 INVOICE-VAT                    PIC S9(1)V9(2) COMP-3.
