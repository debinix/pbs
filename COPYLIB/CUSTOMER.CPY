
           EXEC SQL DECLARE TUTORIAL.CUSTOMER TABLE
           (
              CUST_ID                        INT
                                             NOT NULL,
              CUSTNO                         CHAR (10)
                                             NOT NULL,
              NAME                           CHAR (40)
                                             NOT NULL,
              BOARDPLACE                     CHAR (30)
                                             NOT NULL,
              OURCONTACT                     CHAR (40)
                                             NOT NULL,
              OURNOTE                        CHAR (60),
              TEL                            CHAR (20)
                                             NOT NULL,
              EMAIL                          CHAR (40)
                                             NOT NULL,
              WEB                            CHAR (40),
              ORGNO                          CHAR (11)
                                             NOT NULL,
              ACTIVE                         CHAR (1)
                                             NOT NULL,
              ADDR_ID                        INT
                                             NOT NULL,
              FIN_ID                         INT
                                             NOT NULL,
              DELRATE                        DECIMAL (3, 2)
                                             NOT NULL,
              DUEDAYS                        SMALLINT
                                             NOT NULL
           )
           END-EXEC.


       01  CUSTOMER.
           03 CUSTOMER-CUST-ID               PIC S9(9) COMP.
           03 CUSTOMER-CUSTNO                PIC X(10).
           03 CUSTOMER-NAME                  PIC X(40).
           03 CUSTOMER-BOARDPLACE            PIC X(30).
           03 CUSTOMER-OURCONTACT            PIC X(40).
           03 CUSTOMER-OURNOTE               PIC X(60).
           03 CUSTOMER-TEL                   PIC X(20).
           03 CUSTOMER-EMAIL                 PIC X(40).
           03 CUSTOMER-WEB                   PIC X(40).
           03 CUSTOMER-ORGNO                 PIC X(11).
           03 CUSTOMER-ACTIVE                PIC X.
           03 CUSTOMER-ADDR-ID               PIC S9(9) COMP.
           03 CUSTOMER-FIN-ID                PIC S9(9) COMP.
           03 CUSTOMER-DELRATE               PIC S9(1)V9(2) COMP-3.
           03 CUSTOMER-DUEDAYS               PIC S9(4) COMP.
