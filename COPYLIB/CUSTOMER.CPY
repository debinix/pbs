
           EXEC SQL DECLARE TUTORIAL.CUSTOMER TABLE
           (
              CUST_ID                        INT
                                             NOT NULL,
              CUSTNO                         VARCHAR (254)
                                             NOT NULL,
              NAME                           VARCHAR (254)
                                             NOT NULL,
              BOARDPLACE                     VARCHAR (254)
                                             NOT NULL,
              OURCONTACT                     VARCHAR (254)
                                             NOT NULL,
              OURNOTE                        VARCHAR (254),
              TEL                            VARCHAR (254)
                                             NOT NULL,
              EMAIL                          VARCHAR (254)
                                             NOT NULL,
              WEB                            VARCHAR (254),
              ORGNO                          CHAR (14)
                                             NOT NULL,
              ACTIVE                         CHAR (1)
                                             NOT NULL,
              ADDR_ID                        INT
                                             NOT NULL,
              FIN_ID                         INT
                                             NOT NULL
           )
           END-EXEC.


       01  CUSTOMER.
           03 CUSTOMER-CUST-ID               PIC S9(9) COMP.
           03 CUSTOMER-CUSTNO.
              49 CUSTOMER-CUSTNO-LEN         PIC S9(4) COMP.
              49 CUSTOMER-CUSTNO-TEXT        PIC X(254).
           03 CUSTOMER-NAME.
              49 CUSTOMER-NAME-LEN           PIC S9(4) COMP.
              49 CUSTOMER-NAME-TEXT          PIC X(254).
           03 CUSTOMER-BOARDPLACE.
              49 CUSTOMER-BOARDPLACE-LEN     PIC S9(4) COMP.
              49 CUSTOMER-BOARDPLACE-TEXT    PIC X(254).
           03 CUSTOMER-OURCONTACT.
              49 CUSTOMER-OURCONTACT-LEN     PIC S9(4) COMP.
              49 CUSTOMER-OURCONTACT-TEXT    PIC X(254).
           03 CUSTOMER-OURNOTE.
              49 CUSTOMER-OURNOTE-LEN        PIC S9(4) COMP.
              49 CUSTOMER-OURNOTE-TEXT       PIC X(254).
           03 CUSTOMER-TEL.
              49 CUSTOMER-TEL-LEN            PIC S9(4) COMP.
              49 CUSTOMER-TEL-TEXT           PIC X(254).
           03 CUSTOMER-EMAIL.
              49 CUSTOMER-EMAIL-LEN          PIC S9(4) COMP.
              49 CUSTOMER-EMAIL-TEXT         PIC X(254).
           03 CUSTOMER-WEB.
              49 CUSTOMER-WEB-LEN            PIC S9(4) COMP.
              49 CUSTOMER-WEB-TEXT           PIC X(254).
           03 CUSTOMER-ORGNO                 PIC X(14).
           03 CUSTOMER-ACTIVE                PIC X.
           03 CUSTOMER-ADDR-ID               PIC S9(9) COMP.
           03 CUSTOMER-FIN-ID                PIC S9(9) COMP.
