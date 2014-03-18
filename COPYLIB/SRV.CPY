
           EXEC SQL DECLARE TUTORIAL.SRV TABLE
           (
              SRV_ID                         INT
                                             NOT NULL,
              ARTNO                          VARCHAR (254),
              DESCRIPTION                    VARCHAR (254)
                                             NOT NULL,
              CHARGE                         DECIMAL (5, 2)
                                             NOT NULL,
              CUST_ID                        INT
                                             NOT NULL
           )
           END-EXEC.


       01  SRV.
           03 SRV-SRV-ID                     PIC S9(9) COMP.
           03 SRV-ARTNO.
              49 SRV-ARTNO-LEN               PIC S9(4) COMP.
              49 SRV-ARTNO-TEXT              PIC X(254).
           03 SRV-DESCRIPTION.
              49 SRV-DESCRIPTION-LEN         PIC S9(4) COMP.
              49 SRV-DESCRIPTION-TEXT        PIC X(254).
           03 SRV-CHARGE                     PIC S9(3)V9(2) COMP-3.
           03 SRV-CUST-ID                    PIC S9(9) COMP.
