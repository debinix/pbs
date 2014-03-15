
           EXEC SQL DECLARE TUTORIAL.DEBTOR TABLE
           (
              DEBT_ID                        INT
                                             NOT NULL,
              NAME                           VARCHAR (254)
                                             NOT NULL,
              CONTACT                        VARCHAR (254)
                                             NOT NULL,
              NOTE                           VARCHAR (254),
              ADDR_ID                        INT
                                             NOT NULL
           )
           END-EXEC.


       01  DEBTOR.
           03 DEBTOR-DEBT-ID                 PIC S9(9) COMP.
           03 DEBTOR-NAME.
              49 DEBTOR-NAME-LEN             PIC S9(4) COMP.
              49 DEBTOR-NAME-TEXT            PIC X(254).
           03 DEBTOR-CONTACT.
              49 DEBTOR-CONTACT-LEN          PIC S9(4) COMP.
              49 DEBTOR-CONTACT-TEXT         PIC X(254).
           03 DEBTOR-NOTE.
              49 DEBTOR-NOTE-LEN             PIC S9(4) COMP.
              49 DEBTOR-NOTE-TEXT            PIC X(254).
           03 DEBTOR-ADDR-ID                 PIC S9(9) COMP.
