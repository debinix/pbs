
           EXEC SQL DECLARE TUTORIAL.ITEM TABLE
           (
              ITEM_ID                        INT
                                             NOT NULL,
              DESCRIPTION                    VARCHAR (254)
                                             NOT NULL,
              ARTNO                          VARCHAR (254),
              UNITDESC                       VARCHAR (254),
              QTY                            DECIMAL (7, 2)
                                             NOT NULL,
              PRICE                          DECIMAL (9, 2)
                                             NOT NULL
           )
           END-EXEC.


       01  ITEM.
           03 ITEM-ITEM-ID                   PIC S9(9) COMP.
           03 ITEM-DESCRIPTION.
              49 ITEM-DESCRIPTION-LEN        PIC S9(4) COMP.
              49 ITEM-DESCRIPTION-TEXT       PIC X(254).
           03 ITEM-ARTNO.
              49 ITEM-ARTNO-LEN              PIC S9(4) COMP.
              49 ITEM-ARTNO-TEXT             PIC X(254).
           03 ITEM-UNITDESC.
              49 ITEM-UNITDESC-LEN           PIC S9(4) COMP.
              49 ITEM-UNITDESC-TEXT          PIC X(254).
           03 ITEM-QTY                       PIC S9(5)V9(2) COMP-3.
           03 ITEM-PRICE                     PIC S9(7)V9(2) COMP-3.
