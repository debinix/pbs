
           EXEC SQL DECLARE TUTORIAL.INVITEM TABLE
           (
              INV_ID                         INT
                                             NOT NULL,
              ITEM_ID                        INT
                                             NOT NULL
           )
           END-EXEC.


       01  INVITEM.
           03 INVITEM-INV-ID                 PIC S9(9) COMP.
           03 INVITEM-ITEM-ID                PIC S9(9) COMP.
