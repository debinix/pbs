
           EXEC SQL DECLARE TUTORIAL.ITEM TABLE
           (
              ITEM_ID                        INT
                                             NOT NULL,
              DESCRIPTION                    CHAR (30)
                                             NOT NULL,
              ARTNO                          CHAR (10)
                                             NOT NULL,
              UNITDESC                       CHAR (10),
              QTY                            DECIMAL (7, 2)
                                             NOT NULL,
              PRICE                          DECIMAL (9, 2)
                                             NOT NULL
           )
           END-EXEC.


       01  ITEM.
           03 ITEM-ITEM-ID                   PIC S9(9) COMP.
           03 ITEM-DESCRIPTION               PIC X(30).
           03 ITEM-ARTNO                     PIC X(10).
           03 ITEM-UNITDESC                  PIC X(10).
           03 ITEM-QTY                       PIC S9(5)V9(2) COMP-3.
           03 ITEM-PRICE                     PIC S9(7)V9(2) COMP-3.
