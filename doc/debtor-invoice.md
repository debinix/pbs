## Invoice template for debtor

This document describes fields used on the debtor invoice and related tables.
If uncertain, look at *sql/insert_data.sql* file.

### Debtor invoice fields

Fields read from top to bottom on the invoice page. 

#### Heading

|Invoice text       |Table/column           |Note                          |
|-------------------|-----------------------|------------------------------|
|Fakturanummer      |INVOICE/invno          |                              |
|Kundnummer         |INVOICE/custno         |                              |
|Fakturadatum       |INVOICE/invdate        |                              |
|Förfallodatum      |CUSTOMER/duedays       |Add to invoice date           |
|Vår referens       |CUSTOMER/ourcontact    |                              |
|Telefon            |CUSTOMER/tel           |                              |
|Kundens namn       |DEBTOR/name            |Company name                  |
|Er referens        |DEBTOR/contact         |Contact person                |
|                   |ADDR/street            |Debtor street address         |
|                   |ADDR/postno            |Post number                   |
|                   |ADDR/place             |Debtor office location        |

In addition, a note to the debtor can be printed on the debtor invoice using
CUSTOMER/ournote i.e. "Betala till vårt BG 9999-9999".

#### Purchased items

First row is a text heading for all detail items.

|Invoice text 1   |Invoice text 2   |Invoice text 3   |Invoice text 4   |Invoice text 5    |
|-----------------|-----------------|-----------------|-----------------|------------------|
|Benämning        |Enhet            |Antal            |Pris/enhet       |Summma            |
|ITEM/description |ITEM/unitdesc    |ITEM/qty         |ITEM/price       |*Note 1*          |

Note 1: Coloumn 5 is calculated.

#### Item summaries

These are all calculated.

|Invoice text       |Table/column           |Note                                  |
|-------------------|-----------------------|--------------------------------------|
|Netto              |                       |Calculated by adding sub-items sums   |
|Moms 12%           |INVOICE/vat            |Actual VAT used on debtor invoice     |
|Öresutjämning      |INVOICE/invdate        |Calculated accoring to rounding rules |
|Summa att betala   |                       |Calculated total order sum            |

#### Footer invoice fields

Before the footer details, a note to the debtor about potental delay payment is printed.
I.e. "Efter förfallodagen debiteras dröjsmålsränta med 10%" were rate is from 
table/coloumn *SRV/delrate*.

|Table/column           |Note                                     |
|-----------------------|-----------------------------------------|
|CUSTOMER/name          |Customer/PBS customer name               |
|ADDR/street            |Customer/PBS customer street address     |
|ADDR/postno            |Customer/PBS customer post number        |
|ADDR/place             |Customer/PBS office location             |
|CUSTOMER/boardplace    |Text: 'Företagets säte XXXXXXXXXX'       |


|Table/column           |Note                                     |
|-----------------------|-----------------------------------------|
|CUSTOMER/tel           |Text 'Tel. XX-XXXXXXXX'                  |
|CUSTOMER/web           |                                         |
|CUSTOMER/email         |                                         |

|Table/column           |Note                                     |
|-----------------------|-----------------------------------------|
|FIN/bankgiro           |Text: 'Bankgiro XXXX-XXXXX'              |
|FIN/postgiro           |Text: 'Postgiro XXXXXX-X'                |
|CUSTOMER/orgno         |Text: 'Org nr XXXXXX-XXXX'               |
|FIN/vatregno           |Text: 'Moms nr XXXXXXXX-XXXXXX'          |


