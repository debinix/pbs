## Postformat (Swedish version)

En transaktionsfil består av ett antal efter varandra följande poster uppbyggda på samma sätt. Varje post inleds med ett fält som anger vilken typ av post det är frågan om. Efter posttypen följer ett antal fält bestående av ett bestämt antal tecken och med ett fast fältformat. Respektive posttyp beskrivs nedan.
Varje post består av 300 tecken.

### Fältformat

Posterna beskrivs nedan som ett antal fält som ska följa ett specifikt format.
Innehållet i fälten ska följa teckenformatet ISO 8859-1.

|Format  |Betydelse                                                                                                                           |
|--------|------------------------------------------------------------------------------------------------------------------------------------|
|Blankt  |Fältet ska vara fyllt med blanktecken (Space)                                                                                       |
|X       |Godkända tecken är: abcdefghijklmnopqrstuvwxyzåäöéABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖÉ!"§%&/()=?1234567890+*<>;:_,.-üÜ                    |
|9       |Fältet innehåller numeriska tecken och ska högerjusteras med inledande nollor "0"                                                   |
|9.99    |Fältet innehåller ett tal med två decimaler. Punkt eller kommatecken ska ej anges. Talet ska högerjusteras med inledande nollor "0" |

#### Startpost (10)

Startpost används för att unikt identifiera användaren och säkerställa att alla filer överförs utan dubbletter.

|Position|Antal tecken|Format|Fält        |Beskrivning                                                                                                     |
|--------|------------|------|------------|----------------------------------------------------------------------------------------------------------------|
|1-2     |2           |X     |POSTTYP-10  |Startpost = "10"                                                                                                |
|3-10    |8           |X     |SYSTEM-10   |Namn på levererande system = "ORDER IN". Används internt på PBS för att skilja ut och ingående orderinformation |
|11-20   |10          |X     |KNDNR-10    |Kundnummer i (alfa)numeriskt format                                                                             |
|21-28   |8           |X     |FILDAT-10   |Framställningsdatum i formen ÅÅÅÅMMDD                                                                           |
|29-33   |5           |X     |FILNR-10    |Filens löpnummer. Numeriskt utfyllt med nollor. Dvs. 00001 etc.                                                 |
|34-300  |266         |Blankt|FILLER      |Reserv, blank                                                                                                   |

#### Faktura (20)

Posten innehåller information om fakturan.

|Position|Antal tecken|Format|Fält       |Beskrivning                                    |
|--------|------------|------|-----------|-----------------------------------------------|
|1-2     |2           |X     |POSTTYP-20 |Order = "20"                                   |
|3-12    |10          |9     |FKTNR1-20  |Kundens fakturanummer i numerisk form          |
|13-24   |10          |X     |GELNR2-20  |Gäldenärens kundnummer i alfanumeriskt format  |
|25-32   |8           |9     |ORDDAT-20  |Orderdatum i formen ÅÅÅÅMMDD                   |
|33-34   |2           |X     |ORDRAB-20  |Ev. rabatt i formen XX. Kan vara blank         |
|35-36   |2           |9     |MOMS-20    |Momssats i numeriskt format                    |
|37-44   |8           |9     |ORDSUM-20  |Total summa för ordern                         |
|45-60   |16          |X     |FAKTNR-20  |Kundens fakturanummer                          |
|61-100  |40          |X     |GELNMN-20  |Gäldenärens namn                               |
|101-130 |30          |X     |GELADD1-20 |Gäldenärens adress 1                           |
|131-160 |30          |X     |GELADD2-20 |Gäldenärens adress 2                           |  
|161-190 |30          |X     |GELADD3-20 |Gäldenärens adress 3                           | 
|191-195 |5           |X     |GELPNR-20  |Gäldenärens postnummer                         |
|196-225 |30          |X     |GELPOR-20  |Gäldenärens postort                            |
|226-235 |10          |9     |GELONR     |Gäldenärens organisationsnummer                |
|236-300 |65          |X     |FILLER     |Reserv, blank                                  |

#### Artikel (30)

Posten innehåller information om artiklarna i en faktura.

|Position|Antal tecken|Format|Fält       |Beskrivning                                                               |
|--------|------------|------|-----------|--------------------------------------------------------------------------|
|1-2     |2           |X     |POSTTYP-30 |Order = "30"                                                              |
|3-12    |10          |9     |ARTNR-30   |Artikelnummer i numerisk form                                             |
|13-16   |4           |9     |ARTANT-30  |Antal artiklar i numeriskt format                                         |
|17-22   |6           |9     |ARTPRS-30  |Artikel pris i numeriskt format                                           |
|23-31   |9           |9     |SUMMA-30   |Artikelsumma i numeriskt form                                             |
|32-200  |168         |X     |BESKRIV-30 |Beskrivning i alfanumeriskt format                                        |
|201-300 |100         |X     |FILLER     |Reserv                                                                    |

#### Slutpost (90)

Posten anger slutet på filen och innehåller totala antalet poster i filen, som kontroll att filen är intakt.

|Position|Antal tecken|Format|Fält       |Beskrivning                                         |
|--------|------------|------|-----------|----------------------------------------------------|
|1-2     |2           |9     |POSTTYP-90 |Slutpost = "90"                                     |
|3-15    |13          |X     |FILLER     |Ledtext 0 "ANTAL POSTER "                           |
|16-21   |6           |9     |ANTAL-90   |Antal poster på filen inkl. start- och slutposterna |
|22-300  |279         |Blankt|FILLER     |Reserv, blank                                       |



