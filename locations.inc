; Locations
data1:
  .byte "YOU ARE STANDING AT THE END OF A ROAD BEFORE A SMALL BRICK BUILDING. AROUND"
  .byte $0D, $0A
  .asciiz "YOU IS A FOREST. A SMALL STREAM FLOWS OUT OF THE BUILDING AND DOWN A GULLY" ; 1
  .byte 17           ; IN
  .addr data3
  .byte 50           ; EAST
  .addr data3
  .byte 5            ; SOUTH
  .addr data4
  .byte 60           ; WEST
  .addr data2
  .byte 7            ; UP
  .addr data2
  .byte 8            ; DOWN
  .addr data4
  .byte 3            ; NORTH
  .addr data5
  .byte $fe          ; Delimeter
data2:
  .byte "YOU HAVE WALKED UP A HILL, STILL IN THE FOREST THE ROAD NOW SLOPES BACK DOWN"
  .byte $0D, $0A
  .asciiz "THE OTHER SIDE OF THE HILL. THERE IS A BUILDING IN THE DISTANCE" ; 2
  .byte 3            ; NORTH
  .addr data1
  .byte 50           ; EAST
  .addr data1
  .byte 5            ; SOUTH
  .addr data5
  .byte 8            ; DOWN
  .addr data1
  .byte $fe          ; Delimeter
data3:
  .asciiz "YOU ARE INSIDE A BUILDING, A WELL HOUSE FOR A LARGE SPRING" ; 3
  .byte 60          ; WEST
  .addr data1
  .byte 27          ; OUT
  .addr data1
  .byte $fe         ; Delimeter
data4:
  .asciiz "YOU ARE IN A VALLEY IN THE FOREST BESIDE A STREAM TUMBLING ALONG A ROCKY BED" ; 4
  .byte 3          ; NORTH
  .addr data1 ; NORTH
  .byte 50         ; EAST
  .addr data5   ; EAST
  .byte 5          ; SOUTH
  .addr data7   ; SOUTH
  .byte 60         ; WEST
  .addr data5   ; WEST
  .byte 7          ; UP
  .addr data5   ; UP
  .byte 8          ; DOWN
  .addr data5   ; DOWN
  .byte $fe         ; Delimeter
data5:
  .asciiz "YOU ARE IN OPEN FOREST, WITH A DEEP VALLEY TO ONE SIDE" ; 5
  .byte 3          ; NORTH
  .addr data6   ; NORTH
  .byte 50         ; EAST
  .addr data4   ; EAST
  .byte 5          ; SOUTH
  .addr data5   ; SOUTH
  .byte 60         ; WEST
  .addr data5   ; WEST
  .byte 8          ; DOWN
  .addr data4   ; DOWN
  .byte $fe         ; Delimeter
data6:
  .asciiz "YOU ARE IN OPEN FOREST NEAR BOTH A VALLEY AND A ROAD" ; 6
  .byte 50         ; EAST
  .addr data4      ; EAST
  .byte 60         ; WEST
  .addr data4      ; WEST
  .byte 3          ; NORTH
  .addr data1      ; NORTH
  .byte $fe         ; Delimeter
data7:
  .byte "AT YOUR FEET ALL THE WATER OF THE STREAM SPLASHES INTO A 2 INCH SLIT IN THE"
  .byte $0D, $0A
  .asciiz "ROCK. DOWNSTREAM THE STREAMBED IS BARE ROCK" ; 7
  .byte 3          ; NORTH
  .addr data4      ; NORTH
  .byte 50         ; EAST
  .addr data5      ; EAST
  .byte 5          ; SOUTH
  .addr data8      ; SOUTH
  .byte 60         ; WEST
  .addr data5      ; WEST
  .byte $fe         ; Delimeter
data8:
  .byte "YOU ARE IN A 20 FOOT DEPRESSION FLOORED WITH BARE DIRT. SET INTO THE DIRT IS"
  .byte $0D, $0A
  .byte "A STRONG STEEL GRATE MOUNTED IN CONCRETE. A DRY STREAMBED LEADS INTO THE"
  .byte $0D, $0A
  .asciiz "DEPRESSION" ; 8
  .byte 3          ; NORTH
  .addr data7      ; NORTH
  .byte 50         ; EAST
  .addr data5      ; EAST
  .byte 5          ; SOUTH
  .addr data5      ; SOUTH
  .byte 60         ; WEST
  .addr data5      ; WEST
  .byte 8          ; DOWN
  .addr data9      ; DOWN
  .byte $fe         ; Delimeter
data9:
  .byte "YOU ARE IN A SMALL CHAMBER BENEATH A 3X3 STEEL GRATE TO THE SURFACE. A LOW"
  .byte $0D, $0A
  .asciiz "CRAWL OVER COBBLES LEADS INWARD TO THE WEST" ; 9
  .byte 60          ; WEST
  .addr data10      ; WEST
  .byte 7           ; UP
  .addr data8       ; UP
  .byte $fe         ; Delimeter
data10:
  .byte "YOU ARE CRAWLING OVER COBBLES IN A LOW PASSAGE. THERE IS A DIM LIGHT AT THE"
  .byte $0D, $0A
  .asciiz " EAST END OF THE PASSAGE" ; 10
  .byte 50           ; EAST
  .addr data9     ; EAST
  .byte 60           ; WEST
  .addr data11    ; WEST
  .byte $fe         ; Delimeter
data11:
  .byte "YOU ARE IN A DEBRIS ROOM, FILLED WITH STUFF WASHED IN FROM THE SURFACE. A LOW"
  .byte $0D, $0A
  .byte "WIDE PASSAGE WITH COBBLES BECOMES PLUGGED WITH MUD AND DEBRIS HERE, BUT AN"
  .byte $0D, $0A
  .asciiz "AWKWARD CANYON LEADS UPWARD AND WEST. A NOTE ON THE WALL SAYS 'MAGIC WORD XYZZY'"
  .byte 50           ; EAST
  .addr data10    ; EAST
  .byte 60           ; WEST
  .addr data12    ; WEST
  .byte 7            ; UP
  .addr data12    ; UP
  .byte $fe         ; Delimeter
data12:
  .asciiz "YOU ARE IN AN AWKWARD SLOPING EAST/WEST CANYON" ; 12
  .byte 50           ; EAST
  .addr data11    ; EAST
  .byte 60           ; WEST
  .addr data13    ; WEST
  .byte 7            ; UP
  .addr data13    ; UP
  .byte 8            ; DOWN
  .addr data11    ; DOWN
  .byte $fe         ; Delimeter
data13:
  .byte "YOU ARE IN A SPLENDID CHAMBER THIRTY FEET HIGH. THE WALLS ARE FROZEN RIVERS OF"
  .byte $0D, $0A
  .byte "ORANGE STONE. AN AWKWARD CANYON AND A GOOD PASSAGE EXIT FROM EAST AND WEST SIDES"
  .byte $0D, $0A
  .asciiz "OF THE CHAMBER" ; 13
  .byte 50           ; EAST
  .addr data12    ; EAST
  .byte 60           ; WEST
  .addr data14    ; WEST
  .byte $fe         ; Delimeter
data14:
  .byte "AT YOUR FEET IS A SMALL PIT BREATHING TRACES OF WHITE MIST. AN EAST PASSAGE ENDS"
  .byte $0D, $0A
  .asciiz "HERE EXCEPT FOR A SMALL CRACK LEADING ON" ; 14
  .byte 50           ; EAST
  .addr data13    ; EAST
  .byte 60           ; WEST
  .addr data16    ; WEST
  .byte 7            ; UP
  .addr data15    ; UP
  .byte $fe         ; Delimeter
data15:
  .byte "YOU ARE AT ONE END OF A VAST HALL STRETCHING FORWARD OUT OF SIGHT TO THE WEST."
  .byte $0D, $0A
  .byte "THERE ARE OPENINGS TO EITHER SIDE. NEARBY, A WIDE STONE STAIRCASE LEADS"
  .byte $0D, $0A
  .asciiz "DOWNWARD. THE HALL IS FILLED WITH WISPS OF WHITE MIST SWAYING TO AND FRO ALMOST"
  ; TODO jump tp data15b
data15b:
  .byte "AS IF ALIVE. A COLD WIND BLOWS UP THE STAIRCASE. THERE IS A PASSAGE AT THE TOP"
  .byte $0D, $0A
  .asciiz "OF A DOME BEHIND YOU" ; 15
  .byte 3            ; NORTH
  .addr data19    ; NORTH
  .byte 50           ; EAST
  .addr data22 ; EAST
  .byte 5            ; SOUTH
  .addr data18    ; SOUTH
  .byte 8            ; DOWN
  .addr data19    ; DOWN
  .byte 60           ; WEST
  .addr data17    ; WEST
  .byte 7            ; UP
  .addr data22 ; UP
  .byte $fe         ; Delimeter
data16:
  .asciiz "THE CRACK IS FAR TOO SMALL FOR YOU TO FOLLOW" ; 16
  ; TODO 
  .addr data14
  .byte $fe         ; Delimeter
data17:
  .byte "YOU ARE ON THE EAST BANK OF A FISSURE SLICING CLEAR ACROSS THE HALL. THE MIST IS"
  .byte $0D, $0A
  .asciiz "QUITE THICK HERE, AND THE FISSURE IS TOO WIDE TO JUMP" ; 17
  .byte 50           ; EAST
  .addr data15    ; EAST
  .byte 60           ; WEST
  .addr data27 ; WEST
  .byte 9            ; JUMP
  .addr data21    ; JUMP
  .byte 10           ; OVER
  .addr data27 ; OVER TODO does this need to be over + west?
  .byte $fe         ; Delimeter
data18:
  .byte "THIS IS A LOW ROOM WITH A CRUDE NOTE ON THE WALL. IT SAYS 'YOU WON'T GET IT UP"
  .byte $0D, $0A
  .asciiz "THE STEPS'" ; 18
  .byte 3            ; NORTH
  .addr data15    ; NORTH
  .byte $fe         ; Delimeter
data19:
  .asciiz "YOU ARE IN THE HALL OF THE MOUNTAIN KING, WITH PASSAGES OFF IN ALL DIRECTIONS" ; 19
  ; TODO varify Green Snake behavior
  ;jmp location_32
  .byte 3            ; NORTH
  .addr data28 ; NORTH
  .byte 50           ; EAST
  .addr data15 ; EAST
  .byte 5            ; SOUTH
  .addr data29 ; SOUTH
  .byte 60           ; WEST
  .addr data30 ; WEST
  .byte 7            ; UP
  .addr data15 ; UP TODO verify both up and east should go here
  .byte $fe         ; Delimeter
data20:
  .asciiz "YOU ARE AT THE BOTTOM OF THE PIT WITH A BROKEN NECK" ; 20
  .addr EXIT ; TODO handle death
  .byte $fe         ; Delimeter
data21:
  .asciiz "YOU DIDN'T MAKE IT" ; 21
  .addr EXIT ; TODO handle death
  .byte $fe         ; Delimeter
data22:
  .asciiz "THE DOME IS UNCLIMBABLE" ; 22
  .byte $fe         ; Delimeter
data23:
  .asciiz "YOU CAN'T GO IN THROUGH A LOCKED STEEL GRATE!" ; 23
  .byte 50           ; EAST
  .addr data67 ; EAST
  .byte 60           ; WEST
  .addr data68 ; WEST
  .byte 8            ; DOWN
  .addr data25    ; DOWN
  .byte $fe         ; Delimeter
data24:
  .asciiz "YOU DON'T FIT DOWN A TWO INCH HOLE!" ; 24
  .byte 7               ; UP
  .addr data24          ; UP
  ; Only plays when trying to go into holw
  .addr data67 ; TODO investigate location67?
  .byte $fe         ; Delimeter
data25:
  .asciiz "YOU CAN'T GO THROUGH A LOCKED STEEL GRATE" ; 25
  .byte 7            ; UP
  .addr data23    ; UP
  .byte $fe         ; Delimeter
; no data 26
data27:
  .asciiz "YOU ARE ON THE WEST SIDE OF THE FISSURE IN THE HALL OF MISTS" ; 26
  .byte 3            ; NORTH
  .addr data40 ; NORTH
  .byte 60           ; WEST
  .addr data41 ; WEST
  .byte 9            ; JUMP
  .addr data21    ; JUMP
  .byte 10           ; OVER
  .addr data17 ; OVER
  .byte $fe         ; Delimeter
data28:
  .byte "YOU ARE IN A LOW N/S PASSAGE AT A HOLE IN THE FLOOR. THE HOLE GOES DOWN TO AN"
  .byte $0D, $0A
  .asciiz "E/W PASSAGE" ; 28
  .byte 3            ; NORTH
  .addr data33 ; NORTH
  .byte 5            ; SOUTH
  .addr data19 ; SOUTH
  .byte 8            ; DOWN
  .addr data36 ; DOWN
  .byte $fe         ; Delimeter
data29:
  .asciiz "YOU ARE IN THE SOUTH SIDE CHAMBER" ; 29
  .byte 3            ; NORTH
  .addr data19      ; NORTH
  .byte $fe         ; Delimeter
data30:
  .byte "YOU ARE IN THE WEST SIDE CHAMBER OF HALL OF MT KING. A PASSAGE CONTINUES WEST"
  .byte $0D, $0A
  .asciiz "AND UP HERE" ; 30
  .byte 50           ; EAST
  .addr data19 ; EAST
  .byte 60           ; WEST
  .addr data62 ; WEST
  .byte 7            ; UP
  .addr data62 ; UP
  .byte $fe         ; Delimeter
data32:
  .asciiz "YOU CAN'T GET BY THE SNAKE"
  ; TODO handle green snake
  .addr data19
  .byte $fe         ; Delimeter
data33:
  .byte "YOU ARE IN A LARGE ROOM, WITH A PASSAGE TO THE SOUTH, A PASSAGE TO THE WEST,"
  .byte $0D, $0A
  .byte "AND A WALL OF BROKEN ROCK TO THE EAST. THERE IS A LARGE 'Y2' ON A ROCK IN THE"
  .byte $0D, $0A
  .asciiz "ROOM'S CENTER"
  .byte 50           ; EAST
  .addr data34    ; EAST
  .byte 5            ; SOUTH
  .addr data35    ; SOUTH
  .byte 60           ; WEST
  .addr data35    ; WEST
  .byte $fe         ; Delimeter
data34:
  .asciiz "YOU ARE IN A JUMBLE OF ROCK, WITH CRACKS EVERYWHERE"
  .byte 8            ; DOWN
  .addr data33    ; DOWN
  .byte 7            ; UP
  .addr data15    ; UP
  .byte $fe         ; Delimeter
data35:
  .byte "YOU'RE AT A LOW WINDOW OVERLOOKING A HUGE PIT, WHICH EXTENDS UP OUT OF SIGHT. A"
  .byte $0D, $0A
  .byte "FLOOR IS INDISTINCTLY VISIBLE OVER 50 FEET BELOW. TRACES OF WHITE MIST COVER"
  .byte $0D, $0A
  .byte "THE FLOOR OF THE PIT, BECOMING THICKER TO THE RIGHT. MARKS IN THE DUST AROUND"
  .byte $0D, $0A
  .byte "THE WINDOW WOULD SEEM TO INDICATE THAT SOMEONE HAS BEEN HERE RECENTLY. DIRECTLY"
  .byte $0D, $0A
  .byte "ACROSS THE PIT FROM YOU AND 25 FEET AWAY THERE IS A SIMILAR WINDOW LOOKING"
  .byte $0D, $0A
  .asciiz "INTO A LIGHTED ROOM. A SHADOWY FIGURE CAN BE SEEN THERE PEERING BACK AT YOU"
  .byte 50           ; EAST
  .addr data33    ; EAST
  .byte 9            ; JUMP
  .addr data20    ; JUMP
  .byte $fe         ; Delimeter
data36:
  .byte "YOU ARE IN A DIRTY BROKEN PASSAGE. TO THE EAST IS A CRAWL. TO THE WEST IS A"
  .byte $0D, $0A
  .asciiz "LARGE PASSAGE. ABOVE YOU IS ANOTHER PASSAGE"
  .byte 50           ; EAST
  .addr data37    ; EAST
  .byte 60           ; WEST
  .addr data39    ; WEST
  .byte 7            ; UP
  .addr data28    ; UP
  .byte $fe         ; Delimeter
data37:
  .asciiz "YOU ARE ON THE BRINK OF A SMALL CLEAN CLIMBABLE PIT. A CRAWL LEADS WEST"
  .byte 8            ; DOWN
  .addr data38    ; DOWN
  .byte 60           ; WEST
  .addr data36    ; WEST
  .byte 7            ; CLIMB
  .addr data38    ; CLIMB
data38:
  .byte "YOU ARE IN THE BOTTOM OF A SMALL PIT WITH A LITTLE STREAM, WHICH ENTERS AND"
  .byte $0D, $0A
  .asciiz "EXITS THROUGH TINY SLITS"
  .byte 7            ; UP
  .addr data37    ; UP
  .byte 11           ; CLIMB
  .addr data37    ; CLIMB
  .byte $fe         ; Delimeter
data39:
  .byte "YOU ARE IN A LARGE ROOM FULL OF DUSTY ROCKS. THERE IS A BIG HOLE IN THE FLOOR."
  .byte $0D, $0A
  .asciiz "THERE ARE CRACKS EVERYWHERE, AND A PASSAGE LEADING EAST"
  .byte 50           ; EAST
  .addr data36    ; EAST
  .byte 8            ; DOWN
  .addr data64    ; DOWN
  .byte $fe         ; Delimeter
data40:
  .byte "YOU HAVE CRAWLED THROUGH A VERY LOW WIDE PASSAGE PARALLEL TO AND NORTH OF THE"
  .byte $0D, $0A
  .asciiz "HALL OF MISTS"
  ; TODO
  .addr EXIT
  .byte $fe         ; Delimeter
data41:
  .byte "YOU ARE AT THE WEST END OF HALL OF MISTS. A LOW WIDE CRAWL CONTINUES WEST AND"
  .byte $0D, $0A
  .asciiz "ANOTHER GOES NORTH. TO THE SOUTH IS A LITTLE PASSAGE 6 FEET OFF THE FLOOR"
  .byte 3            ; NORTH
  .addr data59 ; NORTH
  .byte 50           ; EAST
  .addr data27 ; EAST
  .byte 5            ; SOUTH
  .addr data42    ; SOUTH
  .byte 60           ; WEST
  .addr data60 ; WEST
  .byte 7            ; UP
  .addr data42    ; UP
  .byte 11           ; CLIMB
  .addr data42    ; CLIMB
  .byte $fe         ; Delimeter
data42:
data80:
data83:
data84:
data87:
  .asciiz "YOU ARE IN A MAZE OF TWISTY LITTLE PASSAGES, ALL ALIKE"
  .byte 3            ; NORTH
  .addr data42    ; NORTH
  .byte 7            ; UP
  .addr data41    ; UP
  .byte 50           ; EAST
  .addr data57    ; EAST
  .byte $fe         ; Delimeter
data57:
  .byte "YOU ARE ON THE BRINK OF A THIRTY FOOT PIT WITH A MASSIVE ORANGE COLUMN DOWN ONE"
  .byte $0D, $0A
  .byte "WALL. YOU COULD CLIMB DOWN HERE BUT YOU COULD NOT GET BACK UP. THE MAZE"
  .byte $0D, $0A
  .asciiz "CONTINUES AT THIS LEVEL"
  .byte 5            ; SOUTH
  .addr data58    ; SOUTH
  .byte 8            ; DOWN
  .addr data13 ; DOWN
  .byte 60           ; WEST
  .addr data42    ; WEST
  .byte 11           ; CLIMB
  .addr data13 ; CLIMB
  .byte $fe         ; Delimeter
data58:
  .asciiz "DEAD END"
  .byte 50           ; EAST
  .addr data57    ; EAST
  .byte $fe         ; Delimeter
data59:
  .byte "YOU HAVE CRAWLED THROUGH A VERY LOW WIDE PASSAGE PARALLED TO AND NORTH OF THE"
  .byte $0D, $0A
  .asciiz "HALL OF MISTS"
  ; TODO are we dead? I think this message is here twice
  jsr EXIT
  .byte $fe         ; Delimeter
data60:
  .byte "YOU ARE AT THE EAST END OF A VERY LONG HALL APPARENTLY WITHOUT SIDE CHAMBERS."
  .byte $0D, $0A
  .byte "TO THE EAST A LOW WIDE CRAWL SLANTS UP. TO THE NORTH A ROUND TWO FOOT HOLE"
  .byte $0D, $0A
  .asciiz "SLANTS DOWN"
  .byte 3            ; NORTH
  .addr data62    ; NORTH
  .byte 50           ; EAST
  .addr data41 ; EAST
  .byte 8            ; DOWN
  .addr data62    ; DOWN
  .byte 60           ; WEST
  .addr data61    ; WEST
  .byte 7            ; UP
  .addr data41 ; UP
  .byte $fe         ; Delimeter
data61:
  .byte "YOU ARE AT THE WEST END OF A VERY LONG FEATURELESS HALL. THE HALL JOINS UP"
  .byte $0D, $0A
  .asciiz "WITH A NARROW NORTH/SOUTH PASSAGE"
  .byte 3            ; NORTH
  .addr data62    ; NORTH
  .byte 50           ; EAST
  .addr data60    ; EAST
  .byte 5            ; SOUTH
  .addr data107   ; SOUTH
  .byte $fe         ; Delimeter
  .byte $fe         ; Delimeter
data62:
  .asciiz "YOU ARE AT A CROSSOVER OF A HIGH N/S PASSAGE AND A LOW E/W ONE"
  .byte 3            ; NORTH
  .addr data63    ; NORTH
  .byte 5            ; SOUTH
  .addr data61    ; SOUTH
  .byte 60           ; WEST
  .addr data60    ; WEST
  .byte 50           ; EAST
  .addr data30    ; EAST
  .byte $fe         ; Delimeter
data63:
  .asciiz "DEAD END"
  .byte 5           ; SOUTH
  .addr data62   ; SOUTH
  .byte $fe         ; Delimeter
data64:
  .byte "YOU ARE AT A COMPLEX JUNCTION. A LOW HANDS AND KNEES PASSAGE FROM THE NORTH"
  .byte $0D, $0A
  .byte "JOINS A HIGHER CRAWL FROM THE EAST TO MAKE A WALKING PASSAGE GOING WEST. THERE"
  .byte $0D, $0A
  .asciiz "IS ALSO A LARGE ROOM ABOVE. THE AIR IS DAMP HERE"
  .byte 3             ; NORTH
  .addr data103 ; NORTH
  .byte 50            ; EAST
  .addr data106 ; EAST
  .byte 60            ; WEST
  .addr data65     ; WEST
  .byte 7             ; UP
  .addr data39  ; UP
  .byte 7             ; CLIMB
  .addr data39  ; CLIMB
  .byte $fe         ; Delimeter
data65:
  .byte "YOU ARE IN BEDQUILT, A LONG EAST/WEST PASSAGE WITH HOLES EVERYWHERE. TO EXPLORE"
  .byte $0D, $0A
  .asciiz "AT RANDOM SELECT NORTH, SOUTH, UP OR DOWN"
  ;.byte 3            ; NORTH TODO RANDOM exits on this room
  ;.addr dataX  ; NORTH
  .byte 50           ; EAST
  .addr data64    ; EAST
  ;.byte 5            ; SOUTH
  ;.addr dataX  ; SOUTH
  ;.byte 8            ; DOWN
  ;.addr dataX  ; DOWN TODO is this random or now
  .byte 60           ; WEST
  .addr data66    ; WEST
  ;.byte 7            ; UP
  ;.addr dataX  ; UP
  .byte $fe         ; Delimeter
data66:
  .byte "YOU ARE IN A ROOM WHOSE WALLS RESEMBLE SWISS CHEESE. OBVIOUS PASSAGES GO WEST,"
  .byte $0D, $0A
  .asciiz "EAST, NE, AND NW. PART OF THE ROOM IS OCCUPIED BY A LARGE BEDROCK BLOCK"
  .byte 53           ; NORTHEAST
  .addr data65    ; NORTHEAST
  .byte 55           ; SOUTHEAST
  .addr data97 ; SOUTHEAST
  .byte 50           ; EAST
  .addr data65    ; EAST
  .byte 5            ; SOUTH
  .addr data77 ; SOUTH
  .byte 60           ; WEST
  .addr data67    ; WEST
  .byte $fe         ; Delimeter
data67:
  .byte "YOU ARE AT THE EAST END OF THE TWOPIT ROOM. THE FLOOR HERE IS LITTERED WITH"
  .byte $0D, $0A
  .byte "THIN ROCK SLABS, WHICH MAKE IT EASY TO DESCEND THE PITS. THERE IS A PATH HERE"
  .byte $0D, $0A
  .byte "BYPASSING THE PITS TO CONNECT PASSAGES FROM EAST AND WEST THERE ARE HOLES ALL"
  .byte $0D, $0A
  .byte "OVER, BUT THE ONLY BIT ONE IS ON THE WALL DIRECTLY OVER THE WEST PIT WHERE YOU"
  .byte $0D, $0A
  .asciiz "CAN'T GET AT IT"
  .byte 50           ; EAST
  .addr data66    ; EAST
  .byte 8            ; DOWN
  .addr data24 ; DOWN
  .byte 60           ; WEST
  .addr data23 ; WEST
  .byte $fe         ; Delimeter
data68:
  .byte "YOU ARE IN A LARGE LOW CIRCULAR CHAMBER WHOSE FLOOR IS AN IMMENSE SLAB FALLEN"
  .byte $0D, $0A
  .byte "FROM THE CEILING (SLAB ROOM). EAST AND WEST THERE ONCE WERE LARGE PASSAGES, BUT"
  .byte $0D, $0A
  .byte "THEY ARE NOW FILLED WITH BOULDERS. LOW SMALL PASSAGES GO NORTH AND SOUTH, AND"
  .byte $0D, $0A
  .asciiz "THE SOUTH ONE QUICKLY BENDS WEST AROUND THE BOULDERS"
  .byte 3            ; NORTH
  .addr data65 ; NORTH
  .byte 7            ; UP
  .addr data69    ; UP
  .byte 7            ; CLIMB
  .addr data69    ; CLIMB
  .byte 5            ; SOUTH
  .addr data23    ; SOUTH
  .byte $fe         ; Delimeter
data69:
  .asciiz "YOU ARE IN A SECRET N/S CANYON ABOVE A LARGE ROOM"
  .byte 3              ; NORTH
  .addr data109  ; NORTH
  .byte 5              ; SOUTH
  .addr data119  ; SOUTH
  .byte 8              ; DOWN
  .addr data68      ; DOWN
  .byte $fe         ; Delimeter
data70:
  .asciiz "YOU ARE IN A SECRET N/S CANYON ABOVE A SIZABLE PASSAGE"
  .byte 3             ; NORTH
  .addr data71     ; NORTH
  .byte 5             ; SOUTH
  .addr data111 ; SOUTH
  .byte 8             ; DOWN
  .addr data65  ; DOWN
  .byte $fe         ; Delimeter
data71:
  .byte "YOU ARE IN A SECRET CANYON AT A JUNCTION OF THREE CANYONS, BEARING NORTH, SOUTH"
  .byte $0D, $0A
  .asciiz "AND SE. THE NORTH ONE IS AS TALL AS THE OTHER TWO COMBINED"
  .byte 3              ; NORTH
  .addr data110  ; NORTH
  .byte 55             ; SOUTHEAST
  .addr data71      ; SOUTHEAST
  .byte 5              ; SOUTH
  .addr data70      ; SOUTH
  .byte $fe         ; Delimeter
data72:
  .asciiz "YOU ARE IN A LARGE LOW ROOM. CRAWLS LEAD NORTH, SE, AND SW"
  .byte 3            ; NORTH
  .addr data73    ; NORTH
  ; .byte 55           ; SOUTHEAST TODO
  ; .addr dataX  ; SOUTHEAST TODO
  ; .byte 65           ; SOUTHWEST TODO
  ; .addr dataX  ; SOUTHWEST TODO
  .byte $fe         ; Delimeter
data73:
  .asciiz "DEAD END CRAWL"
  .byte 5            ; SOUTH
  .addr data72    ; SOUTH
  .byte $fe         ; Delimeter
data74:
  .byte "YOU ARE IN A SECRET CANYON WHICH HERE RUNS E/W. IT CROSSES OVER A VERY TIGHT"
  .byte $0D, $0A
  .asciiz "CANYON 15 FEET BELOW. IF YOU GO DOWN YOU MAY NOT BE ABLE TO GET BACK UP"
  .byte 50            ; EAST
  .addr data19  ; EAST
  .byte 8             ; DOWN
  .addr data75     ; DOWN
  .byte 60            ; WEST
  .addr data120 ; WEST
  .byte $fe         ; Delimeter
data75:
  .asciiz "YOU ARE AT A WIDE PLACE IN A VERY TIGHT N/S CANYON"
  .byte 3            ; NORTH
  .addr data77    ; NORTH
  .byte 5            ; SOUTH
  .addr data76    ; SOUTH
  .byte $fe         ; Delimeter
data76:
  .asciiz "THE CANYON HERE BECOMES TOO TIGHT TO GO FURTHER SOUTH"
  .byte 3            ; NORTH
  .addr data75    ; NORTH
  .byte $fe         ; Delimeter
data77:
  .byte "YOU ARE IN A TALL E/W CANYON. A LOW TIGHT CRAWL GOES 3 FEET NORTH AND SEEMS"
  .byte $0D, $0A
  .asciiz "TO OPEN UP"
  .byte 50           ; EAST
  .addr data75    ; EAST
  .byte 60           ; WEST
  .addr data78    ; WEST
  .byte 3            ; NORTH
  .addr data66    ; NORTH
  .byte $fe         ; Delimeter
data78:
  .asciiz "THE CANYON RUNS INTO A MASS OF BOULDERS -- DEAD END"
  .byte 5            ; SOUTH
  .addr data77    ; SOUTH
  .byte $fe         ; Delimeter
data79:
  .byte "THE STREAM FLOWS OUT THROUGH A PAIR OF 1 FOOT DIAMETER SEWER PIPES. IT WOULD"
  .byte $0D, $0A
  .asciiz "BE ADVISABLE TO USE THE EXIT"
  .addr EXIT
  .byte $fe         ; Delimeter
data88:
  .byte "YOU ARE IN A LONG, NARROW CORRIDOR STRETCHING OUT OF SIGHT TO THE WEST. AT THE"
  .byte $0D, $0A
  .asciiz "EASTERN END IS A HOLE THROUGH WHICH YOU CAN SEE A PROFUSION OF LEAVES."
  .byte 50           ; EAST
  .addr data25 ; EAST
  .byte 8            ; DOWN
  .addr data25 ; DOWN
  .byte 60           ; WEST
  .addr data92    ; WEST
  .byte 9            ; JUMP
  .addr data20 ; JUMP
  .byte 11           ; CLIMB
  .addr data25 ; CLIM
  .byte $fe         ; Delimeter
data91:
  .byte "YOU ARE AT THE TOP OF A STEEP INCLINE ABOVE A LARGE ROOM YOU COULD CLIMB DOWN"
  .byte $0D, $0A
  .byte "HERE, BUT YOU WOULD NOT BE ABLE TO CLIMB UP. THERE IS A PASSAGE LEADING BACK TO"
  .byte $0D, $0A
  .asciiz "THE NORTH"
  .byte 3            ; NORTH
  .addr data95    ; NORTH
  .byte 8            ; DOWN
  .addr data72 ; DOWN
  .byte 11           ; CLIMB
  .addr data72 ; CLIMB
  .byte $fe         ; Delimeter
data92:
  .byte "YOU ARE IN THE GIANT ROOM. THE CEILING IS TOO HIGH UP FOR YOUR LAMP TO SHOW IT,"
  .byte $0D, $0A
  .byte "CAVERNOUS PASSAGES LEAD EAST, NORTH, AND SOUTH. ON THE WEST WALL IS SCRAWLED"
  .byte $0D, $0A
  .asciiz "THE INSCRIPTION: 'FEE FIE FOE FOO' {SIC}"
  .byte 5            ; SOUTH
  .addr data88    ; SOUTH
  .byte 3            ; NORTH
  .addr data94    ; NORTH
  .byte 50           ; EAST
  .addr data93    ; EAST
  .byte $fe         ; Delimeter
data93:
  .asciiz "THE PASSAGE HERE IS BLOCKED BY A RECENT CAVE-IN"
  .byte 5            ; SOUTH
  .addr data92    ; SOUTH
  .byte $fe         ; Delimeter
data94:
  .asciiz "YOU ARE AT ONE END OF AN IMMENSE NORTH/SOUTH PASSAGE"
  .byte 3            ; NORTH
  .addr data95    ; NORTH
  .byte 5            ; SOUTH
  .addr data92    ; SOUTH
  .byte $fe         ; Delimeter
data95:
  .byte "YOU ARE IN A MAGNIFICENT CAVERN WITH A RUSHING STREAM, WHICH CASCADES OVER A"
  .byte $0D, $0A
  .byte "SPARKLING WATERFALL INTO A ROARING WHIRLPOOL WHICH DISAPPEARS THROUGH A HOLE IN"
  .byte $0D, $0A
  .asciiz "THE FLOOR. PASSAGES EXIT TO THE SOUTH AND WEST"
  .byte 5              ; SOUTH
  .addr data94      ; SOUTH
  .byte 60             ; WEST
  .addr data91      ; WEST
  .byte $fe         ; Delimeter
data96:
  .byte "YOU ARE IN THE SOFT ROOM. THE WALLS ARE COVERED WITH HEAVY CURTAINS, THE FLOOR"
  .byte $0D, $0A
  .asciiz "WITH A THICK PILE CARPET MOSS COVERS THE CEILING"
  .byte 60             ; WEST
  .addr data66      ; WEST
  .byte $fe         ; Delimeter
data97:
  .byte "THIS IS THE ORIENTAL ROOM. ANCIENT ORIENTAL CAVE DRAWINGS COVER THE WALLS. A"
  .byte $0D, $0A
  .byte "GENTLY SLOPING PASSAGE LEADS UPWARD TO THE NORTH, ANOTHER PASSAGE LEADS SE,"
  .byte $0D, $0A
  .asciiz "AND A HANDS AND KNEES CRAWL LEADS WEST"
  .byte 3            ; NORTH
  .addr data98    ; NORTH
  .byte 55           ; SOUTHEAST
  .addr data66 ; SOUTHEAST
  .byte 60           ; WEST
  .addr data72 ; WEST
  .byte 7            ; UP
  .addr data98    ; UP
  .byte $fe         ; Delimeter
data98:
  .byte "YOU ARE FOLLOWING A WIDE PATH AROUND THE OUTER EDGE OF A LARGE CAVERN. FAR"
  .byte $0D, $0A
  .byte "BELOW, THROUGH A HEAVY WHITE MIST, STRANGE SPLASHING NOISES CAN BE HEARD. THE"
  .byte $0D, $0A
  .byte "MIST RISES UP THROUGH A FISSURE IN THE CEILING. THE PATH EXITS TO THE SOUTH"
  .byte $0D, $0A
  .asciiz "AND WEST"
  .byte 5            ; SOUTH
  .addr data97    ; SOUTH
  .byte 60           ; WEST
  .addr data99    ; WEST
  .byte $fe         ; Delimeter
data99:
  .byte "YOU ARE IN AN ALCOVE. A SMALL NW PATH SEEMS TO WIDEN AFTER A SHORT DISTANCE. AN"
  .byte $0D, $0A
  .byte "EXTREMELY TIGHT TUNNEL LEADS EAST. IT LOOKS LIKE A VERY TIGHT SQUEEZE. AN EERIE"
  .byte $0D, $0A
  .asciiz "LIGHT CAN BE SEEN AT THE OTHER END"
  .byte 63           ; NORTHWEST
  .addr data98    ; NORTHWEST
  .byte 50           ; EAST
  .addr data100   ; EAST
  .byte $fe         ; Delimeter
data100:
  .asciiz "YOU'RE IN A SMALL CHAMBER LIT BY AN EERIE GREEN LIGHT. AN EXTREMELY NARROW"
  .byte $0D, $0A
  .asciiz "TUNNEL EXITS TO THE WEST. A DARK CORRIDOR LEADS NE"
  .byte 53             ; NORTHEAST
  .addr data101     ; NORTHEAST
  .byte 60             ; WEST
  .addr data99      ; WEST
  .byte $fe         ; Delimeter
data101:
  .asciiz "YOU'RE IN THE DARK-ROOM. A CORRIDOR LEADING SOUTH IS THE ONLY EXIT"
  .byte 5            ; SOUTH
  .addr data100   ; SOUTH
  .byte $fe         ; Delimeter
data102:
  .byte "YOU ARE IN AN ARCHED HALL. A CORAL PASSAGE ONCE CONTINUED UP AND EAST FROM"
  .byte $0D, $0A
  .asciiz "HERE, BUT IS NOW BLOCKED BY DEBRIS. THE AIR SMELLS OF SEA WATER"
  .byte 8            ; DOWN
  .addr data103   ; DOWN
  .byte $fe         ; Delimeter
data103:
  .byte "YOU'RE IN A LARGE ROOM CARVED OUT OF SEDIMENTARY ROCK. THE FLOOR AND WALLS ARE"
  .byte $0D, $0A
  .byte "LITTERED WITH BITS OF SHELLS IMBEDDED IN THE STONE. A SHALLOW PASSAGE PROCEEDS"
  .byte $0D, $0A
  .byte "DOWNWARD, AND A SOMEWHAT STEEPER ONE LEADS UP. A LOW HANDS AND KNEES PASSAGE"
  .byte $0D, $0A
  .asciiz "ENTERS FROM THE SOUTH"
  .byte 8            ; DOWN
  .addr data104   ; DOWN
  .byte 7            ; UP
  .addr data102   ; UP
  .byte 5            ; SOUTH
  .addr data64    ; SOUTH
  .byte $fe         ; Delimeter
data104:
  .asciiz "YOU ARE IN A LONG SLOPING CORRIDOR WITH RAGGED SHARP WALLS"
  .byte 8            ; DOWN
  .addr data105   ; DOWN
  .byte 7            ; UP
  .addr data103   ; UP
  .byte $fe         ; Delimeter
data105:
  .asciiz "YOU ARE IN A CUL-DE-SAC ABOUT EIGHT FEET ACROSS"
  .byte 7            ; UP
  .addr data104   ; UP
  .byte $fe         ; Delimeter
data106:
  .byte "YOU ARE IN AN ANTEROOM LEADING TO A LARGE PASSAGE TO THE EAST. SMALL PASSAGES"
  .byte $0D, $0A
  .byte "GO WEST AND UP. THE REMNANTS OF RECENT DIGGING ARE EVIDENT. A SIGN IN MIDAIR"
  .byte $0D, $0A
  .byte "HERE SAYS: CAVE UNDER CONSTRUCTION BEYOND THIS POINT PROCEED AT YOUR OWN RISK"
  .byte $0D, $0A
  .asciiz "WITT CONSTRUCTION COMPANY"
  .byte 50              ; EAST
  .addr data108      ; EAST
  .byte 60              ; WEST
  .addr data65   ; WEST
  .byte 7               ; UP
  .addr data64   ; UP
  .byte $fe         ; Delimeter
data107:
  .asciiz "YOU ARE IN A MAZE OF TWISTY LITTLE PASSAGES, ALL DIFFERENT"
  .byte 5            ; SOUTH
  .addr data107   ; SOUTH
  .byte 8            ; DOWN
  .addr data61    ; DOWN
  .byte $fe         ; Delimeter
data108:
  .asciiz "YOU ARE AT WITT'S END. PASSAGES LEAD OFF IN ALL DIRECTIONS"
  .byte 9            ; ANY DIRECTION
  .addr data106   ; ANY DIRECTION
  .byte $fe         ; Delimeter
data109:
  .byte "YOU ARE IN A NORTH/SOUTH CANYON ABOUT 25 FEET ACROSS. THE FLOOR IS COVERED BY"
  .byte $0D, $0A
  .byte "WHITE MIST SEEPING IN FROM THE NORTH. THE WALLS EXTEND UPWARD FOR WELL OVER"
  .byte $0D, $0A
  .byte "100 FEET. SUSPENDED FROM SOME UNSEEN POINT FAR ABOVE YOU, AN ENORMOUS TWO-"
  .byte $0D, $0A
  .byte "SIDED MIRROR IS HANGING PARALLED TO AND MIDWAY BETWEEN THE CANYON WALLS. (THE"
  .byte $0D, $0A
  .byte "MIRROR IS OBVIOUSLY PROVIDED FOR THE USE OF THE DWARVES, WHO AS YOU KNOW, ARE"
  .byte $0D, $0A
  .asciiz "EXTREMELY VAIN.) A SMALL WINDOW CAN BE SEEN IN EITHER WALL, SOME FIFTY FEET UP"
  .byte 3            ; NORTH
  .addr data113   ; NORTH
  .byte 5            ; SOUTH
  .addr data60    ; SOUTH
  .byte $fe         ; Delimeter
data110:
  .byte "YOU'RE AT A LOW WINDOW OVERLOOKING A HUGE PIT, WHICH EXTENDS UP OUT OF SIGHT."
  .byte $0D, $0A
  .byte "A FLOOR IS INDISTINCTLY VISIBLE OVER 50 FEET BELOW. TRACES OF WHITE MIST COVER"
  .byte $0D, $0A
  .byte "THE FLOOR OF THE PIT, BECOMING THICKER TO THE LEFT. MARKS IN THE DUST AROUND"
  .byte $0D, $0A
  .byte "THE WINDOW WOULD SEEM TO INDICATE THAT SOMEONE HAS BEEN HERE RECENTLY. DIRECTLY"
  .byte $0D, $0A
  .byte "ACROSS THE PIT FROM YOU AND 25 FEET AWAY THERE IS A SIMILAR WINDOW LOOKING INTO"
  .byte $0D, $0A
  .asciiz "A LIGHTED ROOM. A SHADOWY FIGURE CAN BE SEEN THERE PEERING BACK AT YOU"
  .byte 60              ; WEST
  .addr data71          ; WEST
  .byte 9               ; JUMP
  .addr data20          ; JUMP
  .byte $fe             ; Delimeter
data111:
  .byte "A LARGE STALACTITE EXTENDS FROM THE ROOF AND ALMOST REACHES THE FLOOR BELOW."
  .byte $0D, $0A
  .byte "YOU COULD CLIMB DOWN IT, AND JUMP FROM IT TO THE FLOOR, BUT HAVING DONE SO YOU"
  .byte $0D, $0A
  .asciiz "WOULD BE UNABLE TO REACH IT TO CLIMB BACK UP"
  .byte 3             ; NORTH
  .addr data70 ; NORTH
  .byte 8             ; DOWN
  .addr data42 ; DOWN
  .byte 9             ; JUMP
  .addr data42 ; JUMP
  .byte 11            ; CLIMB
  .addr data42 ; CLIMB
  .byte $fe         ; Delimeter
data113:
  .byte "YOU ARE AT THE EDGE OF A LARGE UNDERGROUND RESERVOIR. AN OPAQUE CLOUD OF WHITE"
  .byte $0D, $0A
  .byte "MIST FILLS THE ROOM AND RISES RAPIDLY UPWARD. THE LAKE IS FED BY A STREAM WHICH"
  .byte $0D, $0A
  .byte "TUMBLES OUT OF A HOLE IN THE WALL ABOUT 10 FEET OVERHEAD AND SPLASHES NOISILY"
  .byte $0D, $0A
  .byte "INTO THE WATER SOMEWHERE WITHIN THE MIST THE ONLY PASSAGE GOES BACK TOWARD"
  .byte $0D, $0A
  .asciiz "THE SOUTH"
  .byte 5            ; SOUTH
  .addr data109   ; SOUTH
  .byte $fe         ; Delimeter
data115:
  .byte "YOU ARE AT THE NORTHEAST END OF AN IMMENSE ROOM, EVEN LARGER THAN THE GIANT"
  .byte $0D, $0A
  .byte "ROOM. IT APPEARS TO BE A REPOSITORY FOR THE 'ADVENTURE' PROGRAM. MASSIVE"
  .byte $0D, $0A
  .byte "TORCHES FAR OVERHEAD BATHE THE ROOM WITH SMOKY YELLOW LIGHT. SCATTERED ABOUT"
  .byte $0D, $0A
  .byte "YOU CAN BE SEEN A PILE OF BOTTLES (ALL OF THEM EMPTY), A NURSERY OF YOUNG"
  .byte $0D, $0A
  .byte "BEANSTALKS MURMURING QUIETLY, A BED OF OYSTERS, A BUNDLE OF BLACK RODS WITH"
  .byte $0D, $0A
  .byte "RUSTY STARS ON THEIR ENDS, AND A COLLECTION OF BRASS LANTERNS. OFF TO ONE SIDE"
  .byte $0D, $0A
  .byte "A GREAT MANY DWARVES ARE SLEEPING ON THE FLOOR, SNORING LOUDLY. A SIGN NEARBY"
  .byte $0D, $0A
  .byte "READS: DO NOT DISTURB THE DWARVES!"
  .byte $0D, $0A
  .byte "AN IMMENSE MIRROR IS HANGING AGAINST ONE WALL, AND STRETCHES TO THE OTHER END"
  .byte $0D, $0A
  .byte "OF THE ROOM, WHERE VARIOUS OTHER SUNDRY OBJECTS CAN BE GLIMPSED DIMLY IN THE"
  .byte $0D, $0A
  .asciiz "DISTANCE"
  .byte 65            ; SOUTHWEST
  .addr data116    ; SOUTH
  .byte $fe         ; Delimeter
data116:
  .byte "YOU ARE AT THE SOUTHWEST END OF THE REPOSITORY. TO ONE SIDE IS A PIT FULL OF"
  .byte $0D, $0A
  .byte "FIERCE GREEN SNAKES. ON THE OTHER SIDE IS A ROW OF SMALL WICKER CAGES, EACH OF"
  .byte $0D, $0A
  .byte "WHICH CONTAINS A LITTLE SULKING BIRD. IN ONE CORNER IS A BUNDLE OF BLACK RODS"
  .byte $0D, $0A
  .byte "WITH RUSTY MARKS ON THEIR ENDS. A LARGE NUMBER OF VELVET PILLOWS ARE SCATTERED"
  .byte $0D, $0A
  .byte "ABOUT ON THE FLOOR A VAST MIRROR STRETCHES OFF TO THE NORTHEAST. AT YOUR FEET"
  .byte $0D, $0A
  .byte "IS A LARGE STEEL GRATE, NEXT TO WHICH IS A SIGN WHICH READS:"
  .byte $0D, $0A
  .asciiz "TREASURE VAULT. KEYS IN MAIN OFFICE"
  .byte 53           ; NORTHEAST
  .addr data115   ; NORTH
  .byte $fe         ; Delimeter
data117:
  .byte "YOU ARE ON ONE SIDE OF A LARGE DEEP CHASM. A HEAVY WHITE MIST RISING UP FROM"
  .byte $0D, $0A
  .byte "BELOW OBSCURES ALL VIEW OF THE FAR SIDE. A SW PATH LEADS AWAY FROM THE CHASM"
  .byte $0D, $0A
  .asciiz "INTO A WINDING CORRIDOR"
  .byte 65             ; SOUTHWEST
  .addr data118     ; SOUTH
  .byte 10             ; OVER
  .addr data122 ; OVER
  .byte 9              ; JUMP
  .addr data21  ; JUMP
  .byte $fe         ; Delimeter
data118:
  .asciiz "YOU ARE IN A LONG WINDING CORRIDOR SLOPING OUT OF SIGHT IN BOTH DIRECTIONS"
  .byte 7            ; UP
  .addr data117   ; UP
  .byte 8            ; DOWN
  .addr data72    ; DOWN
  .byte $fe         ; Delimeter
data119:
  .asciiz "YOU ARE IN A SECRET CANYON WHICH EXITS TO THE NORTH AND EAST"
  .byte 3            ; NORTH
  .addr data69    ; NORTH
  .byte $fe         ; Delimeter
data120:
  .asciiz "YOU ARE IN A SECRET CANYON WHICH EXITS TO THE NORTH AND EAST"
  .byte 3               ; NORTH
  .addr data69   ; NORTH
  .byte 50              ; EAST
  .addr data74   ; EAST
  .byte $fe         ; Delimeter
data121:
  .asciiz "YOU ARE IN A SECRET CANYON WHICH EXITS TO THE NORTH AND EAST"
  .byte 50             ; EAST
  .addr data74      ; EAST
  .byte $fe         ; Delimeter
data122:
  .byte "YOU ARE ON THE FAR SIDE OF THE CHASM. A NE PATH LEADS AWAY FROM THE CHASM ON"
  .byte $0D, $0A
  .asciiz "THIS SIDE"
  .byte 53             ; NORTHEAST
  .addr data123     ; NORTHEAST
  .byte 65             ; SOUTHWEST
  .addr data117 ; SOUTH
  .byte 6              ; JUMP
  .addr data124     ; JUMP
  .byte 7              ; OVER
  .addr data117 ; OVER
  .byte $fe         ; Delimeter
data123:
  .byte "YOU'RE IN A LONG EAST/WEST CORRIDOR. A FAINT RUMBLING NOISE CAN BE HEARD IN"
  .byte $0D, $0A
  .asciiz "THE DISTANCE"
  .byte 50             ; EAST
  .addr data124     ; EAST
  .byte 60             ; WEST
  .addr data122     ; WEST
  .byte $fe         ; Delimeter
data124:
  .byte "THE PATH FORKS HERE. THE LEFT FORK LEADS NORTHEAST. A DULL RUMBLING SEEMS TO"
  .byte $0D, $0A
  .byte "GET LOUDER IN THAT DIRECTION. THE RIGHT FORK LEADS SOUTHEAST DOWN A GENTLE"
  .byte $0D, $0A
  .byte "SLOPE. THE MAIN CORRIDOR ENTERS FROM THE WEST"
  .byte 53           ; NORTHEAST
  .addr data125   ; NORTHEAST
  .byte 60           ; WEST
  .addr data123   ; WEST
  .byte 5            ; SOUTHEAST
  .addr data128   ; SOUTHEAST
  .byte 55           ; DOWN
  .addr data128   ; DOWN
  .byte $fe         ; Delimeter
data125:
  .byte "THE WALLS ARE QUITE WARM HERE. FROM THE NORTH CAN BE HEARD A STEADY ROAR, SO"
  .byte $0D, $0A
  .byte "LOUD THAT THE ENTIRE CAVE SEEMS TO BE TREMBLING. ANOTHER PASSAGE LEADS SOUTH,"
  .byte $0D, $0A
  .asciiz "AND A LOW CRAWL GOES EAST"
  .byte 3            ; NORTH
  .addr data126   ; NORTH
  .byte 50           ; EAST
  .addr data127   ; EAST
  .byte 5            ; SOUTH
  .addr data124   ; SOUTH
  .byte $fe         ; Delimeter
data126:
  .byte "YOU ARE ON THE EDGE OF A BREATH-TAKING VIEW. FAR BELOW YOU IS AN"
  .byte $0D, $0A
  .byte "ACTIVE VOLCANO, FROM WHICH GREAT GOUTS OF MOLTEN LAVA COME SURGING"
  .byte $0D, $0A
  .byte "OUT, CASCADING BACK DOWN INTO THE DEPTHS. THE GLOWING ROCK FILLS THE"
  .byte $0D, $0A
  .byte "FARTHEST REACHES OF THE CAVERN WITH A BLOOD-RED GLARE, GIVING"
  .byte $0D, $0A
  .byte "EVERYTHING AN EERIE, MACABRE APPEARANCE. THE AIR IS FILLED WITH"
  .byte $0D, $0A
  .byte "FLICKERING SPARKS OF ASH AND A HEAVY SMELL OF BRIMSTONE. THE WALLSF"
  .byte $0D, $0A
  .byte "ARE HOT TO THE TOUCH, AND THE THUNDERING OF THE VOLCANO DROWNS OUT ALL"
  .byte $0D, $0A
  .byte "OTHER SOUNDS. EMBEDDED IN THE JAGGED ROOF FAR OVERHEAD ARE MYRIAD"
  .byte $0D, $0A
  .byte "FORMATIONS COMPOSED OF PURE WHITE ALABASTER, WHICH SCATTER THEIR MURKY"
  .byte $0D, $0A
  .byte "LIGHT INTO SINISTER APPARITIONS UPON THE WALLS. TO ONE SIDE IS A DEEP"
  .byte $0D, $0A
  .byte "GORGE, FILLED WITH A BIZARRE CHAOS OF TORTURED ROCK WHICH SEEMS TO HAVE"
  .byte $0D, $0A
  .byte "BEEN CRAFTED BY THE DEVIL HIMSELF. AN IMMENSE RIVER OF FIRE CRASHES OUT"
  .byte $0D, $0A
  .byte "FROM THE DEPTHS OF THE VOLCANO, BURNS ITS WAY THROUGH THE GORGE, AND"
  .byte $0D, $0A
  .byte "PLUMMETS INTO A BOTTOMLESS PIT FAR OFF TO YOUR LEFT. TO THE RIGHT, AN"
  .byte $0D, $0A
  .byte "IMMENSE GEYSER OF BLISTERING STEAM ERUPTS CONTINUOUSLY FROM A BARREN"
  .byte $0D, $0A
  .byte "ISLAND IN THE CENTER OF A SULFUROUS LAKE, WHICH BUBBLES OMINOUSLY. THE"
  .byte $0D, $0A
  .byte "FAR RIGHT WALL IS AFLAME WITH AN INCANDESCENCE OF ITS OWN, WHICH LENDS"
  .byte $0D, $0A
  .byte "AN ADDITIONAL INFERNAL SPLENDOR TO THE ALREADY HELLISH SCENE."
  .byte $0D, $0A
  .asciiz "A DARK, FOREBODING PASSAGE EXITS TO THE SOUTH"
  .byte 5            ; SOUTH
  .addr data125   ; SOUTH
  .byte $fe         ; Delimeter
data127:
  .byte "YOU ARE IN A SMALL CHAMBER FILLED WITH LARGE BOULDERS. THE WALLS ARE VERY WARM,"
  .byte $0D, $0A
  .byte "CAUSING THE AIR IN THE ROOM TO BE ALMOST STIFLING FROM THE HEAT. THE ONLY EXIT"
  .byte $0D, $0A
  .byte "IS A CRAWL HEADING WEST, THROUGH WHICH IS COMING A LOW RUMBLING"
  .byte 60            ; WEST
  .addr data125    ; WEST
  .byte $fe         ; Delimeter
data128:
  .byte "YOU ARE WALKING ALONG A GENTLY SLOPING NORTH/SOUTH PASSAGE LINED WITH ODDLY"
  .byte $0D, $0A
  .asciiz "SHAPED LIMESTONE FORMATIONS"
  .byte 7              ; UP
  .addr data124 ; UP
  .byte 3              ; NORTH
  .addr data124 ; NORTH
  .byte 5              ; SOUTH
  .addr data129     ; SOUTH
  .byte 8              ; DOWN
  .addr data129     ; DOWN
  .byte $fe         ; Delimeter
data129:
  .byte "YOU ARE STANDING AT THE ENTRANCE TO A LARGE, BARREN ROOM. A SIGN POSTED ABOVE"
  .byte $0D, $0A
  .byte "THE ENTRANCE READS: CAUTION! BEAR IN ROOM!"
  .byte 50            ; EAST
  .addr data130    ; EAST
  .byte 60            ; WEST
  .addr data128    ; WEST
  .byte 7             ; UP
  .addr data128    ; UP
  .byte $fe         ; Delimeter
data130:
  .byte "YOU ARE INSIDE A BARREN ROOM. THE CENTER OF THE ROOM IS COMPLETELY EMPTY EXCEPT"
  .byte $0D, $0A
  .byte "FOR SOME DUST. MARKS IN THE DUST LEAD AWAY TOWARD THE FAR END OF THE ROOM. THE"
  .byte $0D, $0A
  .asciiz "ONLY EXIT IS THE WAY YOU CAME IN"
  .byte 60            ; WEST
  .addr data129    ; WEST
  .byte $fe         ; Delimeter
data112:
data131:
data132:
data133:
data134:
data135:
data136:
data137:
data138:
data139:
  .asciiz "YOU ARE IN A MAZE OF LITTLE TWISTY PASSAGES, ALL DIFFERENT"
  .byte $fe         ; Delimeter
data81:
data82:
data85:
data86:
data114:
data140:
  .asciiz "DEAD END"
  .byte $fe         ; Delimeter