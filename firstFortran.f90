PROGRAM firstFortran
   IMPLICIT NONE
   LOGICAL :: correct = .FALSE.
   INTEGER :: change = 0
   INTEGER :: quarter = 0, dime = 0, nickel = 0, penny = 0
   CHARACTER(len=8) :: DateINFO !format yyyymmdd
   CHARACTER(len=4) :: year, month*2, day*2
   CHARACTER(len=10) :: TimeINFO !format hhmmss.sss
   CHARACTER(len = 2) :: hour, minute, second*6

   CALL DATE_AND_TIME(DateINFO, TimeINFO) !get current date and time

   DO WHILE (correct .eqv. .FALSE.)
      write(*,*) "Input a number from 1 to 99 that is the number of U.S. money that will need to be converted to coins."
      read(*,*) change
      IF(change < 1) THEN
        write(*,*) "Error incorrect input."
      ELSE IF(change > 99) THEN
        write(*,*) "Error incorrect input."
      ELSE
        correct = .TRUE.
      END IF
   END DO

   CALL makeChange(change, quarter, dime, nickel, penny) 

   IF (quarter > 0) THEN
      write(*,*) quarter, "Quarter"
   END IF
   IF (dime > 0) THEN
      write(*,*) dime, "Dime"
   END IF
   IF (nickel > 0) THEN
      write(*,*) nickel, "Nickel"
   END IF
   IF (penny > 0) THEN
      write(*,*) penny, "Penny"
   END IF

   write(*,*) " "

   IF ((quarter > 0) .AND.(dime == 0) .AND. (nickel == 0) .AND. (penny == 0)) THEN
      write(*,*) quarter, "Quarter"
   ELSE IF (quarter == 0 .AND. dime > 0 .AND. nickel == 0 .AND. penny == 0) THEN
      write(*,*) dime, "Dime"
   ELSE IF (quarter == 0 .AND. dime == 0 .AND. nickel > 0 .AND. penny == 0) THEN
      write(*,*) nickel, "Nickel"
   ELSE IF (quarter == 0 .AND. dime == 0 .AND. nickel == 0 .AND. penny > 0) THEN
      write(*,*) penny, "Penny"
   ELSE IF (quarter > 0 .AND. dime > 0 .AND. nickel == 0 .AND. penny == 0) THEN
      write(*,*) quarter, "Quarter", dime, "Dime"
   ELSE IF (quarter > 0 .AND. dime == 0 .AND. nickel > 0 .AND. penny == 0) THEN
      write(*,*) quarter, "Quarter", nickel, "Nickel"
   ELSE IF (quarter > 0 .AND. dime == 0 .AND. nickel == 0 .AND. penny > 0 ) THEN
      write(*,*) quarter, "Quarter", penny, "Penny"
   ELSE IF (quarter == 0 .AND. dime > 0 .AND. nickel > 0 .AND. penny == 0) THEN
      write(*,*) dime, "Dime", nickel, "Nickel"
   ELSE IF (quarter == 0 .AND. dime > 0 .AND. nickel == 0 .AND. penny > 0) THEN
      write(*,*) dime, "Dime", penny, "Penny"
   ELSE IF (quarter == 0 .AND. dime == 0 .AND. nickel > 0 .AND. penny > 0) THEN
      write(*,*) nickel, "Nickel", penny, "Penny"
   ELSE IF (quarter > 0 .AND. dime > 0 .AND. nickel > 0 .AND. penny == 0) THEN
      write(*,*) quarter, "Quarter", dime, "Dime", nickel, "Nickel"
   ELSE IF (quarter > 0 .AND. dime > 0 .AND. nickel == 0 .AND. penny > 0) THEN
      write(*,*) quarter, "Quarter", dime, "Dime", penny, "Penny"
   ELSE IF (quarter > 0 .AND. dime == 0 .AND. nickel > 0 .AND. penny > 0) THEN
      write(*,*) quarter, "Quarter", nickel, "Nickel", penny, "Penny"
   ELSE IF (quarter == 0 .AND. dime > 0 .AND. nickel > 0 .AND. penny > 0) THEN
      write(*,*) dime, "Dime", nickel, "Nickel", penny, "Penny"
   ELSE IF (quarter > 0 .AND. dime > 0 .AND. nickel > 0 .AND. penny > 0) THEN
      write(*,*) quarter, "Quarter", dime, "Dime", nickel, "Nickel", penny, "Penny"
   END IF

   year = DateINFO(1:4)
   month = DateINFO(5:6)
   day = DateINFO(7:8)
   hour = TimeINFO(1:2)
   minute = TimeINFO(3:4)
   second = TimeINFO(5:10)

   write(*,*) month,"-", day,"-", year,",", hour,":", minute

END PROGRAM firstFortran

SUBROUTINE makeChange(change, quarter, dime, nickel, penny)
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: change
   INTEGER, INTENT(OUT) :: quarter, dime, nickel, penny
   INTEGER :: hold, leftover
   hold = change
   leftover = mod(hold, 25)
   hold = hold - leftover
   quarter = hold/25
   hold = leftover

   leftover = mod(hold, 10)
   hold = hold - leftover
   dime = hold/10
   hold = leftover
  
   leftover = mod(hold, 5)
   hold = hold - leftover
   nickel = hold/5
   hold = leftover
  
   penny = hold
   RETURN
END
