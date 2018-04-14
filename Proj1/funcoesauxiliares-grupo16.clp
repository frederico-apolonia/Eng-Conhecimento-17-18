; Grupo 16
; Frederico Apolónia
; Tiago Santos
; Renato Gondin

; calcula os dias passados desde 1 1 2010
(deffunction data-em-dias (?dia ?mes ?ano)
    ; dias ja passados de um ano no mes de...
    (bind ?bissexto <- (eq (mod ?ano 4) 0))

    (if (eq (- ?mes 1) 0)  then (bind ?dias-mes 0))   ; janeiro
    (if (eq (- ?mes 1) 1)  then (bind ?dias-mes 31))  ; fevereiro
    (if (eq (- ?mes 1) 2)  then                       ; março
        (if ?bissexto ; ano bissexto
            then (bind ?dias-mes 60)
            else (bind ?dias-mes 59)))
    (if (eq (- ?mes 1) 3)  then                       ; abril
        (if ?bissexto
            then (bind ?dias-mes 91)
            else (bind ?dias-mes 90)))
    (if (eq (- ?mes 1) 4)  then
        (if ?bissexto
            then (bind ?dias-mes 121)
            else (bind ?dias-mes 120)))               ; maio
    (if (eq (- ?mes 1) 5)  then
        (if ?bissexto
            then (bind ?dias-mes 152)
            else (bind ?dias-mes 151)))               ; junho
    (if (eq (- ?mes 1) 6)  then
        (if ?bissexto
            then (bind ?dias-mes 182)
            else (bind ?dias-mes 181)))               ; julho
    (if (eq (- ?mes 1) 7)  then
        (if ?bissexto
            then (bind ?dias-mes 213)
            else (bind ?dias-mes 212)))               ; agosto
    (if (eq (- ?mes 1) 8)  then
        (if ?bissexto
            then (bind ?dias-mes 244)
            else (bind ?dias-mes 243)))               ; setembro
    (if (eq (- ?mes 1) 9)  then
        (if ?bissexto
            then (bind ?dias-mes 274)
            else (bind ?dias-mes 273)))               ; outubro
    (if (eq (- ?mes 1) 10) then
        (if ?bissexto
            then (bind ?dias-mes 306)
            else (bind ?dias-mes 305)))               ; novembro
    (if (eq (- ?mes 1) 11) then
        (if ?bissexto
            then (bind ?dias-mes 336)
            else (bind ?dias-mes 335)))               ; dezembro

    (return (+ ?dia ?dias-mes (soma-anos-em-dias 2010 (- ?ano 1)))
)

; calcula o numero de dias desde um ano ate outro
; ano1 < ano2
(deffunction soma-anos-em-dias (?ano1 ?ano2)
    (bind ?result 0)
    (loop-for-count (?ano ?ano1 ?ano2) do
        (if (eq (mod ?ano 4) 0)
            then (bind ?result (+ ?result 366))
            else (bind ?result (+ ?result 365))
        )
    )
    (return ?result)
)

; calcula a idade em anos
(deffunction calcula-anos (?d ?m ?a ?d1 ?m1 ?a1 ?idademinima)
    (if (>= (- ?a ?a1) ?idademinima)
        then
        (if (eq (- ?a ?a1) ?idademinima)
            then
            (if (< (- ?m ?m1) 0)
                then
                (return ?idademinima)
                else
                (if (eq 0 (- ?m ?m1))
                    then
                    (if (> 0 (- ?a ?a1))
                        then
                        (return ?idademinima)
                        else
                        (return (- ?idademinima 1))
                    )
                    else
                    (return (- ?idademinima 1))
                )
            )
            else
            (return (- ?a ?a1))
        )
        else
        (return (- ?a ?a1))
    )
)
