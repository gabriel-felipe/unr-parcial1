;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname parcial-1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Parcial 1: Ejercicio 17


#|
Se quiere diseñar un programa que muestre una escena con fondo negro, de 500 píxeles de ancho por 250 de alto, en la cual
un círculo pequeño de radio 10, que llamaremos satélite, gire alrededor de otro círculo de radio 20, donde este último será ubicado
en el centro de la escena.
Para desplazarse, el satélite seguirá una trayectoria circular de radio 100 alrededor del círculo central,
trayectoria que será mostrada en la escena con una línea roja. El color del círculo central será amarillo.

El estado del sistema será una estructura constituida por el ángulo de rotación  y el color del satélite.

( define-struct  sysSatelite  [ ang  color ] )

El satélite se desplazará, a través del tiempo, en su trayectoria en el sentido de las agujas del reloj (hacia la derecha).
Mientras el color de satélite no sea rojo, el desplazamiento tendrá un avance de 0.05 radianes.
Cuando el color del satélite sea rojo, el avance será tres veces el valor mencionado.
Si se presiona la tecla 'c' el satélite asumirá el color cyan y si se presiona la tecla 'v' cambiará a verde; y entonces, en ambos casos, se desplazará con un avance de 0.05 radianes.
Si se presiona la tecla 'r' el satélite central asumirá el color rojo y entonces se desplazará con un avance que triplique el valor 0.05.
Si se presiona la tecla 'x', el círculo se posicionará en 0 y el programa se detendrá.
El estado inicial del sistema será la posición angular pi y el color cyan.

|#

; Estructura del estado y estado inicial
(define-struct sysSatelite [ang color])
(define ESTADOINICIAL (make-sysSatelite pi "cyan"))


; Constantes y funciones utilitarias para la funcion dibujar
(define ALTO 250)
(define ANCHO 500)
(define RADIO-SATALITE 10)
(define RADIO-ORBITADO 10)
(define RADIO-TRAYECTORIA 100)
(define ORBITADO (circle RADIO-ORBITADO "solid" "yellow"))
(define TRAYECTORIA (circle RADIO-TRAYECTORIA "outline" "red"))
(define BACKGROUND (empty-scene ANCHO ALTO "black"))
(define CENTRO (make-posn (/ ANCHO 2) (/ ALTO 2)))

; Toma un sysSatelite y retorna un posn con la posicion donde se deberia ubicar el
; saletite de acuerdo con el estado
; pos-satelite sysSatelite -> posn
(define (pos-satelite s)
  (make-posn
   (+ (posn-x CENTRO) (* RADIO-TRAYECTORIA (cos (sysSatelite-ang s))))
   (+ (posn-y CENTRO) (* RADIO-TRAYECTORIA (sin (sysSatelite-ang s))))))

(check-expect (=(posn-x (pos-satelite (make-sysSatelite pi "red")))
              (posn-x (make-posn
               (- (posn-x CENTRO) RADIO-TRAYECTORIA)
               (posn-y CENTRO)))) #t)

; Toma un sysSatelite y retorna una imagen del satelite
; SATELITE sysSatelite -> image
(define (SATELITE s) (circle RADIO-SATALITE "solid" (sysSatelite-color s)))

; Toma un sysSatelite y retorna una imagen que representa la escena del estado
; dibujar sysSatelite -> image
(define (dibujar s)
  (place-images (list (SATELITE s) ORBITADO TRAYECTORIA)
                (list
                   (pos-satelite s)
                   CENTRO
                   CENTRO)            
                BACKGROUND))


; Toma un sysSatelite y retorna la velocidade de acuerdo con el color del estado
; velocidade? sysSatelite -> Number
(define (velocidade? s)
  (if
   (equal? (sysSatelite-color s) "cyan")
   0.05
   0.15))

(check-expect (velocidade? (make-sysSatelite pi "cyan")) 0.05)
(check-expect (velocidade? (make-sysSatelite pi "red")) 0.15)
(check-expect (velocidade? (make-sysSatelite pi "green")) 0.15)

; Modifica el estado añandiendo la velocidad al angulo actual del estado
; mover? sysSatelite -> sysSatelite
(define (mover s)
  (make-sysSatelite
   (+ (sysSatelite-ang s) (velocidade? s))
   (sysSatelite-color s)))

(check-expect (sysSatelite-ang (mover (make-sysSatelite 1 "green"))) 1.15) ; Cambia posicion
(check-expect (sysSatelite-ang (mover (make-sysSatelite 1 "cyan"))) 1.05) ; Cambia posicion
(check-expect (sysSatelite-color (mover (make-sysSatelite 1 "green"))) "green") ; Mantiene el color sin cambiar

; Cambia el color de un sysSatelite
; cambia-color? sysSatelite color -> sysSatelite
(define (cambia-color s color)
  (make-sysSatelite (sysSatelite-ang s) color))

(check-expect (sysSatelite-color (cambia-color (make-sysSatelite 1 "green") "red")) "red")
(check-expect (sysSatelite-ang (cambia-color (make-sysSatelite 1 "green") "red")) 1)


; Cambia el angulo de posicion de un sysSatelite
; cambia-ang? sysSatelite Number -> sysSatelite
(define (cambia-ang s ang)
  (make-sysSatelite ang (sysSatelite-color s)))

(check-expect (sysSatelite-color (cambia-ang (make-sysSatelite 1 "green") 2)) "green")
(check-expect (sysSatelite-ang (cambia-ang (make-sysSatelite 1 "green") 2)) 2)
; Interpreta los eventos de teclado 
; manejar-teclado sysSatelite String -> sysSatelite
; Si presionada la tecla c cambia el color para cyan
; Si presionada la tecla r cambia el color para rojo
; Si presionada la tecla v cambia el color para verde
; Si presionada la tecla x cambia la posicion para 0
; Si presionada cualquiera otra tecla retorna el mismo sysSatelite sin alteraciones
(define (manejar-teclado s k)
  (cond
    [(key=? k "c") (cambia-color s "cyan")]
    [(key=? k "r") (cambia-color s "red")]
    [(key=? k "v") (cambia-color s "green")]
    [(key=? k "x") (cambia-ang s 0)]
    [else s]))

(check-expect (sysSatelite-color (manejar-teclado (make-sysSatelite 1 "green") "c")) "cyan")
(check-expect (sysSatelite-color (manejar-teclado (make-sysSatelite 1 "green") "v")) "green")
(check-expect (sysSatelite-color (manejar-teclado (make-sysSatelite 1 "green") "r")) "red")
(check-expect (sysSatelite-ang (manejar-teclado (make-sysSatelite 1 "green") "x")) 0)

; Decide si terminar el programa o no, termina el programa si la posicion es igual a 0
; termina? sysSatelite -> Boolean
(define (termina? s)
  (= (sysSatelite-ang s) 0))

(check-expect (termina? (make-sysSatelite 1 "green")) #f)
(check-expect (termina? (make-sysSatelite 0 "green")) #t)

(big-bang ESTADOINICIAL
 [to-draw dibujar]
 [on-tick mover]
 [on-key manejar-teclado ]
 [stop-when termina? dibujar])