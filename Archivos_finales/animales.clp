;;;=============================================================================
;;;
;;; Sistema experto para identificar animales
;;;
;;; Hecho por:
;;; Rafael Herrero Álvarez <alu0100812275@ull.edu.es>
;;; Airam Manuel Navas Simón <alu0100618426@ull.edu.es>
;;; Daniel Ramos Acosta <alu0100843095@ull.edu.es>
;;;
;;; Este sistma experto se encarga de identificar animales
;;; según el tipo que sean (vertebrados, invertebrados, alimentación
;;; movimiento, reporoduccion, etc.) mediante respuestas
;;; de sí o no.
;;;
;;; Guía que usamos para el SE:
;;; http://thales.cica.es/rd/Recursos/rd99/ed99-0040-02/clasific.html
;;;
;;; Para ejecutar:
;;; (load animales.clp)
;;; (reset)
;;; (run)
;;;
;;; ============================================================================

(deffacts el-programa
	(versiones 1 2 3 4))

(deftemplate usuario
   (multislot nombre)
   (slot edad)
   (slot sexo))

	
;;;*****************************************************************************
;;;* MOSTRAR MENSAJE INICIAL Y SALIDA
;;;*****************************************************************************

(defrule system_banner ""
	(declare (salience 10))
	=>
	(printout t crlf crlf)
	(printout t "Sistema experto para averiguar qué animal estás pensando")
	(printout t crlf crlf))
	
(defrule datos_usuario ""
	(declare (salience 10))
	=>
	(printout t "Introduzca su nombre: ")
	(bind ?nombre_usuario (read))
	(printout t "Introduzca su edad: ")
	(bind ?edad_usuario (read))
	(printout t "Introduzca su sexo: ")
	(bind ?sexo_usuario (read))
	(printout t crlf crlf)
	(assert (usuario (nombre ?nombre_usuario)(edad ?edad_usuario)(sexo ?sexo_usuario))))
	
(defrule print_especie ""
	(declare (salience 10))
	(especie ?animal)
	(usuario (nombre ?i))
	=>
	(printout t crlf crlf)
	(printout t "Especie de animal: ")
	(printout t crlf crlf)
	(format t " %s%n%n%n" ?animal)
	(printout t "Hasta pronto " ?i "." crlf))

;;;*****************************************************************************
;;;   FUNCIONES PARA SACAR LA PREGUNTA Y QUE EL USUARIO LA RESPONDA
;;;*****************************************************************************

(deffunction pregunta (?pregunta $?valores)
	(printout t ?pregunta)
	(bind ?respuesta (read))
	(if (lexemep ?respuesta)
		then (bind ?respuesta (lowcase ?respuesta)))
	(while (not (member ?respuesta ?valores)) do
		(printout t ?pregunta)
		(bind ?respuesta (read))
		(if (lexemep ?respuesta)
			then (bind ?respuesta (lowcase ?respuesta))))
	?respuesta)

(deffunction si_or_no_p (?pregunta)
	(bind ?respuesta (pregunta ?pregunta si no s n))
	(if (or (eq ?respuesta si) (eq ?respuesta s))
		then si
		else no))

;;;*****************************************************************************
;;; DEFINCIONES DE LAS CLASES E INSTANCIAS INICIALES
;;;*****************************************************************************

;;; Definiendo clase ANIMAL
(defclass ANIMAL
	(is-a USER)
	(slot clasificacion (type SYMBOL) (allowed-symbols vertebrado invertebrado))
	(multislot nombre (type STRING))
	(slot extremidades (type INTEGER))
	(slot alimentacion (type SYMBOL) (allowed-symbols herbivoro carnivoro omnivoro))
	(slot reproduccion (type SYMBOL) (allowed-symbols oviparo viviparo ovoviviparo))
	(slot movimiento (type SYMBOL) (allowed-symbols andar volar nadar reptar))
	(slot clase (type SYMBOL)))

;;;;Ejemplo de instacia:
;;;;(make-instance Cabra of ANIMAL (clasificacion vertebrado) (nombre Cabra) (extremidades 4) (alimentacion ) (reproduccion viviparos) (movimiento andar) (clase mamifero))

;;; Definiendo los mensajes para leer los atributos
(defmessage-handler ANIMAL get_clasificacion()
	(printout t "Clasificación: " ?self:clasificacion crlf))

(defmessage-handler ANIMAL get_nombre()
	(printout t "Nombre: " ?self:nombre crlf))

(defmessage-handler ANIMAL get_extremidades()
	(printout t "Extremidades: " ?self:extremidades crlf))

(defmessage-handler ANIMAL get_alimentacion()
	(printout t "Alimentacion: " ?self:alimentacion crlf))

(defmessage-handler ANIMAL get_reproduccion()
	(printout t "Reproduccion: " ?self:reproduccion crlf))

(defmessage-handler ANIMAL get_movimiento()
	(printout t "Movimiento: " ?self:movimiento crlf))

(defmessage-handler ANIMAL get_clase()
	(printout t "Clase: " ?self:clase crlf))
	

;;; Definiendo los mensajes para establecer los valores de los atributos

(defmessage-handler ANIMAL set_clasificacion($?valores)
	(bind ?self:clasificacion $?valores))

(defmessage-handler ANIMAL set_nombre($?valores)
	(bind ?self:nombre $?valores))

(defmessage-handler ANIMAL set_extremidades($?valores)
	(bind ?self:extremidades $?valores))

(defmessage-handler ANIMAL set_alimentacion($?valores)
	(bind ?self:alimentacion $?valores))

(defmessage-handler ANIMAL set_reproduccion($?valores)
	(bind ?self:reproduccion $?valores))

(defmessage-handler ANIMAL set_movimiento($?valores)
	(bind ?self:movimiento $?valores))

(defmessage-handler ANIMAL set_clase($?valores)
	(bind ?self:clase $?valores))

(defmessage-handler ANIMAL get_informacion()
	(printout t "Clasificación: " ?self:clasificacion crlf)
	(printout t "Nombre: " ?self:nombre crlf)
	(printout t "Extremidades: " ?self:extremidades crlf)
	(printout t "Alimentacion: " ?self:alimentacion crlf)
	(printout t "Reproduccion: " ?self:reproduccion crlf)
	(printout t "Movimiento: " ?self:movimiento crlf)
	(printout t "Clase: " ?self:clase crlf))

;;; Crear instancia de ANIMAL que será donde inferamos el conocimiento.
(definstances objeto_animal_actual
	(animal_actual of ANIMAL))

;;; ESTO DE ABAJO NO FUNCIONA Y NO SABEMOS POR QUE
;;;;(defclass GRANJA
;;;;	(is-a OBJECT)
;;;;	(multislot animales (type ANIMAL)))

;;;;(defclass SALVAJE
;;;;	(is-a OBJECT)
;;;;	(multislot animales (type ANIMAL)))

;;;;(defclass DOMESTICO
;;;;	(is-a OBJECT)
;;;;	(multislot animales (type ANIMAL)))

;;;*****************************************************************************
;;;* REGLAS PARA PREGUNTAR
;;;*****************************************************************************

(defrule determinar_esqueleto ""
	(not (esqueleto ?))
	=>
	(assert (esqueleto (si_or_no_p "El animal tiene esqueleto (si/no)? "))))

(defrule determinar_vientre ""
	(not (vientre ?))
	=>
	(assert (vientre (si_or_no_p "El animal nace del vientre de la madre (si/no)? "))))

(defrule determinar_huevo ""
	(not (huevo ?))
	=>
	(assert (huevo (si_or_no_p "El animal nace de un huevo (si/no)? "))))

(defrule determinar_come_carne ""
	(not (come_carne ?))
	=>
	(assert (come_carne (si_or_no_p "El animal come otros animales (si/no)? "))))

(defrule determinar_come_plantas ""
	(not (come_plantas ?))
	=>
	(assert (come_plantas (si_or_no_p "El animal come plantas (si/no)? "))))

(defrule determinar_andar ""
	(not (puede_andar ?))
	=>
	(assert (puede_andar (si_or_no_p "El animal anda (si/no)? "))))

(defrule determinar_volar ""
	(not (puede_volar ?))
	=>
	(assert (puede_volar (si_or_no_p "El animal vuela (si/no)? "))))

(defrule determinar_nadar ""
	(not (puede_nadar ?))
	=>
	(assert (puede_nadar (si_or_no_p "El animal nada (si/no)? "))))

(defrule determinar_reptar ""
	(not (puede_reptar ?))
	=>
	(assert (puede_reptar (si_or_no_p "El animal repta (si/no)? "))))

(defrule determinar_aguijon_volar ""
	(clasificacion invertebrado)
	(reproduccion oviparo)
	(alimentacion herbivoro)
	(movimiento volar)
	(not (aguijon_volar ?))
	=>
	(assert (aguijon_volar (si_or_no_p "El animal tiene un aguijon (si/no)? "))))

(defrule determinar_aguijon_andar ""
	(clasificacion invertebrado)
	(reproduccion oviparo)
	(alimentacion carnivoro)
	(movimiento andar)
	(not (aguijon_andar ?))
	=>
	(assert (aguijon_andar (si_or_no_p "El animal tiene un aguijon (si/no)? "))))
	
(defrule determinar_come_muebles ""
	(esqueleto no)
	(huevo si)
	(or (come_plantas si) (come_carne si))
	(or (puede_andar si) (puede_volar si))
	(aguijon_volar no)
	(not (mueble ?))
	=>
	(assert (mueble (si_or_no_p "El animal siente devocion por la madera (si/no)? "))))
	
(defrule duerme_de_dia ""
	(esqueleto si)
	(huevo si)
	(come_plantas no)
	(puede_volar si)
	(not (dormir_de_dia ?))
	=>
	(assert (dormir_de_dia (si_or_no_p "El animal duerme de dia (si/no)? "))))

(defrule teme_ron ""
	(esqueleto no)
	(huevo si)
	(come_plantas no)
	(puede_andar si)
	(aguijon_andar si)
	(not (ron ?))
	=>
	(assert (ron (si_or_no_p "Ronald Weasly teme a este animal (si/no)? "))))
	
(defrule es_domestico ""
	(esqueleto si)
	(huevo no)
	(vientre si)
	(come_carne si)
	(come_plantas si)
	(puede_andar si)
	(not (domestico ?))
	=>
	(assert (domestico (si_or_no_p "Es un animal domestico (si/no)? "))))
	
(defrule es_mejor_amigo ""
	(domestico si)
	(not (mejor_amigo ?))
	=>
	(assert (mejor_amigo (si_or_no_p "Es el mejor amigo del hombre (si/no)? "))))
	
(defrule es_simio ""
	(domestico no)
	(not (simio ?))
	=>
	(assert (simio (si_or_no_p "Es un simio (si/no)? "))))
	
(defrule es_simio_grande ""
	(simio si)
	(not (simio_grande ?))
	=>
	(assert (simio_grande (si_or_no_p "Es grande de tamano (si/no)? "))))
	
(defrule tiene_culo_rojo ""
	(simio_grande no)
	(not (culo_rojo ?))
	=>
	(assert (culo_rojo (si_or_no_p "Tiene el culo rojo (si/no)? "))))
	
(defrule vive_nidos ""
	(culo_rojo no)
	(not (nidos ?))
	=>
	(assert (nidos (si_or_no_p "Hace nidos para dormir (si/no)? "))))

(defrule es_dos_patas ""
	(simio no)
	(not (dos_patas ?))
	=>
	(assert (dos_patas (si_or_no_p "De vez en cuando se pone sobre sus dos patas (si/no)? "))))

(defrule es_cola_rayas ""
	(dos_patas si)
	(not (cola_rayas ?))
	=>
	(assert (cola_rayas (si_or_no_p "Su cola es a rayas blancas y negras (si/no)? "))))

(defrule es_cazar_pescado ""
	(cola_rayas no)
	(not (cazar_pescado ?))
	=>
	(assert (cazar_pescado (si_or_no_p "Le gusta la miel y/o cazar salmones de los rios (si/no)? "))))

(defrule es_tiene_escamas ""
	(dos_patas no)
	(not (tiene_escamas ?))
	=>
	(assert (tiene_escamas (si_or_no_p "Tu animal tiene escamas (si/no)? "))))

(defrule es_fama_ladron ""
	(tiene_escamas no)
	(not (fama_ladron ?))
	=>
	(assert (fama_ladron (si_or_no_p "Tu animal tiene fama de ser ladrón (si/no)? "))))
	
(defrule es_enlatado ""
	(esqueleto si)
	(come_plantas no)
	(huevo si)
	(vientre no)
	(puede_nadar si)
	(not (enlatado ?))
	=>
	(assert (enlatado (si_or_no_p "Suele comerse enlatado en aceite (si/no)? "))))
	
(defrule es_un_avion ""
	(esqueleto si)
	(come_plantas no)
	(huevo no)
	(vientre si)
	(puede_nadar si)
	(not (avion ?))
	=>
	(assert (avion (si_or_no_p "Es tambien un tipo de avion (si/no)? "))))
	
(defrule es_unicornio ""
	(avion no)
	(not (unicornio ?))
	=>
	(assert (unicornio (si_or_no_p "Es considerado un unicornio marino (si/no)? "))))
	
(defrule tiene_pelota
	(unicornio no)
	(not (pelota ?))
	=>
	(assert (pelota (si_or_no_p "Se asocia a este animal con una pelota en su nariz (si/no)? "))))
	
(defrule tiene_pulmones ""
	(pelota no)
	(not (pulmones ?))
	=>
	(assert (pulmones (si_or_no_p "Tiene pulmones (si/no)? "))))
	
(defrule cambia_color ""
	(esqueleto si)
	(come_plantas no)
	(huevo si)
	(vientre no)
	(puede_reptar si)
	(not (cambio_color ?))
	=>
	(assert (cambio_color (si_or_no_p "Cambia el color de su piel para adaptarse al entorno (si/no)? "))))
	
(defrule puede_estrangular ""
	(cambio_color no)
	(not (estrangular ?))
	=>
	(assert (estrangular (si_or_no_p "Puede estrangular (si/no)? "))))
	
(defrule casa_a_cuestas ""
	(esqueleto no)
	(come_carne no)
	(huevo si)
	(vientre no)
	(puede_reptar si)
	(not (casa ?))
	=>
	(assert (casa (si_or_no_p "Lleva la casa a cuestas (si/no)? "))))
	
(defrule traer_bebe ""
	(esqueleto si)
	(come_carne no)
	(huevo si)
	(vientre no)
	(puede_volar si)
	(not (bebe ?))
	=>
	(assert (bebe (si_or_no_p "Se dice que este animal se encarga de traer los bebes humanos (si/no)? "))))
	
(defrule repetir_decir ""
	(bebe no)
	(not (repetir_habla ?))
	=>
	(assert (repetir_habla (si_or_no_p "Suele repetir lo que oye (si/no)? "))))
	
(defrule mueve_alas_rapido ""
	(repetir_habla no)
	(not (alas_rapido ?))
	=>
	(assert (alas_rapido (si_or_no_p "Mueve las alas hasta 5400 veces por minuto (si/no)? "))))
	
(defrule animal_lento ""
	(esqueleto si)
	(huevo si)
	(vientre no)
	(come_carne si)
	(come_plantas si)
	(puede_andar si)
	(not (lento ?))
	=>
	(assert (lento (si_or_no_p "Es un animal muy lento (si/no)? "))))
	
(defrule accion_de_gracias ""
	(lento no)
	(not (relleno ?))
	=>
	(assert (relleno (si_or_no_p "Se come relleno en accion de gracias (si/no)? "))))
	
(defrule cuello_alargado ""
	(relleno no)
	(not (cuello_grande ?))
	=>
	(assert (cuello_grande (si_or_no_p "Tiene un cuello grande (si/no)? "))))
	
(defrule es_cobarde ""
	(cuello_grande no)
	(not (cobarde ?))
	=>
	(assert (cobarde (si_or_no_p "Se utiliza como sinonimo de cobarde (si/no)? "))))
	
(defrule puede_picar ""
	(esqueleto no)
	(huevo si)
	(vientre no)
	(come_carne si)
	(come_plantas si)
	(puede_volar si)
	(not (picar ?))
	=>
	(assert (picar (si_or_no_p "Puede picar (si/no)? "))))
	
(defrule es_verde ""
	(esqueleto si)
	(huevo si)
	(vientre no)
	(come_plantas no)
	(puede_andar si)
	(not (verde ?))
	=>
	(assert (verde (si_or_no_p "Suele ser, mayoritariamente, de color verde (si/no)? "))))

;;;*********************;;;
;;;   PREGUNTAS AIRAM   ;;;
;;;*********************;;;

(defrule animal_carga ""
	(esqueleto si)
	(huevo no)
	(come_carne no)
	(puede_andar si)
	=>
	(assert (carga (si_or_no_p "Tu animal se suele utilizar como animal de carga (si/no)? "))))

(defrule animal_granja ""
	(or (carga si)(carga no))
	=>
	(assert (granja (si_or_no_p "Tu animal lo puedes encontrar en una granja (si/no)? "))))

(defrule animal_cuernos ""
	(or(carga si)(carga no))
	(or(granja si)(granja no))
	=>
	(assert (cuernos (si_or_no_p "Tu animal tiene cuernos (si/no)? "))))

(defrule animal_galopa ""
	(carga si)
	(granja si)
	(cuernos no)
	=>
	(assert (galopa (si_or_no_p "Tu animal galopa (si/no)? "))))
	
(defrule animal_joroba ""
	(carga si)
	(granja no)
	(cuernos no)
	=>
	(assert (joroba (si_or_no_p "Tu animal tiene joroba (si/no)? "))))
	
(defrule animal_joroba_si ""
	(carga si)
	(granja no)
	(cuernos no)
	(joroba si)
	=>
	(assert (dos_jorobas (si_or_no_p "Tu animal tiene dos jorobas (si/no)? "))))
	
(defrule animal_joroba_no ""
	(carga si)
	(granja no)
	(cuernos no)
	(joroba no)
	=>
	(assert (trompa (si_or_no_p "Tu animal tiene trompa (si/no)? "))))
	
(defrule animal_lana ""
	(carga no)
	(granja si)
	(cuernos no)
	=>
	(assert (lana (si_or_no_p "Tu animal tiene lana (si/no)? "))))
	
(defrule animal_blanquinegra ""
	(carga no)
	(granja si)
	(cuernos si)
	=>
	(assert (blanquinegra (si_or_no_p "Tu animal, en general, es blanco con manchas negras (si/no)? "))))
	
(defrule animal_pequenio ""
	(carga no)
	(granja no)
	(cuernos no)
	=>
	(assert (pequenio (si_or_no_p "Tu animal es tan pequeño como un perro o menos (si/no)? "))))
	
(defrule animal_pequenio_si ""
	(carga no)
	(granja no)
	(cuernos no)
	(pequenio si)
	=>
	(assert (salta (si_or_no_p "Tu animal salta (si/no)? "))))
	
(defrule animal_pequenio_no ""
	(carga no)
	(granja no)
	(cuernos no)
	(pequenio no)
	=>
	(assert (salta (si_or_no_p "Tu animal salta (si/no)? "))))
	
(defrule animal_rayas ""
	(carga no)
	(granja no)
	(cuernos no)
	(pequenio no)
	(salta no)
	=>
	(assert (rayas (si_or_no_p "Tu animal tiene rayas (si/no)? "))))
	
(defrule animal_rayas_no ""
	(carga no)
	(granja no)
	(cuernos no)
	(pequenio no)
	(rayas no)
	=>
	(assert (cuello_largo (si_or_no_p "Tu animal tiene el cuello largo (si/no)? "))))
	
(defrule animal_tala ""
	(carga no)
	(granja no)
	(cuernos no)
	(pequenio si)
	(salta no)
	=>
	(assert (tala_arboles (si_or_no_p "Tu animal tala arboles (si/no)? "))))
	
(defrule animal_tala_no ""
	(carga no)
	(granja no)
	(cuernos no)
	(pequenio si)
	(salta no)
	(tala_arboles no)
	=>
	(assert (come_bellotas (si_or_no_p "A tu animal le gustan mucho las bellotas (si/no)? "))))
	
(defrule animal_gris ""
	(carga no)
	(granja no)
	(cuernos si)
	=>
	(assert (gris (si_or_no_p "Tu animal es gris (si/no)? "))))
	
(defrule animal_gris_no ""
	(carga no)
	(granja no)
	(cuernos si)
	(gris no)
	=>
	(assert (cuernos_extravagantes (si_or_no_p "Tu animal tiene cuernos extravagantes (si/no)? "))))
	
(defrule animal_negro ""
	(esqueleto si)
	(vientre no)
	(come_carne si)
	(come_plantas si)
	(puede_volar si)
	=>
	(assert (negro (si_or_no_p "Tu animal es de color negro (si/no)? "))))
	
(defrule animal_negro_no ""
	(negro no)
	=>
	(assert (enfermedades (si_or_no_p "Tu animal es famoso por transmitir enfermedades (si/no)? "))))

(defrule animal_pequenio_carnivoro ""
	(esqueleto si)
	(vientre si)
	(huevo no)
	(come_carne si)
	(come_plantas no)
	(puede_andar si)
	=>
	(assert (pequenio_c (si_or_no_p "Tu animal tan pequeño como un perro o menos (si/no)? "))))

(defrule animal_pequenio_carnivoro_si ""
	(pequenio_c si)
	=>
	(assert (felino (si_or_no_p "Tu animal es un felino (si/no)? "))))
	
(defrule animal_pequenio_carnivoro_no ""
	(pequenio_c no)
	=>
	(assert (negro (si_or_no_p "Tu animal puede ser negro (si/no)? "))))
	
(defrule animal_felino_si ""
	(pequenio_c si)
	(felino si)
	=>
	(assert (domestico2 (si_or_no_p "Tu animal es un animal domestico (si/no)? "))))

(defrule animal_felino_no ""
	(pequenio_c si)
	(felino no)
	=>
	(assert (madriguera (si_or_no_p "Tu animal vive en una madriguera bajo tierra (si/no)? "))))

(defrule animal_madriguera_si ""
	(pequenio_c si)
	(felino no)
	(madriguera si)
	=>
	(assert (pinchos (si_or_no_p "Tu animal tiene pinchos en su cuerpo (si/no)? "))))
	
(defrule animal_madriguera_no ""
	(pequenio_c si)
	(felino no)
	(madriguera no)
	=>
	(assert (solo_america (si_or_no_p "Tu animal habita solo en america (si/no)? "))))

(defrule animal_pinchos_no ""
	(pequenio_c si)
	(felino no)
	(madriguera si)
	(pinchos no)
	=>
	(assert (garras (si_or_no_p "Tu animal tiene garras para excavar (si/no)? "))))
	
(defrule animal_negro_no_2 ""
	(pequenio_c no)
	(negro no)
	=>
	(assert (rayas (si_or_no_p "Tu animal tiene rayas (si/no)? "))))

;;; PREGUNTAS DANIEL

(defrule es_tentaculos ""
	(clasificacion invertebrado)
	(alimentacion carnivoro)
	(movimiento nadar)
	(not (tentaculos))
	=>
	(assert (tentaculos (si_or_no_p "Tiene tentaculos (si/no)? "))))

(defrule es_tentaculos_ocho ""
	(tentaculos si)
	(not (tentaculos_ocho))
	=>
	(assert (tentaculos_ocho (si_or_no_p "Tiene ocho tentaculos (si/no)? "))))
	
(defrule es_comido_gallega ""
	(tentaculos_ocho si)
	(not (comido_gallega))
	=>
	(assert (comido_gallega (si_or_no_p "Se suele comer a la gallega (si/no)? "))))

(defrule es_agresivo ""
	(tentaculos no)
	(not (agresivo))
	=>
	(assert (agresivo (si_or_no_p "Es un animal peligroso/agresivo (si/no)? "))))

(defrule es_pequenio_falange ""
	(clasificacion invertebrado)
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(not (pequenio_falange))
	=>
	(assert (pequenio_falange (si_or_no_p "Es mas pequeño que una falanje de un dedo (si/no)? "))))

(defrule es_come_polvo ""
	(pequenio_falange si)
	(not (come_polvo))
	=>
	(assert (come_polvo (si_or_no_p "Se come el polvo (si/no)? "))))

(defrule es_vive_wajo_mar ""
	(pequenio_falange no)
	(not (vive_wajo_mar))
	=>
	(assert (vive_wajo_mar (si_or_no_p "Vive wajoooo del maaaaar (si/no)? "))))

(defrule es_puede_cantar ""
	(vive_wajo_mar no)
	(not (puede_cantar))
	=>
	(assert (puede_cantar (si_or_no_p "Puede cantar (si/no)? "))))

(defrule es_roja_con_manchas ""
	(mueble no)
	(not (roja_con_manchas))
	=>
	(assert (roja_con_manchas (si_or_no_p "Es rojo con manchas (si/no)? "))))

(defrule es_saltarin ""
	(clasificacion invertebrado)
	(alimentacion herbivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(not (saltarin))
	=>
	(assert (saltarin (si_or_no_p "Su movimiento consiste en ir dando saltos (si/no)? "))))

;;;*****************************************************************************
;;; REGLAS PARA GUARDAR LAS COSAS EN EL OBJETO animal_actual Y CONDICIONES DE INTEGRIDAD
;;;*****************************************************************************

(defrule es_vertebrado ""
	(esqueleto si)
	=>
	(assert (clasificacion vertebrado))
	(send [animal_actual] set_clasificacion vertebrado))

(defrule es_invertebrado ""
	(esqueleto no)
	=>
	(assert (clasificacion invertebrado))
	(send [animal_actual] set_clasificacion invertebrado))

(defrule es_viviparo ""
	(huevo no)
	=>
	(assert (reproduccion viviparo))
	(assert (vientre si))
	(send [animal_actual] set_reproduccion viviparo))

(defrule es_oviparo ""
	(vientre no)
	=>
	(assert (reproduccion oviparo))
	(assert (huevo si))
	(send [animal_actual] set_reproduccion oviparo))

(defrule es_ovoviviparo ""
	(vientre si)
	(huevo si)
	=>
	(assert (reproduccion ovoviviparo))
	(send [animal_actual] set_reproduccion ovoviviparo))
	
(defrule es_herbivoro ""
	(come_carne no)
	=>
	(assert (alimentacion herbivoro))
	(assert (come_plantas si))
	(send [animal_actual] set_alimentacion herbivoro))
	
(defrule es_carnivoro ""
	(come_plantas no)
	=>
	(assert (alimentacion carnivoro))
	(assert (come_carne si))
	(send [animal_actual] set_alimentacion carnivoro))

(defrule es_omnivoro ""
	(come_plantas si)
	(come_carne si)
	=>
	(assert (alimentacion omnivoro))
	(send [animal_actual] set_alimentacion omnivoro))
	
(defrule es_andador ""
	(puede_andar si)
	=>
	(assert (movimiento andar))
	(assert (puede_volar no))
	(assert (puede_nadar no))
	(assert (puede_reptar no))
	(send [animal_actual] set_movimiento andar))
	
(defrule es_volador ""
	(puede_volar si)
	=>
	(assert (movimiento volar))
	(assert (puede_andar no))
	(assert (puede_nadar no))
	(assert (puede_reptar no))
	(send [animal_actual] set_movimiento volar))
	
(defrule es_nadador ""
	(puede_nadar si)
	=>
	(assert (movimiento nadar))
	(assert (puede_andar no))
	(assert (puede_volar no))
	(assert (puede_reptar no))
	(send [animal_actual] set_movimiento nadar))
	
(defrule es_reptador ""
	(puede_reptar si)
	=>
	(assert (movimiento reptar))
	(assert (puede_andar no))
	(assert (puede_volar no))
	(assert (puede_nadar no))
	(send [animal_actual] set_movimiento reptar))

;;;*****************************************************************************
;;;* REGLAS PARA DEDUCIR LA ESPECIE
;;;*****************************************************************************

(defrule Abeja ""
	(clasificacion invertebrado) 
	(alimentacion herbivoro)
	(reproduccion oviparo)
	(movimiento volar)
	(aguijon_volar si)
	=>
	(assert (especie "Tu animal es una abeja."))
	(send [animal_actual] set_nombre Abeja)
	(send [animal_actual] set_extremidades 8)
	(send [animal_actual] set_clase insecta)
)

(defrule Acaro ""
	(clasificacion invertebrado) 
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(come_polvo si)
	=>
	(assert (especie "Tu animal es un acaro."))
	(send [animal_actual] set_nombre Acaro)
	(send [animal_actual] set_extremidades 6)
	(send [animal_actual] set_clase arachnida)
)

(defrule Aguila ""
	(clasificacion vertebrado) 
	(alimentacion carnivoro)
	(reproduccion oviparo)
	(movimiento volar)
	(dormir_de_dia no)
	=>
	(assert (especie "Tu animal es un aguila"))
	(send [animal_actual] set_nombre Aguila)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase aves)
)

(defrule Arania ""
	(clasificacion invertebrado) 
	(alimentacion carnivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(ron si)
	=>
	(assert (especie "Tu animal es una arania"))
	(send [animal_actual] set_nombre Arania)
	(send [animal_actual] set_extremidades 8)
	(send [animal_actual] set_clase arachnida)
)

(defrule Ardilla ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga no)
	(granja no)
	(cuernos no)
	(pequenio si)
	(salta no)
	(tala_arboles no)
	(come_bellotas si)
	=>
	(assert (especie "Tu animal es ardilla"))
	(send [animal_actual] set_nombre Ardilla)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Armadillo ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(domestico no)
	(simio no)
	(dos_patas no)
	(tiene_escamas si)
	=>
	(assert (especie "Tu animal es armadillo"))
	(send [animal_actual] set_nombre Armadillo)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Asno ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga si)
	(granja si)
	(cuernos no)
	(galopa no)
	=>
	(assert (especie "Tu animal es un asno"))
	(send [animal_actual] set_nombre Asno)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Atun ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion oviparo)
	(movimiento nadar)
	(enlatado si)
	=>
	(assert (especie "Tu animal es un atun"))
	(send [animal_actual] set_nombre Atun)
	(send [animal_actual] set_extremidades 3)
	(send [animal_actual] set_clase actinopterygii)
)

(defrule Ballena ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion viviparo)
	(movimiento nadar)
	(pulmones no)
	=>
	(assert (especie "Tu animal es una ballena"))
	(send [animal_actual] set_nombre Ballena)
	(send [animal_actual] set_extremidades 3)
	(send [animal_actual] set_clase mammalia)
)

(defrule Beluga ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion viviparo)
	(movimiento nadar)
	(avion si)
	=>
	(assert (especie "Tu animal es una beluga"))
	(send [animal_actual] set_nombre Beluga)
	(send [animal_actual] set_extremidades 3)
	(send [animal_actual] set_clase mammalia)
)

(defrule Bufalo ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga no)
	(granja no)
	(cuernos si)
	(gris no)
	(cuernos_extravagantes no)
	=>
	(assert (especie "Tu animal es un bufalo"))
	(send [animal_actual] set_nombre Bufalo)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Buho ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion oviparo)
	(movimiento volar)
	(dormir_de_dia si)
	=>
	(assert (especie "Tu animal es un buho"))
	(send [animal_actual] set_nombre Buho)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase aves)
)

(defrule Caballito_de_mar ""
	(clasificacion invertebrado)
	(alimentacion carnivoro)
	(reproduccion oviparo)
	(movimiento nadar)
	(agresivo no)
	=>
	(assert (especie "Tu animal es un caballito de mar"))
	(send [animal_actual] set_nombre Caballito de mar)
	(send [animal_actual] set_extremidades 3)
	(send [animal_actual] set_clase actinopterygii)
)

(defrule Caballo ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga si)
	(granja si)
	(cuernos no)
	(galopa si)
	=>
	(assert (especie "Tu animal es un caballo"))
	(send [animal_actual] set_nombre Caballo)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Cabra ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga no)
	(granja si)
	(cuernos si)
	(blanquinegra no)
	=>
	(assert (especie "Tu animal es una cabra"))
	(send [animal_actual] set_nombre Cabra)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Calamar ""
	(clasificacion invertebrado)
	(alimentacion carnivoro)
	(reproduccion oviparo)
	(movimiento nadar)
	(tentaculos_ocho no)
	=>
	(assert (especie "Tu animal es es un calamar."))
	(send [animal_actual] set_nombre Calamar)
	(send [animal_actual] set_extremidades 10)
	(send [animal_actual] set_clase cephalopoda)
)

(defrule Camaleon ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion oviparo)
	(movimiento reptar)
	(cambio_color si)
	=>
	(assert (especie "Tu animal es un camaleon."))
	(send [animal_actual] set_nombre Camaleon)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase reptilia)
)

(defrule Camello ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga si)
	(granja no)
	(cuernos no)
	(joroba si)
	(dos_jorobas si)
	=>
	(assert (especie "Tu animal es un camello."))
	(send [animal_actual] set_nombre Camello)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Cangrejo ""
	(clasificacion invertebrado)
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(vive_wajo_mar si)
	=>
	(assert (especie "Tu animal es un cangrejo."))
	(send [animal_actual] set_nombre Cangrejo)
	(send [animal_actual] set_extremidades 10)
	(send [animal_actual] set_clase malacostraca)
)

(defrule Canguro ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga no)
	(granja no)
	(cuernos no)
	(pequenio no)
	(salta si)
	=>
	(assert (especie "Tu animal es un canguro."))
	(send [animal_actual] set_nombre Canguro)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Caracol ""
	(clasificacion invertebrado)
	(alimentacion herbivoro)
	(reproduccion oviparo)
	(movimiento reptar)
	(casa si)
	=>
	(assert (especie "Tu animal es un caracol."))
	(send [animal_actual] set_nombre Caracol)
	(send [animal_actual] set_extremidades 0)
	(send [animal_actual] set_clase gastropoda)
)

(defrule Castor ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga no)
	(granja no)
	(cuernos no)
	(pequenio si)
	(salta no)
	(tala_arboles si)
	=>
	(assert (especie "Tu animal es un castor."))
	(send [animal_actual] set_nombre Castor)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Cebra ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga no)
	(granja no)
	(cuernos no)
	(pequenio no)
	(salta no)
	(rayas si)
	=>
	(assert (especie "Tu animal es una cebra."))
	(send [animal_actual] set_nombre Cebra)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Cerdo ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga no)
	(granja si)
	(cuernos no)
	(lana no)
	=>
	(assert (especie "Tu animal es cerdo."))
	(send [animal_actual] set_nombre Cerdo)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Chimpance ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(domestico no)
	(simio si)
	(nidos no)
	=>
	(assert (especie "Tu animal es un chimpance."))
	(send [animal_actual] set_nombre Chimpance)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Ciempies ""
	(clasificacion invertebrado)
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento reptar)
	=>
	(assert (especie "Tu animal es un ciempies."))
	(send [animal_actual] set_nombre Ciempies)
	(send [animal_actual] set_extremidades 100)
	(send [animal_actual] set_clase chilopoda)
)

(defrule Ciervo ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga no)
	(granja no)
	(cuernos si)
	(gris no)
	(cuernos_extravagantes si)
	=>
	(assert (especie "Tu animal es un ciervo."))
	(send [animal_actual] set_nombre Ciervo)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Ciguenia ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion oviparo)
	(movimiento volar)
	(bebe si)
	=>
	(assert (especie "Tu animal es una ciguenia."))
	(send [animal_actual] set_nombre Ciguenia)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase aves)
)

(defrule Cisne ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(cuello_grande si)
	=>
	(assert (especie "Tu animal es cisne."))
	(send [animal_actual] set_nombre Cisne)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase aves)
)

(defrule Cocodrilo ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion oviparo)
	(movimiento reptar)
	(estrangular no)
	=>
	(assert (especie "Tu animal es un cocodrilo."))
	(send [animal_actual] set_nombre Cocodrilo)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase sauropsida)
)

(defrule Codorniz ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento volar)
	(negro no)
	(enfermedades no)
	=>
	(assert (especie "Tu animal es una codorniz."))
	(send [animal_actual] set_nombre Codorniz)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase aves)
)

(defrule Colibri ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion oviparo)
	(movimiento volar)
	(alas_rapido si)
	=>
	(assert (especie "Tu animal es un colibri."))
	(send [animal_actual] set_nombre Colibri)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase aves)
)

(defrule Conejo ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga no)
	(granja no)
	(cuernos no)
	(pequenio si)
	(salta si)
	=>
	(assert (especie "Tu animal es un conejo"))
	(send [animal_actual] set_nombre Conejo)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Coyote ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(pequenio_c si)
	(felino no)
	(madriguera no)
	(solo_america si)
	=>
	(assert (especie "Tu animal es un coyote"))
	(send [animal_actual] set_nombre Coyote)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Cucaracha ""
	(clasificacion invertebrado)
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(puede_cantar no)
	=>
	(assert (especie "Tu animal es una cucaracha"))
	(send [animal_actual] set_nombre Cucaracha)
	(send [animal_actual] set_extremidades 6)
	(send [animal_actual] set_clase insecta)
)

(defrule Cuervo ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento volar)
	(negro si)
	=>
	(assert (especie "Tu animal es un cuervo"))
	(send [animal_actual] set_nombre Cuervo)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase aves)
)

(defrule Delfin ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion viviparo)
	(movimiento nadar)
	(pulmones si)
	=>
	(assert (especie "Tu animal es un delfin"))
	(send [animal_actual] set_nombre Delfin)
	(send [animal_actual] set_extremidades 3)
	(send [animal_actual] set_clase mammalia)
)

(defrule Dromedario ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga si)
	(granja no)
	(cuernos no)
	(joroba si)
	(dos_jorobas no)
	=>
	(assert (especie "Tu animal es un dromedario"))
	(send [animal_actual] set_nombre Dromedario)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Elefante ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga si)
	(granja no)
	(cuernos no)
	(joroba no)
	(trompa si)
	=>
	(assert (especie "Tu animal es un elefante."))
	(send [animal_actual] set_nombre Elefante)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Erizo ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(pequenio_c si)
	(felino no)
	(madriguera si)
	(pinchos si)
	=>
	(assert (especie "Tu animal es un erizo"))
	(send [animal_actual] set_nombre Erizo)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Escarabajo ""
	(clasificacion invertebrado)
	(alimentacion carnivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(aguijon_andar no)
	=>
	(assert (especie "Tu animal es un escarabajo"))
	(send [animal_actual] set_nombre Escarabajo)
	(send [animal_actual] set_extremidades 6)
	(send [animal_actual] set_clase insecta)
)

(defrule Escorpion ""
	(clasificacion invertebrado)
	(alimentacion carnivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(ron no)
	=>
	(assert (especie "Tu animal es un escorpion"))
	(send [animal_actual] set_nombre Escorpion)
	(send [animal_actual] set_extremidades 9)
	(send [animal_actual] set_clase arachnida)
)

(defrule Foca ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion viviparo)
	(movimiento nadar)
	(pelota si)
	=>
	(assert (especie "Tu animal es una foca"))
	(send [animal_actual] set_nombre Foca)
	(send [animal_actual] set_extremidades 3)
	(send [animal_actual] set_clase mammalia)
)

(defrule Gallina ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(cobarde si)
	=>
	(assert (especie "Tu animal es una gallina"))
	(send [animal_actual] set_nombre Gallina)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase aves)
)

(defrule Gato ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(pequenio_c si)
	(felino si)
	(domestico2 si)
	=>
	(assert (especie "Tu animal es un gato"))
	(send [animal_actual] set_nombre Gato)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Gorila ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(domestico no)
	(simio si)
	(simio_grande si)
	=>
	(assert (especie "Tu animal es un gorila"))
	(send [animal_actual] set_nombre Gorila)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase mammalia)
)

(defrule Grillo ""
	(clasificacion invertebrado)
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(puede_cantar si)
	=>
	(assert (especie "Tu animal es un grillo"))
	(send [animal_actual] set_nombre Grillo)
	(send [animal_actual] set_extremidades 6)
	(send [animal_actual] set_clase insecta)
)

(defrule Hamster ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(domestico si)
	(mejor_amigo no)
	=>
	(assert (especie "Tu animal es un hamster"))
	(send [animal_actual] set_nombre Hamster)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Hipopotamo ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga no)
	(granja no)
	(cuernos no)
	(pequenio no)
	(salta no)
	(rayas no)
	(cuello_largo no)
	=>
	(assert (especie "Tu animal es un hipopotamo"))
	(send [animal_actual] set_nombre Hipopotamo)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Hormiga ""
	(clasificacion invertebrado)
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(come_polvo no)
	=>
	(assert (especie "Tu animal es una hormiga"))
	(send [animal_actual] set_nombre Hormiga)
	(send [animal_actual] set_extremidades 6)
	(send [animal_actual] set_clase insecta)
)

(defrule Jabali ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(domestico no)
	(simio no)
	(dos_patas no)
	(fama_ladron no)
	=>
	(assert (especie "Tu animal es un jabali"))
	(send [animal_actual] set_nombre Jabali)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Jirafa ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga no)
	(granja no)
	(cuernos no)
	(pequenio no)
	(salta no)
	(rayas no)
	(cuello_largo si)
	=>
	(assert (especie "Tu animal es una jirafa"))
	(send [animal_actual] set_nombre Jirafa)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Koala ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga no)
	(granja no)
	(cuernos no)
	(pequenio si)
	(salta no)
	(tala_arboles no)
	(come_bellotas no)
	=>
	(assert (especie "Tu animal es un koala"))
	(send [animal_actual] set_nombre Koala)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Lagarto ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento reptar)
	=>
	(assert (especie "Tu animal es un lagarto"))
	(send [animal_actual] set_nombre Lagarto)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase sauropsida)
)

(defrule Lemur ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(domestico no)
	(simio no)
	(dos_patas si)
	(cola_rayas si)
	=>
	(assert (especie "Tu animal es un lemur"))
	(send [animal_actual] set_nombre Lemur)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Leon ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(pequenio_c no)
	(negro no)
	(rayas no)
	=>
	(assert (especie "Tu animal es un leon"))
	(send [animal_actual] set_nombre Leon)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Lince ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(pequenio_c si)
	(felino si)
	(domestico2 no)
	=>
	(assert (especie "Tu animal es un lince"))
	(send [animal_actual] set_nombre Lince)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Llama ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga si)
	(granja no)
	(cuernos no)
	(joroba no)
	(trompa no)
	=>
	(assert (especie "Tu animal es una llama"))
	(send [animal_actual] set_nombre Llama)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Lobo ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(pequenio_c si)
	(felino no)
	(madriguera no)
	(solo_america no)
	=>
	(assert (especie "Tu animal es un lobo"))
	(send [animal_actual] set_nombre Lobo)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Lombriz ""
	(clasificacion invertebrado)
	(alimentacion herbivoro)
	(reproduccion oviparo)
	(movimiento reptar)
	(casa no)
	=>
	(assert (especie "Tu animal es una lombriz"))
	(send [animal_actual] set_nombre Lombriz)
	(send [animal_actual] set_extremidades 0)
	(send [animal_actual] set_clase clitellata)
)

(defrule Loro ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion oviparo)
	(movimiento volar)
	(repetir_habla si)
	=>
	(assert (especie "Tu animal es un loro"))
	(send [animal_actual] set_nombre Loro)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase aves)
)

(defrule Mandril ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(culo_rojo si)
	=>
	(assert (especie "Tu animal es un mandril"))
	(send [animal_actual] set_nombre Mandril)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase mammalia)
)

(defrule Mariposa ""
	(clasificacion invertebrado)
	(alimentacion herbivoro)
	(reproduccion oviparo)
	(movimiento volar)
	(roja_con_manchas no)
	=>
	(assert (especie "Tu animal es una mariposa"))
	(send [animal_actual] set_nombre Mariposa)
	(send [animal_actual] set_extremidades 6)
	(send [animal_actual] set_clase insecta)
)

(defrule Mariquita ""
	(clasificacion invertebrado)
	(alimentacion herbivoro)
	(reproduccion oviparo)
	(movimiento volar)
	(roja_con_manchas si)
	=>
	(assert (especie "Tu animal es una mariquita"))
	(send [animal_actual] set_nombre Mariquita)
	(send [animal_actual] set_extremidades 6)
	(send [animal_actual] set_clase insecta)
)

(defrule Medusa ""
	(clasificacion invertebrado)
	(alimentacion herbivoro)
	(reproduccion oviparo)
	(movimiento nadar)
	=>
	(assert (especie "Tu animal es una medusa"))
	(send [animal_actual] set_nombre Medusa)
	(send [animal_actual] set_extremidades 7)
	(send [animal_actual] set_clase hydrozoa)
)

(defrule Mosca ""
	(clasificacion invertebrado)
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento volar)
	(picar no)
	=>
	(assert (especie "Tu animal es una mosca"))
	(send [animal_actual] set_nombre Mosca)
	(send [animal_actual] set_extremidades 8)
	(send [animal_actual] set_clase insecta)
)

(defrule Mosquito ""
	(clasificacion invertebrado)
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento volar)
	(picar si)
	=>
	(assert (especie "Tu animal es un mosquito"))
	(send [animal_actual] set_nombre Mosquito)
	(send [animal_actual] set_extremidades 8)
	(send [animal_actual] set_clase insecta)
)

(defrule Murcielago ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion viviparo)
	(movimiento volar)
	=>
	(assert (especie "Tu animal es un murcielago"))
	(send [animal_actual] set_nombre Murcielago)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase mammalia)
)

(defrule Narval ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion viviparo)
	(movimiento nadar)
	(unicornio si)
	=>
	(assert (especie "Tu animal es un narval"))
	(send [animal_actual] set_nombre Narval)
	(send [animal_actual] set_extremidades 3)
	(send [animal_actual] set_clase mammalia)
)

(defrule Oca ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion oviparo)
	(movimiento volar)
	(alas_rapido no)
	=>
	(assert (especie "Tu animal es una oca"))
	(send [animal_actual] set_nombre Oca)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase aves)
)

(defrule Orangutan ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(nidos si)
	=>
	(assert (especie "Tu animal es un orangutan"))
	(send [animal_actual] set_nombre Orangutan)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Ornitorrinco ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion ovoviviparo)
	(movimiento andar)
	=>
	(assert (especie "Tu animal es un ornitorrinco"))
	(send [animal_actual] set_nombre Ornitorrinco)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Oruga ""
	(clasificacion invertebrado)
	(alimentacion herbivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(saltarin no)
	=>
	(assert (especie "Tu animal es un oruga"))
	(send [animal_actual] set_nombre oruga)
	(send [animal_actual] set_extremidades 6)
	(send [animal_actual] set_clase insecta)
)

(defrule Oso ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(domestico no)
	(simio no)
	(dos_patas si)
	(cazar_pescado si)
	=>
	(assert (especie "Tu animal es un oso"))
	(send [animal_actual] set_nombre Oso)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase mammalia)
)

(defrule Oveja ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga no)
	(granja si)
	(cuernos no)
	(lana si)
	=>
	(assert (especie "Tu animal es una oveja"))
	(send [animal_actual] set_nombre Oveja)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase mammalia)
)

(defrule Paloma ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento volar)
	(negro no)
	(enfermedades si)
	=>
	(assert (especie "Tu animal es una paloma"))
	(send [animal_actual] set_nombre Paloma)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase aves)
)

(defrule Pantera ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(pequenio_c no)
	(negro si)
	=>
	(assert (especie "Tu animal es una pantera"))
	(send [animal_actual] set_nombre Pantera)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Pato ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(cobarde no)
	=>
	(assert (especie "Tu animal es un pato"))
	(send [animal_actual] set_nombre Pato)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase aves)
)

(defrule Pavo ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(relleno si)
	=>
	(assert (especie "Tu animal es un pavo"))
	(send [animal_actual] set_nombre Pavo)
	(send [animal_actual] set_extremidades 2)
	(send [animal_actual] set_clase aves)
)

(defrule Perro ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(mejor_amigo si)
	=>
	(assert (especie "Tu animal es un perro"))
	(send [animal_actual] set_nombre Perro)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Pez_Espada ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion oviparo)
	(movimiento nadar)
	(enlatado no)
	=>
	(assert (especie "Tu animal es un pez espada"))
	(send [animal_actual] set_nombre Pez Espada)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase actinopterygii)
)

(defrule Pinguino ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(verde no)
	=>
	(assert (especie "Tu animal es un pinguino"))
	(send [animal_actual] set_nombre Pinguino)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase aves)
)

(defrule Pirania ""
	(clasificacion invertebrado)
	(alimentacion carnivoro)
	(reproduccion oviparo)
	(movimiento nadar)
	(agresivo si)
	=>
	(assert (especie "Tu animal es una pirania"))
	(send [animal_actual] set_nombre Pirania)
	(send [animal_actual] set_extremidades 3)
	(send [animal_actual] set_clase actinopterygii)
)

(defrule Polilla ""
	(clasificacion invertebrado)
	(alimentacion herbivoro)
	(reproduccion oviparo)
	(movimiento volar)
	(mueble si)
	=>
	(assert (especie "Tu animal es una polilla"))
	(send [animal_actual] set_nombre Polilla)
	(send [animal_actual] set_extremidades 8)
	(send [animal_actual] set_clase insecta)
)

(defrule Pulpo ""
	(clasificacion invertebrado)
	(alimentacion carnivoro)
	(reproduccion oviparo)
	(movimiento nadar)
	(comido_gallega si)
	=>
	(assert (especie "Tu animal es un pulpo"))
	(send [animal_actual] set_nombre Pulpo)
	(send [animal_actual] set_extremidades 8)
	(send [animal_actual] set_clase cephalopoda)
)

(defrule Rana ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(verde si)
	=>
	(assert (especie "Tu animal es una rana"))
	(send [animal_actual] set_nombre Rana)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase amphibia)
)

(defrule Rata ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(domestico no)
	(simio no)
	(dos_patas si)
	(cazar_pescado no)
	=>
	(assert (especie "Tu animal es una rata"))
	(send [animal_actual] set_nombre Rata)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Rinoceronte ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga no)
	(granja no)
	(cuernos si)
	(gris si)
	=>
	(assert (especie "Tu animal es un rinoceronte"))
	(send [animal_actual] set_nombre Rinoceronte)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Salamandra ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion oviparo)
	(movimiento andar)
	=>
	(assert (especie "Tu animal es una salamandra"))
	(send [animal_actual] set_nombre Salamandra)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase amphibia)
)

(defrule Saltamontes ""
	(clasificacion invertebrado)
	(alimentacion herbivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(saltarin si)
	=>
	(assert (especie "Tu animal es un saltamontes"))
	(send [animal_actual] set_nombre Saltamontes)
	(send [animal_actual] set_extremidades 8)
	(send [animal_actual] set_clase insecta)
)

(defrule Sepia ""
	(clasificacion invertebrado)
	(alimentacion carnivoro)
	(reproduccion oviparo)
	(movimiento nadar)
	(comido_gallega no)
	=>
	(assert (especie "Tu animal es una sepia"))
	(send [animal_actual] set_nombre Sepia)
	(send [animal_actual] set_extremidades 0)
	(send [animal_actual] set_clase cephalopoda)
)

(defrule Serpiente ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion oviparo)
	(movimiento reptar)
	(estrangular si)
	=>
	(assert (especie "Tu animal es una serpiente"))
	(send [animal_actual] set_nombre Serpiente)
	(send [animal_actual] set_extremidades 0)
	(send [animal_actual] set_clase sauropsida)
)

(defrule Suricato ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(pequenio_c si)
	(felino no)
	(madriguera si)
	(pinchos no)
	(garras no)
	=>
	(assert (especie "Tu animal es un suricato"))
	(send [animal_actual] set_nombre Suricato)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Tiburon ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion ovoviviparo)
	(movimiento nadar)
	=>
	(assert (especie "Tu animal es un tiburon"))
	(send [animal_actual] set_nombre Tiburon)
	(send [animal_actual] set_extremidades 4)
	(send [animal_actual] set_clase chondrichthyes)
)

(defrule Tigre ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(pequenio_c no)
	(negro no)
	(rayas si)
	=>
	(assert (especie "Tu animal es un tigre."))
	(send [animal_actual] set_nombre Tigre)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Topo ""
	(clasificacion vertebrado)
	(alimentacion carnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(pequenio_c si)
	(felino no)
	(madriguera si)
	(pinchos no)
	(garras si)
	=>
	(assert (especie "Tu animal es un topo."))
	(send [animal_actual] set_nombre Topo)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Toro ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga si)
	(granja si)
	(cuernos si)
	=>
	(assert (especie "Tu animal es un toro."))
	(send [animal_actual] set_nombre Toro)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Tortuga ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion oviparo)
	(movimiento andar)
	(lento si)
	=>
	(assert (especie "Tu animal es una tortuga."))
	(send [animal_actual] set_nombre Tortuga)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase sauropsida)
)

(defrule Vaca ""
	(clasificacion vertebrado)
	(alimentacion herbivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(carga no)
	(granja si)
	(cuernos si)
	(blanquinegra si)
	=>
	(assert (especie "Tu animal es una vaca."))
	(send [animal_actual] set_nombre Vaca)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)

(defrule Zorro ""
	(clasificacion vertebrado)
	(alimentacion omnivoro)
	(reproduccion viviparo)
	(movimiento andar)
	(domestico no)
	(simio no)
	(dos_patas no)
	(fama_ladron si)
	=>
	(assert (especie "Tu animal es un zorro."))
	(send [animal_actual] set_nombre Zorro)
	(send [animal_actual] set_extremidades 5)
	(send [animal_actual] set_clase mammalia)
)