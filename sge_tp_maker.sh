#!/bin/bash

###############################################################################
######				NOTA				 	#######
###############################################################################
######									#######
######		Todas las opciones de PBS se marcan de la forma:	#######
######									#######
######		#$ -opc argumentos					#######
######									#######
######		Cabe destacar que empiezan con '#$' (gato-pesos)	#######
######		con lo cual se denota que es una opcion de PBS		#######
######		y no un comentario.					#######
######		Si una linea empieza con '##$' (gato-gato-pesos)	#######
######		es un comentario y borrando el primer caracter		#######
######		se obtendria una opcion de PBS	p.e.:			#######
######									#######
######		##$ -N miproceso					#######
######		#$ -N miproceso						#######
######									#######
###############################################################################
###############################################################################

#Algunas de las variables que se utilizan dentro de este script y las cuales no son necesario se han modificadas son:
# $HOME		Nos da el directorio HOME del usuario
# $USER		Nos da el nombre del usuario
# $JOB_ID	Nos da el numero de identificador del proceso
# $JOB_NAME	Nos da el nombre del proceso 
# $HOSTNAME	Nos da el nombre de la maquina donde se esta ejecutando

#-----------------------------------------------------------------------------------

#Nombre de la tarea, solo se permiten letras y numeros, los nombres siempre deben de empezar con letras,en este caso debera de sustituir
#el texto "MI_NOMBRE" por su nombre del proyecto en el cual esta trabajando. Ej. SOL_MTZ_INVERSA
#$ -N Jet_time

#-----------------------------------------------------------------------------------
# Tipo de Shell que se usara, no es necesario modifique este campo.
#$ -S /bin/bash

# Fecha y Hora para mandar a ejecutar, formato: MMDDhhmm Ej.(24 de diciembre a las 12:00).  No es necesario modifique este campo
##$ -a 12241200 

#-----------------------------------------------------------------------------------
# Los errores generados por el proceso seran enviados a un archivo, siendo el nombre del archivo "$proc_MI_NOMBRE.e$JOB_ID" en donde
# $proc_MI_NOMBRE es el valor que definio usted el la opcion del nombre de la tarea.
# $JOB_ID es el identificador de la tarea y este lo asigna de forma automatica el sistema
# La siguiente linea no es necesaria que se modifique, ya que se genera este archivo de forma automatica en su $HOME
# Si se ejecuta en  compute-0-0 pone el stderr en /tmp/stderr_0
# Si se ejecuta en compute-0-2 pone el stderr en /tmp/stderr_2
##$ -e compute-0-0:$HOME/stderr_0,compute-0-2:$HOME/stderr_2,compute-0-4:$HOME/stderr_4

#-----------------------------------------------------------------------------------
# La información adicional durante la ejecucion de su proceso sera enviada enviada a un archivo, el cual tiene por nombre  #"$proc_MI_NOMBRE.o$JOB_ID" en donde
# $proc_MI_NOMBRE es el valor que definio usted el la opcion del nombre de la tarea.
# $JOB_ID es el identificador de la tarea y este lo asigna de forma automatica el sistema
# La siguiente linea no es necesaria que se modifique, ya que se genera este archivo de forma automatica en su $HOME
# Si se ejecuta en  compute-0-0 pone el stdout en /tmp/stdout_0
# Si se ejecuta en compute-0-2 pone el stdout en /tmp/stdout_2
##$ -o compute-0-0:$HOME/stdout_0,compute-0-2:$HOME/stdout_2,compute-0-4:$HOME/stdout_4

#-----------------------------------------------------------------------------------
# El nombre del correo electronico al cual seran enviadas las notificaciones de ejecucion de su proceso
# CAMBIAR AL CORREO DESEADO, O AGREGAR UN '#' A LA OPCION -m
#$ -M  msalinasv@ii.unam.mx

#-----------------------------------------------------------------------------------
# En caso de b|e|a|s|n  => (inicio/beginning) | (fin/end) | (abortado/aborted) | (suspension/supended)
# se les envia un correo a los de la opcion -M
# si no se especifico -M por default se envia al dueño del proceso, en la siguiente linea se enviaran los 4 tipos de notificaciones
#$ -m bes

#-----------------------------------------------------------------------------------
# Se exportan las variables de entorno para usar en otras tareas enviadas a SGE
##$ -v PVM_ROOT,OTRAVAR=VAR


#-----------------------------------------------------------------------------------
# La tarea se empieza a ejecutar donde se invoco, y a su vez la salida standar y 
# el error se coloca en dicha carpeta, a menos que se especifique otra cosa, no es necesario modifique esta linea
#$ -cwd

#-----------------------------------------------------------------------------------
# Priorida la cual va desde -1023 a 1024, por default se asigna 0 y los usarios solo pueden decrementarla
# Solo el administrador puede incrementar la prioridad, la siguiente linea no es necesario se modifique
#$ -p 0


#-----------------------------------------------------------------------------------
# Defeniendo un nodo una una seriede nodos para procesamiento, descomentaria la linea que considere necesaria
##$-q nodo1.q nodo2.q
##$-q maestro.q nodo3.q


###########################################################################
#######  En esta seccion debra de colocar sus comandos a ejecutar. ########
###########################################################################



./crack > out1 





############################################################################
####				FIN DE LA PLANTILLA		    ########
############################################################################

