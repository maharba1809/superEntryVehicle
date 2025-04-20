#!/bin/bash
clear
echo "Bienvenidos a GnuTrivial."
sleep 2
echo "Soy el típico juego de preguntas y respuestas."
sleep 2
echo "Si aciertas todas las preguntas, te concedere el titulo de magister del universo."
echo "¿Como se llamaba el ultimo emperador Romano de occidente, claudio, teodosio
o romulo."
read respuesta1
if test $respuesta1 = romulo
then
echo "Respuesta correcta."
else
echo "Lo siento, la respuesta correcta es: romulo."
fi
sleep 2
echo "Pasemos a la siguiente pregunta. ¿Qué célebre filosofo Griego tuvó por discipulo
a Alejandro Magno, platón, aristoteles o zenon?"
read respuesta2
if test $respuesta2 = aristoteles
then
echo "respuesta correcta."
else
echo "Lo siento, la respuesta correcta es: Aristoteles."
fi
if test $respuesta1 = romulo
test $respuesta2 = aristoteles
then
echo "Eres un pequeño magister del universo."
fi

