# Compilador con PLY

Proyecto final para la materia de diseño de compiladores. 
Utiliza la librería de PLY para hacer uso de las librerías de Lex y Yacc dentro de Python.
- NOTA: Se utilizó la metodología de Git Flow para llevar el control de versiones. 

## Antes de correr el proyecto

Instalar PLY desde pip o pip3 si es que se tiene Python3

```bash
pip install ply
```



## Usage

Tenemos 3 archivos importantes: compilator.py, input.txt y output.txt. Compilator va a leer todo lo que contenga el input.txt y lo va a transformar a un código intermedio dentro de output.txt. Si el archivo de output no existe, el programa lo crea.

. Correr con python2:

```
python compilator.py
```

. Correr con python3:

```
python3 compilator.py
```
