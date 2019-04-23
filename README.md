# ixplorer Pro EN

Esta es la versión in inglés de ixplorer PRO. Es la **base par todo desarrollo
en ixplorer**. 


        --------------
       |   ixplorer   |    version publica en Github
        --------------
               
               | incluye cambios manualmente     github
     ====================================================
               |                                 ixplorer
        --------------
       |   ixplorer   |    version publica (para push en github)
        --------------
             ^   |
            =|=  v         Solo cambios manuales para arriba
        --------------
       |  ixplorer.en  |   se mantiene en sintonia con ES
        --------------
             ^   |
             |   v
        --------------
       |  ixplorer.es |   version central para desarollo
        --------------


## Proceso desarrollo
 - Desarrollamos principalmente en ixplorer.es (es lo que mas usuarios tiene).
 - mantenemos ixplorer.en en sintonia con ixplorer.es a travez de diffs 
   (todo requiere inspeccion visual para ver si esta bien).
 - El estilo de código va ser muy estricto para poder hacer los diffs con 
   facilidad. 
 - Toda funcionalidad PRO se marca con la palabra "overlay". Puede ser en
   funciones lista_abiertos_overlay() o en archivos conexion_overlay.R. Al ver
   overlay sabemos que **NO PUEDE ENTRAR EN IXPLORER (publico)**.
 - Escribimos todo codigo en ingles (y su documentacion roxygen.
 - La version es contiene wrappers a las funciones en ingles, donde solamente
   exportamos las funciones en espanol (los que tienen el formato:

          lista_tiquetes_cerrados <- function() {list_closed_tickets()}


## Proceso actualizaciones
 - Cambios en github/ixplorer: Se jalan al repositorio (bifurcacion) personal.
   Se revisan y se someten con un pull request a ixplorer/ixplorer.
 - Cambios en ixplorer/ixplorer: Se jalan al master de un repositorio
   (bifurcacion) personal y se envían (git fetch github) a la rama master de
   github.
 - Cambios en ixplorer.es: se incluyen en ixplorer.en con un diff (manual / visual)
 - Cambios en ixplorer.en que han de ser públicos (tras validación en equipo se
   incluyen con un diff (manual visual) a un repositorio (bifurcación)
   personal, de allí van a ixplorer/ixplorer tras una solicitud de fusión, y
   después se integran en el master del repositorio personal (bifurcación) de
   la persona que lo va a enviar a github (git push github)
