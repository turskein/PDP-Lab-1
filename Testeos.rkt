#lang racket

(require "main_23537297_PachecoLaos.rkt")
(require "TDAfecha_23537297_PachecoLaos.rkt")
(require "TDAparadigmadocs_23537297_PachecoLaos.rkt")
(require "EncryptFn_DencryptFn_23537297_PachecoLaos.rkt")
(require "TDAaccess_23537297_PachecoLaos.rkt")

;-----Aplicación de testeos presentes en el documento del laboratorio
(define emptyGDocs (paradigmadocs "gDocs" (date 25 10 2021) EncryptFn DencryptFn))
(define gDocs1
(register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 25 10 2021) "user2" "pass2") (date 25 10 2021) "user3" "pass3"))
(define gDocsn ((login gDocs1 "user1" "pass1" create) (date 30 08 2021) "doc0" "contenido doc0"))
(define gDocs2 ((login gDocsn "user1" "pass1" create) (date 30 08 2021) "doc1" "contenido doc1"))
(define gDocs3 ((login gDocs2 "user2" "pass2" create) (date 30 08 2021) "doc2" "contenido doc2"))
(define gDocs6 ((login gDocs3 "user1" "pass1" share) 0 (access "user1" #\r) (access "user2" #\w)(access "user3" #\c)))
(define gDocs7 ((login gDocs6 "user3" "pass3" share) 0 (access "user1" #\c)))
(define gDocs8 ((login gDocs7 "user1" "pass1" add) 0 (date 30 11 2021) " mas contenido en doc0"))
(define gDocs9 ((login gDocs8 "user3" "pass3" add) 0 (date 30 11 2021) " mas contenido en doc3"))
(define gDocs10 ((login gDocs9 "user1" "pass1" restoreVersion) 0 0))
(define gDocs11 (login gDocs10 "user2" "pass2" revokeAllAccesses))
(define gDocs12 ((login gDocs11 "user1" "pass1" delete) 1 (date 30 11 2021) 5))
(define gDocs13 ((login gDocs12 "user3" "pass3" searchAndReplace) 0 (date 30 11 2021) "contenido" "content"))
(define gDocs14 ((login gDocs13 "user2" "pass2" applyStyles) 0 (date 30 11 2021) "content" #\b #\i))
(define gDocs15 ((login gDocs14 "user1" "pass1" comment) 0 (date 30 12 2021) "contenido" "Este es un comentario"))
(define gDocs16 ((login gDocs15 "user1" "pass1" add) 0 (date 30 11 2021) " este es "))
(define gDocs17 ((login gDocs16 "user1" "pass1" add) 0 (date 30 11 2021) "un nuevo "))
(define gDocs18 ((login gDocs17 "user1" "pass1" add) 0 (date 30 11 2021) "text en doc0"))
(define gDocs19 ((login gDocs18 "user1" "pass1" ctrlZ) 0 5))
(define gDocs20 ((login gDocs19 "user1" "pass1" ctrlY) 0 2))
(define gDocs21 ((login gDocs20 "user1" "pass1" add) 0 (date 30 11 2021) " este es "))

;((login gDocs11 "user1" "pass1" search) "contenido")
;(display (login gDocs14 "user1" "pass1" paradigmadocs->string))
;(display "\n\n")
;(display (paradigmadocs->string gDocs14))


;---EJEMPLIFICACIONES---
;paradigmadocs
(define pgDocs (paradigmadocs "pgDocs" (date 28 10 2021) EncryptFn DencryptFn))
(define gDocs45 (paradigmadocs "gDocs" (date 28 10 2021) EncryptFn DencryptFn))
(define googleDocsdoniapepa (paradigmadocs "hackerman" (date 28 10 2021) EncryptFn DencryptFn))

;Register
(define pgDocs0 (register (register pgDocs (date 28 10 2021) "sebastian" "123456") (date 26 10 2021) "rodrigo" "aaaaaa"))
(define pgDocs0-1 (register (register pgDocs0 (date 28 10 2021) "rodrigo" "asddvasdv") (date 26 10 2021) "maria" "bbbbbb"))
(define pgDocs0-2 (register pgDocs0-1 (date 28 10 2021) "maria" "asdsdva"))
;(display (paradigmadocs->string pgDocs0-2))

;Create
(define pgDocs1 ((login pgDocs0-2 "sebastian" "123456" create) (date 28 10 2021) "documento de sebastian" "Tengo que hacer mi tarea lo antes posible"));generación de documentos
(define pgDocs1-1 ((login pgDocs1 "maria" "bbbbbb" create) (date 28 10 2021) "Ciencia de la computación" "aquí irán mis apuntes de ciencias de la computación: "))
(define pgDocs1-2 ((login pgDocs1-1 "maria" "bbbbbb" create)(date 28 10 2021) "Lista de deseos" "1ro.- una bicicleta"))
;(display (paradigmadocs->string pgDocs1-2))

;Share
(define pgDocs2 ((login pgDocs1-2 "sebastian" "123456" share) 0 (access "maria" #\w) (access "rodrigo" #\r)));sebastian da permisos sobre doc de id 0 a maria y rodrigo
(define pgDocs2-1 ((login pgDocs2 "maria" "bbbbbb" share) 0 (access "rodrigo" #\w) (access "sebastian" #\r)));maria trata de compartir un documento que no es suyo
(define pgDocs2-2 ((login pgDocs2 "maria" "bbbbbb" share) 1 (access "rodrigo" #\w) (access "sebastian" #\r))); maria comparte su documento con rodrigo y sebastian
;(display (paradigmadocs->string pgDocs2-2))

;Add
(define pgDocs3 ((login pgDocs2-2 "maria" "bbbbbb" add) 1 (date 28 10 2021) "Para mañana se necesitarán nuevos plumones de color azul")); nuevo contenido sobre doc 0
(define pgDocs3-1 ((login pgDocs3 "sebastian" "123456" add) 1 (date 28 10 2021) "Ciencias de la computación es muy aburridooooo")); intento de agregar contenido en doc sin permiso
(define pgDocs3-2 ((login pgDocs3 "rodrigo" "aaaaaa" add) 1 (date 28 10 2021) " para obtener ATP las células procariontes... "));rodrigo agrega contenido al doc 1
;(display (paradigmadocs->string pgDocs3-2))

;RestoreVersion
(define pgDocs4 ((login pgDocs3-2 "maria" "bbbbbb" restoreVersion) 1 1)); se restaura la versión de id 1 del doc 1
(define pgDocs4-1 ((login pgDocs4 "maria" "bbbbbb" restoreVersion) 1 2)); se restaura la versión de id 2 del doc 1
(define pgDocs4-2 ((login pgDocs4-1 "rodrigo" "aaaaaa" restoreVersion) 1 2)); rodrigo trata de restaurar una versión, pero no puede porque no es un el owner
;(display (paradigmadocs->string pgDocs4-2))

;RevokeAllAccesses
(define pgDocs5 (login pgDocs4-2 "maria" "bbbbbb" revokeAllAccesses))
(define pgDocs5-1 (login pgDocs5 "maria" "bbbbbb" revokeAllAccesses))
(define pgDocs5-2 (login pgDocs5-1 "sebastian" "123456" revokeAllAccesses))
;(display (paradigmadocs->string pgDocs5-2))

;----agregando nuevos permisos----
(define pgDocsA ((login ((login pgDocs5-2 "sebastian" "123456" share) 0 (access "maria" #\w) (access "rodrigo" #\r) (access "sebastian" #\r))  "maria" "bbbbbb" share) 1 (access "rodrigo" #\w) (access "sebastian" #\r)))

;Delete
(define pgDocs6 ((login pgDocsA "maria" "bbbbbb" delete) 1 (date 28 10 2021) 20)) ;se eliminan 20 caracteres del doc 1 
(define pgDocs6-1 ((login pgDocs6 "maria" "bbbbbb" delete) 1 (date 28 10 2021) 200)) ;se eliminan todos los caracteres de un documento
(define pgDocs6-2 ((login pgDocs6-1 "rodrigo" "aaaaaa" delete) 0 (date 28 10 2021) 10)) ; rodrigo trata de modificar un documento en el que no tiene permisos
;(display (paradigmadocs->string pgDocs6-2))

;---agregando nuevo contenido---
(define pgDocsB ((login pgDocs6 "maria" "bbbbbb" restoreVersion) 1 4))

;SearchAndReplace
(define pgDocs7 ((login pgDocsB "maria" "bbbbbb" searchAndReplace) 1 (date 28 10 2021) "azul" "verde")); se reemplazan la palabra azul por verde
(define pgDocs7-1 ((login pgDocs7 "maria" "bbbbbb" searchAndReplace) 1 (date 28 10 2021) "plumones" "lapices")); se reemplazan la palabra plumones por lapices
(define pgDocs7-2 ((login pgDocs7-1 "sebastian" "123456" searchAndReplace) 1 (date 28 10 2021) "lapices" "popo"));sebastian no puede escribir en el documento 1
;(display (paradigmadocs->string pgDocs7-2))

;ApplyStyles
(define pgDocs8 ((login pgDocs7-2 "maria" "bbbbbb" applyStyles) 1 (date 30 11 2021) "procariontes" #\b #\i));se le aplica el estilo b e i a la palabra "procarionte" en el doc 1
(define pgDocs8-1 ((login pgDocs8 "maria" "bbbbbb" applyStyles) 0 (date 30 11 2021) "hacer" #\b));se le aplica el estilo b a la palabra hacer en el doc 0
(define pgDocs8-2 ((login pgDocs8-1 "sebastian" "123456" applyStyles) 0 (date 30 11 2021) "posible" #\k));se le trata de aplicar un estilo inexistente a la palabra "posible" del doc 0
;(display (paradigmadocs->string pgDocs8-2))

;Comment
(define pgDocs9 ((login pgDocs8-2 "maria" "bbbbbb" comment) 1 (date 30 12 2021) "procariontes" "preguntarle a la profesora qué es procarionte"));se comenta la palabra procariontes
(define pgDocs9-1((login pgDocs9 "rodrigo" "aaaaaa" comment) 1 (date 30 12 2021) "ATP" "energía de las células")); se comenta la palabra ATP
(define pgDocs9-2((login pgDocs9-1 "rodrigo" "aaaaaa" comment) 1 (date 30 12 2021) "sopaipilla" "energía de las células")); se comenta una palabra inexistente, por ende, no se genera el comentario
;(display (paradigmadocs->string pgDocs9-2))

;CtrlZ
(define pgDocs10 ((login pgDocs9-2 "maria" "bbbbbb" ctrlZ) 1 5))
(define pgDocs10-1 ((login pgDocs10 "maria" "bbbbbb" ctrlZ) 1 2))
(define pgDocs10-2 ((login pgDocs10-1 "maria" "bbbbbb" ctrlZ) 0 1))
;(display (paradigmadocs->string pgDocs10-2))

;CtrlY
(define pgDocs11 ((login pgDocs10-2 "rodrigo" "aaaaaa" ctrlY) 1 6))
(define pgDocs11-1 ((login pgDocs11 "maria" "bbbbbb" ctrlY) 0 10))
(define pgDocs11-2 ((login pgDocs11-1 "sebastian" "123456" ctrlY) 1 10))
;(display (paradigmadocs->string pgDocs11-2))

;Search
;((login pgDocs11-2 "sebastian" "123456" search) "lapices")
;((login pgDocs11-2 "sebastian" "123456" search) "a")
;((login pgDocs11-2 "sebastian" "123456" search) "zapatillas")

