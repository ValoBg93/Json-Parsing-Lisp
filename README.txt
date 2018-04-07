% Progetto Prolog e Common Lisp 2018-01-15 (E1P)
% - json_parsing.lisp -
%

La libreria utilizza la funzione json-parse per analizzare l'oggetto in input da parsare e fa 
un primo controllo sul tipo dell'oggetto passato sfruttando la funzione check-json. 
Per completare l'analisi dell'oggetto la funzione json-parse utilizza un set di funzioni che 
iterano ricorsivamente su una sottosequenza della stringa dell'oggetto.

La funzione json-get va a cercare l'elemento nella lista con la funzione find-element, 
una volta trovato invoca la funzione find-pair per trovare ed estrarre l'oggetto richiesto 
basandosi sul matching della key.

La funzione json-write scrive l'oggetto json-obj in filename mediante l'utilizzo di 
due funzioni che analizzano l'oggetto passato in input e costruiscono la stringa in sintassi JSON. 
Le funzioni che permettono questa costruzione sono:
- js-wr che analizza se l'oggetto passato è array o object e invoca una funzione tra csa/cso
- csa/cso che costruiscono rispettivamente array/object da scrivere nel filename
Durante la costruzione degli oggetti nella cso viene utilizzata verifica-cdr, 
una funzione di supporto che analizza il value della pair analizzata.

La json-load verifica che il filename esista e, prima di effettuare il parsing dell'oggetto, 
legge tutto il contenuto del filename utilizzando la funzione readfile.